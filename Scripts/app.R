# ==============================================================================
# VIRGINIA TRAFFIC SAFETY EXECUTIVE DASHBOARD (ACCESSIBLE & WCAG-ENHANCED)
# Office of the Governor | Highway Safety & Infrastructure Analytics
# Authors: Matthew Goldenberg & Maria Wood (STAT 663)
# ==============================================================================
# Required packages:
# install.packages(c("shiny", "bslib", "tidyverse", "lubridate", "plotly",
#                    "leaflet", "sf", "tigris", "DT", "scales", "bsicons", "writexl"))

library(shiny)
library(bslib)
library(tidyverse)
library(lubridate)
library(plotly)
library(leaflet)
library(sf)
library(tigris)
library(DT)
library(scales)
library(bsicons)
library(writexl)

options(tigris_use_cache = TRUE)

# ==============================================================================
# 1. REPRODUCIBLE DATA LOADING & PREPROCESSING
# ==============================================================================
data_paths <- c(
  "../Data/Virginia_Accidents_With_Pop.csv",
  "Data/Virginia_Accidents_With_Pop.csv",
  "Virginia_Accidents_With_Pop.csv",
  "../Data/SmallerExtractVAPop.csv",
  "Data/SmallerExtractVAPop.csv",
  "SmallerExtractVAPop.csv"
)

existing_path <- data_paths[file.exists(data_paths)][1]

if (is.na(existing_path)) {
  stop(
    "Data file 'Virginia_Accidents_With_Pop.csv' not found. ",
    "Ensure it is located in '../Data/' or 'Data/' relative to this script."
  )
}

acc_raw <- read.csv(existing_path, na.strings = c("", "NA"), stringsAsFactors = FALSE)

# Group the 73 raw NOAA weather strings into 5 intuitive Weather Categories
classify_weather_simple <- function(w) {
  w_low <- str_to_lower(coalesce(w, ""))
  case_when(
    w_low == "" ~ NA_character_,
    str_detect(w_low, "t-storm|thunder|heavy rain|squall|tornado|hail") ~ "Thunderstorms / Heavy Rain",
    str_detect(w_low, "snow|sleet|ice|freezing|wintry|blizzard|fog|mist|haze") ~ "Snow / Ice / Fog",
    str_detect(w_low, "rain|drizzle|shower|precip") ~ "Light-Moderate Rain / Drizzle",
    str_detect(w_low, "cloud|overcast") ~ "Cloudy / Overcast",
    str_detect(w_low, "clear|fair") ~ "Clear / Fair",
    TRUE ~ "Other Weather"
  )
}

acc <- acc_raw %>%
  mutate(
    Start_Time        = ymd_hms(Start_Time, tz = "UTC", quiet = TRUE),
    End_Time          = ymd_hms(End_Time, tz = "UTC", quiet = TRUE),
    Accident_Date     = as.Date(Start_Time),
    Year              = year(Start_Time),
    month_year        = floor_date(Start_Time, unit = "month"),
    severe            = Severity >= 3,
    GEOID             = str_pad(as.character(GEOID), width = 5, side = "left", pad = "0"),
    Weather_Condition = str_squish(Weather_Condition),
    Weather_Category  = classify_weather_simple(Weather_Condition),
    Street            = str_squish(Street),
    City              = str_squish(coalesce(City, "")),
    census_name       = str_squish(census_name),
    is_city           = str_detect(census_name, regex("\\bcity$", ignore_case = TRUE))
  ) %>%
  filter(!is.na(census_name), !is.na(Severity), !is.na(Year))

min_acc_date <- min(acc$Accident_Date, na.rm = TRUE)
max_acc_date <- max(acc$Accident_Date, na.rm = TRUE)

# Pre-compute static county/city population & statewide accident summary
county_pop_ranks <- acc %>%
  group_by(GEOID, census_name, is_city) %>%
  summarise(
    population       = first(na.omit(population)),
    total_accidents  = n(),
    severe_accidents = sum(severe, na.rm = TRUE),
    severe_rate      = ifelse(total_accidents > 0, severe_accidents / total_accidents, 0),
    severe_ratio     = (severe_accidents / population) * 100000,
    .groups = "drop"
  ) %>%
  arrange(desc(population)) %>%
  mutate(pop_rank = row_number())

statewide_total_pop <- sum(county_pop_ranks$population, na.rm = TRUE)
all_years           <- sort(unique(acc$Year))
all_counties        <- county_pop_ranks$census_name
only_counties       <- county_pop_ranks %>% filter(!is_city) %>% pull(census_name)

# Load all 133 Virginia county/city boundaries
va_boundaries_raw <- tryCatch({
  counties(state = "VA", cb = TRUE, year = 2022, progress_bar = FALSE) %>%
    st_transform(4326) %>%
    left_join(county_pop_ranks %>% select(GEOID, census_name, is_city, population), by = "GEOID") %>%
    mutate(display_name = coalesce(census_name, NAMELSAD, NAME))
}, error = function(e) {
  NULL
})

# ==============================================================================
# SIMPLE, STANDARD STATISTICAL HELPER FUNCTIONS (CONFIDENCE INTERVALS)
# ==============================================================================
get_adjusted_alpha <- function(m, method = c("bonferroni", "sidak", "none"), alpha = 0.05) {
  method <- match.arg(method)
  m <- max(1, as.integer(m))
  switch(
    method,
    "bonferroni" = alpha / m,
    "sidak"      = 1 - (1 - alpha)^(1 / m),
    "none"       = alpha
  )
}

calc_binom_ci <- function(x, n, bound = c("lower", "upper"), conf_level = 0.95) {
  bound <- match.arg(bound)
  idx <- ifelse(bound == "lower", 1, 2)
  conf_level <- pmin(pmax(conf_level, 0.50), 0.999999)
  map2_dbl(x, n, function(k, total) {
    if (is.na(k) || is.na(total) || total <= 0) return(NA_real_)
    binom.test(k, total, conf.level = conf_level)$conf.int[idx]
  })
}

calc_poisson_ci <- function(k_vec, bound = c("lower", "upper"), conf_level = 0.95) {
  bound <- match.arg(bound)
  idx <- ifelse(bound == "lower", 1, 2)
  conf_level <- pmin(pmax(conf_level, 0.50), 0.999999)
  map_dbl(k_vec, function(k) {
    if (is.na(k) || k < 0) return(NA_real_)
    poisson.test(as.integer(round(k)), conf.level = conf_level)$conf.int[idx]
  })
}

compute_simple_ci <- function(df, group_col, value_col, pop_col = NULL,
                              mult_method = "bonferroni", upper_cap = Inf,
                              min_months = 10) {
  grouped <- df %>%
    filter(!is.na(.data[[value_col]]) & !is.nan(.data[[value_col]])) %>%
    group_by(Group = .data[[group_col]]) %>%
    summarise(
      pop      = if (!is.null(pop_col)) first(.data[[pop_col]]) else NA_real_,
      N_Units  = n(),
      Mean_Est = mean(.data[[value_col]], na.rm = TRUE),
      variance = var(.data[[value_col]], na.rm = TRUE),
      stdev    = sqrt(pmax(0, variance)),
      SE       = stdev / sqrt(N_Units),
      .groups  = "drop"
    ) %>%
    filter(Mean_Est > 0)
  
  filtered <- grouped %>% filter(N_Units >= min_months)
  if (nrow(filtered) == 0) filtered <- grouped %>% filter(N_Units >= 5)
  if (nrow(filtered) == 0) filtered <- grouped %>% filter(N_Units >= 3)
  
  m_groups  <- max(1, nrow(filtered))
  alpha_adj <- get_adjusted_alpha(m_groups, mult_method, alpha = 0.05)
  
  filtered %>%
    mutate(
      m_comparisons = m_groups,
      conf_level    = 1 - alpha_adj,
      t_val         = qt(1 - (alpha_adj / 2), df = pmax(1, N_Units - 1)),
      CI_Lower      = pmax(0, Mean_Est - (t_val * SE)),
      CI_Upper      = pmin(upper_cap, Mean_Est + (t_val * SE))
    )
}

# Clean, icon-only accessible summary box
render_a11y_box <- function(text_content) {
  tags$div(
    class = "a11y-summary-box mt-2 mb-0 py-2 px-3",
    role = "region",
    `aria-live` = "polite",
    `aria-label` = "Accessible Chart Summary",
    tags$span(
      class = "text-primary me-2",
      title = "Accessible Summary",
      bs_icon("universal-access", size = "1.1rem")
    ),
    tags$span(class = "fw-medium text-dark", text_content)
  )
}

exec_navy    <- "#0F2042"
exec_crimson <- "#9E1B32"
exec_teal    <- "#0072B2"
exec_amber   <- "#D55E00"
exec_slate   <- "#2D3748"

# ==============================================================================
# 2. USER INTERFACE
# ==============================================================================
ui <- page_navbar(
  title = tags$span(
    class = "fw-bold text-dark",
    bs_icon("shield-shaded", size = "1.3rem"),
    " Commonwealth of Virginia | Traffic Severity Analytics"
  ),
  theme = bs_theme(
    version = 5,
    primary = exec_navy,
    secondary = exec_slate,
    danger = exec_crimson,
    base_font = font_google("Inter"),
    heading_font = font_google("Merriweather")
  ),
  fillable = FALSE,
  header = tagList(
    tags$head(
      tags$style(HTML("
        /* High-contrast pill buttons for all top navigation tabs on a light navbar */
        .navbar {
          border-bottom: 2px solid #CBD5E0 !important;
          padding-top: 0.5rem !important;
          padding-bottom: 0.5rem !important;
        }
        .navbar-nav {
          gap: 0.6rem;
          padding: 0.2rem 0;
        }
        .navbar-nav .nav-link {
          background-color: #EDF2F7 !important;
          color: #0F2042 !important;
          border: 2px solid #0F2042 !important;
          border-radius: 8px !important;
          padding: 0.45rem 1.05rem !important;
          font-weight: 700 !important;
          font-size: 0.95rem !important;
          transition: all 0.15s ease-in-out;
        }
        .navbar-nav .nav-link:hover {
          background-color: #CBD5E0 !important;
          color: #0F2042 !important;
        }
        .navbar-nav .nav-link.active {
          background-color: #0F2042 !important;
          color: #FFFFFF !important;
          border: 2px solid #0F2042 !important;
          font-weight: 800 !important;
          box-shadow: 0 3px 6px rgba(15, 32, 66, 0.28);
        }
        /* Keep the Full-Screen Modal Expand button permanently visible on every card */
        .bslib-full-screen-enter {
          opacity: 1 !important;
          visibility: visible !important;
          background-color: #0F2042 !important;
          color: #FFFFFF !important;
          border: 1px solid #FFFFFF !important;
          border-radius: 6px !important;
          padding: 0.25rem 0.5rem !important;
          box-shadow: 0 2px 6px rgba(0, 0, 0, 0.3) !important;
        }
        .bslib-full-screen-enter:hover {
          background-color: #9E1B32 !important;
          color: #FFFFFF !important;
        }
        /* Replace light-gray captions with high-contrast dark text */
        .card-footer, .text-muted, .form-text {
          color: #1A202C !important;
          font-size: 0.93rem !important;
          font-weight: 500 !important;
        }
        .card-footer {
          background-color: #EDF2F7 !important;
          border-top: 1px solid #CBD5E0 !important;
        }
        .a11y-summary-box {
          background-color: #F8FAFC;
          border-left: 4px solid #0F2042;
          border-top: 1px solid #CBD5E0;
          border-right: 1px solid #CBD5E0;
          border-bottom: 1px solid #CBD5E0;
          border-radius: 4px;
          font-size: 0.94rem;
          color: #1A202C;
        }
      "))
    )
  ),
  
  # ============================================================================
  # PAGE 1: STATEWIDE & TIERED COUNTY EXECUTIVE SUMMARY
  # ============================================================================
  nav_panel(
    title = "Tab 1: County & Weather Rankings",
    icon = bs_icon("bar-chart-line-fill"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Ranking Filters",
        width = 325,
        open  = "always",
        radioButtons(
          "p1_jurisdiction",
          "1. Jurisdiction Type:",
          choices = c(
            "Counties Only (Excludes Cities)"    = "counties_only",
            "All Localities (Counties & Cities)" = "all_localities"
          ),
          selected = "counties_only"
        ),
        selectInput(
          "p1_pop_tier",
          "2. Population Tier:",
          choices = c(
            "All Qualifying Localities" = "all",
            "Top 5 in Population"       = "top5",
            "Top 10 in Population"      = "top10",
            "Middle 5 in Population"    = "mid5",
            "Bottom 5 in Population"    = "bot5"
          ),
          selected = "all"
        ),
        selectInput(
          "p1_year",
          "3. Analysis Year:",
          choices = c("All Years (2016–2023)" = "All", as.character(all_years)),
          selected = "All"
        ),
        radioButtons(
          "p1_metric",
          "4. Top-Left Bar Chart Metric:",
          choices = c(
            "Severe Accident Rate (% of Crashes)"        = "severe_rate",
            "Severe Crashes per 100k Residents (Ratio)"  = "severe_ratio",
            "Total Recorded Crashes (Raw Count)"         = "total_accidents"
          ),
          selected = "severe_rate"
        ),
        hr(),
        tags$div(
          class = "p-2 rounded border bg-light text-dark small",
          tags$strong(bs_icon("info-circle-fill"), " Quick Guide:"),
          tags$ul(
            class = "mb-2 ps-3 mt-1",
            tags$li(tags$strong("Severe Crash:"), " Severity level 3 or 4 (on 1–4 impact scale)."),
            tags$li(tags$strong("Enlarge Any Chart:"), " Click the dark expand button in the bottom-right corner of any chart card to view it in a large pop-up window.")
          ),
          downloadButton(
            "download_top5_excel",
            "Download County Ranking Summary (.xlsx)",
            class = "btn-sm btn-outline-primary w-100 fw-bold",
            `aria-label` = "Download Excel workbook summarizing Virginia county traffic accident rates, averages, and per-capita ratios"
          )
        )
      ),
      
      # Accessible summary for KPI banner
      uiOutput("p1_kpi_sr_summary"),
      
      # KPI Value Boxes with built-in Virginia Statewide comparison benchmarks
      layout_column_wrap(
        width = 1/3,
        value_box(
          title = "Total Recorded Crashes (Selected Group)",
          value = textOutput("p1_kpi_total"),
          p(class = "small mb-0 fw-semibold", textOutput("p1_kpi_total_sub")),
          showcase = bs_icon("car-front-fill"),
          theme = "primary"
        ),
        value_box(
          title = "Severe Accident Rate (% of Crashes)",
          value = textOutput("p1_kpi_rate"),
          p(class = "small mb-0 fw-semibold", textOutput("p1_kpi_rate_sub")),
          showcase = bs_icon("exclamation-triangle-fill"),
          theme = "danger"
        ),
        value_box(
          title = "Severe Crashes per 100k Residents",
          value = textOutput("p1_kpi_ratio"),
          p(class = "small mb-0 fw-semibold", textOutput("p1_kpi_ratio_sub")),
          showcase = bs_icon("people-fill"),
          theme = "secondary"
        )
      ),
      
      layout_column_wrap(
        width = 1/2,
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white fw-bold",
            textOutput("p1_county_title")
          ),
          card_body(
            plotlyOutput("p1_county_plot", height = "390px"),
            uiOutput("p1_county_sr_summary")
          ),
          card_footer(
            "Direct values are shown next to each bar. Click the expand icon in the bottom-right corner of this card to open a larger view."
          )
        ),
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white fw-bold",
            textOutput("p1_weather_title")
          ),
          card_body(
            plotlyOutput("p1_weather_plot", height = "390px"),
            uiOutput("p1_weather_sr_summary")
          ),
          card_footer(
            "Shows the 10 weather conditions with the highest severe accident rate (among weather types with at least 20 crashes in the selected group)."
          )
        )
      ),
      
      # Filter for the Confidence Interval chart placed directly inside the card header
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-dark text-white d-flex flex-wrap justify-content-between align-items-center gap-2 py-2",
          tags$span(class = "fw-bold", textOutput("p1_ci_header")),
          tags$div(
            class = "d-flex align-items-center gap-2",
            tags$label(
              `for` = "p1_ci_type",
              class = "fw-bold text-warning mb-0 small",
              "Select Bottom Chart Metric:"
            ),
            selectInput(
              "p1_ci_type",
              label = NULL,
              choices = c(
                "Monthly Severe Accident Rate (% of Crashes)"       = "monthly_rate_ci",
                "Monthly Severe Crashes per 100k Residents (Ratio)" = "monthly_ratio_ci",
                "Weather Severe Accident Rate (% by Weather Type)"  = "weather_binom_ci"
              ),
              selected = "monthly_rate_ci",
              width = "350px"
            )
          )
        ),
        card_body(
          layout_column_wrap(
            width = 1/2,
            tags$div(
              plotlyOutput("p1_ci_plot", height = "430px"),
              uiOutput("p1_ci_sr_summary")
            ),
            DTOutput("p1_ci_table")
          )
        ),
        card_footer(
          "How to read this chart: Red diamond = Average Estimate; Blue horizontal bar with short vertical end-caps (|---|) = 95% Confidence Interval (Likely True Range). Hover over any diamond or blue bar to view exact numbers."
        )
      )
    )
  ),
  
  # ============================================================================
  # PAGE 2: COUNTY DEEP-DIVE, TIME SERIES & SIMPLE MONTHLY WEATHER CIs
  # ============================================================================
  nav_panel(
    title = "Tab 2: Single-County Trends & Weather",
    icon = bs_icon("graph-up"),
    layout_sidebar(
      sidebar = sidebar(
        title = "County Deep-Dive Controls",
        width = 325,
        open  = "always",
        selectInput(
          "p2_county",
          "1. Select Virginia Locality:",
          choices = all_counties,
          selected = if ("Fairfax County" %in% all_counties) "Fairfax County" else all_counties[1]
        ),
        radioButtons(
          "p2_ts_metric",
          "2. Monthly Trend Metric (Left Chart):",
          choices = c(
            "Monthly Severe Crash Count"          = "count",
            "Monthly Severe Accident Rate (%)"    = "rate"
          ),
          selected = "count"
        ),
        radioButtons(
          "p2_weather_metric",
          "3. Weather Category Metric (Right & Bottom):",
          choices = c(
            "Severe Accident Rate (% of Crashes)"      = "severe_rate",
            "Severe Crashes per 100k County Residents" = "severe_ratio"
          ),
          selected = "severe_rate"
        ),
        hr(),
        tags$div(
          class = "p-2 rounded border bg-light text-dark small",
          tags$strong(bs_icon("check-circle-fill"), " How Weather is Grouped:"),
          " All 73 weather descriptions are grouped into 5 Core Weather Categories. Only months with at least 5 crashes are averaged, and categories with at least 10 active months are plotted.",
          tags$hr(class = "my-2"),
          downloadButton(
            "download_page_status",
            "Download County Data & Summary (.xlsx)",
            class = "btn-sm btn-outline-primary w-100 fw-bold"
          )
        )
      ),
      
      # Page 2 KPI banner with Statewide comparison benchmarks
      uiOutput("p2_kpi_sr_summary"),
      layout_column_wrap(
        width = 1/3,
        value_box(
          title = "Total Recorded Crashes (Selected Locality)",
          value = textOutput("p2_kpi_total"),
          p(class = "small mb-0 fw-semibold", textOutput("p2_kpi_total_sub")),
          showcase = bs_icon("car-front-fill"),
          theme = "primary"
        ),
        value_box(
          title = "Overall Severe Accident Rate (%)",
          value = textOutput("p2_kpi_rate"),
          p(class = "small mb-0 fw-semibold", textOutput("p2_kpi_rate_sub")),
          showcase = bs_icon("exclamation-triangle-fill"),
          theme = "danger"
        ),
        value_box(
          title = "Severe Crashes per 100k Residents",
          value = textOutput("p2_kpi_ratio"),
          p(class = "small mb-0 fw-semibold", textOutput("p2_kpi_ratio_sub")),
          showcase = bs_icon("people-fill"),
          theme = "secondary"
        )
      ),
      
      layout_column_wrap(
        width = 1/2,
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white fw-bold",
            textOutput("p2_ts_title")
          ),
          card_body(
            plotlyOutput("p2_ts_plot", height = "390px"),
            uiOutput("p2_ts_sr_summary")
          ),
          card_footer(
            "Red points show each month's value; shaded band shows the 95% Confidence Interval (Likely True Range) around each month."
          )
        ),
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white fw-bold",
            textOutput("p2_weather_title")
          ),
          card_body(
            plotlyOutput("p2_weather_plot", height = "390px"),
            uiOutput("p2_weather_sr_summary")
          ),
          card_footer(
            "Compares the 5 Core Weather Categories in the selected locality across 2016–2023."
          )
        )
      ),
      
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-dark text-white fw-bold",
          textOutput("p2_ci_header")
        ),
        card_body(
          layout_column_wrap(
            width = 1/2,
            tags$div(
              plotlyOutput("p2_ci_plot", height = "420px"),
              uiOutput("p2_ci_sr_summary")
            ),
            DTOutput("p2_ci_table")
          )
        ),
        card_footer(
          "How to read this chart: Red diamond = Monthly Average across active months; Blue horizontal bar with short vertical end-caps (|---|) = 95% Confidence Interval (Likely True Range)."
        )
      ),
      
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-secondary text-white fw-bold",
          "Complete County Summary Table: Counts, Active Months, Severe Rates, and Per-Capita Ratios by Core Weather Category"
        ),
        card_body(
          DTOutput("p2_summary_table")
        )
      )
    )
  ),
  
  # ============================================================================
  # PAGE 3: INTERACTIVE VIRGINIA COUNTY RATE & STREET ZONE HOTSPOT GIS MAP
  # ============================================================================
  nav_panel(
    title = "Tab 3: Virginia & Street Hotspot Map",
    icon = bs_icon("geo-alt-fill"),
    layout_sidebar(
      sidebar = sidebar(
        title = "GIS Map Controls",
        width = 325,
        open  = "always",
        radioButtons(
          "p3_jurisdiction",
          "1. Jurisdiction Type:",
          choices = c(
            "Counties Only (Excludes Cities)"    = "counties_only",
            "All Localities (Counties & Cities)" = "all_localities"
          ),
          selected = "counties_only"
        ),
        selectInput(
          "p3_county",
          "2. Select Virginia Locality (or Click Map):",
          choices = c("All Qualifying Localities (Statewide Map)" = "ALL_VA", only_counties),
          selected = "ALL_VA"
        ),
        actionButton(
          "p3_reset_va",
          "Zoom Out to Full Virginia Map",
          icon = icon("globe-americas"),
          class = "btn-primary w-100 mb-3 fw-bold"
        ),
        hr(),
        selectInput(
          "p3_year",
          "3. Analysis Year:",
          choices = c("All Years (Full Date Range)" = "All", as.character(all_years)),
          selected = "All"
        ),
        dateRangeInput(
          "p3_date_range",
          "4. Custom Date Range:",
          start = min_acc_date,
          end   = max_acc_date,
          min   = min_acc_date,
          max   = max_acc_date
        ),
        hr(),
        tags$div(
          class = "p-2 rounded border bg-light text-dark small",
          tags$strong(bs_icon("info-circle-fill"), " Two-Step Map Workflow:"),
          tags$br(),
          "• ", tags$strong("Step 1 (Statewide View):"), " Localities with ≥20 crashes are colored by Severe Accident Rate (% of crashes that are severe). Excluded or low-volume (<20 crashes) areas are shaded gray.",
          tags$br(),
          "• ", tags$strong("Step 2 (County Zone View):"), " Click any locality on the map to zoom in and view its Top 30 highest-risk ~1 km street zones."
        )
      ),
      
      layout_column_wrap(
        width = 1/2,
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white fw-bold",
            textOutput("p3_map_title")
          ),
          card_body(
            leafletOutput("p3_map", height = "550px"),
            uiOutput("p3_map_sr_summary")
          ),
          card_footer(
            textOutput("p3_map_subtitle")
          )
        ),
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-dark text-white fw-bold",
            textOutput("p3_table_title")
          ),
          card_body(
            DTOutput("p3_street_table")
          ),
          card_footer(
            textOutput("p3_table_footer")
          )
        )
      )
    )
  )
)

# ==============================================================================
# 3. SERVER LOGIC
# ==============================================================================
server <- function(input, output, session) {
  
  # ----------------------------------------------------------------------------
  # EXCEL DOWNLOAD HANDLER: COUNTY RANKING SUMMARY
  # ----------------------------------------------------------------------------
  top5_worst_excel_sheets <- reactive({
    df_filtered <- if (input$p1_year == "All") {
      acc
    } else {
      acc %>% filter(Year == as.numeric(input$p1_year))
    }
    
    all_months <- seq(min(df_filtered$month_year, na.rm = TRUE),
                      max(df_filtered$month_year, na.rm = TRUE),
                      by = "1 month")
    
    monthly_cnty <- df_filtered %>%
      filter(!is.na(month_year)) %>%
      group_by(census_name, month_year) %>%
      summarise(
        severe_accidents = sum(severe, na.rm = TRUE),
        accidents        = n(),
        .groups = "drop"
      ) %>%
      complete(
        census_name = all_counties,
        month_year  = all_months,
        fill = list(severe_accidents = 0L, accidents = 0L)
      ) %>%
      left_join(county_pop_ranks %>% select(census_name, population, is_city), by = "census_name") %>%
      mutate(
        severe_ratio = (severe_accidents / population) * 100000,
        severe_rate  = ifelse(accidents == 0, NA_real_, severe_accidents / accidents)
      )
    
    monthly_stats <- monthly_cnty %>%
      group_by(census_name) %>%
      summarise(
        Total_Months_Observed        = n(),
        Active_Months_With_Crashes   = sum(accidents > 0, na.rm = TRUE),
        Mean_Monthly_Total_Acc       = round(mean(accidents, na.rm = TRUE), 2),
        Mean_Monthly_Severe_Acc      = round(mean(severe_accidents, na.rm = TRUE), 2),
        Mean_Monthly_Severe_Ratio    = round(mean(severe_ratio, na.rm = TRUE), 2),
        SE_Monthly_Ratio             = sd(severe_ratio, na.rm = TRUE) / sqrt(n()),
        Monthly_Ratio_95CI_Low       = round(pmax(0, Mean_Monthly_Severe_Ratio - qt(0.975, df = pmax(1, n() - 1)) * SE_Monthly_Ratio), 2),
        Monthly_Ratio_95CI_High      = round(Mean_Monthly_Severe_Ratio + qt(0.975, df = pmax(1, n() - 1)) * SE_Monthly_Ratio, 2),
        Mean_Monthly_Severe_Rate_Pct = round(mean(severe_rate, na.rm = TRUE) * 100, 2),
        SE_Monthly_Rate              = sd(severe_rate, na.rm = TRUE) / sqrt(pmax(1, sum(!is.na(severe_rate)))),
        Monthly_Rate_95CI_Low_Pct    = round(pmax(0, (mean(severe_rate, na.rm = TRUE) - qt(0.975, df = pmax(1, sum(!is.na(severe_rate)) - 1)) * SE_Monthly_Rate) * 100), 2),
        Monthly_Rate_95CI_High_Pct   = round(pmin(100, (mean(severe_rate, na.rm = TRUE) + qt(0.975, df = pmax(1, sum(!is.na(severe_rate)) - 1)) * SE_Monthly_Rate) * 100), 2),
        .groups = "drop"
      ) %>%
      select(-SE_Monthly_Ratio, -SE_Monthly_Rate)
    
    overall_cnty <- county_pop_ranks %>%
      select(GEOID, census_name, is_city, population) %>%
      left_join(
        df_filtered %>%
          group_by(census_name) %>%
          summarise(
            Total_Accidents  = n(),
            Severe_Accidents = sum(severe, na.rm = TRUE),
            .groups = "drop"
          ),
        by = "census_name"
      ) %>%
      mutate(
        Jurisdiction_Type         = ifelse(is_city, "Independent City", "County"),
        Total_Accidents           = replace_na(Total_Accidents, 0L),
        Severe_Accidents          = replace_na(Severe_Accidents, 0L),
        Overall_Severe_Rate_Pct   = round(ifelse(Total_Accidents > 0, (Severe_Accidents / Total_Accidents) * 100, 0), 2),
        Overall_Severe_Ratio_100k = round((Severe_Accidents / population) * 100000, 2)
      ) %>%
      select(-is_city) %>%
      left_join(monthly_stats, by = "census_name") %>%
      rename(Locality = census_name, Census_Population_2023 = population)
    
    list(
      Current_Selection_Summary = overall_cnty %>%
        filter(Locality %in% p1_selected_counties()) %>%
        arrange(desc(Overall_Severe_Rate_Pct)),
      Top5_By_Severe_Rate_Pct = overall_cnty %>%
        filter(Total_Accidents >= 50) %>%
        arrange(desc(Overall_Severe_Rate_Pct)) %>%
        slice_head(n = 5) %>%
        mutate(Ranking_Criterion = "Top 5 Highest Severe Rate % (Min 50 Crashes)", .before = 1),
      Top5_By_Severe_Count = overall_cnty %>%
        arrange(desc(Severe_Accidents)) %>%
        slice_head(n = 5) %>%
        mutate(Ranking_Criterion = "Top 5 Highest Severe Crash Count", .before = 1),
      Top5_By_Severe_Ratio_100k = overall_cnty %>%
        arrange(desc(Overall_Severe_Ratio_100k)) %>%
        slice_head(n = 5) %>%
        mutate(Ranking_Criterion = "Top 5 Highest Severe Crashes per 100k Residents", .before = 1)
    )
  })
  
  output$download_top5_excel <- downloadHandler(
    filename = function() {
      yr_tag <- ifelse(input$p1_year == "All", "2016_2023", input$p1_year)
      ext <- if (requireNamespace("writexl", quietly = TRUE)) ".xlsx" else ".csv"
      paste0("VA_Traffic_County_Ranking_Summary_", yr_tag, ext)
    },
    content = function(file) {
      sheets <- top5_worst_excel_sheets()
      if (requireNamespace("writexl", quietly = TRUE)) {
        writexl::write_xlsx(sheets, path = file)
      } else {
        write.csv(sheets$Current_Selection_Summary, file, row.names = FALSE)
      }
    }
  )
  
  # ----------------------------------------------------------------------------
  # PAGE 1 SERVER
  # ----------------------------------------------------------------------------
  p1_selected_counties <- reactive({
    base_ranks <- county_pop_ranks
    if (input$p1_jurisdiction == "counties_only") {
      base_ranks <- base_ranks %>% filter(!is_city)
    }
    
    n_loc   <- nrow(base_ranks)
    mid_s   <- max(1, floor((n_loc - 5) / 2) + 1)
    mid_idx <- mid_s:min(n_loc, mid_s + 4)
    
    switch(
      input$p1_pop_tier,
      "top5"  = base_ranks %>% slice_head(n = 5) %>% pull(census_name),
      "mid5"  = base_ranks %>% slice(mid_idx) %>% pull(census_name),
      "bot5"  = base_ranks %>% slice_tail(n = 5) %>% pull(census_name),
      "top10" = base_ranks %>% slice_head(n = 10) %>% pull(census_name),
      "all"   = base_ranks %>% pull(census_name)
    )
  })
  
  p1_statewide_year_acc <- reactive({
    if (input$p1_year != "All") {
      acc %>% filter(Year == as.numeric(input$p1_year))
    } else {
      acc
    }
  })
  
  p1_filtered_acc <- reactive({
    p1_statewide_year_acc() %>%
      filter(census_name %in% p1_selected_counties())
  })
  
  p1_county_summary <- reactive({
    df <- p1_filtered_acc()
    base_counties <- county_pop_ranks %>%
      select(GEOID, census_name, population) %>%
      filter(census_name %in% p1_selected_counties())
    
    agg <- df %>%
      group_by(census_name) %>%
      summarise(
        total_accidents  = n(),
        severe_accidents = sum(severe, na.rm = TRUE),
        .groups = "drop"
      )
    
    base_counties %>%
      left_join(agg, by = "census_name") %>%
      mutate(
        total_accidents  = replace_na(total_accidents, 0L),
        severe_accidents = replace_na(severe_accidents, 0L),
        severe_rate      = ifelse(total_accidents > 0, severe_accidents / total_accidents, 0),
        severe_ratio     = (severe_accidents / population) * 100000
      )
  })
  
  # Page 1 KPIs with Virginia Statewide comparison context
  output$p1_kpi_total <- renderText({
    comma(sum(p1_county_summary()$total_accidents, na.rm = TRUE))
  })
  
  output$p1_kpi_total_sub <- renderText({
    sel_tot <- sum(p1_county_summary()$total_accidents, na.rm = TRUE)
    va_tot  <- nrow(p1_statewide_year_acc())
    if (va_tot == 0) return("Virginia Statewide: 0")
    paste0(percent(sel_tot / va_tot, accuracy = 0.1), " of all ", comma(va_tot), " VA crashes")
  })
  
  output$p1_kpi_rate <- renderText({
    tot_acc <- sum(p1_county_summary()$total_accidents, na.rm = TRUE)
    tot_sev <- sum(p1_county_summary()$severe_accidents, na.rm = TRUE)
    if (tot_acc == 0) return("0.0%")
    percent(tot_sev / tot_acc, accuracy = 0.1)
  })
  
  output$p1_kpi_rate_sub <- renderText({
    df_va   <- p1_statewide_year_acc()
    va_rate <- ifelse(nrow(df_va) > 0, sum(df_va$severe, na.rm = TRUE) / nrow(df_va), 0)
    paste0("Compared to ", percent(va_rate, accuracy = 0.1), " Virginia Statewide Rate")
  })
  
  output$p1_kpi_ratio <- renderText({
    tot_pop <- sum(p1_county_summary()$population, na.rm = TRUE)
    tot_sev <- sum(p1_county_summary()$severe_accidents, na.rm = TRUE)
    if (tot_pop == 0) return("0.0")
    number((tot_sev / tot_pop) * 100000, accuracy = 0.1, big.mark = ",")
  })
  
  output$p1_kpi_ratio_sub <- renderText({
    df_va    <- p1_statewide_year_acc()
    va_ratio <- ifelse(statewide_total_pop > 0, (sum(df_va$severe, na.rm = TRUE) / statewide_total_pop) * 100000, 0)
    paste0("Compared to ", number(va_ratio, accuracy = 0.1, big.mark = ","), " VA Statewide per 100k")
  })
  
  output$p1_kpi_sr_summary <- renderUI({
    df <- p1_county_summary()
    tot_acc <- sum(df$total_accidents, na.rm = TRUE)
    tot_sev <- sum(df$severe_accidents, na.rm = TRUE)
    tot_pop <- sum(df$population, na.rm = TRUE)
    rate_str  <- if (tot_acc > 0) percent(tot_sev / tot_acc, 0.1) else "0.0%"
    ratio_str <- if (tot_pop > 0) number((tot_sev / tot_pop) * 100000, 0.1, big.mark = ",") else "0.0"
    
    msg <- paste0(
      "Selected group (", nrow(df), " localities, Year: ", input$p1_year, "): ",
      comma(tot_acc), " total crashes recorded, of which ", comma(tot_sev),
      " (", rate_str, ") were severe (", ratio_str, " severe crashes per 100,000 residents)."
    )
    render_a11y_box(msg)
  })
  
  # Dynamic title naming the exact selected metric
  output$p1_county_title <- renderText({
    metric_title <- switch(
      input$p1_metric,
      "total_accidents" = "Total Recorded Crashes",
      "severe_rate"     = "Severe Accident Rate (% of Crashes)",
      "severe_ratio"    = "Severe Crashes per 100,000 Residents"
    )
    jur_lbl <- ifelse(input$p1_jurisdiction == "counties_only", "Virginia Counties", "Virginia Localities")
    yr_lbl  <- ifelse(input$p1_year == "All", "2016–2023", input$p1_year)
    paste0(jur_lbl, " Ranked by ", metric_title, " (", yr_lbl, ")")
  })
  
  output$p1_weather_title <- renderText({
    yr_lbl <- ifelse(input$p1_year == "All", "2016–2023", input$p1_year)
    paste0("Top 10 Weather Conditions with Highest Severe Accident Rate % (", yr_lbl, ")")
  })
  
  output$p1_county_plot <- renderPlotly({
    df <- p1_county_summary() %>% filter(total_accidents > 0)
    if (input$p1_pop_tier == "all") {
      if (input$p1_metric == "severe_rate") {
        df <- df %>% filter(total_accidents >= 20)
      }
      df <- df %>% slice_max(.data[[input$p1_metric]], n = 15, with_ties = FALSE)
    }
    
    validate(need(nrow(df) > 0, "No localities match the selected filters."))
    
    metric_label <- switch(
      input$p1_metric,
      "total_accidents" = "Total Recorded Crashes",
      "severe_rate"     = "Severe Accident Rate (% of County Crashes)",
      "severe_ratio"    = "Severe Crashes per 100,000 Residents"
    )
    
    max_val <- max(df[[input$p1_metric]], na.rm = TRUE)
    if (!is.finite(max_val) || max_val <= 0) max_val <- 1
    
    df <- df %>%
      mutate(
        val_label = case_when(
          input$p1_metric == "severe_rate"  ~ percent(severe_rate, accuracy = 0.1),
          input$p1_metric == "severe_ratio" ~ number(severe_ratio, accuracy = 0.1, big.mark = ","),
          TRUE                              ~ comma(total_accidents)
        ),
        label_y = .data[[input$p1_metric]] + (max_val * 0.085)
      )
    
    p <- ggplot(
      df,
      aes(
        x = reorder(census_name, .data[[input$p1_metric]]),
        y = .data[[input$p1_metric]],
        text = paste0(
          "<b>", census_name, "</b><br>",
          "Population: ", comma(population), "<br>",
          "Total Crashes: ", comma(total_accidents), "<br>",
          "Severe Crashes: ", comma(severe_accidents), "<br>",
          "Severe Rate: ", percent(severe_rate, accuracy = 0.1), "<br>",
          "Severe Crashes per 100k: ", round(severe_ratio, 1)
        )
      )
    ) +
      geom_col(fill = exec_navy, alpha = 0.92, width = 0.68) +
      geom_text(
        aes(y = label_y, label = val_label),
        size     = 3.7,
        fontface = "bold",
        color    = "#1A202C"
      ) +
      coord_flip() +
      labs(x = NULL, y = metric_label) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(face = "bold", color = "#1A202C")
      )
    
    if (input$p1_metric == "severe_rate") {
      p <- p + scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.22)))
    } else {
      p <- p + scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(0, 0.22)))
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$p1_county_sr_summary <- renderUI({
    df <- p1_county_summary() %>% filter(total_accidents > 0)
    if (input$p1_pop_tier == "all" && input$p1_metric == "severe_rate") {
      df <- df %>% filter(total_accidents >= 20)
    }
    df <- df %>% arrange(desc(.data[[input$p1_metric]]))
    if (nrow(df) == 0) return(NULL)
    top_row <- df %>% slice(1)
    bot_row <- df %>% slice(min(nrow(df), 15))
    
    fmt_val <- function(r) {
      switch(
        input$p1_metric,
        "total_accidents" = paste0(comma(r$total_accidents), " total crashes"),
        "severe_rate"     = paste0(percent(r$severe_rate, 0.1), " severe accident rate"),
        "severe_ratio"    = paste0(round(r$severe_ratio, 1), " severe crashes per 100k residents")
      )
    }
    
    msg <- paste0(
      "Highest locality is ", top_row$census_name, " with ", fmt_val(top_row),
      " (", comma(top_row$severe_accidents), " severe out of ", comma(top_row$total_accidents), " total crashes). ",
      "Lowest shown is ", bot_row$census_name, " with ", fmt_val(bot_row), "."
    )
    render_a11y_box(msg)
  })
  
  # Default minimum accidents per weather condition set to 20
  p1_weather_summary <- reactive({
    df <- p1_filtered_acc() %>%
      filter(!is.na(Weather_Condition), Weather_Condition != "") %>%
      group_by(Weather_Condition) %>%
      summarise(
        total_accidents  = n(),
        severe_accidents = sum(severe, na.rm = TRUE),
        severe_percent   = severe_accidents / total_accidents,
        .groups = "drop"
      ) %>%
      filter(total_accidents >= 20) %>%
      slice_max(severe_percent, n = 10, with_ties = FALSE)
    
    m_groups  <- max(1, nrow(df))
    alpha_adj <- get_adjusted_alpha(m_groups, "bonferroni", alpha = 0.05)
    
    df %>%
      mutate(
        m_comparisons = m_groups,
        conf_level    = 1 - alpha_adj,
        lower_ci      = calc_binom_ci(severe_accidents, total_accidents, "lower", conf_level = 1 - alpha_adj),
        upper_ci      = calc_binom_ci(severe_accidents, total_accidents, "upper", conf_level = 1 - alpha_adj)
      )
  })
  
  output$p1_weather_plot <- renderPlotly({
    df <- p1_weather_summary()
    validate(
      need(nrow(df) > 0, "No weather conditions meet the minimum 20-crash threshold for this selection.")
    )
    
    max_val <- max(df$severe_percent, na.rm = TRUE)
    if (!is.finite(max_val) || max_val <= 0) max_val <- 1
    
    df <- df %>%
      mutate(
        val_label = percent(severe_percent, accuracy = 0.1),
        label_y   = severe_percent + (max_val * 0.085)
      )
    
    p <- ggplot(
      df,
      aes(
        x = reorder(Weather_Condition, severe_percent),
        y = severe_percent,
        text = paste0(
          "<b>", Weather_Condition, "</b><br>",
          "Severe Rate: ", percent(severe_percent, accuracy = 0.1), "<br>",
          "Severe / Total: ", comma(severe_accidents), " / ", comma(total_accidents)
        )
      )
    ) +
      geom_col(fill = exec_teal, alpha = 0.92, width = 0.68) +
      geom_text(
        aes(y = label_y, label = val_label),
        size     = 3.7,
        fontface = "bold",
        color    = "#1A202C"
      ) +
      coord_flip() +
      scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.22))) +
      labs(
        x = NULL,
        y = "Severe Accident Rate (% of Crashes in Weather Condition)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(face = "bold", color = "#1A202C")
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$p1_weather_sr_summary <- renderUI({
    df <- p1_weather_summary() %>% arrange(desc(severe_percent))
    if (nrow(df) == 0) return(NULL)
    w1 <- df %>% slice(1)
    w2 <- if (nrow(df) >= 2) df %>% slice(2) else NULL
    
    msg <- paste0(
      "Rank 1 weather condition is ", w1$Weather_Condition, " at ", percent(w1$severe_percent, 0.1),
      " severe (", comma(w1$severe_accidents), " of ", comma(w1$total_accidents), " crashes). ",
      if (!is.null(w2)) paste0("Rank 2 is ", w2$Weather_Condition, " at ", percent(w2$severe_percent, 0.1), ".") else ""
    )
    render_a11y_box(msg)
  })
  
  p1_monthly_variance <- reactive({
    df <- p1_filtered_acc() %>% filter(!is.na(month_year))
    validate(need(nrow(df) > 0, "No monthly records available for this filter selection."))
    
    all_months <- seq(min(df$month_year), max(df$month_year), by = "1 month")
    
    df %>%
      group_by(census_name, month_year) %>%
      summarise(
        severe_accidents = sum(severe, na.rm = TRUE),
        accidents        = n(),
        .groups = "drop"
      ) %>%
      complete(
        census_name = p1_selected_counties(),
        month_year  = all_months,
        fill = list(severe_accidents = 0L, accidents = 0L)
      ) %>%
      left_join(county_pop_ranks %>% select(census_name, population), by = "census_name") %>%
      mutate(
        severe_ratio = (severe_accidents / population) * 100000,
        severe_rate  = ifelse(accidents == 0, NA_real_, severe_accidents / accidents)
      )
  })
  
  # Aligns the >=20 total crash threshold when "All Qualifying Localities" is active
  p1_ci_data <- reactive({
    if (input$p1_ci_type == "weather_binom_ci") {
      df <- p1_weather_summary() %>%
        transmute(
          Group         = Weather_Condition,
          N_Units       = total_accidents,
          Mean_Est      = severe_percent,
          CI_Lower      = lower_ci,
          CI_Upper      = upper_ci,
          m_comparisons = m_comparisons,
          conf_level    = conf_level
        )
      return(df)
    }
    
    qualifying_names <- p1_county_summary() %>%
      filter(if (input$p1_pop_tier == "all") total_accidents >= 20 else total_accidents > 0) %>%
      pull(census_name)
    
    monthly <- p1_monthly_variance() %>%
      filter(census_name %in% qualifying_names)
    
    target_col <- ifelse(input$p1_ci_type == "monthly_ratio_ci", "severe_ratio", "severe_rate")
    upper_cap  <- ifelse(input$p1_ci_type == "monthly_ratio_ci", Inf, 1.0)
    
    ci_all <- compute_simple_ci(
      df          = monthly,
      group_col   = "census_name",
      value_col   = target_col,
      pop_col     = "population",
      mult_method = "bonferroni",
      upper_cap   = upper_cap,
      min_months  = 6
    ) %>%
      arrange(desc(Mean_Est))
    
    if (input$p1_pop_tier == "all") {
      ci_all <- ci_all %>% slice_head(n = 15)
    }
    ci_all
  })
  
  output$p1_ci_header <- renderText({
    switch(
      input$p1_ci_type,
      "monthly_ratio_ci" = "Monthly Severe Crashes per 100k Residents — 95% Confidence Interval (Likely True Range)",
      "monthly_rate_ci"  = "Monthly Severe Accident Rate (%) — 95% Confidence Interval (Likely True Range)",
      "weather_binom_ci" = "Weather Severe Accident Rate (%) — 95% Confidence Interval (Likely True Range)"
    )
  })
  
  output$p1_ci_plot <- renderPlotly({
    df <- p1_ci_data()
    validate(need(nrow(df) > 0, "Insufficient data to compute confidence intervals."))
    
    is_pct <- input$p1_ci_type %in% c("monthly_rate_ci", "weather_binom_ci")
    x_lab  <- switch(
      input$p1_ci_type,
      "monthly_ratio_ci" = "Monthly Severe Crashes per 100k Residents (Mean ± 95% Likely Range)",
      "monthly_rate_ci"  = "Monthly Severe Accident Rate % (Mean ± 95% Likely Range)",
      "weather_binom_ci" = "Weather Severe Accident Rate % (Rate ± 95% Likely Range)"
    )
    
    # Assign integer row positions (1..N) and attach hover_text to both segments and diamond
    df <- df %>%
      arrange(Mean_Est) %>%
      mutate(
        y_num      = row_number(),
        cap_low    = y_num - 0.18,
        cap_high   = y_num + 0.18,
        hover_text = paste0(
          "<b>", Group, "</b><br>",
          "Average / Estimate: ", ifelse(is_pct, percent(Mean_Est, 0.1), round(Mean_Est, 2)), "<br>",
          "95% Confidence Interval (Likely Range): [",
          ifelse(is_pct, percent(CI_Lower, 0.1), round(CI_Lower, 2)), ", ",
          ifelse(is_pct, percent(CI_Upper, 0.1), round(CI_Upper, 2)), "]<br>",
          "Months / Crashes Included (n): ", N_Units
        )
      )
    
    p <- ggplot(df) +
      # Horizontal confidence interval bar (with hover tooltip)
      geom_segment(
        aes(x = CI_Lower, xend = CI_Upper, y = y_num, yend = y_num, text = hover_text),
        color     = exec_teal,
        linewidth = 1.25
      ) +
      # Left vertical end-cap strictly confined to row (+/- 0.18 row height)
      geom_segment(
        aes(x = CI_Lower, xend = CI_Lower, y = cap_low, yend = cap_high, text = hover_text),
        color     = exec_teal,
        linewidth = 1.25
      ) +
      # Right vertical end-cap strictly confined to row (+/- 0.18 row height)
      geom_segment(
        aes(x = CI_Upper, xend = CI_Upper, y = cap_low, yend = cap_high, text = hover_text),
        color     = exec_teal,
        linewidth = 1.25
      ) +
      # Red diamond point estimate
      geom_point(
        aes(x = Mean_Est, y = y_num, text = hover_text),
        color = exec_crimson,
        shape = 18,
        size  = 4.0
      ) +
      scale_y_continuous(
        breaks = df$y_num,
        labels = df$Group,
        expand = expansion(add = c(0.6, 0.6))
      ) +
      labs(x = x_lab, y = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(face = "bold", color = "#1A202C")
      )
    
    if (is_pct) {
      p <- p + scale_x_continuous(labels = percent_format())
    } else {
      p <- p + scale_x_continuous(labels = comma_format())
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$p1_ci_sr_summary <- renderUI({
    df <- p1_ci_data() %>% arrange(desc(Mean_Est))
    if (nrow(df) == 0) return(NULL)
    is_pct <- input$p1_ci_type %in% c("monthly_rate_ci", "weather_binom_ci")
    top_g  <- df %>% slice(1)
    fmt    <- function(v) if (is_pct) percent(v, 0.1) else round(v, 2)
    
    top5_names <- paste(head(df$Group, 5), collapse = ", ")
    msg <- paste0(
      "Top 5 ranked in this chart: ", top5_names, ". ",
      "Highest is ", top_g$Group, " at ", fmt(top_g$Mean_Est),
      " (95% Confidence Interval: ", fmt(top_g$CI_Lower), " to ", fmt(top_g$CI_Upper), ")."
    )
    render_a11y_box(msg)
  })
  
  output$p1_ci_table <- renderDT({
    df <- p1_ci_data() %>% arrange(desc(Mean_Est))
    is_pct <- input$p1_ci_type %in% c("monthly_rate_ci", "weather_binom_ci")
    
    tbl <- df %>%
      select(Group, N_Units, Mean_Est, CI_Lower, CI_Upper)
    
    dt <- datatable(
      tbl,
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = "caption-side: top; font-weight: bold; color: #0F2042;",
        "Sortable Data Table: Estimates & 95% Confidence Intervals"
      ),
      colnames = c("Locality / Weather", "n (Months/Crashes)", "Estimate", "95% CI Low", "95% CI High"),
      options = list(pageLength = 8, dom = "tip", scrollX = TRUE)
    )
    
    if (is_pct) {
      dt <- dt %>% formatPercentage(c("Mean_Est", "CI_Lower", "CI_Upper"), digits = 1)
    } else {
      dt <- dt %>% formatRound(c("Mean_Est", "CI_Lower", "CI_Upper"), digits = 2)
    }
    dt
  })
  
  # ----------------------------------------------------------------------------
  # PAGE 2 SERVER: SINGLE-COUNTY TRENDS & WEATHER CONFIDENCE INTERVALS
  # ----------------------------------------------------------------------------
  p2_county_acc <- reactive({
    acc %>% filter(census_name == input$p2_county)
  })
  
  p2_kpi_summary <- reactive({
    df <- p2_county_acc()
    pop <- county_pop_ranks %>%
      filter(census_name == input$p2_county) %>%
      pull(population)
    pop <- if (length(pop) > 0) pop[1] else NA_real_
    total <- nrow(df)
    severe_total <- sum(df$severe, na.rm = TRUE)
    
    list(
      total           = total,
      severe_rate     = if (total > 0) severe_total / total else NA_real_,
      severe_per_100k = if (!is.na(pop) && pop > 0) severe_total / pop * 100000 else NA_real_,
      severe_total    = severe_total,
      pop             = pop
    )
  })
  
  output$p2_kpi_total <- renderText({
    comma(p2_kpi_summary()$total)
  })
  
  output$p2_kpi_total_sub <- renderText({
    x <- p2_kpi_summary()
    paste0(comma(x$severe_total), " severe crashes | Pop: ", comma(x$pop))
  })
  
  output$p2_kpi_rate <- renderText({
    x <- p2_kpi_summary()$severe_rate
    if (is.na(x)) "N/A" else percent(x, accuracy = 0.1)
  })
  
  output$p2_kpi_rate_sub <- renderText({
    va_rate <- sum(acc$severe, na.rm = TRUE) / nrow(acc)
    paste0("Compared to ", percent(va_rate, accuracy = 0.1), " VA Statewide Rate")
  })
  
  output$p2_kpi_ratio <- renderText({
    x <- p2_kpi_summary()$severe_per_100k
    if (is.na(x)) "N/A" else number(x, accuracy = 0.1, big.mark = ",")
  })
  
  output$p2_kpi_ratio_sub <- renderText({
    va_ratio <- (sum(acc$severe, na.rm = TRUE) / statewide_total_pop) * 100000
    paste0("Compared to ", number(va_ratio, accuracy = 0.1, big.mark = ","), " VA Statewide per 100k")
  })
  
  output$p2_kpi_sr_summary <- renderUI({
    x <- p2_kpi_summary()
    msg <- paste0(
      input$p2_county, ": ", comma(x$total), " recorded crashes; ",
      comma(x$severe_total), " severe crashes (",
      if (is.na(x$severe_rate)) "N/A" else percent(x$severe_rate, 0.1), " severe rate; ",
      if (is.na(x$severe_per_100k)) "N/A" else round(x$severe_per_100k, 1), " severe crashes per 100,000 residents)."
    )
    render_a11y_box(msg)
  })
  
  output$p2_ts_title <- renderText({
    metric_lbl <- ifelse(
      input$p2_ts_metric == "count",
      "Monthly Severe Crash Count",
      "Monthly Severe Accident Rate (%)"
    )
    paste0(metric_lbl, " & 95% Confidence Band: ", input$p2_county)
  })
  
  output$p2_weather_title <- renderText({
    metric_lbl <- ifelse(
      input$p2_weather_metric == "severe_rate",
      "Severe Accident Rate (%)",
      "Severe Crashes per 100,000 Residents"
    )
    paste0("5 Core Weather Categories by ", metric_lbl, ": ", input$p2_county)
  })
  
  p2_ts_computed <- reactive({
    df <- p2_county_acc() %>% filter(!is.na(month_year))
    validate(need(nrow(df) > 0, "No time-series data available for this locality."))
    
    all_months <- seq(min(df$month_year), max(df$month_year), by = "1 month")
    
    ts_base <- df %>%
      group_by(month_year) %>%
      summarise(
        total_accidents  = n(),
        severe_accidents = sum(severe, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      complete(month_year = all_months, fill = list(total_accidents = 0L, severe_accidents = 0L))
    
    m_months  <- max(1, nrow(ts_base))
    alpha_adj <- get_adjusted_alpha(m_months, "bonferroni", alpha = 0.05)
    c_level   <- 1 - alpha_adj
    
    ts_base %>%
      mutate(
        conf_level  = c_level,
        pois_low    = calc_poisson_ci(severe_accidents, "lower", conf_level = c_level),
        pois_high   = calc_poisson_ci(severe_accidents, "upper", conf_level = c_level),
        severe_rate = ifelse(total_accidents > 0, severe_accidents / total_accidents, NA_real_),
        rate_low    = calc_binom_ci(severe_accidents, total_accidents, "lower", conf_level = c_level),
        rate_high   = calc_binom_ci(severe_accidents, total_accidents, "upper", conf_level = c_level)
      )
  })
  
  output$p2_ts_plot <- renderPlotly({
    ts_df <- p2_ts_computed()
    
    if (input$p2_ts_metric == "count") {
      p <- ggplot(ts_df, aes(x = month_year, y = severe_accidents)) +
        geom_ribbon(aes(ymin = pois_low, ymax = pois_high), fill = exec_teal, alpha = 0.25) +
        geom_line(color = exec_navy, linewidth = 0.9) +
        geom_point(
          aes(
            text = paste0(
              "<b>", format(month_year, "%b %Y"), "</b><br>",
              "Severe Crashes: ", comma(severe_accidents), "<br>",
              "95% Confidence Interval (Likely Range): [", round(pois_low, 1), ", ", round(pois_high, 1), "]<br>",
              "Total Crashes: ", comma(total_accidents)
            )
          ),
          color = exec_crimson, size = 1.9
        ) +
        labs(x = "Month", y = "Number of Severe Crashes")
    } else {
      ts_valid <- ts_df %>% filter(!is.na(severe_rate))
      p <- ggplot(ts_valid, aes(x = month_year, y = severe_rate)) +
        geom_ribbon(aes(ymin = rate_low, ymax = rate_high), fill = exec_amber, alpha = 0.22) +
        geom_line(color = exec_navy, linewidth = 0.9) +
        geom_point(
          aes(
            text = paste0(
              "<b>", format(month_year, "%b %Y"), "</b><br>",
              "Severe Rate: ", percent(severe_rate, accuracy = 0.1), "<br>",
              "95% Confidence Interval (Likely Range): [", percent(rate_low, 0.1), ", ", percent(rate_high, 0.1), "]<br>",
              "Severe / Total: ", severe_accidents, " / ", total_accidents
            )
          ),
          color = exec_crimson, size = 1.9
        ) +
        scale_y_continuous(labels = percent_format()) +
        labs(x = "Month", y = "Severe Accident Rate (% of Monthly Crashes)")
    }
    
    p <- p +
      scale_x_datetime(date_labels = "%b %Y", date_breaks = "6 months") +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "#1A202C"))
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$p2_ts_sr_summary <- renderUI({
    ts_df <- p2_ts_computed()
    if (nrow(ts_df) == 0) return(NULL)
    if (input$p2_ts_metric == "count") {
      pk    <- ts_df %>% arrange(desc(severe_accidents)) %>% slice(1)
      avg_m <- round(mean(ts_df$severe_accidents, na.rm = TRUE), 1)
      msg   <- paste0(
        "Average monthly severe count is ", avg_m, " crashes. ",
        "Peak month occurred in ", format(pk$month_year, "%B %Y"), " with ", comma(pk$severe_accidents),
        " severe crashes."
      )
    } else {
      ts_v  <- ts_df %>% filter(!is.na(severe_rate))
      pk    <- ts_v %>% arrange(desc(severe_rate)) %>% slice(1)
      avg_r <- percent(mean(ts_v$severe_rate, na.rm = TRUE), 0.1)
      msg   <- paste0(
        "Average monthly severe rate is ", avg_r, ". ",
        "Highest monthly rate occurred in ", format(pk$month_year, "%B %Y"), " at ",
        percent(pk$severe_rate, 0.1), " (", pk$severe_accidents, " of ", pk$total_accidents, " crashes)."
      )
    }
    render_a11y_box(msg)
  })
  
  # Monthly Weather Table for Selected County (5 Core Weather Categories, min. monthly accidents = 5):
  p2_qualified_monthly_weather <- reactive({
    df_county <- p2_county_acc() %>% filter(!is.na(month_year))
    cnty_pop  <- first(df_county$population)
    
    monthly_all <- df_county %>%
      filter(!is.na(Weather_Category), Weather_Category != "") %>%
      group_by(Weather_Group = Weather_Category, month_year) %>%
      summarise(
        severe_accidents = sum(severe, na.rm = TRUE),
        accidents        = n(),
        .groups = "drop"
      ) %>%
      mutate(
        population   = cnty_pop,
        severe_rate  = severe_accidents / accidents,
        severe_ratio = (severe_accidents / population) * 100000
      )
    
    qual <- monthly_all %>% filter(accidents >= 5)
    if (nrow(qual) < 5) {
      qual <- monthly_all %>% filter(accidents >= 2)
    }
    if (nrow(qual) < 3) {
      qual <- monthly_all
    }
    qual
  })
  
  p2_weather_data <- reactive({
    qual_counts <- p2_qualified_monthly_weather() %>%
      count(Weather_Group, name = "qualifying_months")
    
    df <- p2_county_acc() %>%
      filter(!is.na(Weather_Category), Weather_Category != "") %>%
      group_by(Weather_Group = Weather_Category) %>%
      summarise(
        accidents            = n(),
        severe_accidents     = sum(severe, na.rm = TRUE),
        not_severe_accidents = sum(!severe, na.rm = TRUE),
        population           = first(population),
        severe_ratio         = (severe_accidents / population) * 100000,
        severe_rate          = severe_accidents / accidents,
        .groups = "drop"
      ) %>%
      left_join(qual_counts, by = "Weather_Group") %>%
      mutate(qualifying_months = replace_na(qualifying_months, 0L))
    
    m_all     <- max(1, nrow(df))
    alpha_adj <- get_adjusted_alpha(m_all, "bonferroni", alpha = 0.05)
    
    df %>%
      mutate(
        rate_ci_low  = calc_binom_ci(severe_accidents, accidents, "lower", conf_level = 1 - alpha_adj),
        rate_ci_high = calc_binom_ci(severe_accidents, accidents, "upper", conf_level = 1 - alpha_adj)
      )
  })
  
  output$p2_weather_plot <- renderPlotly({
    df <- p2_weather_data() %>%
      filter(qualifying_months >= 10) %>%
      slice_max(.data[[input$p2_weather_metric]], n = 8, with_ties = FALSE)
    
    if (nrow(df) == 0) {
      df <- p2_weather_data() %>%
        filter(qualifying_months >= 3) %>%
        slice_max(.data[[input$p2_weather_metric]], n = 8, with_ties = FALSE)
    }
    
    validate(
      need(nrow(df) > 0, "No weather categories in this locality meet the minimum active months threshold.")
    )
    
    is_rate <- input$p2_weather_metric == "severe_rate"
    y_lab   <- ifelse(
      is_rate,
      "Overall Severe Accident Rate (% of Weather Crashes)",
      "Cumulative Severe Crashes per 100,000 Residents"
    )
    
    max_val <- max(df[[input$p2_weather_metric]], na.rm = TRUE)
    if (!is.finite(max_val) || max_val <= 0) max_val <- 1
    
    df <- df %>%
      mutate(
        val_label = if (is_rate) percent(severe_rate, 0.1) else round(severe_ratio, 1),
        label_y   = .data[[input$p2_weather_metric]] + (max_val * 0.085)
      )
    
    p <- ggplot(
      df,
      aes(
        x = reorder(Weather_Group, .data[[input$p2_weather_metric]]),
        y = .data[[input$p2_weather_metric]],
        text = paste0(
          "<b>", Weather_Group, "</b><br>",
          "Active Months (n): ", qualifying_months, "<br>",
          "Overall Severe Rate: ", percent(severe_rate, accuracy = 0.1), "<br>",
          "Cumulative Severe Ratio (per 100k): ", round(severe_ratio, 2), "<br>",
          "Severe / Total Crashes: ", comma(severe_accidents), " / ", comma(accidents)
        )
      )
    ) +
      geom_col(fill = "#483D8B", alpha = 0.9, width = 0.65) +
      geom_text(
        aes(y = label_y, label = val_label),
        size     = 3.7,
        fontface = "bold",
        color    = "#1A202C"
      ) +
      coord_flip() +
      labs(x = NULL, y = y_lab) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(face = "bold", color = "#1A202C")
      )
    
    if (is_rate) {
      p <- p + scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.22)))
    } else {
      p <- p + scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(0, 0.22)))
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$p2_weather_sr_summary <- renderUI({
    df <- p2_weather_data() %>%
      filter(qualifying_months >= 10) %>%
      arrange(desc(.data[[input$p2_weather_metric]]))
    if (nrow(df) == 0) {
      df <- p2_weather_data() %>%
        filter(qualifying_months >= 3) %>%
        arrange(desc(.data[[input$p2_weather_metric]]))
    }
    if (nrow(df) == 0) return(NULL)
    top_w <- df %>% slice(1)
    
    msg <- paste0(
      "Top core weather category in ", input$p2_county, " is ", top_w$Weather_Group,
      " with an overall severe rate of ", percent(top_w$severe_rate, 0.1),
      " and ", round(top_w$severe_ratio, 2), " severe crashes per 100k residents."
    )
    render_a11y_box(msg)
  })
  
  p2_ci_data <- reactive({
    w_monthly  <- p2_qualified_monthly_weather()
    validate(need(nrow(w_monthly) > 0, "Insufficient monthly weather records in this locality."))
    
    target_col <- input$p2_weather_metric
    upper_cap  <- ifelse(target_col == "severe_rate", 1.0, Inf)
    
    compute_simple_ci(
      df          = w_monthly,
      group_col   = "Weather_Group",
      value_col   = target_col,
      pop_col     = "population",
      mult_method = "bonferroni",
      upper_cap   = upper_cap,
      min_months  = 10
    ) %>%
      slice_max(Mean_Est, n = 8, with_ties = FALSE)
  })
  
  output$p2_ci_header <- renderText({
    metric_lbl <- ifelse(
      input$p2_weather_metric == "severe_rate",
      "Monthly Severe Accident Rate (%)",
      "Monthly Severe Crashes per 100k Residents"
    )
    paste0(metric_lbl, " by Core Weather Category — 95% Confidence Intervals (Likely True Range): ", input$p2_county)
  })
  
  output$p2_ci_plot <- renderPlotly({
    df <- p2_ci_data()
    validate(need(nrow(df) > 0, "No weather categories have enough active months in this locality."))
    
    is_pct <- (input$p2_weather_metric == "severe_rate")
    x_lab  <- ifelse(
      is_pct,
      "Monthly Severe Accident Rate % (Monthly Mean ± 95% Likely Range)",
      "Monthly Severe Crashes per 100k Residents (Monthly Mean ± 95% Likely Range)"
    )
    
    # Assign integer row positions (1..N) and attach hover_text to both segments and diamond
    df <- df %>%
      arrange(Mean_Est) %>%
      mutate(
        y_num      = row_number(),
        cap_low    = y_num - 0.18,
        cap_high   = y_num + 0.18,
        hover_text = paste0(
          "<b>", Group, " (", input$p2_county, ")</b><br>",
          "Active Months Included (n): ", N_Units, " months<br>",
          "Monthly Average (Mean): ", ifelse(is_pct, percent(Mean_Est, 0.1), round(Mean_Est, 2)), "<br>",
          "95% Confidence Interval (Likely Range): [",
          ifelse(is_pct, percent(CI_Lower, 0.1), round(CI_Lower, 2)), ", ",
          ifelse(is_pct, percent(CI_Upper, 0.1), round(CI_Upper, 2)), "]"
        )
      )
    
    p <- ggplot(df) +
      # Horizontal confidence interval bar (with hover tooltip)
      geom_segment(
        aes(x = CI_Lower, xend = CI_Upper, y = y_num, yend = y_num, text = hover_text),
        color     = exec_teal,
        linewidth = 1.25
      ) +
      # Left vertical end-cap strictly confined to row (+/- 0.18 row height)
      geom_segment(
        aes(x = CI_Lower, xend = CI_Lower, y = cap_low, yend = cap_high, text = hover_text),
        color     = exec_teal,
        linewidth = 1.25
      ) +
      # Right vertical end-cap strictly confined to row (+/- 0.18 row height)
      geom_segment(
        aes(x = CI_Upper, xend = CI_Upper, y = cap_low, yend = cap_high, text = hover_text),
        color     = exec_teal,
        linewidth = 1.25
      ) +
      # Red diamond point estimate
      geom_point(
        aes(x = Mean_Est, y = y_num, text = hover_text),
        color = exec_crimson,
        shape = 18,
        size  = 4.0
      ) +
      scale_y_continuous(
        breaks = df$y_num,
        labels = df$Group,
        expand = expansion(add = c(0.6, 0.6))
      ) +
      labs(x = x_lab, y = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(face = "bold", color = "#1A202C")
      )
    
    if (is_pct) {
      p <- p + scale_x_continuous(labels = percent_format())
    } else {
      p <- p + scale_x_continuous(labels = comma_format())
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$p2_ci_sr_summary <- renderUI({
    df <- p2_ci_data() %>% arrange(desc(Mean_Est))
    if (nrow(df) == 0) return(NULL)
    is_pct <- (input$p2_weather_metric == "severe_rate")
    fmt    <- function(v) if (is_pct) percent(v, 0.1) else round(v, 2)
    top_c  <- df %>% slice(1)
    
    msg <- paste0(
      top_c$Group, " has the highest monthly average at ", fmt(top_c$Mean_Est),
      " (95% Confidence Interval: ", fmt(top_c$CI_Lower), " to ", fmt(top_c$CI_Upper),
      " across n = ", top_c$N_Units, " active months)."
    )
    render_a11y_box(msg)
  })
  
  output$p2_ci_table <- renderDT({
    df <- p2_ci_data() %>% arrange(desc(Mean_Est))
    is_pct <- (input$p2_weather_metric == "severe_rate")
    
    tbl <- df %>%
      select(Group, N_Units, Mean_Est, CI_Lower, CI_Upper)
    
    dt <- datatable(
      tbl,
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = "caption-side: top; font-weight: bold; color: #0F2042;",
        paste0("Sortable Data Table: Monthly Core Weather Category Averages & 95% CIs for ", input$p2_county)
      ),
      colnames = c("Core Weather Category", "Active Months (n)", "Monthly Average", "95% CI Low", "95% CI High"),
      options = list(pageLength = 8, dom = "tip", scrollX = TRUE)
    )
    
    if (is_pct) {
      dt <- dt %>% formatPercentage(c("Mean_Est", "CI_Lower", "CI_Upper"), digits = 1)
    } else {
      dt <- dt %>% formatRound(c("Mean_Est", "CI_Lower", "CI_Upper"), digits = 2)
    }
    dt
  })
  
  output$p2_summary_table <- renderDT({
    df <- p2_weather_data() %>%
      arrange(desc(severe_accidents)) %>%
      select(
        Weather_Group, qualifying_months, accidents, severe_accidents, not_severe_accidents,
        severe_rate, rate_ci_low, rate_ci_high, severe_ratio
      )
    
    datatable(
      df,
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = "caption-side: top; font-weight: bold; color: #0F2042;",
        paste0("Complete Core Weather Category Breakdown for ", input$p2_county)
      ),
      colnames = c(
        "Core Weather Category", "Active Months (n)", "Total Crashes", "Severe Crashes",
        "Non-Severe Crashes", "Overall Severe Rate (%)", "95% CI Low",
        "95% CI High", "Severe Crashes per 100k"
      ),
      options = list(pageLength = 10, scrollX = TRUE)
    ) %>%
      formatPercentage(c("severe_rate", "rate_ci_low", "rate_ci_high"), digits = 1) %>%
      formatRound("severe_ratio", digits = 2) %>%
      formatCurrency(c("accidents", "severe_accidents", "not_severe_accidents"), currency = "", interval = 3, mark = ",", digits = 0)
  })
  
  # Simple audit checks for the selected county
  p2_audit_results <- reactive({
    ts <- p2_ts_computed()
    weather <- p2_weather_data()
    ci <- tryCatch(p2_ci_data(), error = function(e) tibble())
    raw_weather <- p2_county_acc() %>%
      filter(!is.na(Weather_Condition), Weather_Condition != "") %>%
      mutate(Expected_Category = classify_weather_simple(Weather_Condition))
    
    check_status <- function(ok, has_data = TRUE) {
      if (!has_data) "REVIEW" else if (isTRUE(ok)) "PASS" else "FAIL"
    }
    
    poisson_ok <- nrow(ts) > 0 && all(
      ts$pois_low <= ts$severe_accidents & ts$pois_high >= ts$severe_accidents,
      na.rm = TRUE
    )
    rate_ok <- nrow(ts) > 0 && all(
      ts$rate_low <= ts$severe_rate & ts$rate_high >= ts$severe_rate &
        ts$rate_low >= 0 & ts$rate_high <= 1,
      na.rm = TRUE
    )
    weather_ci_ok <- nrow(weather) > 0 && all(
      weather$rate_ci_low <= weather$severe_rate &
        weather$rate_ci_high >= weather$severe_rate &
        weather$rate_ci_low >= 0 & weather$rate_ci_high <= 1,
      na.rm = TRUE
    )
    monthly_ci_ok <- nrow(ci) > 0 && all(
      ci$CI_Lower <= ci$Mean_Est & ci$CI_Upper >= ci$Mean_Est &
        ci$CI_Lower >= 0 &
        (input$p2_weather_metric != "severe_rate" | ci$CI_Upper <= 1),
      na.rm = TRUE
    )
    weather_counts_ok <- nrow(weather) > 0 && all(
      weather$severe_accidents + weather$not_severe_accidents == weather$accidents
    )
    grouping_ok <- nrow(raw_weather) > 0 && all(
      !is.na(raw_weather$Weather_Category) &
        raw_weather$Weather_Category == raw_weather$Expected_Category
    )
    
    tibble(
      Check = c(
        "Monthly Poisson CIs contain severe counts",
        "Monthly binomial CIs contain severe rates",
        "Weather binomial CIs contain severe rates",
        "Monthly weather CIs contain their means",
        "Weather severe + non-severe counts equal total",
        "Weather descriptions match assigned categories"
      ),
      Status = c(
        check_status(poisson_ok, nrow(ts) > 0),
        check_status(rate_ok, any(!is.na(ts$severe_rate))),
        check_status(weather_ci_ok, nrow(weather) > 0),
        check_status(monthly_ci_ok, nrow(ci) > 0),
        check_status(weather_counts_ok, nrow(weather) > 0),
        check_status(grouping_ok, nrow(raw_weather) > 0)
      )
    )
  })
  
  page_status_excel_sheets <- reactive({
    audit_checks <- tryCatch(
      p2_audit_results(),
      error = function(e) tibble(Check = "Audit calculation", Status = "REVIEW", Details = conditionMessage(e))
    )
    monthly_weather_ci <- tryCatch(
      p2_ci_data(),
      error = function(e) tibble(Note = paste("Unavailable:", conditionMessage(e)))
    )
    monthly_time_series_ci <- tryCatch(
      p2_ts_computed(),
      error = function(e) tibble(Note = paste("Unavailable:", conditionMessage(e)))
    )
    weather_binomial_ci <- tryCatch(
      p2_weather_data(),
      error = function(e) tibble(Note = paste("Unavailable:", conditionMessage(e)))
    )
    weather_classification <- p2_county_acc() %>%
      filter(!is.na(Weather_Condition), Weather_Condition != "") %>%
      mutate(Expected_Category = classify_weather_simple(Weather_Condition)) %>%
      count(Weather_Condition, Weather_Category, Expected_Category, name = "Accidents") %>%
      mutate(Check = if_else(Weather_Category == Expected_Category, "PASS", "FAIL")) %>%
      arrange(desc(Check), desc(Accidents))
    
    export_settings <- tibble(
      Setting = c(
        "Selected County", "Weather Grouping", "Time-Series Metric",
        "Weather Metric", "Multiple Comparisons Method",
        "Minimum Accidents per Month", "Minimum Qualifying Months"
      ),
      Value = c(
        input$p2_county, "Weather_Category (5 Core Weather Categories)",
        input$p2_ts_metric, input$p2_weather_metric,
        "bonferroni", "5", "10"
      )
    )
    
    list(
      Page_Status            = audit_checks,
      Monthly_Weather_CIs    = monthly_weather_ci,
      Time_Series_CIs        = monthly_time_series_ci,
      Weather_Binomial_CIs   = weather_binomial_ci,
      Weather_Classification = weather_classification,
      Export_Settings        = export_settings
    )
  })
  
  output$download_page_status <- downloadHandler(
    filename = function() {
      county_name <- gsub("[^A-Za-z0-9]+", "_", input$p2_county)
      paste0("Page_Status_", county_name, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      if (!requireNamespace("writexl", quietly = TRUE)) {
        stop("Please install writexl first using: install.packages('writexl')")
      }
      writexl::write_xlsx(page_status_excel_sheets(), path = file)
    }
  )
  
  # ----------------------------------------------------------------------------
  # PAGE 3 SERVER: STATEWIDE COUNTY SEVERE RATE MAP -> COUNTY STREET ZONES
  # ----------------------------------------------------------------------------
  observeEvent(input$p3_jurisdiction, {
    loc_choices <- if (input$p3_jurisdiction == "counties_only") only_counties else all_counties
    current_sel <- isolate(input$p3_county)
    new_sel     <- if (!is.null(current_sel) && current_sel %in% loc_choices) current_sel else "ALL_VA"
    updateSelectInput(
      session,
      "p3_county",
      choices  = c("All Qualifying Localities (Statewide Map)" = "ALL_VA", loc_choices),
      selected = new_sel
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$p3_reset_va, {
    updateSelectInput(session, "p3_county", selected = "ALL_VA")
  })
  
  observeEvent(input$p3_map_shape_click, {
    click <- input$p3_map_shape_click
    if (!is.null(click$id) && click$id %in% all_counties) {
      # If user clicks an independent city while "counties_only" is active, switch to all_localities automatically
      is_clicked_city <- county_pop_ranks %>%
        filter(census_name == click$id) %>%
        pull(is_city)
      if (length(is_clicked_city) > 0 && isTRUE(is_clicked_city[1]) && input$p3_jurisdiction == "counties_only") {
        updateRadioButtons(session, "p3_jurisdiction", selected = "all_localities")
        updateSelectInput(
          session,
          "p3_county",
          choices  = c("All Qualifying Localities (Statewide Map)" = "ALL_VA", all_counties),
          selected = click$id
        )
      } else {
        updateSelectInput(session, "p3_county", selected = click$id)
      }
    }
  })
  
  observeEvent(input$p3_year, {
    if (input$p3_year == "All") {
      updateDateRangeInput(
        session,
        "p3_date_range",
        start = min_acc_date,
        end   = max_acc_date
      )
    } else {
      yr_num <- as.numeric(input$p3_year)
      yr_dates <- acc %>%
        filter(Year == yr_num) %>%
        pull(Accident_Date)
      if (length(yr_dates) > 0) {
        updateDateRangeInput(
          session,
          "p3_date_range",
          start = min(yr_dates, na.rm = TRUE),
          end   = max(yr_dates, na.rm = TRUE)
        )
      }
    }
  }, ignoreInit = TRUE)
  
  p3_date_filtered_acc <- reactive({
    df <- acc %>% filter(!is.na(Start_Lat), !is.na(Start_Lng))
    if (!is.null(input$p3_date_range) && length(input$p3_date_range) == 2) {
      df <- df %>%
        filter(
          Accident_Date >= as.Date(input$p3_date_range[1]),
          Accident_Date <= as.Date(input$p3_date_range[2])
        )
    }
    if (input$p3_year != "All") {
      df <- df %>% filter(Year == as.numeric(input$p3_year))
    }
    df
  })
  
  p3_statewide_county_rates <- reactive({
    df_acc <- p3_date_filtered_acc()
    
    agg <- df_acc %>%
      group_by(census_name) %>%
      summarise(
        total_accidents  = n(),
        severe_accidents = sum(severe, na.rm = TRUE),
        .groups = "drop"
      )
    
    cnty_df <- county_pop_ranks %>%
      select(GEOID, census_name, is_city, population) %>%
      left_join(agg, by = "census_name") %>%
      mutate(
        include_jur       = if (input$p3_jurisdiction == "counties_only") !is_city else TRUE,
        total_accidents   = replace_na(total_accidents, 0L),
        severe_accidents  = replace_na(severe_accidents, 0L),
        severe_rate       = ifelse(total_accidents > 0, severe_accidents / total_accidents, 0),
        qualifies_rate    = include_jur & (total_accidents >= 20),
        severe_rate_map   = ifelse(qualifies_rate, severe_rate, NA_real_)
      )
    
    m_cnty    <- max(1, sum(cnty_df$include_jur & cnty_df$total_accidents > 0))
    alpha_adj <- get_adjusted_alpha(m_cnty, "bonferroni", alpha = 0.05)
    
    cnty_df %>%
      mutate(
        rate_ci_low  = calc_binom_ci(severe_accidents, total_accidents, "lower", conf_level = 1 - alpha_adj),
        rate_ci_high = calc_binom_ci(severe_accidents, total_accidents, "upper", conf_level = 1 - alpha_adj)
      ) %>%
      arrange(desc(qualifies_rate), desc(severe_rate), desc(severe_accidents))
  })
  
  p3_street_zone_summary <- reactive({
    req(input$p3_county != "ALL_VA")
    df_raw <- p3_date_filtered_acc() %>%
      filter(census_name == input$p3_county, !is.na(Street), Street != "")
    
    if (nrow(df_raw) == 0) return(tibble())
    
    grouped <- df_raw %>%
      mutate(
        lat_zone = round(Start_Lat, 2),
        lng_zone = round(Start_Lng, 2)
      ) %>%
      group_by(Street, census_name, lat_zone, lng_zone) %>%
      summarise(
        area_name        = first(City[City != ""]),
        total_accidents  = n(),
        severe_accidents = sum(severe, na.rm = TRUE),
        severe_rate      = severe_accidents / total_accidents,
        lat              = median(Start_Lat, na.rm = TRUE),
        lng              = median(Start_Lng, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        area_label  = ifelse(!is.na(area_name) & area_name != "", paste0("near ", area_name), "Zone"),
        Street_Zone = paste0(Street, " (", area_label, " [", sprintf("%.2f, %.2f", lat_zone, lng_zone), "])")
      )
    
    filtered <- grouped %>% filter(total_accidents >= 3)
    if (nrow(filtered) == 0) {
      filtered <- grouped
    }
    
    filtered <- filtered %>%
      arrange(desc(severe_accidents), desc(total_accidents), desc(severe_rate)) %>%
      slice_head(n = 30)
    
    m_zones   <- max(1, nrow(filtered))
    alpha_adj <- get_adjusted_alpha(m_zones, "bonferroni", alpha = 0.05)
    
    filtered %>%
      mutate(
        rate_ci_low  = calc_binom_ci(severe_accidents, total_accidents, "lower", conf_level = 1 - alpha_adj),
        rate_ci_high = calc_binom_ci(severe_accidents, total_accidents, "upper", conf_level = 1 - alpha_adj)
      )
  })
  
  output$p3_map_title <- renderText({
    if (input$p3_county == "ALL_VA") {
      jur_lbl <- ifelse(input$p3_jurisdiction == "counties_only", "Virginia Counties", "Virginia Localities")
      paste0("Step 1 — Statewide Map: ", jur_lbl, " Severe Accident Rate (Severe / Total Crashes)")
    } else {
      paste0("Step 2 — Street Zone Hotspots: ", input$p3_county, " (Top 30 Localized Zones)")
    }
  })
  
  output$p3_map_subtitle <- renderText({
    if (input$p3_county == "ALL_VA") {
      "Qualifying localities with ≥20 total crashes are shaded by Severe Accident Rate (%). Low-volume (<20 crashes) or excluded areas are shaded gray. Click any locality to inspect its street zones."
    } else {
      paste0(
        "Showing the Top 30 localized (~1 km) street zones in ", input$p3_county,
        ". Circle color = Severe Crash Count; Circle size = Total Crashes in zone. Click 'Zoom Out to Full Virginia Map' to return."
      )
    }
  })
  
  output$p3_table_title <- renderText({
    if (input$p3_county == "ALL_VA") {
      jur_lbl <- ifelse(input$p3_jurisdiction == "counties_only", "Virginia Counties", "Virginia Localities")
      paste0("Statewide ", jur_lbl, " Ranked by Severe Accident Rate (Severe / Total)")
    } else {
      paste0("Top Localized Street Zones in ", input$p3_county, " (Ranked by Severe Crashes)")
    }
  })
  
  output$p3_table_footer <- renderText({
    if (input$p3_county == "ALL_VA") {
      "Sortable Table: Lists severe accident rates and 95% Confidence Intervals (Likely True Ranges). Localities with ≥20 total crashes are ranked first."
    } else {
      "Sortable Street Zone Table: Lists the ~1 km corridor zones, severe crash counts, total crash counts, severe rates, and 95% Confidence Intervals."
    }
  })
  
  output$p3_map <- renderLeaflet({
    m <- leaflet() %>%
      addTiles(
        urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
        attribution = '© <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
        group = "Street Map"
      ) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Imagery")
    
    cnty_rates <- p3_statewide_county_rates()
    
    if (!is.null(va_boundaries_raw) && nrow(va_boundaries_raw) > 0) {
      va_map_df <- va_boundaries_raw %>%
        left_join(
          cnty_rates %>%
            select(GEOID, include_jur, total_accidents, severe_accidents, severe_rate, qualifies_rate, severe_rate_map, rate_ci_low, rate_ci_high),
          by = "GEOID"
        ) %>%
        mutate(
          total_accidents  = replace_na(total_accidents, 0L),
          severe_accidents = replace_na(severe_accidents, 0L),
          severe_rate      = replace_na(severe_rate, 0),
          qualifies_rate   = replace_na(qualifies_rate, FALSE)
        )
      
      rate_domain <- va_map_df$severe_rate_map[!is.na(va_map_df$severe_rate_map)]
      if (length(rate_domain) == 0) rate_domain <- c(0, 1)
      
      va_pal <- colorNumeric(
        palette  = "YlOrRd",
        domain   = rate_domain,
        na.color = "#CBD5E1"
      )
      
      m <- m %>%
        addPolygons(
          data        = va_map_df,
          layerId     = ~display_name,
          group       = "Virginia Counties",
          fillColor   = ~va_pal(severe_rate_map),
          fillOpacity = ifelse(input$p3_county == "ALL_VA", 0.68, 0.15),
          color       = "#334155",
          weight      = 1.2,
          opacity     = 0.85,
          highlightOptions = highlightOptions(
            weight       = 3,
            color        = exec_navy,
            fillOpacity  = ifelse(input$p3_county == "ALL_VA", 0.82, 0.30),
            bringToFront = FALSE
          ),
          label = ~paste0(
            display_name, " | Severe Rate: ", percent(severe_rate, accuracy = 0.1),
            " (", comma(severe_accidents), " severe / ", comma(total_accidents), " total)",
            ifelse(qualifies_rate, "", " [Excluded or <20 crashes]")
          ),
          popup = ~paste0(
            "<b>Locality: </b>", display_name, "<br>",
            "<b>Severe Accident Rate: </b>", percent(severe_rate, accuracy = 0.1),
            ifelse(!is.na(rate_ci_low), paste0(" (95% CI: ", percent(rate_ci_low, 0.1), "–", percent(rate_ci_high, 0.1), ")"), ""), "<br>",
            "<b>Severe Crashes: </b>", comma(severe_accidents), "<br>",
            "<b>Total Crashes: </b>", comma(total_accidents)
          )
        )
      
      if (input$p3_county == "ALL_VA") {
        m <- m %>%
          fitBounds(lng1 = -83.68, lat1 = 36.54, lng2 = -75.24, lat2 = 39.47) %>%
          addLegend(
            position  = "bottomright",
            pal       = va_pal,
            values    = rate_domain,
            title     = "Severe Rate (%)<br><small>(Gray = <20 crashes / Excluded)</small>",
            labFormat = labelFormat(suffix = "%", transform = function(x) round(x * 100, 0)),
            opacity   = 0.85
          )
      } else {
        selected_poly <- va_map_df %>% filter(display_name == input$p3_county)
        if (nrow(selected_poly) > 0) {
          bbox <- st_bbox(selected_poly)
          m <- m %>%
            addPolygons(
              data        = selected_poly,
              fill        = TRUE,
              fillColor   = exec_crimson,
              fillOpacity = 0.05,
              color       = exec_crimson,
              weight      = 3.5,
              opacity     = 1.0,
              group       = "Selected County Boundary"
            ) %>%
            fitBounds(
              lng1 = as.numeric(bbox["xmin"]),
              lat1 = as.numeric(bbox["ymin"]),
              lng2 = as.numeric(bbox["xmax"]),
              lat2 = as.numeric(bbox["ymax"])
            )
        }
        
        zones_df <- p3_street_zone_summary()
        if (nrow(zones_df) > 0) {
          zone_pal <- colorNumeric(
            palette = "plasma",
            domain  = zones_df$severe_accidents,
            reverse = TRUE
          )
          
          max_acc <- max(zones_df$total_accidents, 1)
          radii   <- 7 + 14 * sqrt(zones_df$total_accidents / max_acc)
          
          m <- m %>%
            addCircleMarkers(
              data        = zones_df,
              lng         = ~lng,
              lat         = ~lat,
              radius      = radii,
              fillColor   = ~zone_pal(severe_accidents),
              color       = "#1A202C",
              weight      = 1.5,
              fillOpacity = 0.90,
              group       = "Street Zone Hotspots",
              label       = ~paste0(
                Street_Zone, " — ", comma(severe_accidents), " severe / ",
                comma(total_accidents), " total (", percent(severe_rate, accuracy = 0.1), ")"
              ),
              popup       = ~paste0(
                "<b>Street Zone: </b>", Street_Zone, "<br>",
                "<b>Locality: </b>", census_name, "<br>",
                "<b>Severe Crashes: </b>", comma(severe_accidents), "<br>",
                "<b>Total Crashes in Zone: </b>", comma(total_accidents), "<br>",
                "<b>Zone Severe Rate: </b>", percent(severe_rate, accuracy = 0.1),
                " (95% CI: ", percent(rate_ci_low, 0.1), "–", percent(rate_ci_high, 0.1), ")"
              )
            ) %>%
            addLegend(
              position = "bottomright",
              pal      = zone_pal,
              values   = zones_df$severe_accidents,
              title    = "Zone Severe Crashes<br><small>(Size = Total Crashes)</small>",
              opacity  = 0.90
            )
        }
      }
    }
    
    m %>%
      addLayersControl(
        baseGroups = c("Street Map", "Satellite Imagery"),
        options    = layersControlOptions(collapsed = TRUE)
      )
  })
  
  output$p3_map_sr_summary <- renderUI({
    if (input$p3_county == "ALL_VA") {
      df <- p3_statewide_county_rates() %>% filter(qualifies_rate)
      if (nrow(df) == 0) return(NULL)
      c1 <- df %>% slice(1)
      c2 <- if (nrow(df) >= 2) df %>% slice(2) else NULL
      msg <- paste0(
        "Highest severe accident rate is in ", c1$census_name, " at ", percent(c1$severe_rate, 0.1),
        " (", comma(c1$severe_accidents), " severe out of ", comma(c1$total_accidents), " crashes). ",
        if (!is.null(c2)) paste0("Second highest is ", c2$census_name, " at ", percent(c2$severe_rate, 0.1), ".") else ""
      )
      render_a11y_box(msg)
    } else {
      df <- p3_street_zone_summary()
      if (nrow(df) == 0) return(NULL)
      z1 <- df %>% slice(1)
      z2 <- if (nrow(df) >= 2) df %>% slice(2) else NULL
      msg <- paste0(
        "Top hotspot zone in ", input$p3_county, " is ", z1$Street_Zone, " with ",
        comma(z1$severe_accidents), " severe crashes out of ", comma(z1$total_accidents),
        " total crashes (", percent(z1$severe_rate, 0.1), " severe rate). ",
        if (!is.null(z2)) paste0("Second highest zone is ", z2$Street_Zone, " with ", comma(z2$severe_accidents), " severe crashes.") else ""
      )
      render_a11y_box(msg)
    }
  })
  
  output$p3_street_table <- renderDT({
    if (input$p3_county == "ALL_VA") {
      df <- p3_statewide_county_rates() %>%
        filter(include_jur, total_accidents > 0) %>%
        mutate(Volume_Note = ifelse(total_accidents >= 20, "≥20 Crashes", "<20 Crashes (Low n)")) %>%
        select(
          census_name, total_accidents, severe_accidents,
          severe_rate, rate_ci_low, rate_ci_high, Volume_Note
        )
      
      validate(need(nrow(df) > 0, "No locality records available for the selected date range."))
      
      datatable(
        df,
        rownames = FALSE,
        caption = htmltools::tags$caption(
          style = "caption-side: top; font-weight: bold; color: #0F2042;",
          "Statewide Severe Accident Rate Ranking Table"
        ),
        colnames = c(
          "Locality", "Total Crashes", "Severe Crashes",
          "Severe Rate (%)", "95% CI Low", "95% CI High", "Sample Size"
        ),
        options = list(pageLength = 12, scrollX = TRUE)
      ) %>%
        formatPercentage(c("severe_rate", "rate_ci_low", "rate_ci_high"), digits = 1) %>%
        formatCurrency(c("total_accidents", "severe_accidents"), currency = "", interval = 3, mark = ",", digits = 0)
    } else {
      df <- p3_street_zone_summary()
      validate(need(nrow(df) > 0, "No geocoded street zone records available for this locality and date range."))
      
      tbl <- df %>%
        select(
          Street_Zone, severe_accidents, total_accidents,
          severe_rate, rate_ci_low, rate_ci_high
        )
      
      datatable(
        tbl,
        rownames = FALSE,
        caption = htmltools::tags$caption(
          style = "caption-side: top; font-weight: bold; color: #0F2042;",
          paste0("Top Localized ~1 km Street Zones in ", input$p3_county)
        ),
        colnames = c(
          "Street & Localized Zone (~1 km)", "Severe Crashes", "Total Crashes",
          "Zone Severe Rate (%)", "95% CI Low", "95% CI High"
        ),
        options = list(pageLength = 12, scrollX = TRUE)
      ) %>%
        formatPercentage(c("severe_rate", "rate_ci_low", "rate_ci_high"), digits = 1) %>%
        formatCurrency(c("severe_accidents", "total_accidents"), currency = "", interval = 3, mark = ",", digits = 0)
    }
  })
}

shinyApp(ui = ui, server = server)