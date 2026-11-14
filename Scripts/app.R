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

options(tigris_use_cache = TRUE)

# ==============================================================================
# 1. REPRODUCIBLE DATA LOADING & PREPROCESSING
# ==============================================================================
data_paths <- c(
  "../Data/Virginia_Accidents_With_Pop.csv",
  "Data/Virginia_Accidents_With_Pop.csv",
  "Virginia_Accidents_With_Pop.csv"
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
    Year              = year(Start_Time),
    month_year        = floor_date(Start_Time, unit = "month"),
    severe            = Severity >= 3,
    GEOID             = str_pad(as.character(GEOID), width = 5, side = "left", pad = "0"),
    Weather_Condition = str_squish(Weather_Condition),
    Weather_Category  = classify_weather_simple(Weather_Condition),
    Street            = str_squish(Street),
    census_name       = str_squish(census_name)
  ) %>%
  filter(!is.na(census_name), !is.na(Severity), !is.na(Year))

# Pre-compute static county population & statewide accident summary for the VA map
county_pop_ranks <- acc %>%
  group_by(GEOID, census_name) %>%
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

n_localities <- nrow(county_pop_ranks)
mid_start    <- max(1, floor((n_localities - 5) / 2) + 1)
mid_indices  <- mid_start:min(n_localities, mid_start + 4)

top5_pop_names    <- county_pop_ranks %>% slice_head(n = 5) %>% pull(census_name)
middle5_pop_names <- county_pop_ranks %>% slice(mid_indices) %>% pull(census_name)
bottom5_pop_names <- county_pop_ranks %>% slice_tail(n = 5) %>% pull(census_name)

all_years    <- sort(unique(acc$Year))
all_counties <- county_pop_ranks$census_name

# Load all 133 Virginia county/city boundaries and join county summary metrics
va_boundaries <- tryCatch({
  counties(state = "VA", cb = TRUE, year = 2022, progress_bar = FALSE) %>%
    st_transform(4326) %>%
    left_join(county_pop_ranks, by = "GEOID") %>%
    mutate(
      display_name     = coalesce(census_name, NAMELSAD, NAME),
      total_accidents  = replace_na(total_accidents, 0L),
      severe_accidents = replace_na(severe_accidents, 0L),
      severe_rate      = replace_na(severe_rate, 0),
      severe_ratio     = replace_na(severe_ratio, 0)
    )
}, error = function(e) {
  NULL
})

# ==============================================================================
# SIMPLE, STANDARD STATISTICAL HELPER FUNCTIONS (CONFIDENCE INTERVALS ONLY)
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

# Helper to wrap dynamic screen-reader / plain-language summaries
render_a11y_box <- function(text_content, show_visible = TRUE) {
  div_class <- if (isTRUE(show_visible)) {
    "alert alert-secondary mt-2 mb-0 py-2 px-3 small"
  } else {
    "visually-hidden"
  }
  tags$div(
    class = div_class,
    role = "region",
    `aria-live` = "polite",
    `aria-label` = "Accessible Chart Summary",
    tags$strong(bs_icon("universal-access"), " Screen-Reader & Text Summary: "),
    text_content
  )
}

exec_navy    <- "#0F2042"
exec_crimson <- "#9E1B32"
exec_teal    <- "#0072B2"
exec_amber   <- "#D55E00"
exec_slate   <- "#4A5568"

# ==============================================================================
# 2. USER INTERFACE
# ==============================================================================
ui <- page_navbar(
  title = tags$span(
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
  header = tags$div(
    class = "bg-light border-bottom px-3 py-2 d-flex flex-wrap justify-content-between align-items-center",
    role = "region",
    `aria-label` = "Global Accessibility and Excel Export Toolbar",
    tags$div(
      class = "d-flex align-items-center gap-3",
      tags$span(
        class = "fw-bold text-primary",
        bs_icon("universal-access-circle"),
        " Accessibility Controls:"
      ),
      checkboxInput(
        "a11y_show_text",
        "Show Visible Screen-Reader Text Summaries Below Visuals",
        value = TRUE,
        width = "auto"
      ),
      checkboxInput(
        "a11y_high_contrast",
        "High-Contrast / Large-Print & Direct Value Labels on Charts",
        value = FALSE,
        width = "auto"
      )
    ),
    tags$div(
      downloadButton(
        "download_top5_excel",
        "Download Top 5 Worst Counties Summary (.xlsx)",
        class = "btn-sm btn-danger fw-bold",
        `aria-label` = "Download Excel workbook summarizing the Top 5 worst Virginia counties for traffic accidents including rates, means, and ratios"
      )
    )
  ),
  
  # ============================================================================
  # PAGE 1: STATEWIDE & TIERED COUNTY EXECUTIVE SUMMARY
  # ============================================================================
  nav_panel(
    title = "1. Statewide & Population Tier Summary",
    icon = bs_icon("bar-chart-line-fill"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Executive Filters",
        width = 320,
        selectInput(
          "p1_pop_tier",
          "1. Locality Population Tier:",
          choices = c(
            "Top 5 in Population"     = "top5",
            "Middle 5 in Population"  = "mid5",
            "Bottom 5 in Population"  = "bot5",
            "Top 10 in Population"    = "top10",
            "All Virginia Localities" = "all"
          ),
          selected = "top5"
        ),
        selectInput(
          "p1_year",
          "2. Analysis Year:",
          choices = c("All Years (2016–2023)" = "All", as.character(all_years)),
          selected = "All"
        ),
        radioButtons(
          "p1_metric",
          "3. Primary Bar Chart Metric:",
          choices = c(
            "Total Accidents (Raw Count)"                = "total_accidents",
            "Severe Accident Rate (% of Accidents)"      = "severe_rate",
            "Severe Accident Ratio (per 100k Residents)" = "severe_ratio"
          ),
          selected = "severe_ratio"
        ),
        hr(),
        selectInput(
          "p1_ci_type",
          "4. Dedicated Confidence Interval View:",
          choices = c(
            "Monthly Severe Ratio 95% CI (CIPI)"      = "CIPI",
            "Monthly Severe Rate 95% CI (CIPIrate)"   = "CIPIrate",
            "Weather Severe Rate Binomial CI (SARWC)" = "SARWC"
          ),
          selected = "CIPI"
        ),
        radioButtons(
          "p1_mult_method",
          "5. Multiple Comparisons Adjustment (FWER = 5%):",
          choices = c(
            "Bonferroni Simultaneous (α / m)"        = "bonferroni",
            "Šidák Simultaneous (1 - (1 - α)^(1/m))" = "sidak",
            "Unadjusted Marginal (α = 0.05)"         = "none"
          ),
          selected = "bonferroni"
        ),
        sliderInput(
          "p1_min_weather_n",
          "Min. Accidents per Weather Condition:",
          min = 5, max = 100, value = 20, step = 5
        )
      ),
      
      # Screen-reader live region for KPI banner
      uiOutput("p1_kpi_sr_summary"),
      
      layout_column_wrap(
        width = 1/3,
        value_box(
          title = "Total Recorded Accidents (Selection)",
          value = textOutput("p1_kpi_total"),
          showcase = bs_icon("car-front-fill"),
          theme = "primary"
        ),
        value_box(
          title = "Overall Severe Accident Rate",
          value = textOutput("p1_kpi_rate"),
          showcase = bs_icon("exclamation-triangle-fill"),
          theme = "danger"
        ),
        value_box(
          title = "Severe Accidents per 100k Residents",
          value = textOutput("p1_kpi_ratio"),
          showcase = bs_icon("people-fill"),
          theme = "secondary"
        )
      ),
      
      layout_column_wrap(
        width = 1/2,
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "County Comparison by Selected Metric & Population Tier"
          ),
          card_body(
            plotlyOutput("p1_county_plot", height = "390px"),
            uiOutput("p1_county_sr_summary")
          ),
          card_footer(
            class = "text-muted small",
            "Point estimates shown for clear magnitude comparison. See dedicated Confidence Interval panel below for simultaneous 95% CIs."
          )
        ),
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Top 10 Weather Conditions Associated with Severe Accidents"
          ),
          card_body(
            plotlyOutput("p1_weather_plot", height = "390px"),
            uiOutput("p1_weather_sr_summary")
          ),
          card_footer(
            class = "text-muted small",
            "Filtered dynamically by the selected Population Tier and Year. Select 'SARWC' below to inspect simultaneous 95% CIs."
          )
        )
      ),
      
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-dark text-white",
          textOutput("p1_ci_header")
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
          class = "text-muted small",
          "Red diamond = Point Estimate (Monthly Average or Overall Rate); Blue horizontal bar = Simultaneous 95% Confidence Interval adjusted for multiple comparisons."
        )
      )
    )
  ),
  
  # ============================================================================
  # PAGE 2: COUNTY DEEP-DIVE, TIME SERIES & SIMPLE MONTHLY WEATHER CIs
  # ============================================================================
  nav_panel(
    title = "2. County Deep-Dive & Time Series",
    icon = bs_icon("graph-up"),
    layout_sidebar(
      sidebar = sidebar(
        title = "County Deep-Dive Controls",
        width = 330,
        selectInput(
          "p2_county",
          "1. Select Virginia Locality:",
          choices = all_counties,
          selected = if ("Fairfax County" %in% all_counties) "Fairfax County" else all_counties[1]
        ),
        radioButtons(
          "p2_weather_grouping",
          "2. Weather Grouping Level:",
          choices = c(
            "5 Core Weather Categories (Best Sample Size n)" = "Weather_Category",
            "Raw NOAA Weather Strings (Only High-n Shown)"   = "Weather_Condition"
          ),
          selected = "Weather_Category"
        ),
        radioButtons(
          "p2_ts_metric",
          "3. Time Series Metric (Plot ts):",
          choices = c(
            "Monthly Severe Accident Count (Poisson CI)" = "count",
            "Monthly Severe Rate % (Binomial CI)"        = "rate"
          ),
          selected = "count"
        ),
        radioButtons(
          "p2_weather_metric",
          "4. Weather Bar Chart & 95% CI Metric:",
          choices = c(
            "Severe Rate (% of Accidents in Weather)"  = "severe_rate",
            "Severe Ratio (per 100k County Residents)" = "severe_ratio"
          ),
          selected = "severe_rate"
        ),
        hr(),
        radioButtons(
          "p2_mult_method",
          "5. Multiple Comparisons Adjustment (FWER = 5%):",
          choices = c(
            "Bonferroni Simultaneous (α / m)"        = "bonferroni",
            "Šidák Simultaneous (1 - (1 - α)^(1/m))" = "sidak",
            "Unadjusted Marginal (α = 0.05)"         = "none"
          ),
          selected = "bonferroni"
        ),
        sliderInput(
          "p2_min_monthly_acc",
          "Min. Accidents in a Month to Count as a Valid Month:",
          min = 1, max = 15, value = 5, step = 1
        ),
        sliderInput(
          "p2_min_months",
          "Min. Valid Months Required per Weather Bar (n):",
          min = 5, max = 30, value = 10, step = 1
        ),
        hr(),
        tags$small(
          class = "text-muted",
          bs_icon("check-circle"),
          " Simple Rule: Only months with at least 5 accidents are averaged, and only weather types with at least 10 such months are plotted. This keeps the standard Average ± Margin of Error (95% CI) simple, accurate, and easy to explain."
        )
      ),
      
      # Page 2 KPI banner (updates with the selected county)
      uiOutput("p2_kpi_sr_summary"),
      layout_column_wrap(
        width = 1/3,
        value_box(
          title = "Total Recorded Accidents (Selection)",
          value = textOutput("p2_kpi_total"),
          showcase = bs_icon("car-front-fill"),
          theme = "primary"
        ),
        value_box(
          title = "Overall Severe Accident Rate",
          value = textOutput("p2_kpi_rate"),
          showcase = bs_icon("exclamation-triangle-fill"),
          theme = "danger"
        ),
        value_box(
          title = "Severe Accidents per 100k Residents",
          value = textOutput("p2_kpi_ratio"),
          showcase = bs_icon("people-fill"),
          theme = "secondary"
        )
      ),
      
      layout_column_wrap(
        width = 1/2,
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            textOutput("p2_ts_title")
          ),
          card_body(
            plotlyOutput("p2_ts_plot", height = "390px"),
            uiOutput("p2_ts_sr_summary")
          ),
          card_footer(
            class = "text-muted small",
            "Shaded ribbon shows simultaneous 95% confidence intervals surrounding each month."
          )
        ),
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            textOutput("p2_weather_title")
          ),
          card_body(
            plotlyOutput("p2_weather_plot", height = "390px"),
            uiOutput("p2_weather_sr_summary")
          ),
          card_footer(
            class = "text-muted small",
            "Clean bar chart (Plot 'b') of top weather conditions in the selected locality without overlapping error bars."
          )
        )
      ),
      
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-dark text-white",
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
          class = "text-muted small",
          "How to read this chart: For each weather type with at least n qualifying months, the red diamond is the simple monthly average (Mean) and the blue horizontal bar is the Simultaneous 95% Confidence Interval (Mean ± t * SE)."
        )
      ),
      
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-secondary text-white",
          "Complete County Summary Table: Counts, Qualifying Months, Severe Rates, and Per-Capita Ratios by Weather"
        ),
        card_body(
          DTOutput("p2_summary_table")
        )
      ),
      
      # Page 2 download button
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-dark text-white",
          "Page 2 Confidence Interval Audit"
        ),
        card_body(
          downloadButton(
            "download_page_status",
            "Download Page Status",
            class = "btn-primary"
          )
        )
      )
    )
  ),
  
  # ============================================================================
  # PAGE 3: INTERACTIVE VIRGINIA & COUNTY STREET HOTSPOT GIS MAP
  # ============================================================================
  nav_panel(
    title = "3. Virginia & Street Hotspot GIS Map",
    icon = bs_icon("geo-alt-fill"),
    layout_sidebar(
      sidebar = sidebar(
        title = "GIS Map Controls",
        width = 320,
        selectInput(
          "p3_county",
          "1. Select Virginia Locality (or Click Map):",
          choices = c("All Virginia (Statewide Overview)" = "ALL_VA", all_counties),
          selected = if ("Fairfax County" %in% all_counties) "Fairfax County" else "ALL_VA"
        ),
        actionButton(
          "p3_reset_va",
          "Zoom Out to Full Virginia Map",
          icon = icon("globe-americas"),
          class = "btn-outline-primary w-100 mb-3"
        ),
        radioButtons(
          "p3_map_mode",
          "2. County Detail Overlay:",
          choices = c(
            "Street-Level Hotspots (Grouped by Street)" = "streets",
            "Individual Severe Accident Points"         = "points"
          ),
          selected = "streets"
        ),
        radioButtons(
          "p3_street_metric",
          "3. Street Ranking & Color Metric:",
          choices = c(
            "Severe Accident Count (Volume Hotspot)"     = "severe_accidents",
            "Severe Accident Rate (% Severe on Street)"  = "severe_rate",
            "Severe Accidents per 100k County Residents" = "severe_ratio"
          ),
          selected = "severe_accidents"
        ),
        sliderInput(
          "p3_min_street_n",
          "Min. Total Accidents on Street:",
          min = 1, max = 30, value = 3, step = 1
        ),
        sliderInput(
          "p3_top_n_streets",
          "Number of Top Streets to Display:",
          min = 5, max = 100, value = 25, step = 5
        ),
        hr(),
        tags$small(
          class = "text-muted",
          bs_icon("hand-index-thumb"),
          " Tip: Click any county directly on the Virginia map to zoom in and inspect its highest-risk streets, or use the accessible Street Table on the right for full keyboard and screen-reader access."
        )
      ),
      
      layout_column_wrap(
        width = 1/2,
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            textOutput("p3_map_title")
          ),
          card_body(
            leafletOutput("p3_map", height = "540px"),
            uiOutput("p3_map_sr_summary")
          ),
          card_footer(
            class = "text-muted small",
            textOutput("p3_map_subtitle")
          )
        ),
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-dark text-white",
            textOutput("p3_table_title")
          ),
          card_body(
            DTOutput("p3_street_table")
          ),
          card_footer(
            class = "text-muted small",
            "Non-interactive accessible path: Contains the exact street-level counts, rates, ratios, and simultaneous 95% binomial CIs plotted on the map."
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
  # EXCEL DOWNLOAD HANDLER: TOP 5 WORST COUNTIES COMPREHENSIVE SUMMARY
  # ----------------------------------------------------------------------------
  top5_worst_excel_sheets <- reactive({
    # Filter by selected year on Page 1 (or all years)
    df_filtered <- if (input$p1_year == "All") {
      acc
    } else {
      acc %>% filter(Year == as.numeric(input$p1_year))
    }
    
    all_months <- seq(min(df_filtered$month_year, na.rm = TRUE),
                      max(df_filtered$month_year, na.rm = TRUE),
                      by = "1 month")
    
    # 1. Monthly metrics per county (for Means and 95% CIs)
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
      left_join(county_pop_ranks %>% select(census_name, population), by = "census_name") %>%
      mutate(
        severe_ratio = (severe_accidents / population) * 100000,
        severe_rate  = ifelse(accidents == 0, NA_real_, severe_accidents / accidents)
      )
    
    monthly_stats <- monthly_cnty %>%
      group_by(census_name) %>%
      summarise(
        Total_Months_Observed      = n(),
        Active_Months_With_Crashes = sum(accidents > 0, na.rm = TRUE),
        Mean_Monthly_Total_Acc     = round(mean(accidents, na.rm = TRUE), 2),
        Mean_Monthly_Severe_Acc    = round(mean(severe_accidents, na.rm = TRUE), 2),
        Mean_Monthly_Severe_Ratio  = round(mean(severe_ratio, na.rm = TRUE), 2),
        SE_Monthly_Ratio           = sd(severe_ratio, na.rm = TRUE) / sqrt(n()),
        Monthly_Ratio_95CI_Low     = round(pmax(0, Mean_Monthly_Severe_Ratio - qt(0.975, df = pmax(1, n() - 1)) * SE_Monthly_Ratio), 2),
        Monthly_Ratio_95CI_High    = round(Mean_Monthly_Severe_Ratio + qt(0.975, df = pmax(1, n() - 1)) * SE_Monthly_Ratio, 2),
        Mean_Monthly_Severe_Rate_Pct = round(mean(severe_rate, na.rm = TRUE) * 100, 2),
        SE_Monthly_Rate            = sd(severe_rate, na.rm = TRUE) / sqrt(pmax(1, sum(!is.na(severe_rate)))),
        Monthly_Rate_95CI_Low_Pct  = round(pmax(0, (mean(severe_rate, na.rm = TRUE) - qt(0.975, df = pmax(1, sum(!is.na(severe_rate)) - 1)) * SE_Monthly_Rate) * 100), 2),
        Monthly_Rate_95CI_High_Pct = round(pmin(100, (mean(severe_rate, na.rm = TRUE) + qt(0.975, df = pmax(1, sum(!is.na(severe_rate)) - 1)) * SE_Monthly_Rate) * 100), 2),
        .groups = "drop"
      ) %>%
      select(-SE_Monthly_Ratio, -SE_Monthly_Rate)
    
    # 2. Overall cumulative county metrics
    overall_cnty <- county_pop_ranks %>%
      select(GEOID, census_name, population) %>%
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
        Total_Accidents          = replace_na(Total_Accidents, 0L),
        Severe_Accidents         = replace_na(Severe_Accidents, 0L),
        Overall_Severe_Rate_Pct  = round(ifelse(Total_Accidents > 0, (Severe_Accidents / Total_Accidents) * 100, 0), 2),
        Overall_Severe_Ratio_100k = round((Severe_Accidents / population) * 100000, 2)
      ) %>%
      left_join(monthly_stats, by = "census_name") %>%
      rename(Locality = census_name, Census_Population_2023 = population)
    
    list(
      Top5_By_Severe_Count = overall_cnty %>%
        arrange(desc(Severe_Accidents)) %>%
        slice_head(n = 5) %>%
        mutate(Ranking_Criterion = "Top 5 Worst by Raw Severe Accident Count", .before = 1),
      Top5_By_Severe_Ratio_100k = overall_cnty %>%
        arrange(desc(Overall_Severe_Ratio_100k)) %>%
        slice_head(n = 5) %>%
        mutate(Ranking_Criterion = "Top 5 Worst by Severe Accidents per 100k Residents", .before = 1),
      Top5_By_Severe_Rate_Pct = overall_cnty %>%
        filter(Total_Accidents >= 50) %>%
        arrange(desc(Overall_Severe_Rate_Pct)) %>%
        slice_head(n = 5) %>%
        mutate(Ranking_Criterion = "Top 5 Worst by Severe Rate % (Min 50 Accidents)", .before = 1),
      Current_Tier_Summary = overall_cnty %>%
        filter(Locality %in% p1_selected_counties()) %>%
        arrange(desc(Overall_Severe_Ratio_100k))
    )
  })
  
  output$download_top5_excel <- downloadHandler(
    filename = function() {
      yr_tag <- ifelse(input$p1_year == "All", "2016_2023", input$p1_year)
      ext <- if (requireNamespace("writexl", quietly = TRUE)) ".xlsx" else ".csv"
      paste0("VA_Top5_Worst_Traffic_Counties_Summary_", yr_tag, ext)
    },
    content = function(file) {
      sheets <- top5_worst_excel_sheets()
      if (requireNamespace("writexl", quietly = TRUE)) {
        writexl::write_xlsx(sheets, path = file)
      } else {
        # Fallback if writexl is not installed: bind top 5 tables into a single CSV
        combined_csv <- bind_rows(
          sheets$Top5_By_Severe_Count,
          sheets$Top5_By_Severe_Ratio_100k,
          sheets$Top5_By_Severe_Rate_Pct
        )
        write.csv(combined_csv, file, row.names = FALSE)
      }
    }
  )
  
  # ----------------------------------------------------------------------------
  # PAGE 1 SERVER
  # ----------------------------------------------------------------------------
  p1_selected_counties <- reactive({
    switch(
      input$p1_pop_tier,
      "top5"  = top5_pop_names,
      "mid5"  = middle5_pop_names,
      "bot5"  = bottom5_pop_names,
      "top10" = county_pop_ranks %>% slice_head(n = 10) %>% pull(census_name),
      "all"   = all_counties
    )
  })
  
  p1_filtered_acc <- reactive({
    df <- acc %>% filter(census_name %in% p1_selected_counties())
    if (input$p1_year != "All") {
      df <- df %>% filter(Year == as.numeric(input$p1_year))
    }
    df
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
  
  output$p1_kpi_total <- renderText({
    comma(sum(p1_county_summary()$total_accidents, na.rm = TRUE))
  })
  
  output$p1_kpi_rate <- renderText({
    tot_acc <- sum(p1_county_summary()$total_accidents, na.rm = TRUE)
    tot_sev <- sum(p1_county_summary()$severe_accidents, na.rm = TRUE)
    if (tot_acc == 0) return("0.0%")
    percent(tot_sev / tot_acc, accuracy = 0.1)
  })
  
  output$p1_kpi_ratio <- renderText({
    tot_pop <- sum(p1_county_summary()$population, na.rm = TRUE)
    tot_sev <- sum(p1_county_summary()$severe_accidents, na.rm = TRUE)
    if (tot_pop == 0) return("0.0")
    number((tot_sev / tot_pop) * 100000, accuracy = 0.1, big.mark = ",")
  })
  
  # Screen-reader summary for Page 1 KPIs
  output$p1_kpi_sr_summary <- renderUI({
    df <- p1_county_summary()
    tot_acc <- sum(df$total_accidents, na.rm = TRUE)
    tot_sev <- sum(df$severe_accidents, na.rm = TRUE)
    tot_pop <- sum(df$population, na.rm = TRUE)
    rate_str <- if (tot_acc > 0) percent(tot_sev / tot_acc, 0.1) else "0.0%"
    ratio_str <- if (tot_pop > 0) number((tot_sev / tot_pop) * 100000, 0.1, big.mark = ",") else "0.0"
    
    msg <- paste0(
      "Executive Summary for selected tier (", nrow(df), " localities, Year: ", input$p1_year, "): ",
      comma(tot_acc), " total accidents recorded, of which ", comma(tot_sev),
      " (", rate_str, ") were severe, equating to ", ratio_str, " severe accidents per 100,000 residents."
    )
    render_a11y_box(msg, input$a11y_show_text)
  })
  
  output$p1_county_plot <- renderPlotly({
    df <- p1_county_summary()
    if (input$p1_pop_tier == "all") {
      df <- df %>% slice_max(.data[[input$p1_metric]], n = 15)
    }
    
    metric_label <- switch(
      input$p1_metric,
      "total_accidents" = "Total Recorded Accidents",
      "severe_rate"     = "Severe Accident Rate (% of County Accidents)",
      "severe_ratio"    = "Severe Accidents per 100,000 Residents"
    )
    
    df <- df %>%
      mutate(
        val_label = case_when(
          input$p1_metric == "severe_rate"     ~ percent(severe_rate, accuracy = 0.1),
          input$p1_metric == "severe_ratio"    ~ number(severe_ratio, accuracy = 0.1, big.mark = ","),
          TRUE                                 ~ comma(total_accidents)
        )
      )
    
    base_sz <- ifelse(isTRUE(input$a11y_high_contrast), 14, 12)
    
    p <- ggplot(
      df,
      aes(
        x = reorder(census_name, .data[[input$p1_metric]]),
        y = .data[[input$p1_metric]],
        text = paste0(
          "<b>", census_name, "</b><br>",
          "Population: ", comma(population), "<br>",
          "Total Accidents: ", comma(total_accidents), "<br>",
          "Severe Accidents: ", comma(severe_accidents), "<br>",
          "Severe Rate: ", percent(severe_rate, accuracy = 0.1), "<br>",
          "Severe Ratio (per 100k): ", round(severe_ratio, 1)
        )
      )
    ) +
      geom_col(
        fill = exec_navy,
        color = ifelse(isTRUE(input$a11y_high_contrast), "#000000", NA),
        alpha = 0.92,
        width = 0.68
      )
    
    if (isTRUE(input$a11y_high_contrast)) {
      p <- p + geom_text(aes(label = val_label), hjust = -0.1, size = 3.8, fontface = "bold", color = "#000000")
    }
    
    p <- p +
      coord_flip() +
      labs(x = NULL, y = metric_label) +
      theme_minimal(base_size = base_sz) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(face = "bold", color = "#1A202C")
      )
    
    mult_hi <- ifelse(isTRUE(input$a11y_high_contrast), 0.22, 0.08)
    if (input$p1_metric == "severe_rate") {
      p <- p + scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, mult_hi)))
    } else {
      p <- p + scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(0, mult_hi)))
    }
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  output$p1_county_sr_summary <- renderUI({
    df <- p1_county_summary() %>% arrange(desc(.data[[input$p1_metric]]))
    if (nrow(df) == 0) return(NULL)
    top_row <- df %>% slice(1)
    bot_row <- df %>% slice(n())
    
    fmt_val <- function(r) {
      switch(
        input$p1_metric,
        "total_accidents" = paste0(comma(r$total_accidents), " total accidents"),
        "severe_rate"     = paste0(percent(r$severe_rate, 0.1), " severe accident rate"),
        "severe_ratio"    = paste0(round(r$severe_ratio, 1), " severe accidents per 100k residents")
      )
    }
    
    msg <- paste0(
      "County bar chart ranked by ", input$p1_metric, ". ",
      "Highest locality is ", top_row$census_name, " with ", fmt_val(top_row),
      " (", comma(top_row$severe_accidents), " severe out of ", comma(top_row$total_accidents), " total crashes). ",
      "Lowest shown is ", bot_row$census_name, " with ", fmt_val(bot_row), "."
    )
    render_a11y_box(msg, input$a11y_show_text)
  })
  
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
      filter(total_accidents >= input$p1_min_weather_n) %>%
      slice_max(severe_percent, n = 10)
    
    m_groups  <- max(1, nrow(df))
    alpha_adj <- get_adjusted_alpha(m_groups, input$p1_mult_method, alpha = 0.05)
    
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
      need(nrow(df) > 0, "No weather conditions meet the minimum accident count threshold for this selection. Lower the threshold slider.")
    )
    
    base_sz <- ifelse(isTRUE(input$a11y_high_contrast), 14, 12)
    mult_hi <- ifelse(isTRUE(input$a11y_high_contrast), 0.22, 0.08)
    
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
      geom_col(
        fill = exec_teal,
        color = ifelse(isTRUE(input$a11y_high_contrast), "#000000", NA),
        alpha = 0.92,
        width = 0.68
      )
    
    if (isTRUE(input$a11y_high_contrast)) {
      p <- p + geom_text(
        aes(label = percent(severe_percent, accuracy = 0.1)),
        hjust = -0.1, size = 3.8, fontface = "bold", color = "#000000"
      )
    }
    
    p <- p +
      coord_flip() +
      scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, mult_hi))) +
      labs(
        x = NULL,
        y = "Severe Accidents (% of Total in Weather Condition)"
      ) +
      theme_minimal(base_size = base_sz) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(face = "bold", color = "#1A202C")
      )
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  output$p1_weather_sr_summary <- renderUI({
    df <- p1_weather_summary() %>% arrange(desc(severe_percent))
    if (nrow(df) == 0) return(NULL)
    w1 <- df %>% slice(1)
    w2 <- if (nrow(df) >= 2) df %>% slice(2) else NULL
    
    msg <- paste0(
      "Top weather conditions ranked by severe accident rate (minimum ", input$p1_min_weather_n, " accidents). ",
      "Rank 1 is ", w1$Weather_Condition, " at ", percent(w1$severe_percent, 0.1),
      " severe (", comma(w1$severe_accidents), " of ", comma(w1$total_accidents), " accidents, 95% CI: ",
      percent(w1$lower_ci, 0.1), " to ", percent(w1$upper_ci, 0.1), "). ",
      if (!is.null(w2)) paste0("Rank 2 is ", w2$Weather_Condition, " at ", percent(w2$severe_percent, 0.1), ".") else ""
    )
    render_a11y_box(msg, input$a11y_show_text)
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
  
  p1_ci_data <- reactive({
    if (input$p1_ci_type == "SARWC") {
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
    
    monthly <- p1_monthly_variance()
    if (input$p1_pop_tier == "all") {
      top15_names <- p1_county_summary() %>%
        slice_max(.data[[input$p1_metric]], n = 15) %>%
        pull(census_name)
      monthly <- monthly %>% filter(census_name %in% top15_names)
    }
    
    target_col <- ifelse(input$p1_ci_type == "CIPI", "severe_ratio", "severe_rate")
    upper_cap  <- ifelse(input$p1_ci_type == "CIPI", Inf, 1.0)
    
    compute_simple_ci(
      df          = monthly,
      group_col   = "census_name",
      value_col   = target_col,
      pop_col     = "population",
      mult_method = input$p1_mult_method,
      upper_cap   = upper_cap,
      min_months  = 6
    ) %>%
      arrange(desc(pop))
  })
  
  output$p1_ci_header <- renderText({
    df <- p1_ci_data()
    m_val <- if (nrow(df) > 0) df$m_comparisons[1] else 1
    c_lvl <- if (nrow(df) > 0) percent(df$conf_level[1], accuracy = 0.01) else "95.0%"
    method_name <- switch(
      input$p1_mult_method,
      "bonferroni" = "Bonferroni Simultaneous",
      "sidak"      = "Šidák Simultaneous",
      "none"       = "Unadjusted Marginal"
    )
    paste0(
      "Dedicated 95% Confidence Interval Chart | ", method_name,
      " (m = ", m_val, " comparisons, ", c_lvl, " individual interval level)"
    )
  })
  
  output$p1_ci_plot <- renderPlotly({
    df <- p1_ci_data()
    validate(need(nrow(df) > 0, "Insufficient data to compute confidence intervals."))
    
    is_pct <- input$p1_ci_type %in% c("CIPIrate", "SARWC")
    y_lab  <- switch(
      input$p1_ci_type,
      "CIPI"     = "Monthly Severe Accidents per 100k Residents (Mean ± 95% CI)",
      "CIPIrate" = "Monthly Severe Accident Rate % (Mean ± 95% CI)",
      "SARWC"    = "Weather Severe Accident Rate % (Simultaneous 95% Binomial CI)"
    )
    
    base_sz <- ifelse(isTRUE(input$a11y_high_contrast), 14, 12)
    
    p <- ggplot(df, aes(x = reorder(Group, Mean_Est), y = Mean_Est)) +
      geom_errorbar(
        aes(ymin = CI_Lower, ymax = CI_Upper),
        width = 0, color = exec_teal, linewidth = ifelse(isTRUE(input$a11y_high_contrast), 3.2, 2.4)
      ) +
      geom_point(
        aes(
          text = paste0(
            "<b>", Group, "</b><br>",
            "Estimate: ", ifelse(is_pct, percent(Mean_Est, 0.1), round(Mean_Est, 2)), "<br>",
            "Simultaneous 95% CI (", percent(conf_level, 0.01), "): [",
            ifelse(is_pct, percent(CI_Lower, 0.1), round(CI_Lower, 2)), ", ",
            ifelse(is_pct, percent(CI_Upper, 0.1), round(CI_Upper, 2)), "]<br>",
            "Months / Units (n): ", N_Units, " | Comparisons (m): ", m_comparisons
          )
        ),
        color = exec_crimson, shape = 18, size = ifelse(isTRUE(input$a11y_high_contrast), 4.5, 3.6)
      ) +
      coord_flip() +
      labs(x = NULL, y = y_lab) +
      theme_minimal(base_size = base_sz) +
      theme(axis.text.y = element_text(face = "bold"))
    
    if (is_pct) {
      p <- p + scale_y_continuous(labels = percent_format())
    } else {
      p <- p + scale_y_continuous(labels = comma_format())
    }
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  output$p1_ci_sr_summary <- renderUI({
    df <- p1_ci_data() %>% arrange(desc(Mean_Est))
    if (nrow(df) == 0) return(NULL)
    is_pct <- input$p1_ci_type %in% c("CIPIrate", "SARWC")
    top_g <- df %>% slice(1)
    fmt <- function(v) if (is_pct) percent(v, 0.1) else round(v, 2)
    
    msg <- paste0(
      "Simultaneous 95% Confidence Interval chart across ", nrow(df), " groups. ",
      "Highest estimate is ", top_g$Group, " with mean ", fmt(top_g$Mean_Est),
      " and 95% confidence interval from ", fmt(top_g$CI_Lower), " to ", fmt(top_g$CI_Upper),
      " based on n = ", comma(top_g$N_Units), " observations. Full numerical values are in the adjacent table."
    )
    render_a11y_box(msg, input$a11y_show_text)
  })
  
  output$p1_ci_table <- renderDT({
    df <- p1_ci_data()
    is_pct <- input$p1_ci_type %in% c("CIPIrate", "SARWC")
    
    tbl <- df %>%
      select(Group, N_Units, Mean_Est, CI_Lower, CI_Upper)
    
    dt <- datatable(
      tbl,
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = "caption-side: top; font-weight: bold; color: #0F2042;",
        "Accessible Data Table: Simultaneous 95% Confidence Intervals for Selected Page 1 Metric"
      ),
      colnames = c("Locality / Condition", "n (Months/Acc)", "Estimate", "Sim. 95% CI Low", "Sim. 95% CI High"),
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
  # PAGE 2 SERVER: SIMPLE, CLEAR MONTHLY WEATHER CONFIDENCE INTERVALS
  # ----------------------------------------------------------------------------
  p2_county_acc <- reactive({
    acc %>% filter(census_name == input$p2_county)
  })
  
  # Page 2 KPI calculations: all three values use the selected county
  p2_kpi_summary <- reactive({
    df <- p2_county_acc()
    pop <- county_pop_ranks %>%
      filter(census_name == input$p2_county) %>%
      pull(population)
    pop <- if (length(pop) > 0) pop[1] else NA_real_
    total <- nrow(df)
    severe_total <- sum(df$severe, na.rm = TRUE)
    
    list(
      total = total,
      severe_rate = if (total > 0) severe_total / total else NA_real_,
      severe_per_100k = if (!is.na(pop) && pop > 0) severe_total / pop * 100000 else NA_real_,
      severe_total = severe_total
    )
  })
  
  output$p2_kpi_total <- renderText({
    comma(p2_kpi_summary()$total)
  })
  
  output$p2_kpi_rate <- renderText({
    x <- p2_kpi_summary()$severe_rate
    if (is.na(x)) "N/A" else percent(x, accuracy = 0.1)
  })
  
  output$p2_kpi_ratio <- renderText({
    x <- p2_kpi_summary()$severe_per_100k
    if (is.na(x)) "N/A" else number(x, accuracy = 0.1)
  })
  
  output$p2_kpi_sr_summary <- renderUI({
    x <- p2_kpi_summary()
    msg <- paste0(
      input$p2_county, ": ", comma(x$total), " recorded accidents; ",
      comma(x$severe_total), " severe accidents; ",
      if (is.na(x$severe_rate)) "severe rate unavailable" else paste0(percent(x$severe_rate, 0.1), " severe rate"),
      "; ",
      if (is.na(x$severe_per_100k)) "per-capita rate unavailable" else paste0(round(x$severe_per_100k, 1), " severe accidents per 100,000 residents"),
      "."
    )
    render_a11y_box(msg, input$a11y_show_text)
  })
  
  output$p2_ts_title <- renderText({
    paste0("Monthly Severe Accidents & Simultaneous 95% Confidence Band: ", input$p2_county)
  })
  
  output$p2_weather_title <- renderText({
    grp_label <- ifelse(input$p2_weather_grouping == "Weather_Category", "Core Weather Categories", "Weather Conditions")
    paste0("Top ", grp_label, " Associated with Severe Accidents (Plot b): ", input$p2_county)
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
    alpha_adj <- get_adjusted_alpha(m_months, input$p2_mult_method, alpha = 0.05)
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
    ts_df   <- p2_ts_computed()
    c_level <- ts_df$conf_level[1]
    base_sz <- ifelse(isTRUE(input$a11y_high_contrast), 14, 12)
    
    if (input$p2_ts_metric == "count") {
      p <- ggplot(ts_df, aes(x = month_year, y = severe_accidents)) +
        geom_ribbon(aes(ymin = pois_low, ymax = pois_high), fill = exec_teal, alpha = 0.25) +
        geom_line(color = exec_navy, linewidth = 0.9) +
        geom_point(
          aes(
            text = paste0(
              "<b>", format(month_year, "%b %Y"), "</b><br>",
              "Severe Accidents: ", comma(severe_accidents), "<br>",
              "Simultaneous Poisson CI (", percent(c_level, 0.01), "): [",
              round(pois_low, 1), ", ", round(pois_high, 1), "]<br>",
              "Total Accidents: ", comma(total_accidents)
            )
          ),
          color = exec_crimson, size = 1.8
        ) +
        labs(x = "Month", y = "Number of Severe Accidents")
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
              "Simultaneous Binomial CI (", percent(c_level, 0.01), "): [",
              percent(rate_low, 0.1), ", ", percent(rate_high, 0.1), "]<br>",
              "Severe / Total: ", severe_accidents, " / ", total_accidents
            )
          ),
          color = exec_crimson, size = 1.8
        ) +
        scale_y_continuous(labels = percent_format()) +
        labs(x = "Month", y = "Severe Accident Rate (% of Monthly Accidents)")
    }
    
    p <- p +
      scale_x_datetime(date_labels = "%b %Y", date_breaks = "6 months") +
      theme_minimal(base_size = base_sz) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  output$p2_ts_sr_summary <- renderUI({
    ts_df <- p2_ts_computed()
    if (nrow(ts_df) == 0) return(NULL)
    if (input$p2_ts_metric == "count") {
      pk <- ts_df %>% arrange(desc(severe_accidents)) %>% slice(1)
      avg_m <- round(mean(ts_df$severe_accidents, na.rm = TRUE), 1)
      msg <- paste0(
        "Monthly time series for ", input$p2_county, " across ", nrow(ts_df), " months. ",
        "Average monthly severe count is ", avg_m, " crashes. ",
        "Peak month occurred in ", format(pk$month_year, "%B %Y"), " with ", comma(pk$severe_accidents),
        " severe crashes (95% Poisson CI: ", round(pk$pois_low, 1), " to ", round(pk$pois_high, 1), ")."
      )
    } else {
      ts_v <- ts_df %>% filter(!is.na(severe_rate))
      pk <- ts_v %>% arrange(desc(severe_rate)) %>% slice(1)
      avg_r <- percent(mean(ts_v$severe_rate, na.rm = TRUE), 0.1)
      msg <- paste0(
        "Monthly severe accident rate time series for ", input$p2_county, ". ",
        "Average monthly severe rate is ", avg_r, ". ",
        "Highest monthly rate occurred in ", format(pk$month_year, "%B %Y"), " at ",
        percent(pk$severe_rate, 0.1), " (", pk$severe_accidents, " of ", pk$total_accidents, " crashes)."
      )
    }
    render_a11y_box(msg, input$a11y_show_text)
  })
  
  # Monthly Weather Table for Selected County:
  p2_qualified_monthly_weather <- reactive({
    df_county <- p2_county_acc() %>% filter(!is.na(month_year))
    w_col     <- input$p2_weather_grouping
    cnty_pop  <- first(df_county$population)
    
    monthly_all <- df_county %>%
      filter(!is.na(.data[[w_col]]), .data[[w_col]] != "") %>%
      group_by(Weather_Group = .data[[w_col]], month_year) %>%
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
    
    qual <- monthly_all %>% filter(accidents >= input$p2_min_monthly_acc)
    if (nrow(qual) < 5) {
      qual <- monthly_all %>% filter(accidents >= 2)
    }
    if (nrow(qual) < 3) {
      qual <- monthly_all
    }
    qual
  })
  
  p2_weather_data <- reactive({
    w_col <- input$p2_weather_grouping
    qual_counts <- p2_qualified_monthly_weather() %>%
      count(Weather_Group, name = "qualifying_months")
    
    df <- p2_county_acc() %>%
      filter(!is.na(.data[[w_col]]), .data[[w_col]] != "") %>%
      group_by(Weather_Group = .data[[w_col]]) %>%
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
    alpha_adj <- get_adjusted_alpha(m_all, input$p2_mult_method, alpha = 0.05)
    
    df %>%
      mutate(
        rate_ci_low  = calc_binom_ci(severe_accidents, accidents, "lower", conf_level = 1 - alpha_adj),
        rate_ci_high = calc_binom_ci(severe_accidents, accidents, "upper", conf_level = 1 - alpha_adj)
      )
  })
  
  output$p2_weather_plot <- renderPlotly({
    df <- p2_weather_data() %>%
      filter(qualifying_months >= input$p2_min_months) %>%
      slice_max(.data[[input$p2_weather_metric]], n = 8)
    
    if (nrow(df) == 0) {
      df <- p2_weather_data() %>%
        filter(qualifying_months >= 3) %>%
        slice_max(.data[[input$p2_weather_metric]], n = 8)
    }
    
    validate(
      need(nrow(df) > 0, "No weather categories in this locality meet the minimum qualifying months threshold.")
    )
    
    is_rate <- input$p2_weather_metric == "severe_rate"
    y_lab   <- ifelse(
      is_rate,
      "Overall Severe Accident Rate (% of Weather Accidents)",
      "Cumulative Severe Accidents per 100,000 Residents"
    )
    
    df <- df %>%
      mutate(
        val_label = if (is_rate) percent(severe_rate, 0.1) else round(severe_ratio, 1)
      )
    
    base_sz <- ifelse(isTRUE(input$a11y_high_contrast), 14, 12)
    mult_hi <- ifelse(isTRUE(input$a11y_high_contrast), 0.22, 0.08)
    
    p <- ggplot(
      df,
      aes(
        x = reorder(Weather_Group, .data[[input$p2_weather_metric]]),
        y = .data[[input$p2_weather_metric]],
        text = paste0(
          "<b>", Weather_Group, "</b><br>",
          "Qualifying Months (n): ", qualifying_months, "<br>",
          "Overall Severe Rate: ", percent(severe_rate, accuracy = 0.1), "<br>",
          "Cumulative Severe Ratio (per 100k): ", round(severe_ratio, 2), "<br>",
          "Severe Accidents: ", comma(severe_accidents), " / ", comma(accidents)
        )
      )
    ) +
      geom_col(
        fill = "#483D8B",
        color = ifelse(isTRUE(input$a11y_high_contrast), "#000000", NA),
        alpha = 0.9,
        width = 0.65
      )
    
    if (isTRUE(input$a11y_high_contrast)) {
      p <- p + geom_text(aes(label = val_label), hjust = -0.1, size = 3.8, fontface = "bold", color = "#000000")
    }
    
    p <- p +
      coord_flip() +
      labs(x = NULL, y = y_lab) +
      theme_minimal(base_size = base_sz) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(face = "bold", color = "#1A202C")
      )
    
    if (is_rate) {
      p <- p + scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, mult_hi)))
    } else {
      p <- p + scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(0, mult_hi)))
    }
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  output$p2_weather_sr_summary <- renderUI({
    df <- p2_weather_data() %>%
      filter(qualifying_months >= input$p2_min_months) %>%
      arrange(desc(.data[[input$p2_weather_metric]]))
    if (nrow(df) == 0) return(NULL)
    top_w <- df %>% slice(1)
    
    msg <- paste0(
      "In ", input$p2_county, ", the top weather group by ", input$p2_weather_metric,
      " is ", top_w$Weather_Group, " with an overall severe rate of ",
      percent(top_w$severe_rate, 0.1), " and ", round(top_w$severe_ratio, 2),
      " cumulative severe accidents per 100k residents across ", top_w$qualifying_months, " qualifying months."
    )
    render_a11y_box(msg, input$a11y_show_text)
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
      mult_method = input$p2_mult_method,
      upper_cap   = upper_cap,
      min_months  = input$p2_min_months
    ) %>%
      slice_max(Mean_Est, n = 8)
  })
  
  output$p2_ci_header <- renderText({
    df <- p2_ci_data()
    m_val <- if (nrow(df) > 0) df$m_comparisons[1] else 1
    c_lvl <- if (nrow(df) > 0) percent(df$conf_level[1], accuracy = 0.01) else "95.0%"
    method_name <- switch(
      input$p2_mult_method,
      "bonferroni" = "Bonferroni Simultaneous",
      "sidak"      = "Šidák Simultaneous",
      "none"       = "Unadjusted Marginal"
    )
    paste0(
      "Monthly Weather 95% Confidence Intervals (Mean ± CI) in ", input$p2_county, " | ",
      method_name, " (m = ", m_val, " groups, ", c_lvl, " level)"
    )
  })
  
  output$p2_ci_plot <- renderPlotly({
    df <- p2_ci_data()
    validate(need(nrow(df) > 0, "No weather categories have enough qualifying months. Lower the slider on the left."))
    
    is_pct <- (input$p2_weather_metric == "severe_rate")
    y_lab  <- ifelse(
      is_pct,
      "Monthly Severe Accident Rate % (Simple Monthly Mean ± 95% CI)",
      "Monthly Severe Accidents per 100k Residents (Simple Monthly Mean ± 95% CI)"
    )
    
    base_sz <- ifelse(isTRUE(input$a11y_high_contrast), 14, 12)
    
    p <- ggplot(df, aes(x = reorder(Group, Mean_Est), y = Mean_Est)) +
      geom_errorbar(
        aes(ymin = CI_Lower, ymax = CI_Upper),
        width = 0, color = exec_teal, linewidth = ifelse(isTRUE(input$a11y_high_contrast), 3.2, 2.4)
      ) +
      geom_point(
        aes(
          text = paste0(
            "<b>", Group, " (", input$p2_county, ")</b><br>",
            "Qualifying Months (n): ", N_Units, " months<br>",
            "Monthly Average (Mean): ", ifelse(is_pct, percent(Mean_Est, 0.1), round(Mean_Est, 2)), "<br>",
            "Simultaneous 95% CI (", percent(conf_level, 0.01), "): [",
            ifelse(is_pct, percent(CI_Lower, 0.1), round(CI_Lower, 2)), ", ",
            ifelse(is_pct, percent(CI_Upper, 0.1), round(CI_Upper, 2)), "]"
          )
        ),
        color = exec_crimson, shape = 18, size = ifelse(isTRUE(input$a11y_high_contrast), 4.5, 3.6)
      ) +
      coord_flip() +
      labs(x = NULL, y = y_lab) +
      theme_minimal(base_size = base_sz) +
      theme(axis.text.y = element_text(face = "bold"))
    
    if (is_pct) {
      p <- p + scale_y_continuous(labels = percent_format())
    } else {
      p <- p + scale_y_continuous(labels = comma_format())
    }
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  output$p2_ci_sr_summary <- renderUI({
    df <- p2_ci_data() %>% arrange(desc(Mean_Est))
    if (nrow(df) == 0) return(NULL)
    is_pct <- (input$p2_weather_metric == "severe_rate")
    fmt <- function(v) if (is_pct) percent(v, 0.1) else round(v, 2)
    top_c <- df %>% slice(1)
    
    msg <- paste0(
      "Monthly weather confidence interval summary for ", input$p2_county, ". ",
      top_c$Group, " has the highest monthly mean at ", fmt(top_c$Mean_Est),
      " with a simultaneous 95% CI from ", fmt(top_c$CI_Lower), " to ", fmt(top_c$CI_Upper),
      " across n = ", top_c$N_Units, " qualifying months."
    )
    render_a11y_box(msg, input$a11y_show_text)
  })
  
  output$p2_ci_table <- renderDT({
    df <- p2_ci_data()
    is_pct <- (input$p2_weather_metric == "severe_rate")
    
    tbl <- df %>%
      select(Group, N_Units, Mean_Est, CI_Lower, CI_Upper)
    
    dt <- datatable(
      tbl,
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = "caption-side: top; font-weight: bold; color: #0F2042;",
        paste0("Accessible Data Table: Monthly Weather Means & 95% CIs for ", input$p2_county)
      ),
      colnames = c("Weather Category", "Qualifying Months (n)", "Monthly Mean", "Sim. 95% CI Low", "Sim. 95% CI High"),
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
        paste0("Complete Weather Breakdown for ", input$p2_county, " (Full Non-Interactive Reference Table)")
      ),
      colnames = c(
        "Weather Category", "Qualifying Months (n)", "Total Accidents", "Severe Accidents",
        "Non-Severe Accidents", "Overall Severe Rate (%)", "Sim. 95% CI Low",
        "Sim. 95% CI High", "Cumulative Severe Ratio (per 100k)"
      ),
      options = list(pageLength = 10, scrollX = TRUE)
    ) %>%
      formatPercentage(c("severe_rate", "rate_ci_low", "rate_ci_high"), digits = 1) %>%
      formatRound("severe_ratio", digits = 2) %>%
      formatCurrency(c("accidents", "severe_accidents", "not_severe_accidents"), currency = "", interval = 3, mark = ",", digits = 0)
  })
  
  # Simple audit checks for the selected county. These test CI bounds and
  # whether the displayed intervals contain their point estimates.
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
  
  output$p2_audit_overview <- renderUI({
    x <- p2_audit_results()
    tags$p(
      class = "alert alert-info",
      paste0(
        "County: ", input$p2_county, " | Passed: ", sum(x$Status == "PASS"),
        " | Failed: ", sum(x$Status == "FAIL"),
        " | Needs review: ", sum(x$Status == "REVIEW")
      )
    )
  })
  
  output$p2_audit_table <- renderDT({
    datatable(p2_audit_results(), rownames = FALSE,
              options = list(dom = "t", ordering = FALSE)) %>%
      formatStyle("Status",
                  backgroundColor = styleEqual(
                    c("PASS", "FAIL", "REVIEW"),
                    c("#D4EDDA", "#F8D7DA", "#FFF3CD")
                  ), fontWeight = "bold"
      )
  })
  
  output$p2_weather_audit_table <- renderDT({
    df <- p2_county_acc() %>%
      filter(!is.na(Weather_Condition), Weather_Condition != "") %>%
      mutate(Expected_Category = classify_weather_simple(Weather_Condition)) %>%
      count(Weather_Condition, Weather_Category, Expected_Category, name = "Accidents") %>%
      mutate(Check = if_else(Weather_Category == Expected_Category, "PASS", "FAIL")) %>%
      arrange(desc(Check), desc(Accidents))
    
    datatable(df, rownames = FALSE,
              options = list(pageLength = 8, scrollX = TRUE))
  })
  
  # ----------------------------------------------------------------------------
  # PAGE 2 EXCEL DOWNLOAD: CONFIDENCE INTERVALS & AUDIT STATUS
  # ----------------------------------------------------------------------------
  
  page_status_excel_sheets <- reactive({
    
    # 1. Dashboard quality checks
    audit_checks <- tryCatch(
      p2_audit_results(),
      error = function(e) {
        tibble(
          Check = "Audit calculation",
          Status = "REVIEW",
          Details = conditionMessage(e)
        )
      }
    )
    
    # 2. Monthly weather confidence intervals
    monthly_weather_ci <- tryCatch(
      p2_ci_data(),
      error = function(e) {
        tibble(Note = paste("Unavailable:", conditionMessage(e)))
      }
    )
    
    # 3. Monthly time-series confidence intervals
    monthly_time_series_ci <- tryCatch(
      p2_ts_computed(),
      error = function(e) {
        tibble(Note = paste("Unavailable:", conditionMessage(e)))
      }
    )
    
    # 4. Weather binomial confidence intervals
    weather_binomial_ci <- tryCatch(
      p2_weather_data(),
      error = function(e) {
        tibble(Note = paste("Unavailable:", conditionMessage(e)))
      }
    )
    
    # 5. Weather classification review
    weather_classification <- p2_county_acc() %>%
      filter(
        !is.na(Weather_Condition),
        Weather_Condition != ""
      ) %>%
      mutate(
        Expected_Category = classify_weather_simple(Weather_Condition)
      ) %>%
      count(
        Weather_Condition,
        Weather_Category,
        Expected_Category,
        name = "Accidents"
      ) %>%
      mutate(
        Check = if_else(
          Weather_Category == Expected_Category,
          "PASS",
          "FAIL"
        )
      ) %>%
      arrange(desc(Check), desc(Accidents))
    
    # 6. Selected dashboard settings
    export_settings <- tibble(
      Setting = c(
        "Selected County",
        "Weather Grouping",
        "Time-Series Metric",
        "Weather Metric",
        "Multiple Comparisons Method",
        "Minimum Accidents per Month",
        "Minimum Qualifying Months"
      ),
      
      Value = c(
        input$p2_county,
        input$p2_weather_grouping,
        input$p2_ts_metric,
        input$p2_weather_metric,
        input$p2_mult_method,
        as.character(input$p2_min_monthly_acc),
        as.character(input$p2_min_months)
      )
    )
    
    # Return all worksheets in one Excel workbook
    list(
      Page_Status = audit_checks,
      Monthly_Weather_CIs = monthly_weather_ci,
      Time_Series_CIs = monthly_time_series_ci,
      Weather_Binomial_CIs = weather_binomial_ci,
      Weather_Classification = weather_classification,
      Export_Settings = export_settings
    )
  })
  
  
  # ----------------------------------------------------------------------------
  # DOWNLOAD BUTTON: PAGE 2 STATUS WORKBOOK
  # ----------------------------------------------------------------------------
  
  output$download_page_status <- downloadHandler(
    
    filename = function() {
      county_name <- gsub(
        "[^A-Za-z0-9]+",
        "_",
        input$p2_county
      )
      
      paste0(
        "Page_Status_",
        county_name,
        "_",
        Sys.Date(),
        ".xlsx"
      )
    },
    
    content = function(file) {
      
      if (!requireNamespace("writexl", quietly = TRUE)) {
        stop(
          "Please install writexl first using: ",
          "install.packages('writexl')"
        )
      }
      
      writexl::write_xlsx(
        page_status_excel_sheets(),
        path = file
      )
    }
  )
  
  # ----------------------------------------------------------------------------
  # PAGE 3 SERVER: REAL VIRGINIA GIS MAP + CLICK-TO-SELECT COUNTY + STREETS
  # ----------------------------------------------------------------------------
  observeEvent(input$p3_reset_va, {
    updateSelectInput(session, "p3_county", selected = "ALL_VA")
  })
  
  observeEvent(input$p3_map_shape_click, {
    click <- input$p3_map_shape_click
    if (!is.null(click$id) && click$id %in% all_counties) {
      updateSelectInput(session, "p3_county", selected = click$id)
    }
  })
  
  p3_county_acc <- reactive({
    if (input$p3_county == "ALL_VA") {
      acc %>% filter(!is.na(Start_Lat), !is.na(Start_Lng))
    } else {
      acc %>% filter(census_name == input$p3_county, !is.na(Start_Lat), !is.na(Start_Lng))
    }
  })
  
  p3_street_summary <- reactive({
    df_raw <- p3_county_acc() %>%
      filter(!is.na(Street), Street != "")
    
    if (nrow(df_raw) == 0) return(tibble())
    
    grouped <- df_raw %>%
      group_by(Street, census_name) %>%
      summarise(
        total_accidents  = n(),
        severe_accidents = sum(severe, na.rm = TRUE),
        population       = first(population),
        severe_rate      = severe_accidents / total_accidents,
        severe_ratio     = (severe_accidents / population) * 100000,
        lat              = median(Start_Lat, na.rm = TRUE),
        lng              = median(Start_Lng, na.rm = TRUE),
        .groups = "drop"
      )
    
    filtered <- grouped %>% filter(total_accidents >= input$p3_min_street_n)
    if (nrow(filtered) == 0) {
      filtered <- grouped
    }
    
    filtered <- filtered %>%
      arrange(desc(.data[[input$p3_street_metric]]), desc(total_accidents))
    
    m_streets <- max(1, min(nrow(filtered), input$p3_top_n_streets))
    alpha_adj <- get_adjusted_alpha(m_streets, "bonferroni", alpha = 0.05)
    
    filtered %>%
      mutate(
        rate_ci_low  = calc_binom_ci(severe_accidents, total_accidents, "lower", conf_level = 1 - alpha_adj),
        rate_ci_high = calc_binom_ci(severe_accidents, total_accidents, "upper", conf_level = 1 - alpha_adj)
      )
  })
  
  output$p3_map_title <- renderText({
    if (input$p3_county == "ALL_VA") {
      "Commonwealth of Virginia: Statewide County Severity & Top Street Hotspots"
    } else {
      paste0("Virginia GIS Map — Focused on: ", input$p3_county)
    }
  })
  
  output$p3_map_subtitle <- renderText({
    if (input$p3_county == "ALL_VA") {
      "Showing all 133 Virginia counties/cities shaded by Severe Accidents per 100k Residents. Click any county on the map to zoom in."
    } else {
      paste0(
        "Highlighted in crimson: ", input$p3_county,
        " (surrounding Virginia counties remain visible for geographic context). Click any neighboring county to switch."
      )
    }
  })
  
  output$p3_table_title <- renderText({
    if (input$p3_county == "ALL_VA") {
      "Highest-Risk Streets Across All of Virginia"
    } else {
      paste0("Highest-Risk Streets in ", input$p3_county, " (Grouped by Street)")
    }
  })
  
  output$p3_map <- renderLeaflet({
    m <- leaflet() %>%
      addTiles(
        urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
        attribution = '© <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
        group = "OpenStreetMap (Road Map)"
      ) %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group = "Esri World Street Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri Satellite Imagery")
    
    if (!is.null(va_boundaries) && nrow(va_boundaries) > 0) {
      va_pal <- colorNumeric(
        palette = "YlOrRd",
        domain  = va_boundaries$severe_ratio
      )
      
      m <- m %>%
        addPolygons(
          data        = va_boundaries,
          layerId     = ~display_name,
          group       = "Virginia Counties",
          fillColor   = ~va_pal(severe_ratio),
          fillOpacity = ifelse(input$p3_county == "ALL_VA", 0.55, 0.20),
          color       = "#334155",
          weight      = 1.2,
          opacity     = 0.85,
          highlightOptions = highlightOptions(
            weight = 3,
            color = exec_navy,
            fillOpacity = 0.4,
            bringToFront = FALSE
          ),
          label = ~paste0(
            display_name, " | Severe/100k: ", round(severe_ratio, 1),
            " (Severe: ", comma(severe_accidents), ")"
          ),
          popup = ~paste0(
            "<b>Locality: </b>", display_name, "<br>",
            "<b>Population: </b>", comma(population), "<br>",
            "<b>Total Accidents: </b>", comma(total_accidents), "<br>",
            "<b>Severe Accidents: </b>", comma(severe_accidents), "<br>",
            "<b>Severe Rate: </b>", percent(severe_rate, accuracy = 0.1), "<br>",
            "<b>Severe Ratio (per 100k): </b>", round(severe_ratio, 1)
          )
        )
    }
    
    if (input$p3_county == "ALL_VA") {
      m <- m %>% fitBounds(lng1 = -83.68, lat1 = 36.54, lng2 = -75.24, lat2 = 39.47)
    } else if (!is.null(va_boundaries)) {
      selected_poly <- va_boundaries %>% filter(display_name == input$p3_county)
      if (nrow(selected_poly) > 0) {
        bbox <- st_bbox(selected_poly)
        m <- m %>%
          addPolygons(
            data        = selected_poly,
            fill        = TRUE,
            fillColor   = exec_crimson,
            fillOpacity = 0.08,
            color       = exec_crimson,
            weight      = 4,
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
    }
    
    if (input$p3_map_mode == "streets") {
      streets_df <- p3_street_summary() %>%
        slice_head(n = input$p3_top_n_streets)
      
      if (nrow(streets_df) > 0) {
        pal <- colorNumeric(
          palette = "plasma",
          domain  = streets_df[[input$p3_street_metric]],
          reverse = TRUE
        )
        
        max_acc <- max(streets_df$total_accidents, 1)
        radii   <- 6 + 14 * sqrt(streets_df$total_accidents / max_acc)
        
        m <- m %>%
          addCircleMarkers(
            data        = streets_df,
            lng         = ~lng,
            lat         = ~lat,
            radius      = radii,
            fillColor   = ~pal(streets_df[[input$p3_street_metric]]),
            color       = "#1A202C",
            weight      = 1.5,
            fillOpacity = 0.90,
            group       = "Street Hotspots",
            label       = ~paste0(Street, " (", census_name, "): ", severe_accidents, " severe / ", total_accidents, " total"),
            popup       = ~paste0(
              "<b>Street: </b>", Street, "<br>",
              "<b>Locality: </b>", census_name, "<br>",
              "<b>Severe Accidents: </b>", comma(severe_accidents), "<br>",
              "<b>Total Accidents: </b>", comma(total_accidents), "<br>",
              "<b>Severe Rate: </b>", percent(severe_rate, accuracy = 0.1),
              " (Sim. 95% CI: ", percent(rate_ci_low, 0.1), "–", percent(rate_ci_high, 0.1), ")<br>",
              "<b>Severe Ratio (per 100k): </b>", round(severe_ratio, 2)
            )
          ) %>%
          addLegend(
            position = "bottomright",
            pal      = pal,
            values   = streets_df[[input$p3_street_metric]],
            title    = switch(
              input$p3_street_metric,
              "severe_accidents" = "Street Severe Count",
              "severe_rate"      = "Street Severe Rate",
              "severe_ratio"     = "Street Severe / 100k"
            ),
            opacity  = 0.90
          )
      }
    } else {
      sev_pts <- p3_county_acc() %>%
        filter(severe) %>%
        slice_head(n = 2500)
      
      if (nrow(sev_pts) > 0) {
        m <- m %>%
          addCircleMarkers(
            data        = sev_pts,
            lng         = ~Start_Lng,
            lat         = ~Start_Lat,
            radius      = 4.5,
            fillColor   = exec_crimson,
            color       = "#FFFFFF",
            weight      = 0.8,
            fillOpacity = 0.75,
            group       = "Severe Accident Points",
            popup       = ~paste0(
              "<b>Street: </b>", Street, "<br>",
              "<b>Locality: </b>", census_name, "<br>",
              "<b>Severity: </b>", Severity, "<br>",
              "<b>Weather: </b>", Weather_Condition, "<br>",
              "<b>Time: </b>", Start_Time
            )
          )
      }
    }
    
    m %>%
      addLayersControl(
        baseGroups    = c("OpenStreetMap (Road Map)", "Esri World Street Map", "Esri Satellite Imagery"),
        overlayGroups = c("Virginia Counties", "Street Hotspots"),
        options       = layersControlOptions(collapsed = FALSE)
      )
  })
  
  output$p3_map_sr_summary <- renderUI({
    df <- p3_street_summary()
    if (nrow(df) == 0) return(NULL)
    s1 <- df %>% slice(1)
    s2 <- if (nrow(df) >= 2) df %>% slice(2) else NULL
    loc_label <- ifelse(input$p3_county == "ALL_VA", "all of Virginia", input$p3_county)
    
    msg <- paste0(
      "Geospatial map summary for ", loc_label, ". ",
      "Top street hotspot is ", s1$Street, " (", s1$census_name, ") with ",
      comma(s1$severe_accidents), " severe accidents out of ", comma(s1$total_accidents),
      " total crashes (", percent(s1$severe_rate, 0.1), " severe rate, ",
      round(s1$severe_ratio, 2), " severe crashes per 100k residents). ",
      if (!is.null(s2)) paste0("Second highest is ", s2$Street, " with ", comma(s2$severe_accidents), " severe crashes.") else ""
    )
    render_a11y_box(msg, input$a11y_show_text)
  })
  
  output$p3_street_table <- renderDT({
    df <- p3_street_summary()
    validate(need(nrow(df) > 0, "No geocoded street records available for this locality."))
    
    tbl <- df %>%
      select(
        Street, census_name, total_accidents, severe_accidents,
        severe_rate, rate_ci_low, rate_ci_high, severe_ratio
      )
    
    datatable(
      tbl,
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = "caption-side: top; font-weight: bold; color: #0F2042;",
        "Accessible Street Hotspot Table (Full Keyboard & Screen-Reader Equivalent to GIS Map)"
      ),
      colnames = c(
        "Street / Highway", "Locality", "Total Acc.", "Severe Acc.",
        "Severe Rate (%)", "Sim. 95% CI Low", "Sim. 95% CI High", "Severe per 100k"
      ),
      options = list(pageLength = 10, scrollX = TRUE)
    ) %>%
      formatPercentage(c("severe_rate", "rate_ci_low", "rate_ci_high"), digits = 1) %>%
      formatRound("severe_ratio", digits = 2)
  })
}

shinyApp(ui = ui, server = server)