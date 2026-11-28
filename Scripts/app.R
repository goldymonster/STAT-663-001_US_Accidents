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
    Accident_Date     = as.Date(Start_Time),
    Year              = year(Start_Time),
    month_year        = floor_date(Start_Time, unit = "month"),
    severe            = Severity >= 3,
    GEOID             = str_pad(as.character(GEOID), width = 5, side = "left", pad = "0"),
    Weather_Condition = str_squish(Weather_Condition),
    Weather_Category  = classify_weather_simple(Weather_Condition),
    Street            = str_squish(Street),
    City              = str_squish(coalesce(City, "")),
    census_name       = str_squish(census_name)
  ) %>%
  filter(!is.na(census_name), !is.na(Severity), !is.na(Year))

min_acc_date <- min(acc$Accident_Date, na.rm = TRUE)
max_acc_date <- max(acc$Accident_Date, na.rm = TRUE)

# Pre-compute static county population & statewide accident summary
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

# Default 2-locality comparison for Page 2
default_p2_counties <- intersect(c("Fairfax County", "Arlington County"), all_counties)
if (length(default_p2_counties) == 0) {
  default_p2_counties <- head(all_counties, min(2, length(all_counties)))
}

# Load all 133 Virginia county/city boundaries
va_boundaries_raw <- tryCatch({
  counties(state = "VA", cb = TRUE, year = 2022, progress_bar = FALSE) %>%
    st_transform(4326) %>%
    left_join(county_pop_ranks %>% select(GEOID, census_name, population), by = "GEOID") %>%
    mutate(display_name = coalesce(census_name, NAMELSAD, NAME))
}, error = function(e) {
  NULL
})

# ==============================================================================
# STATISTICAL HELPER FUNCTIONS (INCLUDING NEWEY-WEST HAC STANDARD ERRORS)
# ==============================================================================
get_adjusted_alpha <- function(m, method = c("bonferroni", "none"), alpha = 0.05) {
  method <- match.arg(method)
  m <- max(1, as.integer(m))
  switch(
    method,
    "bonferroni" = alpha / m,
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

# Base-R Newey-West (HAC) Standard Error for Monthly Time Series
# Reconstructs a complete calendar-month grid so lag-1 strictly compares adjacent
# calendar months; uses Bartlett kernel weights (guaranteed PSD) + Newey-West (1994)
# automatic bandwidth selection + small-sample N/(N-1) adjustment.
calc_newey_west_se <- function(dates, values) {
  valid <- !is.na(dates) & !is.na(values) & !is.nan(values)
  d_obs <- as.Date(dates[valid])
  v_obs <- as.numeric(values[valid])
  n_obs <- length(v_obs)
  
  if (n_obs == 0) {
    return(list(
      SE_Naive   = NA_real_,
      SE         = NA_real_,
      NW_Lag     = NA_integer_,
      SE_Mult    = NA_real_,
      Eff_Months = NA_real_
    ))
  }
  
  if (n_obs == 1) {
    return(list(
      SE_Naive   = NA_real_,
      SE         = NA_real_,
      NW_Lag     = 0L,
      SE_Mult    = NA_real_,
      Eff_Months = 1.0
    ))
  }
  
  se_naive <- sd(v_obs) / sqrt(n_obs)
  
  # Reconstruct complete calendar-month grid so missing seasonal months do not distort lags
  full_months <- seq(min(d_obs), max(d_obs), by = "1 month")
  grid <- tibble(m = full_months) %>%
    left_join(
      tibble(m = d_obs, val = v_obs) %>%
        group_by(m) %>%
        summarise(val = mean(val, na.rm = TRUE), .groups = "drop"),
      by = "m"
    )
  
  x     <- grid$val
  T_cal <- length(x)
  n_cal_obs <- sum(!is.na(x))
  x_sd  <- sd(x, na.rm = TRUE)
  
  if (n_cal_obs < 4 || T_cal < 4 || is.na(x_sd) || x_sd == 0) {
    return(list(
      SE_Naive   = se_naive,
      SE         = se_naive,
      NW_Lag     = 0L,
      SE_Mult    = 1.0,
      Eff_Months = as.numeric(n_cal_obs)
    ))
  }
  
  mu <- mean(x, na.rm = TRUE)
  # Zero-pad unobserved calendar months so only true calendar-lag pairs contribute (guaranteed PSD)
  e  <- ifelse(is.na(x), 0, x - mu)
  
  gamma0 <- sum(e^2) / n_cal_obs
  
  calc_gamma <- function(l) {
    if (l <= 0L || l >= T_cal) return(0)
    sum(e[(l + 1L):T_cal] * e[1L:(T_cal - l)]) / n_cal_obs
  }
  
  # Newey-West (1994) automatic Bartlett bandwidth selection (capped at 12 lags or N/3)
  max_L  <- as.integer(min(12L, max(1L, floor(n_cal_obs / 3)), T_cal - 1L))
  n_init <- as.integer(min(max_L, max(1L, floor(4 * (n_cal_obs / 100)^(2 / 9)))))
  
  g_init <- vapply(seq_len(n_init), calc_gamma, numeric(1))
  s0     <- gamma0 + 2 * sum(g_init)
  s1     <- 2 * sum(seq_len(n_init) * g_init)
  
  if (is.finite(s0) && abs(s0) > 1e-10 && is.finite(s1)) {
    alpha1 <- (s1 / s0)^2
    L_opt  <- floor(1.1447 * (alpha1 * n_cal_obs)^(1 / 3))
    L      <- as.integer(min(max_L, max(0L, L_opt)))
  } else {
    L <- n_init
  }
  
  # Bartlett-weighted long-run variance
  lrv <- gamma0
  if (L >= 1L) {
    for (l in seq_len(L)) {
      w_l <- 1 - (l / (L + 1))
      lrv <- lrv + 2 * w_l * calc_gamma(l)
    }
  }
  
  # Small-sample degrees-of-freedom adjustment N / (N - 1)
  lrv_adj <- max(0, lrv * (n_cal_obs / (n_cal_obs - 1)))
  se_nw   <- sqrt(lrv_adj / n_cal_obs)
  
  se_mult <- if (!is.na(se_naive) && se_naive > 0) se_nw / se_naive else 1.0
  eff_n   <- if (!is.na(se_mult) && se_mult > 0) min(as.numeric(n_cal_obs), n_cal_obs / (se_mult^2)) else as.numeric(n_cal_obs)
  
  list(
    SE_Naive   = se_naive,
    SE         = se_nw,
    NW_Lag     = L,
    SE_Mult    = se_mult,
    Eff_Months = eff_n
  )
}

compute_simple_ci <- function(df, group_col, value_col, pop_col = NULL,
                              time_col = "month_year",
                              mult_method = "bonferroni", upper_cap = Inf,
                              min_months = 10) {
  grouped <- df %>%
    filter(!is.na(.data[[value_col]]) & !is.nan(.data[[value_col]]) & !is.na(.data[[time_col]])) %>%
    group_by(Group = .data[[group_col]]) %>%
    summarise(
      pop      = if (!is.null(pop_col)) first(.data[[pop_col]]) else NA_real_,
      N_Units  = n(),
      Mean_Est = mean(.data[[value_col]], na.rm = TRUE),
      variance = var(.data[[value_col]], na.rm = TRUE),
      stdev    = sqrt(pmax(0, variance)),
      nw_res   = list(calc_newey_west_se(.data[[time_col]], .data[[value_col]])),
      .groups  = "drop"
    ) %>%
    filter(Mean_Est > 0)
  
  if (nrow(grouped) == 0) return(tibble())
  
  grouped <- grouped %>% unnest_wider(nw_res)
  
  filtered <- grouped %>% filter(N_Units >= min_months)
  if (nrow(filtered) == 0) filtered <- grouped %>% filter(N_Units >= 5)
  if (nrow(filtered) == 0) filtered <- grouped %>% filter(N_Units >= 3)
  
  if (nrow(filtered) == 0) return(tibble())
  
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

# Colorblind-safe 3-locality palette & redundant shapes for Page 2 comparisons
p2_compare_colors <- c(exec_navy, exec_amber, exec_teal)
p2_compare_shapes <- c(16, 17, 15)

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
  header = tagList(
    tags$head(
      tags$style(HTML("
        .shiny-output-error-validation {
          color: #0F2042;
          background-color: #F8FAFC;
          border: 1px solid #CBD5E1;
          border-left: 5px solid #9E1B32;
          border-radius: 6px;
          padding: 20px 24px;
          margin: 24px 12px;
          font-size: 0.96rem;
          line-height: 1.55;
          white-space: pre-line;
        }
      "))
    ),
    tags$div(
      class = "bg-light border-bottom px-3 py-2 d-flex flex-wrap justify-content-between align-items-center gap-2",
      role = "region",
      `aria-label` = "Global Accessibility, Documentation, and Export Toolbar",
      tags$div(
        class = "d-flex flex-wrap align-items-center gap-3",
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
        class = "d-flex flex-wrap align-items-center gap-2",
        downloadButton(
          "download_data_dict_pdf",
          "Data & Statistics Dictionary (.pdf)",
          class = "btn-sm btn-primary fw-bold",
          `aria-label` = "Download the Virginia Traffic Safety Executive Dashboard Data and Statistics Dictionary PDF"
        ),
        downloadButton(
          "download_top5_excel",
          "Download Top 5 Worst Counties Summary (.xlsx)",
          class = "btn-sm btn-danger fw-bold",
          `aria-label` = "Download Excel workbook summarizing the Top 5 worst Virginia counties for traffic accidents including rates, averages, and per-resident totals"
        )
      )
    )
  ),
  
  # ============================================================================
  # PAGE 1: STATEWIDE OVERVIEW, WEATHER DRIVERS & MONTHLY VARIABILITY
  # ============================================================================
  nav_panel(
    title = "1. Statewide Overview & Weather Drivers",
    icon = bs_icon("bar-chart-line-fill"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Explore Virginia Localities",
        width = 330,
        selectInput(
          "p1_pop_tier",
          "1. Choose Locality Group:",
          choices = c(
            "Top 5 Most Populous VA Localities"     = "top5",
            "Middle 5 VA Localities by Population"  = "mid5",
            "Bottom 5 Least Populous VA Localities" = "bot5",
            "Top 10 Most Populous VA Localities"    = "top10",
            "All Virginia Localities"               = "all"
          ),
          selected = "all"
        ),
        selectInput(
          "p1_year",
          "2. Select Time Period:",
          choices = c("All Years (2016–2023)" = "All", as.character(all_years)),
          selected = "All"
        ),
        radioButtons(
          "p1_metric",
          "3. How to Compare Localities (Top-Left Chart):",
          choices = c(
            "Total Recorded Crashes (Raw Count)"   = "total_accidents",
            "Share of Crashes That Are Severe (%)" = "severe_rate",
            "Severe Crashes per 100k Residents"    = "severe_ratio"
          ),
          selected = "severe_ratio"
        ),
        hr(),
        selectInput(
          "p1_ci_type",
          "4. Monthly Average & Confidence Interval View (Bottom Chart):",
          choices = c(
            "Monthly Severe Crashes per 100k (Time-Adjusted Avg & 95% CI)" = "monthly_ratio_ci",
            "Monthly Severe Crash Share % (Time-Adjusted Avg & 95% CI)"    = "monthly_rate_ci",
            "Weather Severe Crash Share % (Binomial 95% CI)"               = "weather_binom_ci"
          ),
          selected = "monthly_ratio_ci"
        ),
        hr(),
        tags$small(
          class = "text-muted",
          bs_icon("compass"),
          tags$strong(" How to Read This Page: "),
          "First, compare your chosen locality group against Virginia statewide benchmarks in the banner above. Next, see which localities and weather conditions drive severe crashes. Finally, expand the Confidence Interval panel to inspect each locality's monthly average and time-adjusted likely range."
        )
      ),
      
      # Screen-reader live region for KPI banner
      uiOutput("p1_kpi_sr_summary"),
      
      layout_column_wrap(
        width = 1/3,
        value_box(
          title = textOutput("p1_kpi_total_title", inline = TRUE),
          value = textOutput("p1_kpi_total"),
          tags$p(
            class = "small mb-0 opacity-75",
            textOutput("p1_kpi_total_comp", inline = TRUE)
          ),
          showcase = bs_icon("car-front-fill"),
          theme = "primary"
        ),
        value_box(
          title = textOutput("p1_kpi_rate_title", inline = TRUE),
          value = textOutput("p1_kpi_rate"),
          tags$p(
            class = "small mb-0 opacity-75",
            textOutput("p1_kpi_rate_comp", inline = TRUE)
          ),
          showcase = bs_icon("exclamation-triangle-fill"),
          theme = "danger"
        ),
        value_box(
          title = textOutput("p1_kpi_ratio_title", inline = TRUE),
          value = textOutput("p1_kpi_ratio"),
          tags$p(
            class = "small mb-0 opacity-75",
            textOutput("p1_kpi_ratio_comp", inline = TRUE)
          ),
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
            textOutput("p1_county_header", inline = TRUE)
          ),
          card_body(
            tags$p(
              class = "text-muted small mb-1",
              textOutput("p1_county_subtitle", inline = TRUE)
            ),
            plotlyOutput("p1_county_plot", height = "375px"),
            uiOutput("p1_county_sr_summary")
          ),
          card_footer(
            class = "text-muted small",
            "Shows overall crash burden across your selected localities and time period. Switch Filter #3 on the left to weigh raw volume against per-resident impact."
          )
        ),
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            textOutput("p1_weather_header", inline = TRUE)
          ),
          card_body(
            tags$p(
              class = "text-muted small mb-1",
              textOutput("p1_weather_subtitle", inline = TRUE)
            ),
            plotlyOutput("p1_weather_plot", height = "375px"),
            uiOutput("p1_weather_sr_summary")
          ),
          card_footer(
            class = "text-muted small",
            tags$strong("Why this chart stays fixed across Filter #3: "),
            "It updates with your locality group and time period, but always shows the share of crashes that turn severe. Because clear and cloudy days make up most of the year, switching to raw crash counts would just reflect everyday Virginia weather instead of highlighting which conditions are most dangerous on the road."
          )
        )
      ),
      
      # Page 1 Confidence Interval Show/Hide Section (hybrid storytelling + statistical rigor)
      card(
        full_screen = TRUE,
        
        card_header(
          class = "bg-dark text-white",
          "Confidence Interval Analysis"
        ),
        
        card_body(
          actionButton(
            "p1_toggle_ci",
            "Show Confidence Intervals",
            class = "btn-primary w-100 mb-3"
          ),
          
          conditionalPanel(
            condition = "input.p1_toggle_ci % 2 == 1",
            
            tags$div(
              class = "fw-bold mb-1",
              textOutput("p1_ci_header")
            ),
            
            tags$p(
              class = "text-muted small mb-2",
              textOutput("p1_ci_subtitle")
            ),
            
            layout_column_wrap(
              width = 1/2,
              
              tags$div(
                plotlyOutput("p1_ci_plot", height = "430px"),
                uiOutput("p1_ci_sr_summary")
              ),
              
              DTOutput("p1_ci_table")
            ),
            
            tags$div(
              class = "text-muted small mt-2",
              "How to read this chart (not a boxplot): The red diamond marks the Point Estimate (monthly average or overall severe crash share), and the blue horizontal bar shows the Likely Range — a Simultaneous 95% Confidence Interval adjusted for multiple comparisons (Bonferroni). For monthly averages, ranges are also time-adjusted to account for consecutive months following multi-month traffic patterns rather than resetting independently each month. Multi-year averages (2016–2023) span shifting traffic and reporting levels; filter to a single year to inspect a steadier period."
            )
          )
        )
      )
    )
  ),
  
  # ============================================================================
  # PAGE 2: COMPARE LOCALITIES OVER TIME & BY WEATHER (UP TO 3 LOCALITIES)
  # ============================================================================
  nav_panel(
    title = "2. Compare Localities Over Time & Weather",
    icon = bs_icon("graph-up"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Locality Comparison Controls",
        width = 330,
        selectizeInput(
          "p2_county",
          "1. Select 1 to 3 Virginia Localities:",
          choices = all_counties,
          selected = default_p2_counties,
          multiple = TRUE,
          options = list(
            maxItems = 3,
            plugins = list("remove_button"),
            placeholder = "Choose up to 3 localities to compare..."
          )
        ),
        radioButtons(
          "p2_ts_metric",
          "2. Monthly Trend Metric:",
          choices = c(
            "Monthly Severe Crashes per 100k Residents" = "ratio",
            "Monthly Severe Crash Share (%)"            = "rate",
            "Monthly Severe Crash Count (Raw Volume)"   = "count"
          ),
          selected = "ratio"
        ),
        radioButtons(
          "p2_weather_metric",
          "3. Weather Comparison Metric:",
          choices = c(
            "Severe Crash Share (% of Weather Crashes)" = "severe_rate",
            "Severe Crashes per 100k Residents"         = "severe_ratio"
          ),
          selected = "severe_rate"
        ),
        hr(),
        tags$small(
          class = "text-muted",
          bs_icon("check-circle"),
          " Comparison Tip: Select up to 3 localities to compare their monthly crash trends and weather risks head-to-head. Raw crash counts are only displayed for 1 locality at a time so larger and smaller communities are always compared fairly. Weather reports are grouped into 5 Core Weather Categories using months with at least 5 crashes."
        )
      ),
      
      # Page 2 KPI banner (head-to-head comparison across selected localities)
      uiOutput("p2_kpi_sr_summary"),
      layout_column_wrap(
        width = 1/3,
        value_box(
          title = textOutput("p2_kpi_total_title", inline = TRUE),
          value = textOutput("p2_kpi_total"),
          tags$p(
            class = "small mb-0 opacity-75",
            textOutput("p2_kpi_total_comp", inline = TRUE)
          ),
          showcase = bs_icon("car-front-fill"),
          theme = "primary"
        ),
        value_box(
          title = textOutput("p2_kpi_rate_title", inline = TRUE),
          value = textOutput("p2_kpi_rate"),
          tags$p(
            class = "small mb-0 opacity-75",
            textOutput("p2_kpi_rate_comp", inline = TRUE)
          ),
          showcase = bs_icon("exclamation-triangle-fill"),
          theme = "danger"
        ),
        value_box(
          title = textOutput("p2_kpi_ratio_title", inline = TRUE),
          value = textOutput("p2_kpi_ratio"),
          tags$p(
            class = "small mb-0 opacity-75",
            textOutput("p2_kpi_ratio_comp", inline = TRUE)
          ),
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
            textOutput("p2_ts_title", inline = TRUE)
          ),
          card_body(
            plotlyOutput("p2_ts_plot", height = "400px"),
            uiOutput("p2_ts_sr_summary")
          ),
          card_footer(
            class = "text-muted small",
            textOutput("p2_ts_footer", inline = TRUE)
          )
        ),
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            textOutput("p2_weather_title", inline = TRUE)
          ),
          card_body(
            plotlyOutput("p2_weather_plot", height = "400px"),
            uiOutput("p2_weather_sr_summary")
          ),
          card_footer(
            class = "text-muted small",
            "Side-by-side comparison of the 5 Core Weather Categories across your selected localities. Inspect the chart below to see each locality's time-adjusted monthly weather range."
          )
        )
      ),
      
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-dark text-white",
          textOutput("p2_ci_header", inline = TRUE)
        ),
        card_body(
          layout_column_wrap(
            width = 1/2,
            tags$div(
              plotlyOutput("p2_ci_plot", height = "460px"),
              uiOutput("p2_ci_sr_summary")
            ),
            DTOutput("p2_ci_table")
          )
        ),
        card_footer(
          class = "text-muted small",
          "How to read this chart (not a boxplot): Each locality appears on its own separate line within every weather category. The marker shows the Point Estimate (monthly mean), and the horizontal bar shows the Time-Adjusted Likely Range — a Simultaneous 95% Confidence Interval widened to account for consecutive months following multi-month traffic patterns across 2016–2023 and adjusted for multiple comparisons (Bonferroni)."
        )
      ),
      
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-secondary text-white",
          "Full Weather Breakdown: Crash Counts, Qualifying Months, Severe Shares, and Per-100k Totals"
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
          "Locality Comparison Data & Verification Export"
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
  # PAGE 3: HOTSPOT MAP & PRIORITY ROAD CORRIDORS
  # ============================================================================
  nav_panel(
    title = "3. Hotspot Map & Priority Corridors",
    icon = bs_icon("geo-alt-fill"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Map & Corridor Filters",
        width = 320,
        selectInput(
          "p3_county",
          "1. Select Virginia Locality (or Click Map):",
          choices = c("All Virginia (Statewide Overview)" = "ALL_VA", all_counties),
          selected = "ALL_VA"
        ),
        actionButton(
          "p3_reset_va",
          "Zoom Out to Full Virginia Map",
          icon = icon("globe-americas"),
          class = "btn-outline-primary w-100 mb-3"
        ),
        hr(),
        selectInput(
          "p3_year",
          "2. Select Year:",
          choices = c("All Years (Full Date Range)" = "All", as.character(all_years)),
          selected = "All"
        ),
        dateRangeInput(
          "p3_date_range",
          "3. Custom Date Range:",
          start = min_acc_date,
          end   = max_acc_date,
          min   = min_acc_date,
          max   = max_acc_date
        ),
        hr(),
        tags$small(
          class = "text-muted",
          bs_icon("info-circle"),
          " How to Use This Map: ",
          tags$br(),
          "• ", tags$strong("Statewide View:"), " Compare Virginia localities shaded by their share of severe crashes so large and small communities are weighed fairly. Localities with fewer than 20 crashes appear in gray.",
          tags$br(),
          "• ", tags$strong("Street Corridor View:"), " Click any locality on the map (or select above) to pinpoint the Top 30 ~1 km street zones ranked by Severe Crash Count."
        )
      ),
      
      layout_column_wrap(
        width = 1/2,
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            textOutput("p3_map_title", inline = TRUE)
          ),
          card_body(
            leafletOutput("p3_map", height = "560px"),
            uiOutput("p3_map_sr_summary")
          ),
          card_footer(
            class = "text-muted small",
            textOutput("p3_map_subtitle", inline = TRUE)
          )
        ),
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-dark text-white",
            textOutput("p3_table_title", inline = TRUE)
          ),
          card_body(
            DTOutput("p3_street_table")
          ),
          card_footer(
            class = "text-muted small",
            textOutput("p3_table_footer", inline = TRUE)
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
  # PDF DOWNLOAD HANDLER: DATA & STATISTICS DICTIONARY
  # ----------------------------------------------------------------------------
  output$download_data_dict_pdf <- downloadHandler(
    filename = function() {
      "Virginia_Traffic_Safety_Data_Statistics_Dictionary.pdf"
    },
    content = function(file) {
      pdf_paths <- c(
        "www/Virginia_Traffic_Safety_Data_Statistics_Dictionary.pdf",
        "Virginia_Traffic_Safety_Data_Statistics_Dictionary.pdf",
        "www/Virginia Traffic Safety Executive Dashboard — Data & Statistics Dictionary.pdf",
        "Virginia Traffic Safety Executive Dashboard — Data & Statistics Dictionary.pdf",
        "../Virginia_Traffic_Safety_Data_Statistics_Dictionary.pdf"
      )
      existing_pdf <- pdf_paths[file.exists(pdf_paths)][1]
      if (is.na(existing_pdf)) {
        stop(
          "PDF file 'Virginia_Traffic_Safety_Data_Statistics_Dictionary.pdf' not found. ",
          "Place it in the app's 'www/' folder or alongside 'app.R'."
        )
      }
      file.copy(existing_pdf, file, overwrite = TRUE)
    },
    contentType = "application/pdf"
  )
  
  # ----------------------------------------------------------------------------
  # PAGE 1: CONFIDENCE INTERVAL SHOW/HIDE BUTTON
  # ----------------------------------------------------------------------------
  observe({
    toggle_text <- if (input$p1_toggle_ci %% 2 == 1) {
      "Hide Confidence Intervals"
    } else {
      "Show Confidence Intervals"
    }
    
    updateActionButton(
      session,
      "p1_toggle_ci",
      label = toggle_text
    )
  })
  
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
    
    # 1. Monthly metrics per county (for Means and Time-Adjusted Newey-West 95% CIs)
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
        Total_Months_Observed        = n(),
        Active_Months_With_Crashes   = sum(accidents > 0, na.rm = TRUE),
        Mean_Monthly_Total_Acc       = round(mean(accidents, na.rm = TRUE), 2),
        Mean_Monthly_Severe_Acc      = round(mean(severe_accidents, na.rm = TRUE), 2),
        Mean_Monthly_Severe_Ratio    = round(mean(severe_ratio, na.rm = TRUE), 2),
        nw_ratio                     = list(calc_newey_west_se(month_year, severe_ratio)),
        Mean_Monthly_Severe_Rate_Raw = mean(severe_rate, na.rm = TRUE),
        Mean_Monthly_Severe_Rate_Pct = round(Mean_Monthly_Severe_Rate_Raw * 100, 2),
        nw_rate                      = list(calc_newey_west_se(month_year, severe_rate)),
        .groups = "drop"
      ) %>%
      mutate(
        SE_Monthly_Ratio           = map_dbl(nw_ratio, ~ .x$SE),
        Ratio_NW_SE_Multiplier     = round(map_dbl(nw_ratio, ~ .x$SE_Mult), 2),
        Monthly_Ratio_95CI_Low     = round(pmax(0, Mean_Monthly_Severe_Ratio - qt(0.975, df = pmax(1, Total_Months_Observed - 1)) * SE_Monthly_Ratio), 2),
        Monthly_Ratio_95CI_High    = round(Mean_Monthly_Severe_Ratio + qt(0.975, df = pmax(1, Total_Months_Observed - 1)) * SE_Monthly_Ratio, 2),
        SE_Monthly_Rate            = map_dbl(nw_rate, ~ .x$SE),
        Rate_NW_SE_Multiplier      = round(map_dbl(nw_rate, ~ .x$SE_Mult), 2),
        Monthly_Rate_95CI_Low_Pct  = round(pmax(0, (Mean_Monthly_Severe_Rate_Raw - qt(0.975, df = pmax(1, Active_Months_With_Crashes - 1)) * SE_Monthly_Rate) * 100), 2),
        Monthly_Rate_95CI_High_Pct = round(pmin(100, (Mean_Monthly_Severe_Rate_Raw + qt(0.975, df = pmax(1, Active_Months_With_Crashes - 1)) * SE_Monthly_Rate) * 100), 2)
      ) %>%
      select(
        census_name, Total_Months_Observed, Active_Months_With_Crashes,
        Mean_Monthly_Total_Acc, Mean_Monthly_Severe_Acc,
        Mean_Monthly_Severe_Ratio, Monthly_Ratio_95CI_Low, Monthly_Ratio_95CI_High, Ratio_NW_SE_Multiplier,
        Mean_Monthly_Severe_Rate_Pct, Monthly_Rate_95CI_Low_Pct, Monthly_Rate_95CI_High_Pct, Rate_NW_SE_Multiplier
      )
    
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
        Total_Accidents           = replace_na(Total_Accidents, 0L),
        Severe_Accidents          = replace_na(Severe_Accidents, 0L),
        Overall_Severe_Rate_Pct   = round(ifelse(Total_Accidents > 0, (Severe_Accidents / Total_Accidents) * 100, 0), 2),
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
  # PAGE 1 SERVER: DYNAMIC PLAIN-LANGUAGE STORYTELLING & STATEWIDE BENCHMARKS
  # ----------------------------------------------------------------------------
  p1_tier_label <- reactive({
    switch(
      input$p1_pop_tier,
      "top5"  = "the Top 5 Most Populous Virginia Localities",
      "mid5"  = "the Middle 5 Virginia Localities by Population",
      "bot5"  = "the Bottom 5 Least Populous Virginia Localities",
      "top10" = "the Top 10 Most Populous Virginia Localities",
      "all"   = "All Virginia Localities"
    )
  })
  
  p1_tier_short <- reactive({
    switch(
      input$p1_pop_tier,
      "top5"  = "Top 5 VA Localities",
      "mid5"  = "Middle 5 VA Localities",
      "bot5"  = "Bottom 5 VA Localities",
      "top10" = "Top 10 VA Localities",
      "all"   = "All VA Localities"
    )
  })
  
  p1_year_label <- reactive({
    ifelse(input$p1_year == "All", "2016–2023", input$p1_year)
  })
  
  p1_metric_label_clean <- reactive({
    switch(
      input$p1_metric,
      "total_accidents" = "total recorded crashes",
      "severe_rate"     = "share of crashes that are severe",
      "severe_ratio"    = "severe crashes per 100k residents"
    )
  })
  
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
  
  # Statewide Virginia benchmark for the selected time window (used in KPI comparisons)
  p1_statewide_benchmark <- reactive({
    df_va <- if (input$p1_year == "All") {
      acc
    } else {
      acc %>% filter(Year == as.numeric(input$p1_year))
    }
    va_total  <- nrow(df_va)
    va_severe <- sum(df_va$severe, na.rm = TRUE)
    va_pop    <- sum(county_pop_ranks$population, na.rm = TRUE)
    
    list(
      total_accidents  = va_total,
      severe_accidents = va_severe,
      population       = va_pop,
      severe_rate      = ifelse(va_total > 0, va_severe / va_total, 0),
      severe_ratio     = ifelse(va_pop > 0, (va_severe / va_pop) * 100000, 0)
    )
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
  
  # Dynamic Page 1 KPI Titles & Benchmark Comparison Subtitles
  output$p1_kpi_total_title <- renderText({
    if (input$p1_pop_tier == "all") {
      paste0("Total VA Crashes (", p1_year_label(), ")")
    } else {
      paste0("Total Crashes — ", p1_tier_short(), " vs. VA")
    }
  })
  
  output$p1_kpi_rate_title <- renderText({
    if (input$p1_pop_tier == "all") {
      paste0("VA Statewide Severe Share (", p1_year_label(), ")")
    } else {
      paste0("Severe Crash Share — ", p1_tier_short(), " vs. VA")
    }
  })
  
  output$p1_kpi_ratio_title <- renderText({
    if (input$p1_pop_tier == "all") {
      paste0("VA Severe Crashes per 100k (", p1_year_label(), ")")
    } else {
      paste0("Severe per 100k — ", p1_tier_short(), " vs. VA")
    }
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
  
  p1_kpi_comp_texts <- reactive({
    df <- p1_county_summary()
    va <- p1_statewide_benchmark()
    
    tot_acc    <- sum(df$total_accidents, na.rm = TRUE)
    tot_sev    <- sum(df$severe_accidents, na.rm = TRUE)
    tot_pop    <- sum(df$population, na.rm = TRUE)
    tier_rate  <- ifelse(tot_acc > 0, tot_sev / tot_acc, 0)
    tier_ratio <- ifelse(tot_pop > 0, (tot_sev / tot_pop) * 100000, 0)
    
    if (input$p1_pop_tier == "all") {
      total_comp <- paste0(
        comma(tot_sev), " severe crashes across all ", nrow(df), " Virginia localities"
      )
      rate_comp <- paste0(
        "Statewide baseline (", comma(tot_sev), " severe of ", comma(tot_acc), " total)"
      )
      ratio_comp <- paste0(
        "Statewide average across ", comma(tot_pop), " Virginia residents"
      )
    } else {
      crash_share <- ifelse(va$total_accidents > 0, tot_acc / va$total_accidents, 0)
      pop_share   <- ifelse(va$population > 0, tot_pop / va$population, 0)
      total_comp <- paste0(
        percent(crash_share, accuracy = 0.1), " of all VA crashes (vs. ",
        percent(pop_share, accuracy = 0.1), " of VA population)"
      )
      
      diff_pts <- (tier_rate - va$severe_rate) * 100
      va_rate_txt <- percent(va$severe_rate, accuracy = 0.1)
      rate_comp <- if (abs(diff_pts) < 0.05) {
        paste0("Matches VA statewide average (", va_rate_txt, ")")
      } else if (diff_pts > 0) {
        paste0("+", sprintf("%.1f", diff_pts), " pts above VA average (", va_rate_txt, ")")
      } else {
        paste0(sprintf("%.1f", abs(diff_pts)), " pts below VA average (", va_rate_txt, ")")
      }
      
      pct_diff <- ifelse(va$severe_ratio > 0, ((tier_ratio - va$severe_ratio) / va$severe_ratio) * 100, 0)
      va_ratio_txt <- number(va$severe_ratio, accuracy = 0.1, big.mark = ",")
      ratio_comp <- if (abs(pct_diff) < 0.5) {
        paste0("Matches VA statewide average (", va_ratio_txt, " per 100k)")
      } else if (pct_diff > 0) {
        paste0(round(pct_diff), "% higher than VA average (", va_ratio_txt, " per 100k)")
      } else {
        paste0(round(abs(pct_diff)), "% lower than VA average (", va_ratio_txt, " per 100k)")
      }
    }
    
    list(total = total_comp, rate = rate_comp, ratio = ratio_comp)
  })
  
  output$p1_kpi_total_comp <- renderText({ p1_kpi_comp_texts()$total })
  output$p1_kpi_rate_comp  <- renderText({ p1_kpi_comp_texts()$rate })
  output$p1_kpi_ratio_comp <- renderText({ p1_kpi_comp_texts()$ratio })
  
  # Screen-reader summary for Page 1 KPIs (includes statewide comparisons)
  output$p1_kpi_sr_summary <- renderUI({
    df    <- p1_county_summary()
    comps <- p1_kpi_comp_texts()
    tot_acc <- sum(df$total_accidents, na.rm = TRUE)
    tot_sev <- sum(df$severe_accidents, na.rm = TRUE)
    tot_pop <- sum(df$population, na.rm = TRUE)
    rate_str  <- if (tot_acc > 0) percent(tot_sev / tot_acc, 0.1) else "0.0%"
    ratio_str <- if (tot_pop > 0) number((tot_sev / tot_pop) * 100000, 0.1, big.mark = ",") else "0.0"
    
    msg <- paste0(
      "Overview for ", p1_tier_label(), " (", p1_year_label(), "): ",
      comma(tot_acc), " total crashes (", comps$total, "); ",
      rate_str, " severe crash share (", comps$rate, "); and ",
      ratio_str, " severe crashes per 100,000 residents (", comps$ratio, ")."
    )
    render_a11y_box(msg, input$a11y_show_text)
  })
  
  # Chart 1 Dynamic Story Title & Subtitle
  output$p1_county_header <- renderText({
    df <- p1_county_summary() %>% arrange(desc(.data[[input$p1_metric]]))
    if (nrow(df) == 0) {
      return(paste0("Severe Traffic Crashes Across ", p1_tier_short(), " (", p1_year_label(), ")"))
    }
    top_row <- df %>% slice(1)
    top_val <- switch(
      input$p1_metric,
      "total_accidents" = paste0(comma(top_row$total_accidents), " total crashes"),
      "severe_rate"     = paste0(percent(top_row$severe_rate, accuracy = 0.1), " severe crashes"),
      "severe_ratio"    = paste0(number(top_row$severe_ratio, accuracy = 0.1, big.mark = ","), " severe crashes per 100k")
    )
    scope_phrase <- ifelse(input$p1_pop_tier == "all", "Virginia (Top 15 Shown)", paste0("the ", p1_tier_short()))
    paste0(
      top_row$census_name, " Leads ", scope_phrase,
      " with ", top_val, " (", p1_year_label(), ")"
    )
  })
  
  output$p1_county_subtitle <- renderText({
    metric_phrase <- switch(
      input$p1_metric,
      "total_accidents" = "Comparing total recorded crashes (all severities)",
      "severe_rate"     = "Comparing the percentage of crashes classified as severe (Severity 3–4)",
      "severe_ratio"    = "Comparing severe crashes (Severity 3–4) per 100,000 residents"
    )
    scope_note <- ifelse(
      input$p1_pop_tier == "all",
      paste0("Top 15 of ", nrow(county_pop_ranks), " Virginia localities (", p1_year_label(), ")."),
      paste0("Across ", p1_tier_label(), " (", p1_year_label(), ").")
    )
    paste0(metric_phrase, " — ", scope_note)
  })
  
  output$p1_county_plot <- renderPlotly({
    df <- p1_county_summary()
    if (input$p1_pop_tier == "all") {
      df <- df %>% slice_max(.data[[input$p1_metric]], n = 15)
    }
    
    metric_label <- switch(
      input$p1_metric,
      "total_accidents" = "Total Recorded Traffic Crashes",
      "severe_rate"     = "Share of Crashes That Are Severe (%)",
      "severe_ratio"    = "Severe Traffic Crashes per 100,000 Residents"
    )
    
    df <- df %>%
      mutate(
        val_label = case_when(
          input$p1_metric == "severe_rate"  ~ percent(severe_rate, accuracy = 0.1),
          input$p1_metric == "severe_ratio" ~ number(severe_ratio, accuracy = 0.1, big.mark = ","),
          TRUE                              ~ comma(total_accidents)
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
          "Total Crashes: ", comma(total_accidents), "<br>",
          "Severe Crashes: ", comma(severe_accidents), "<br>",
          "Severe Crash Share: ", percent(severe_rate, accuracy = 0.1), "<br>",
          "Severe Crashes per 100k: ", round(severe_ratio, 1)
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
      config(
        displayModeBar = TRUE,
        displaylogo = FALSE
      )
  })
  
  output$p1_county_sr_summary <- renderUI({
    df <- p1_county_summary() %>% arrange(desc(.data[[input$p1_metric]]))
    if (input$p1_pop_tier == "all") {
      df <- df %>% slice_head(n = 15)
    }
    if (nrow(df) == 0) return(NULL)
    top_row <- df %>% slice(1)
    bot_row <- df %>% slice(n())
    
    fmt_val <- function(r) {
      switch(
        input$p1_metric,
        "total_accidents" = paste0(comma(r$total_accidents), " total crashes"),
        "severe_rate"     = paste0(percent(r$severe_rate, 0.1), " severe crash share"),
        "severe_ratio"    = paste0(round(r$severe_ratio, 1), " severe crashes per 100k residents")
      )
    }
    
    msg <- paste0(
      "Locality comparison for ", p1_tier_label(), " (", p1_year_label(), "), ranked by ",
      p1_metric_label_clean(), ". ",
      "Highest locality is ", top_row$census_name, " with ", fmt_val(top_row),
      " (", comma(top_row$severe_accidents), " severe out of ", comma(top_row$total_accidents), " total crashes). ",
      "Lowest shown is ", bot_row$census_name, " with ", fmt_val(bot_row), "."
    )
    render_a11y_box(msg, input$a11y_show_text)
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
      slice_max(severe_percent, n = 10)
    
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
  
  # Chart 2 Dynamic Story Title & Subtitle (Always Severe Crash Share %)
  output$p1_weather_header <- renderText({
    df <- p1_weather_summary() %>% arrange(desc(severe_percent))
    if (nrow(df) == 0) {
      return(paste0(
        "Top Weather Conditions Linked to Severe Crashes in ",
        p1_tier_short(), " (", p1_year_label(), ")"
      ))
    }
    w1 <- df %>% slice(1)
    paste0(
      "In ", p1_tier_short(), ", ", w1$Weather_Condition,
      " Shows the Highest Severe Crash Share (",
      percent(w1$severe_percent, accuracy = 0.1), " | ", p1_year_label(), ")"
    )
  })
  
  output$p1_weather_subtitle <- renderText({
    paste0(
      "Top 10 weather conditions most strongly associated with severe crashes across ",
      p1_tier_label(), " (", p1_year_label(), "; min. 20 crashes). Always displays Severe Crash Share (%) regardless of Filter #3."
    )
  })
  
  output$p1_weather_plot <- renderPlotly({
    df <- p1_weather_summary()
    validate(
      need(nrow(df) > 0, "No weather conditions meet the minimum crash count (20 crashes) for this selection.")
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
          "Locality Group: ", p1_tier_short(), " (", p1_year_label(), ")<br>",
          "Severe Crash Share: ", percent(severe_percent, accuracy = 0.1), "<br>",
          "Severe / Total Crashes: ", comma(severe_accidents), " / ", comma(total_accidents)
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
        y = paste0("Share of Crashes That Are Severe in ", p1_tier_short(), " (%)")
      ) +
      theme_minimal(base_size = base_sz) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(face = "bold", color = "#1A202C")
      )
    
    ggplotly(p, tooltip = "text") %>%
      config(
        displayModeBar = TRUE,
        displaylogo = FALSE
      )
  })
  
  output$p1_weather_sr_summary <- renderUI({
    df <- p1_weather_summary() %>% arrange(desc(severe_percent))
    if (nrow(df) == 0) return(NULL)
    w1 <- df %>% slice(1)
    w2 <- if (nrow(df) >= 2) df %>% slice(2) else NULL
    
    msg <- paste0(
      "Top weather conditions linked to severe crashes across ", p1_tier_label(), " (", p1_year_label(),
      ", minimum 20 crashes). ",
      "Rank 1 is ", w1$Weather_Condition, " at ", percent(w1$severe_percent, 0.1),
      " severe (", comma(w1$severe_accidents), " of ", comma(w1$total_accidents), " crashes; likely range / Bonferroni 95% CI: ",
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
    if (input$p1_ci_type == "weather_binom_ci") {
      df <- p1_weather_summary() %>%
        transmute(
          Group         = Weather_Condition,
          N_Units       = total_accidents,
          Mean_Est      = severe_percent,
          CI_Lower      = lower_ci,
          CI_Upper      = upper_ci,
          m_comparisons = m_comparisons,
          conf_level    = conf_level,
          Eff_Months    = NA_real_,
          NW_Lag        = NA_integer_,
          SE_Mult       = NA_real_
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
    
    target_col <- ifelse(input$p1_ci_type == "monthly_ratio_ci", "severe_ratio", "severe_rate")
    upper_cap  <- ifelse(input$p1_ci_type == "monthly_ratio_ci", Inf, 1.0)
    
    res <- compute_simple_ci(
      df          = monthly,
      group_col   = "census_name",
      value_col   = target_col,
      pop_col     = "population",
      time_col    = "month_year",
      mult_method = "bonferroni",
      upper_cap   = upper_cap,
      min_months  = 6
    )
    if (nrow(res) == 0) return(tibble())
    res %>% arrange(desc(pop))
  })
  
  # Chart 3 Dynamic Story Title
  output$p1_ci_header <- renderText({
    df <- p1_ci_data() %>% arrange(desc(Mean_Est))
    is_pct <- input$p1_ci_type %in% c("monthly_rate_ci", "weather_binom_ci")
    
    if (nrow(df) == 0) {
      return(paste0(
        "Monthly Averages & Variability Across ",
        p1_tier_short(), " (", p1_year_label(), ")"
      ))
    }
    
    top_g <- df %>% slice(1)
    top_fmt <- if (is_pct) percent(top_g$Mean_Est, accuracy = 0.1) else number(top_g$Mean_Est, accuracy = 0.1)
    
    switch(
      input$p1_ci_type,
      "monthly_ratio_ci" = paste0(
        "How Steady Are These Numbers? ", top_g$Group,
        " Averages ", top_fmt, " Severe Crashes per 100k Each Month in ",
        p1_tier_short(), " (", p1_year_label(), ")"
      ),
      "monthly_rate_ci" = paste0(
        "How Steady Are These Numbers? ", top_g$Group,
        " Averages ", top_fmt, " Severe Crashes Each Month in ",
        p1_tier_short(), " (", p1_year_label(), ")"
      ),
      "weather_binom_ci" = paste0(
        "How Reliable Are Weather Risk Differences in ", p1_tier_short(), "? ",
        top_g$Group, " Leads at ", top_fmt, " (", p1_year_label(), ")"
      )
    )
  })
  
  # Chart 3 Hybrid Subtitle (Plain-English Time-Adjusted explanation + Bonferroni metadata)
  output$p1_ci_subtitle <- renderText({
    df <- p1_ci_data()
    m_val <- if (nrow(df) > 0) df$m_comparisons[1] else 1
    c_lvl <- if (nrow(df) > 0) percent(df$conf_level[1], accuracy = 0.01) else "95.0%"
    is_monthly <- input$p1_ci_type != "weather_binom_ci"
    
    stat_note <- if (is_monthly) {
      paste0(
        " [Time-Adjusted Bonferroni Simultaneous 95% CI: m = ", m_val,
        " comparisons, ", c_lvl, " individual interval level.]"
      )
    } else {
      paste0(
        " [Bonferroni Simultaneous 95% CI: m = ", m_val,
        " comparisons, ", c_lvl, " individual interval level.]"
      )
    }
    
    trend_note <- if (is_monthly && input$p1_year == "All") {
      " Multi-year averages span shifting 2016–2023 traffic and reporting levels; select a single year on the left to inspect a steadier period."
    } else {
      ""
    }
    
    plain_note <- switch(
      input$p1_ci_type,
      "monthly_ratio_ci" = paste0(
        "Typical monthly average (red diamond) and time-adjusted likely range (blue bar) across ",
        p1_tier_label(), " (", p1_year_label(),
        "). Widened to account for consecutive months following multi-month traffic patterns rather than resetting independently each month.",
        trend_note
      ),
      "monthly_rate_ci" = paste0(
        "Typical monthly severe crash share (red diamond) and time-adjusted likely range (blue bar) across ",
        p1_tier_label(), " (", p1_year_label(),
        "). Widened to account for consecutive months following multi-month traffic patterns rather than resetting independently each month.",
        trend_note
      ),
      "weather_binom_ci" = paste0(
        "Overall severe crash share (red diamond) and likely range (blue bar) for each top weather condition in ",
        p1_tier_label(), " (", p1_year_label(), "). Narrower bars reflect larger crash counts."
      )
    )
    paste0(plain_note, stat_note)
  })
  
  output$p1_ci_plot <- renderPlotly({
    df <- p1_ci_data()
    validate(need(nrow(df) > 0, "Insufficient data to compute confidence intervals."))
    
    is_pct     <- input$p1_ci_type %in% c("monthly_rate_ci", "weather_binom_ci")
    is_monthly <- input$p1_ci_type != "weather_binom_ci"
    
    y_lab  <- switch(
      input$p1_ci_type,
      "monthly_ratio_ci" = paste0("Monthly Avg Severe Crashes per 100k — Mean ± Time-Adjusted 95% CI (", p1_tier_short(), ")"),
      "monthly_rate_ci"  = paste0("Monthly Avg Severe Crash Share % — Mean ± Time-Adjusted 95% CI (", p1_tier_short(), ")"),
      "weather_binom_ci" = paste0("Severe Crash Share % — Sim. 95% Binomial CI (", p1_tier_short(), ")")
    )
    
    unit_word <- ifelse(is_monthly, "months", "crashes")
    range_lbl <- ifelse(is_monthly, "Time-Adjusted Likely Range (Sim. 95% CI, ", "Likely Range (Sim. 95% CI, ")
    base_sz   <- ifelse(isTRUE(input$a11y_high_contrast), 14, 12)
    
    df <- df %>%
      mutate(
        eff_line = if (is_monthly) {
          paste0("<br>Effective Independent Months: ~", round(Eff_Months, 1), " of ", comma(N_Units))
        } else {
          ""
        },
        hover_txt = paste0(
          "<b>", Group, "</b><br>",
          "Point Estimate (Average): ", ifelse(is_pct, percent(Mean_Est, 0.1), round(Mean_Est, 2)), "<br>",
          range_lbl, percent(conf_level, 0.01), "): [",
          ifelse(is_pct, percent(CI_Lower, 0.1), round(CI_Lower, 2)), ", ",
          ifelse(is_pct, percent(CI_Upper, 0.1), round(CI_Upper, 2)), "]<br>",
          "Observed (n): ", comma(N_Units), " ", unit_word,
          eff_line,
          " | Comparisons (m): ", m_comparisons
        )
      )
    
    p <- ggplot(df, aes(x = reorder(Group, Mean_Est), y = Mean_Est)) +
      geom_errorbar(
        aes(ymin = CI_Lower, ymax = CI_Upper),
        width = 0, color = exec_teal, linewidth = ifelse(isTRUE(input$a11y_high_contrast), 3.2, 2.4)
      ) +
      geom_point(
        aes(text = hover_txt),
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
      config(
        displayModeBar = TRUE,
        displaylogo = FALSE
      )
  })
  
  # Hybrid Page 1 CI Summary (plain-language CI teaching + time-adjustment explanation)
  output$p1_ci_sr_summary <- renderUI({
    df <- p1_ci_data() %>% arrange(desc(Mean_Est))
    if (nrow(df) == 0) return(NULL)
    
    is_pct     <- input$p1_ci_type %in% c("monthly_rate_ci", "weather_binom_ci")
    is_monthly <- input$p1_ci_type != "weather_binom_ci"
    top_g <- df %>% slice(1)
    fmt <- function(v) if (is_pct) percent(v, 0.1) else round(v, 2)
    unit_word <- ifelse(is_monthly, "months", "crashes")
    
    interval_width <- top_g$CI_Upper - top_g$CI_Lower
    precision_msg <- if (
      !is.na(interval_width) && interval_width < abs(top_g$Mean_Est) * 0.25
    ) {
      "The range is relatively narrow, so the estimate is fairly precise."
    } else {
      "The range is relatively wide, so there is more uncertainty around the estimate."
    }
    
    metric_name <- switch(
      input$p1_ci_type,
      "monthly_ratio_ci" = "monthly average severe crashes per 100,000 residents",
      "monthly_rate_ci"  = "monthly average severe crash share",
      "weather_binom_ci" = "severe crash share in this weather condition",
      "crash measure"
    )
    
    time_adj_msg <- if (is_monthly) {
      paste0(
        " Because consecutive months tend to track multi-month traffic patterns rather than resetting independently each month, ",
        "these likely ranges are time-adjusted (about ", round(top_g$Eff_Months, 1),
        " effective independent months out of ", comma(top_g$N_Units), " observed for ", top_g$Group, ") ",
        "so uncertainty is not understated.",
        if (input$p1_year == "All") {
          " Note that 2016–2023 averages span shifting multi-year traffic and reporting levels; filter to a single year to inspect a steadier period."
        } else {
          ""
        }
      )
    } else {
      ""
    }
    
    msg <- paste0(
      "What this means (", p1_tier_label(), ", ", p1_year_label(), "; ", nrow(df), " groups shown): ",
      "A ", ifelse(is_monthly, "time-adjusted simultaneous 95% confidence interval", "simultaneous 95% confidence interval"),
      " (shown as the likely range) gives a span of values consistent with the data, rather than relying on one single estimate. ",
      "For the highest group, ", top_g$Group, ", the estimated ", metric_name, " is ",
      fmt(top_g$Mean_Est), " based on ", comma(top_g$N_Units), " ", unit_word,
      ", with a likely range (Bonferroni 95% CI) from ",
      fmt(top_g$CI_Lower), " to ", fmt(top_g$CI_Upper), ". ",
      precision_msg,
      time_adj_msg,
      " This interval helps communicate how much uncertainty there is in the estimate; ",
      "it does not guarantee that the true value falls within this particular range. ",
      "Full numbers appear in the adjacent table."
    )
    
    render_a11y_box(msg, input$a11y_show_text)
  })
  
  output$p1_ci_table <- renderDT({
    df <- p1_ci_data()
    is_pct     <- input$p1_ci_type %in% c("monthly_rate_ci", "weather_binom_ci")
    is_monthly <- input$p1_ci_type != "weather_binom_ci"
    
    tbl <- df %>%
      select(Group, N_Units, Mean_Est, CI_Lower, CI_Upper)
    
    group_col_label <- ifelse(is_monthly, "Virginia Locality", "Weather Condition")
    n_col_label     <- ifelse(is_monthly, "Months (n)", "Crashes (n)")
    est_col_label   <- switch(
      input$p1_ci_type,
      "monthly_ratio_ci" = "Monthly Avg (per 100k)",
      "monthly_rate_ci"  = "Monthly Avg Share (%)",
      "weather_binom_ci" = "Severe Crash Share (%)"
    )
    low_col_label  <- ifelse(is_monthly, "Time-Adj. Low (95% CI)", "Range Low (95% CI)")
    high_col_label <- ifelse(is_monthly, "Time-Adj. High (95% CI)", "Range High (95% CI)")
    
    cap_metric <- switch(
      input$p1_ci_type,
      "monthly_ratio_ci" = "Monthly Averages & Time-Adjusted Simultaneous 95% CIs (Severe per 100k)",
      "monthly_rate_ci"  = "Monthly Averages & Time-Adjusted Simultaneous 95% CIs (Severe Share %)",
      "weather_binom_ci" = "Weather Severe Crash Shares & Simultaneous 95% Binomial CIs"
    )
    
    dt <- datatable(
      tbl,
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = "caption-side: top; font-weight: bold; color: #0F2042;",
        paste0(cap_metric, " — ", p1_tier_short(), " (", p1_year_label(), ")")
      ),
      colnames = c(group_col_label, n_col_label, est_col_label, low_col_label, high_col_label),
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
  # PAGE 2 SERVER: MULTI-LOCALITY COMPARISON (1 TO 3 LOCALITIES MAX)
  # ----------------------------------------------------------------------------
  p2_selected_locs <- reactive({
    locs <- input$p2_county
    if (is.null(locs) || length(locs) == 0) return(character(0))
    head(locs, 3)
  })
  
  p2_locs_label <- reactive({
    locs <- p2_selected_locs()
    if (length(locs) == 0) return("Selected Localities")
    if (length(locs) == 1) return(locs[1])
    paste(locs, collapse = " vs. ")
  })
  
  p2_color_map <- reactive({
    locs <- p2_selected_locs()
    if (length(locs) == 0) return(character(0))
    setNames(p2_compare_colors[seq_along(locs)], locs)
  })
  
  p2_shape_map <- reactive({
    locs <- p2_selected_locs()
    if (length(locs) == 0) return(integer(0))
    setNames(p2_compare_shapes[seq_along(locs)], locs)
  })
  
  p2_county_acc <- reactive({
    locs <- p2_selected_locs()
    validate(need(length(locs) >= 1, "Please select 1 to 3 Virginia localities in the sidebar to begin."))
    acc %>%
      filter(census_name %in% locs) %>%
      mutate(census_name = factor(census_name, levels = locs))
  })
  
  # Per-locality KPI summary table for the 1 to 3 selected localities
  p2_kpi_df <- reactive({
    locs <- p2_selected_locs()
    if (length(locs) == 0) return(tibble())
    
    agg <- acc %>%
      filter(census_name %in% locs) %>%
      group_by(census_name) %>%
      summarise(
        total        = n(),
        severe_total = sum(severe, na.rm = TRUE),
        .groups      = "drop"
      )
    
    county_pop_ranks %>%
      filter(census_name %in% locs) %>%
      select(census_name, population) %>%
      left_join(agg, by = "census_name") %>%
      mutate(
        total           = replace_na(total, 0L),
        severe_total    = replace_na(severe_total, 0L),
        severe_rate     = ifelse(total > 0, severe_total / total, NA_real_),
        severe_per_100k = ifelse(!is.na(population) & population > 0, (severe_total / population) * 100000, NA_real_),
        short_name      = str_remove(census_name, " (County|city)$")
      )
  })
  
  # Dynamic KPI Titles, Lead Values & Comparison Lines
  output$p2_kpi_total_title <- renderText({
    df <- p2_kpi_df()
    if (nrow(df) == 0) return("Total Recorded Crashes")
    if (nrow(df) == 1) return(paste0("Total Crashes — ", df$census_name[1]))
    top_r <- df %>% arrange(desc(total)) %>% slice(1)
    paste0("Highest Crash Volume: ", top_r$short_name)
  })
  
  output$p2_kpi_total <- renderText({
    df <- p2_kpi_df()
    if (nrow(df) == 0) return("—")
    top_r <- df %>% arrange(desc(total)) %>% slice(1)
    comma(top_r$total)
  })
  
  output$p2_kpi_total_comp <- renderText({
    df <- p2_kpi_df() %>% arrange(desc(total))
    if (nrow(df) == 0) return("Select 1–3 localities")
    if (nrow(df) == 1) return(paste0(comma(df$severe_total[1]), " severe crashes (2016–2023)"))
    others <- df %>% slice(-1)
    paste0("vs. ", paste0(others$short_name, ": ", comma(others$total), collapse = " | "))
  })
  
  output$p2_kpi_rate_title <- renderText({
    df <- p2_kpi_df()
    if (nrow(df) == 0) return("Overall Severe Crash Share")
    if (nrow(df) == 1) return(paste0("Severe Crash Share — ", df$census_name[1]))
    top_r <- df %>% arrange(desc(severe_rate)) %>% slice(1)
    paste0("Highest Severe Share: ", top_r$short_name)
  })
  
  output$p2_kpi_rate <- renderText({
    df <- p2_kpi_df()
    if (nrow(df) == 0) return("—")
    top_r <- df %>% arrange(desc(severe_rate)) %>% slice(1)
    if (is.na(top_r$severe_rate)) "N/A" else percent(top_r$severe_rate, accuracy = 0.1)
  })
  
  output$p2_kpi_rate_comp <- renderText({
    df <- p2_kpi_df() %>% arrange(desc(severe_rate))
    if (nrow(df) == 0) return("Select 1–3 localities")
    if (nrow(df) == 1) {
      return(paste0(comma(df$severe_total[1]), " severe of ", comma(df$total[1]), " total crashes"))
    }
    others <- df %>% slice(-1)
    paste0("vs. ", paste0(others$short_name, ": ", percent(others$severe_rate, accuracy = 0.1), collapse = " | "))
  })
  
  output$p2_kpi_ratio_title <- renderText({
    df <- p2_kpi_df()
    if (nrow(df) == 0) return("Severe Crashes per 100k")
    if (nrow(df) == 1) return(paste0("Severe per 100k — ", df$census_name[1]))
    top_r <- df %>% arrange(desc(severe_per_100k)) %>% slice(1)
    paste0("Highest Per-100k Burden: ", top_r$short_name)
  })
  
  output$p2_kpi_ratio <- renderText({
    df <- p2_kpi_df()
    if (nrow(df) == 0) return("—")
    top_r <- df %>% arrange(desc(severe_per_100k)) %>% slice(1)
    if (is.na(top_r$severe_per_100k)) "N/A" else number(top_r$severe_per_100k, accuracy = 0.1, big.mark = ",")
  })
  
  output$p2_kpi_ratio_comp <- renderText({
    df <- p2_kpi_df() %>% arrange(desc(severe_per_100k))
    if (nrow(df) == 0) return("Select 1–3 localities")
    if (nrow(df) == 1) {
      return(paste0("Across ", comma(df$population[1]), " residents (2016–2023)"))
    }
    others <- df %>% slice(-1)
    paste0("vs. ", paste0(others$short_name, ": ", number(others$severe_per_100k, accuracy = 0.1, big.mark = ","), collapse = " | "))
  })
  
  output$p2_kpi_sr_summary <- renderUI({
    df <- p2_kpi_df()
    if (nrow(df) == 0) {
      return(render_a11y_box("No localities selected. Choose 1 to 3 Virginia localities in the sidebar.", input$a11y_show_text))
    }
    parts <- pmap_chr(df, function(census_name, population, total, severe_total, severe_rate, severe_per_100k, short_name) {
      paste0(
        census_name, ": ", comma(total), " total crashes, ",
        ifelse(is.na(severe_rate), "N/A", percent(severe_rate, 0.1)), " severe share, and ",
        ifelse(is.na(severe_per_100k), "N/A", number(severe_per_100k, accuracy = 0.1, big.mark = ",")),
        " severe crashes per 100k residents"
      )
    })
    msg <- paste0("Locality comparison (2016–2023) — ", paste(parts, collapse = "; "), ".")
    render_a11y_box(msg, input$a11y_show_text)
  })
  
  output$p2_ts_title <- renderText({
    metric_txt <- switch(
      input$p2_ts_metric,
      "ratio" = "Monthly Severe Crashes per 100k Residents",
      "rate"  = "Monthly Severe Crash Share (%)",
      "count" = "Monthly Severe Crash Count"
    )
    paste0("Over-Time Comparison: ", p2_locs_label(), " — ", metric_txt)
  })
  
  output$p2_ts_footer <- renderText({
    n_locs <- length(p2_selected_locs())
    if (n_locs <= 1) {
      "The line tracks monthly results over time, while the shaded band shows the expected range of monthly variability (Bonferroni simultaneous 95% confidence band)."
    } else if (input$p2_ts_metric == "count") {
      "Raw crash counts are only plotted for a single locality at a time so differences in population size do not distort the comparison."
    } else {
      "Each colored line tracks one locality over time on a shared scale. Monthly confidence bands are turned off when comparing multiple localities so lines stay clear and easy to follow."
    }
  })
  
  output$p2_weather_title <- renderText({
    paste0("Core Weather Drivers: ", p2_locs_label())
  })
  
  p2_ts_computed <- reactive({
    locs <- p2_selected_locs()
    df   <- p2_county_acc() %>% filter(!is.na(month_year))
    validate(need(nrow(df) > 0, "No time-series data available for the selected localities."))
    
    all_months <- seq(min(df$month_year), max(df$month_year), by = "1 month")
    
    ts_base <- df %>%
      mutate(census_name = as.character(census_name)) %>%
      group_by(census_name, month_year) %>%
      summarise(
        total_accidents  = n(),
        severe_accidents = sum(severe, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      complete(
        census_name = locs,
        month_year  = all_months,
        fill = list(total_accidents = 0L, severe_accidents = 0L)
      ) %>%
      left_join(county_pop_ranks %>% select(census_name, population), by = "census_name")
    
    m_points  <- max(1, nrow(ts_base))
    alpha_adj <- get_adjusted_alpha(m_points, "bonferroni", alpha = 0.05)
    c_level   <- 1 - alpha_adj
    
    ts_base %>%
      mutate(
        census_name  = factor(census_name, levels = locs),
        conf_level   = c_level,
        pois_low     = calc_poisson_ci(severe_accidents, "lower", conf_level = c_level),
        pois_high    = calc_poisson_ci(severe_accidents, "upper", conf_level = c_level),
        severe_ratio = (severe_accidents / population) * 100000,
        ratio_low    = (pois_low / population) * 100000,
        ratio_high   = (pois_high / population) * 100000,
        severe_rate  = ifelse(total_accidents > 0, severe_accidents / total_accidents, NA_real_),
        rate_low     = calc_binom_ci(severe_accidents, total_accidents, "lower", conf_level = c_level),
        rate_high    = calc_binom_ci(severe_accidents, total_accidents, "upper", conf_level = c_level)
      )
  })
  
  output$p2_ts_plot <- renderPlotly({
    locs   <- p2_selected_locs()
    n_locs <- length(locs)
    
    # Guard: Do not plot raw severe crash count when comparing >1 locality
    if (input$p2_ts_metric == "count" && n_locs > 1) {
      validate(
        need(
          FALSE,
          paste0(
            "Cannot Compare Raw Severe Crash Counts Across Multiple Localities\n\n",
            "Communities with larger populations naturally have many more drivers and roads than smaller ones—so putting raw crash counts on the same chart mostly shows which locality is bigger rather than whose roads carry higher risk.\n\n",
            "How to view this comparison fairly:\n",
            "• Switch 'Monthly Trend Metric' on the left to 'Monthly Severe Crashes per 100k Residents' or 'Monthly Severe Crash Share (%)', OR\n",
            "• Narrow your selection to 1 locality to inspect its raw monthly crash count."
          )
        )
      )
    }
    
    ts_df   <- p2_ts_computed()
    col_map <- p2_color_map()
    shp_map <- p2_shape_map()
    base_sz <- ifelse(isTRUE(input$a11y_high_contrast), 14, 12)
    
    if (input$p2_ts_metric == "ratio") {
      p <- ggplot(ts_df, aes(x = month_year, y = severe_ratio, color = census_name, fill = census_name, group = census_name))
      
      # Show shaded variability band ONLY when 1 locality is selected
      if (n_locs == 1) {
        p <- p + geom_ribbon(aes(ymin = ratio_low, ymax = ratio_high), alpha = 0.24, color = NA)
      }
      
      p <- p +
        geom_line(linewidth = 0.9) +
        geom_point(
          aes(
            shape = census_name,
            text = if (n_locs == 1) {
              paste0(
                "<b>", census_name, " — ", format(month_year, "%b %Y"), "</b><br>",
                "Severe Crashes per 100k: ", round(severe_ratio, 2), "<br>",
                "Expected Range (Sim. Poisson 95% CI): ", round(ratio_low, 2), " to ", round(ratio_high, 2), "<br>",
                "Severe / Total Crashes: ", comma(severe_accidents), " / ", comma(total_accidents)
              )
            } else {
              paste0(
                "<b>", census_name, " — ", format(month_year, "%b %Y"), "</b><br>",
                "Severe Crashes per 100k: ", round(severe_ratio, 2), "<br>",
                "Severe / Total Crashes: ", comma(severe_accidents), " / ", comma(total_accidents)
              )
            }
          ),
          size = 1.9
        ) +
        labs(x = "Month", y = "Monthly Severe Crashes per 100k Residents", color = NULL, fill = NULL, shape = NULL)
    } else if (input$p2_ts_metric == "count") {
      # Only reached when n_locs == 1 due to guard above
      p <- ggplot(ts_df, aes(x = month_year, y = severe_accidents, color = census_name, fill = census_name, group = census_name)) +
        geom_ribbon(aes(ymin = pois_low, ymax = pois_high), alpha = 0.24, color = NA) +
        geom_line(linewidth = 0.9) +
        geom_point(
          aes(
            shape = census_name,
            text = paste0(
              "<b>", census_name, " — ", format(month_year, "%b %Y"), "</b><br>",
              "Severe Crashes: ", comma(severe_accidents), "<br>",
              "Expected Range (Sim. Poisson 95% CI): ", round(pois_low, 1), " to ", round(pois_high, 1), "<br>",
              "Total Crashes: ", comma(total_accidents)
            )
          ),
          size = 1.9
        ) +
        labs(x = "Month", y = "Monthly Severe Traffic Crashes", color = NULL, fill = NULL, shape = NULL)
    } else {
      ts_valid <- ts_df %>% filter(!is.na(severe_rate))
      p <- ggplot(ts_valid, aes(x = month_year, y = severe_rate, color = census_name, fill = census_name, group = census_name))
      
      # Show shaded variability band ONLY when 1 locality is selected
      if (n_locs == 1) {
        p <- p + geom_ribbon(aes(ymin = rate_low, ymax = rate_high), alpha = 0.24, color = NA)
      }
      
      p <- p +
        geom_line(linewidth = 0.9) +
        geom_point(
          aes(
            shape = census_name,
            text = if (n_locs == 1) {
              paste0(
                "<b>", census_name, " — ", format(month_year, "%b %Y"), "</b><br>",
                "Severe Crash Share: ", percent(severe_rate, accuracy = 0.1), "<br>",
                "Expected Range (Sim. Binomial 95% CI): ", percent(rate_low, 0.1), " to ", percent(rate_high, 0.1), "<br>",
                "Severe / Total Crashes: ", comma(severe_accidents), " / ", comma(total_accidents)
              )
            } else {
              paste0(
                "<b>", census_name, " — ", format(month_year, "%b %Y"), "</b><br>",
                "Severe Crash Share: ", percent(severe_rate, accuracy = 0.1), "<br>",
                "Severe / Total Crashes: ", comma(severe_accidents), " / ", comma(total_accidents)
              )
            }
          ),
          size = 1.9
        ) +
        scale_y_continuous(labels = percent_format()) +
        labs(x = "Month", y = "Monthly Severe Crash Share (%)", color = NULL, fill = NULL, shape = NULL)
    }
    
    p <- p +
      scale_color_manual(values = col_map) +
      scale_fill_manual(values = col_map) +
      scale_shape_manual(values = shp_map) +
      scale_x_datetime(date_labels = "%b %Y", date_breaks = "6 months") +
      theme_minimal(base_size = base_sz) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    plt <- ggplotly(p, tooltip = "text") %>%
      config(
        displayModeBar = TRUE,
        displaylogo = FALSE
      )
    
    if (n_locs == 1) {
      plt <- plt %>% layout(showlegend = FALSE)
    } else {
      plt <- plt %>% layout(
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12, title = list(text = "")),
        margin = list(t = 48)
      )
    }
    plt
  })
  
  output$p2_ts_sr_summary <- renderUI({
    locs <- p2_selected_locs()
    if (length(locs) == 0) return(NULL)
    
    if (input$p2_ts_metric == "count" && length(locs) > 1) {
      msg <- paste0(
        "Raw monthly severe crash counts are not charted when multiple localities (", p2_locs_label(),
        ") are selected because differences in population size distort direct comparisons. ",
        "Switch to Monthly Severe Crashes per 100k Residents or Monthly Severe Crash Share (%), or select 1 locality."
      )
      return(render_a11y_box(msg, input$a11y_show_text))
    }
    
    ts_df <- p2_ts_computed()
    if (nrow(ts_df) == 0) return(NULL)
    
    if (input$p2_ts_metric == "ratio") {
      s <- ts_df %>%
        group_by(census_name) %>%
        summarise(
          avg_v = mean(severe_ratio, na.rm = TRUE),
          pk_v  = max(severe_ratio, na.rm = TRUE),
          pk_m  = format(month_year[which.max(severe_ratio)], "%B %Y"),
          .groups = "drop"
        ) %>%
        arrange(desc(avg_v))
      parts <- pmap_chr(s, function(census_name, avg_v, pk_v, pk_m) {
        paste0(census_name, " averages ", round(avg_v, 1), " per 100k/month (peak ", round(pk_v, 1), " in ", pk_m, ")")
      })
      msg <- paste0("Monthly per-100k trend comparison: ", paste(parts, collapse = "; "), ".")
    } else if (input$p2_ts_metric == "count") {
      s <- ts_df %>%
        group_by(census_name) %>%
        summarise(
          avg_v = mean(severe_accidents, na.rm = TRUE),
          pk_v  = max(severe_accidents, na.rm = TRUE),
          pk_m  = format(month_year[which.max(severe_accidents)], "%B %Y"),
          .groups = "drop"
        ) %>%
        arrange(desc(avg_v))
      parts <- pmap_chr(s, function(census_name, avg_v, pk_v, pk_m) {
        paste0(census_name, " averages ", round(avg_v, 1), " severe crashes/month (peak ", comma(pk_v), " in ", pk_m, ")")
      })
      msg <- paste0("Monthly severe count trend: ", paste(parts, collapse = "; "), ".")
    } else {
      s <- ts_df %>%
        filter(!is.na(severe_rate)) %>%
        group_by(census_name) %>%
        summarise(
          avg_v = mean(severe_rate, na.rm = TRUE),
          pk_v  = max(severe_rate, na.rm = TRUE),
          pk_m  = format(month_year[which.max(severe_rate)], "%B %Y"),
          .groups = "drop"
        ) %>%
        arrange(desc(avg_v))
      parts <- pmap_chr(s, function(census_name, avg_v, pk_v, pk_m) {
        paste0(census_name, " averages ", percent(avg_v, 0.1), " severe share (peak ", percent(pk_v, 0.1), " in ", pk_m, ")")
      })
      msg <- paste0("Monthly severe share trend comparison: ", paste(parts, collapse = "; "), ".")
    }
    render_a11y_box(msg, input$a11y_show_text)
  })
  
  # Monthly Weather Table per Selected Locality (5 Core Weather Categories, min. 5 crashes/month)
  p2_qualified_monthly_weather <- reactive({
    locs <- p2_selected_locs()
    df_county <- p2_county_acc() %>%
      filter(!is.na(month_year), !is.na(Weather_Category), Weather_Category != "") %>%
      mutate(census_name = as.character(census_name))
    
    monthly_all <- df_county %>%
      group_by(census_name, Weather_Group = Weather_Category, month_year) %>%
      summarise(
        severe_accidents = sum(severe, na.rm = TRUE),
        accidents        = n(),
        .groups = "drop"
      ) %>%
      left_join(county_pop_ranks %>% select(census_name, population), by = "census_name") %>%
      mutate(
        severe_rate  = severe_accidents / accidents,
        severe_ratio = (severe_accidents / population) * 100000
      )
    
    # Apply qualification threshold independently per locality so small counties don't drop out
    map_dfr(locs, function(loc) {
      sub <- monthly_all %>% filter(census_name == loc)
      qual <- sub %>% filter(accidents >= 5)
      if (nrow(qual) < 5) qual <- sub %>% filter(accidents >= 2)
      if (nrow(qual) < 3) qual <- sub
      qual
    }) %>%
      mutate(census_name = factor(census_name, levels = locs))
  })
  
  p2_weather_data <- reactive({
    locs <- p2_selected_locs()
    qual_counts <- p2_qualified_monthly_weather() %>%
      mutate(census_name = as.character(census_name)) %>%
      count(census_name, Weather_Group, name = "qualifying_months")
    
    df <- p2_county_acc() %>%
      filter(!is.na(Weather_Category), Weather_Category != "") %>%
      mutate(census_name = as.character(census_name)) %>%
      group_by(census_name, Weather_Group = Weather_Category) %>%
      summarise(
        accidents            = n(),
        severe_accidents     = sum(severe, na.rm = TRUE),
        not_severe_accidents = sum(!severe, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(county_pop_ranks %>% select(census_name, population), by = "census_name") %>%
      mutate(
        severe_ratio = (severe_accidents / population) * 100000,
        severe_rate  = severe_accidents / accidents
      ) %>%
      left_join(qual_counts, by = c("census_name", "Weather_Group")) %>%
      mutate(qualifying_months = replace_na(qualifying_months, 0L))
    
    m_all     <- max(1, nrow(df))
    alpha_adj <- get_adjusted_alpha(m_all, "bonferroni", alpha = 0.05)
    
    df %>%
      mutate(
        census_name  = factor(census_name, levels = locs),
        rate_ci_low  = calc_binom_ci(severe_accidents, accidents, "lower", conf_level = 1 - alpha_adj),
        rate_ci_high = calc_binom_ci(severe_accidents, accidents, "upper", conf_level = 1 - alpha_adj)
      )
  })
  
  # Weather bar chart: single locality or side-by-side comparison (up to 3 localities)
  output$p2_weather_plot <- renderPlotly({
    locs    <- p2_selected_locs()
    n_locs  <- length(locs)
    col_map <- p2_color_map()
    
    w_all <- p2_weather_data()
    df <- map_dfr(locs, function(loc) {
      sub <- w_all %>% filter(as.character(census_name) == loc)
      f10 <- sub %>% filter(qualifying_months >= 10)
      if (nrow(f10) > 0) f10 else sub %>% filter(qualifying_months >= 3)
    }) %>%
      mutate(census_name = factor(census_name, levels = locs))
    
    validate(
      need(nrow(df) > 0, "No weather categories in the selected localities meet the minimum qualifying months threshold.")
    )
    
    is_rate <- input$p2_weather_metric == "severe_rate"
    y_lab   <- ifelse(
      is_rate,
      "Share of Crashes That Are Severe (%)",
      "Total Severe Crashes per 100,000 Residents"
    )
    
    # Order weather categories by average value across selected localities
    cat_order <- df %>%
      group_by(Weather_Group) %>%
      summarise(ord_val = mean(.data[[input$p2_weather_metric]], na.rm = TRUE), .groups = "drop") %>%
      arrange(ord_val) %>%
      pull(Weather_Group)
    
    df <- df %>%
      mutate(
        Weather_Group = factor(Weather_Group, levels = cat_order),
        val_label = if (is_rate) percent(severe_rate, 0.1) else round(severe_ratio, 1)
      )
    
    base_sz <- ifelse(isTRUE(input$a11y_high_contrast), 14, 12)
    mult_hi <- ifelse(isTRUE(input$a11y_high_contrast), 0.22, 0.08)
    
    if (n_locs == 1) {
      p <- ggplot(
        df,
        aes(
          x = Weather_Group,
          y = .data[[input$p2_weather_metric]],
          text = paste0(
            "<b>", Weather_Group, " (", census_name, ")</b><br>",
            "Qualifying Months: ", qualifying_months, "<br>",
            "Severe Crash Share: ", percent(severe_rate, accuracy = 0.1), "<br>",
            "Severe Crashes per 100k: ", round(severe_ratio, 2), "<br>",
            "Severe / Total Crashes: ", comma(severe_accidents), " / ", comma(accidents)
          )
        )
      ) +
        geom_col(
          fill = "#483D8B",
          color = ifelse(isTRUE(input$a11y_high_contrast), "#000000", NA),
          alpha = 0.90,
          width = 0.65
        )
      
      if (isTRUE(input$a11y_high_contrast)) {
        p <- p + geom_text(aes(label = val_label), hjust = -0.1, size = 3.8, fontface = "bold", color = "#000000")
      }
    } else {
      p <- ggplot(
        df,
        aes(
          x = Weather_Group,
          y = .data[[input$p2_weather_metric]],
          fill = census_name,
          text = paste0(
            "<b>", census_name, " — ", Weather_Group, "</b><br>",
            "Qualifying Months: ", qualifying_months, "<br>",
            "Severe Crash Share: ", percent(severe_rate, accuracy = 0.1), "<br>",
            "Severe Crashes per 100k: ", round(severe_ratio, 2), "<br>",
            "Severe / Total Crashes: ", comma(severe_accidents), " / ", comma(accidents)
          )
        )
      ) +
        geom_col(
          position = position_dodge(width = 0.78),
          color = ifelse(isTRUE(input$a11y_high_contrast), "#000000", NA),
          alpha = 0.92,
          width = 0.70
        ) +
        scale_fill_manual(values = col_map)
      
      if (isTRUE(input$a11y_high_contrast)) {
        p <- p + geom_text(
          aes(label = val_label),
          position = position_dodge(width = 0.78),
          hjust = -0.1, size = 3.3, fontface = "bold", color = "#000000"
        )
      }
    }
    
    p <- p +
      coord_flip() +
      labs(x = NULL, y = y_lab, fill = NULL) +
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
    
    plt <- ggplotly(p, tooltip = "text") %>%
      config(
        displayModeBar = TRUE,
        displaylogo = FALSE
      )
    
    if (n_locs > 1) {
      plt <- plt %>% layout(
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12, title = list(text = "")),
        margin = list(t = 48)
      )
    }
    plt
  })
  
  output$p2_weather_sr_summary <- renderUI({
    locs  <- p2_selected_locs()
    w_all <- p2_weather_data()
    df <- map_dfr(locs, function(loc) {
      sub <- w_all %>% filter(as.character(census_name) == loc)
      f10 <- sub %>% filter(qualifying_months >= 10)
      if (nrow(f10) > 0) f10 else sub %>% filter(qualifying_months >= 3)
    })
    if (nrow(df) == 0) return(NULL)
    
    is_rate <- input$p2_weather_metric == "severe_rate"
    parts <- map_chr(locs, function(loc) {
      sub <- df %>%
        filter(as.character(census_name) == loc) %>%
        arrange(desc(.data[[input$p2_weather_metric]]))
      if (nrow(sub) == 0) return(paste0(loc, ": insufficient qualifying months"))
      top_w <- sub %>% slice(1)
      val_txt <- if (is_rate) {
        paste0(percent(top_w$severe_rate, 0.1), " severe share")
      } else {
        paste0(round(top_w$severe_ratio, 1), " severe per 100k")
      }
      paste0(loc, " is led by ", top_w$Weather_Group, " (", val_txt, ")")
    })
    
    msg <- paste0("Core weather comparison: ", paste(parts, collapse = "; "), ".")
    render_a11y_box(msg, input$a11y_show_text)
  })
  
  # Monthly Weather Averages & Time-Adjusted (Newey-West HAC) Ranges per Selected Locality
  p2_ci_data <- reactive({
    locs      <- p2_selected_locs()
    w_monthly <- p2_qualified_monthly_weather()
    validate(need(nrow(w_monthly) > 0, "Insufficient monthly weather records for the selected localities."))
    
    target_col <- input$p2_weather_metric
    upper_cap  <- ifelse(target_col == "severe_rate", 1.0, Inf)
    
    grouped <- w_monthly %>%
      mutate(census_name = as.character(census_name)) %>%
      filter(!is.na(.data[[target_col]]) & !is.nan(.data[[target_col]]) & !is.na(month_year)) %>%
      group_by(census_name, Group = Weather_Group) %>%
      summarise(
        pop      = first(population),
        N_Units  = n(),
        Mean_Est = mean(.data[[target_col]], na.rm = TRUE),
        variance = var(.data[[target_col]], na.rm = TRUE),
        stdev    = sqrt(pmax(0, variance)),
        nw_res   = list(calc_newey_west_se(month_year, .data[[target_col]])),
        .groups  = "drop"
      ) %>%
      filter(Mean_Est > 0)
    
    if (nrow(grouped) == 0) return(tibble())
    grouped <- grouped %>% unnest_wider(nw_res)
    
    # Apply min_months = 10 per locality with graceful rural fallback
    filtered <- map_dfr(locs, function(loc) {
      sub <- grouped %>% filter(census_name == loc)
      f <- sub %>% filter(N_Units >= 10)
      if (nrow(f) == 0) f <- sub %>% filter(N_Units >= 5)
      if (nrow(f) == 0) f <- sub %>% filter(N_Units >= 3)
      f
    })
    
    if (nrow(filtered) == 0) return(tibble())
    
    m_groups  <- max(1, nrow(filtered))
    alpha_adj <- get_adjusted_alpha(m_groups, "bonferroni", alpha = 0.05)
    
    filtered %>%
      mutate(
        census_name   = factor(census_name, levels = locs),
        m_comparisons = m_groups,
        conf_level    = 1 - alpha_adj,
        t_val         = qt(1 - (alpha_adj / 2), df = pmax(1, N_Units - 1)),
        CI_Lower      = pmax(0, Mean_Est - (t_val * SE)),
        CI_Upper      = pmin(upper_cap, Mean_Est + (t_val * SE))
      ) %>%
      arrange(desc(Mean_Est))
  })
  
  output$p2_ci_header <- renderText({
    df <- p2_ci_data()
    m_val <- if (nrow(df) > 0) df$m_comparisons[1] else 1
    c_lvl <- if (nrow(df) > 0) percent(df$conf_level[1], accuracy = 0.01) else "95.0%"
    paste0(
      "Monthly Weather Averages & Time-Adjusted Simultaneous 95% Confidence Intervals: ",
      p2_locs_label(), " (Bonferroni, m = ", m_val, ", ", c_lvl, " level)"
    )
  })
  
  # Dedicated separate horizontal rows per Locality x Weather Category (never stacked on the same line)
  output$p2_ci_plot <- renderPlotly({
    df      <- p2_ci_data()
    locs    <- p2_selected_locs()
    n_locs  <- length(locs)
    col_map <- p2_color_map()
    shp_map <- p2_shape_map()
    
    validate(need(nrow(df) > 0, "No weather categories have enough qualifying months in the selected localities."))
    
    is_pct <- (input$p2_weather_metric == "severe_rate")
    x_lab  <- ifelse(
      is_pct,
      "Monthly Avg Severe Crash Share % (Mean ± Time-Adjusted Sim. 95% CI)",
      "Monthly Avg Severe Crashes per 100k Residents (Mean ± Time-Adjusted Sim. 95% CI)"
    )
    
    cat_order <- df %>%
      group_by(Group) %>%
      summarise(ord_val = mean(Mean_Est, na.rm = TRUE), .groups = "drop") %>%
      arrange(ord_val) %>%
      pull(Group)
    
    # Assign each Locality x Weather Category its own strictly separate integer y-position
    df <- df %>%
      mutate(
        short_name = str_remove(as.character(census_name), " (County|city)$"),
        cat_rank   = match(Group, cat_order),
        loc_rank   = match(as.character(census_name), locs)
      ) %>%
      arrange(cat_rank, desc(loc_rank)) %>%
      mutate(
        y_pos     = row_number(),
        row_label = if (n_locs == 1) Group else paste0(Group, " — ", short_name),
        hover_txt = paste0(
          "<b>", census_name, " — ", Group, "</b><br>",
          "Qualifying Months (n): ", N_Units,
          " (Effective Independent Months: ~", round(Eff_Months, 1), ")<br>",
          "Monthly Average (Mean): ", ifelse(is_pct, percent(Mean_Est, 0.1), round(Mean_Est, 2)), "<br>",
          "Time-Adjusted Likely Range (Sim. 95% CI, ", percent(conf_level, 0.01), "): [",
          ifelse(is_pct, percent(CI_Lower, 0.1), round(CI_Lower, 2)), ", ",
          ifelse(is_pct, percent(CI_Upper, 0.1), round(CI_Upper, 2)), "]"
        )
      )
    
    # Subtle horizontal dividers separating each weather category block when comparing 2-3 localities
    divider_y <- if (n_locs > 1 && nrow(df) > 1) {
      which(df$Group != dplyr::lag(df$Group)) - 0.5
    } else {
      numeric(0)
    }
    
    base_sz <- ifelse(isTRUE(input$a11y_high_contrast), 13, 11.5)
    bar_lwd <- ifelse(isTRUE(input$a11y_high_contrast), 3.0, 2.2)
    pt_size <- ifelse(isTRUE(input$a11y_high_contrast), 4.2, 3.4)
    
    p <- ggplot(df)
    
    if (length(divider_y) > 0) {
      p <- p + geom_hline(yintercept = divider_y, linetype = "dashed", color = "#CBD5E1", linewidth = 0.5)
    }
    
    if (n_locs == 1) {
      p <- p +
        geom_segment(
          aes(x = CI_Lower, xend = CI_Upper, y = y_pos, yend = y_pos, text = hover_txt),
          color = exec_teal, linewidth = bar_lwd
        ) +
        geom_point(
          aes(x = Mean_Est, y = y_pos, text = hover_txt),
          color = exec_crimson, shape = 18, size = pt_size
        )
    } else {
      p <- p +
        geom_segment(
          aes(x = CI_Lower, xend = CI_Upper, y = y_pos, yend = y_pos, color = census_name, text = hover_txt),
          linewidth = bar_lwd, alpha = 0.88
        ) +
        geom_point(
          aes(x = Mean_Est, y = y_pos, color = census_name, shape = census_name, text = hover_txt),
          size = pt_size
        ) +
        scale_color_manual(values = col_map) +
        scale_shape_manual(values = shp_map)
    }
    
    p <- p +
      scale_y_continuous(
        breaks = df$y_pos,
        labels = df$row_label,
        expand = expansion(add = 0.6)
      ) +
      labs(x = x_lab, y = NULL, color = NULL, shape = NULL) +
      theme_minimal(base_size = base_sz) +
      theme(
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(face = "bold", color = "#1A202C")
      )
    
    if (is_pct) {
      p <- p + scale_x_continuous(labels = percent_format())
    } else {
      p <- p + scale_x_continuous(labels = comma_format())
    }
    
    plt <- ggplotly(p, tooltip = "text") %>%
      config(
        displayModeBar = TRUE,
        displaylogo = FALSE
      )
    
    if (n_locs == 1) {
      plt <- plt %>% layout(showlegend = FALSE)
    } else {
      plt <- plt %>% layout(
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.10, title = list(text = "")),
        margin = list(t = 48)
      )
    }
    plt
  })
  
  # Hybrid Page 2 CI Summary (plain-language CI teaching + time-adjustment explanation)
  output$p2_ci_sr_summary <- renderUI({
    df <- p2_ci_data() %>% arrange(desc(Mean_Est))
    if (nrow(df) == 0) return(NULL)
    
    is_pct <- input$p2_weather_metric == "severe_rate"
    fmt <- function(v) if (is_pct) percent(v, 0.1) else round(v, 2)
    top_c <- df %>% slice(1)
    
    interval_width <- top_c$CI_Upper - top_c$CI_Lower
    precision_msg <- if (
      !is.na(interval_width) && interval_width < abs(top_c$Mean_Est) * 0.25
    ) {
      "The range is relatively narrow, meaning the estimate is fairly precise."
    } else {
      "The range is relatively wide, meaning there is more uncertainty around the estimate."
    }
    
    metric_name <- if (is_pct) {
      "monthly average severe crash share"
    } else {
      "monthly average severe crashes per 100,000 residents"
    }
    
    top_group_label <- if (length(p2_selected_locs()) > 1) {
      paste0(top_c$Group, " in ", top_c$census_name)
    } else {
      as.character(top_c$Group)
    }
    
    msg <- paste0(
      "What this means: This summary compares monthly crash patterns in ",
      p2_locs_label(),
      " across core weather categories using time-adjusted simultaneous 95% confidence intervals ",
      "(shown as likely ranges). ",
      top_group_label,
      " has the highest estimated ",
      metric_name,
      " at ",
      fmt(top_c$Mean_Est),
      " per month across ",
      top_c$N_Units,
      " qualifying months (about ", round(top_c$Eff_Months, 1),
      " effective independent months after accounting for consecutive months tracking multi-month traffic patterns). ",
      "Based on the available data, the time-adjusted likely range (Bonferroni 95% CI) ",
      "is between ",
      fmt(top_c$CI_Lower),
      " and ",
      fmt(top_c$CI_Upper),
      ". ",
      precision_msg,
      " This interval helps show how much the monthly estimate may vary across 2016–2023; ",
      "it does not guarantee that the true value falls within this particular range."
    )
    
    render_a11y_box(msg, input$a11y_show_text)
  })
  
  output$p2_ci_table <- renderDT({
    df <- p2_ci_data()
    is_pct <- (input$p2_weather_metric == "severe_rate")
    
    tbl <- df %>%
      select(census_name, Group, N_Units, Mean_Est, CI_Lower, CI_Upper)
    
    dt <- datatable(
      tbl,
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = "caption-side: top; font-weight: bold; color: #0F2042;",
        paste0("Monthly Weather Averages & Time-Adjusted Simultaneous 95% CIs: ", p2_locs_label())
      ),
      colnames = c("Locality", "Core Weather Category", "Qualifying Months (n)", "Monthly Mean", "Time-Adj. Low (95% CI)", "Time-Adj. High (95% CI)"),
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
      arrange(census_name, desc(severe_accidents)) %>%
      select(
        census_name, Weather_Group, qualifying_months, accidents, severe_accidents,
        not_severe_accidents, severe_rate, rate_ci_low, rate_ci_high, severe_ratio
      )
    
    datatable(
      df,
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = "caption-side: top; font-weight: bold; color: #0F2042;",
        paste0("Complete Core Weather Breakdown: ", p2_locs_label())
      ),
      colnames = c(
        "Locality", "Core Weather Category", "Qualifying Months (n)", "Total Crashes", "Severe Crashes",
        "Non-Severe Crashes", "Severe Share (%)", "Range Low (95% CI)",
        "Range High (95% CI)", "Severe Crashes per 100k"
      ),
      options = list(pageLength = 10, scrollX = TRUE)
    ) %>%
      formatPercentage(c("severe_rate", "rate_ci_low", "rate_ci_high"), digits = 1) %>%
      formatRound("severe_ratio", digits = 2) %>%
      formatCurrency(c("accidents", "severe_accidents", "not_severe_accidents"), currency = "", interval = 3, mark = ",", digits = 0)
  })
  
  # Simple audit checks across all selected Page 2 localities
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
      ts$pois_low <= ts$severe_accidents & ts$pois_high >= ts$severe_accidents &
        ts$ratio_low <= ts$severe_ratio & ts$ratio_high >= ts$severe_ratio,
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
    nw_se_ok <- nrow(ci) > 0 && all(
      is.finite(ci$SE) & ci$SE >= 0
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
        "Monthly Poisson CIs contain severe counts & per-100k ratios",
        "Monthly binomial CIs contain severe rates",
        "Weather binomial CIs contain severe rates",
        "Time-adjusted (Newey-West) monthly weather CIs contain their means",
        "Newey-West HAC monthly standard errors are finite and non-negative",
        "Weather severe + non-severe counts equal total",
        "Weather descriptions match assigned categories"
      ),
      Status = c(
        check_status(poisson_ok, nrow(ts) > 0),
        check_status(rate_ok, any(!is.na(ts$severe_rate))),
        check_status(weather_ci_ok, nrow(weather) > 0),
        check_status(monthly_ci_ok, nrow(ci) > 0),
        check_status(nw_se_ok, nrow(ci) > 0),
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
        "Localities: ", p2_locs_label(), " | Passed: ", sum(x$Status == "PASS"),
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
      count(census_name, Weather_Condition, Weather_Category, Expected_Category, name = "Accidents") %>%
      mutate(Check = if_else(Weather_Category == Expected_Category, "PASS", "FAIL")) %>%
      arrange(desc(Check), desc(Accidents))
    
    datatable(df, rownames = FALSE,
              options = list(pageLength = 8, scrollX = TRUE))
  })
  
  # ----------------------------------------------------------------------------
  # PAGE 2 EXCEL DOWNLOAD: AUDIT STATUS & UNDERLYING RANGES
  # ----------------------------------------------------------------------------
  page_status_excel_sheets <- reactive({
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
    
    monthly_weather_ci <- tryCatch(
      p2_ci_data(),
      error = function(e) {
        tibble(Note = paste("Unavailable:", conditionMessage(e)))
      }
    )
    
    monthly_time_series_ci <- tryCatch(
      p2_ts_computed(),
      error = function(e) {
        tibble(Note = paste("Unavailable:", conditionMessage(e)))
      }
    )
    
    weather_binomial_ci <- tryCatch(
      p2_weather_data(),
      error = function(e) {
        tibble(Note = paste("Unavailable:", conditionMessage(e)))
      }
    )
    
    weather_classification <- p2_county_acc() %>%
      filter(
        !is.na(Weather_Condition),
        Weather_Condition != ""
      ) %>%
      mutate(
        Expected_Category = classify_weather_simple(Weather_Condition)
      ) %>%
      count(
        census_name,
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
    
    export_settings <- tibble(
      Setting = c(
        "Selected Localities (Max 3)",
        "Weather Grouping",
        "Time-Series Metric",
        "Weather Metric",
        "Monthly CI Standard Error Method",
        "Multiple Comparisons Method",
        "Minimum Accidents per Month",
        "Minimum Qualifying Months"
      ),
      Value = c(
        paste(p2_selected_locs(), collapse = ", "),
        "Weather_Category (5 Core Weather Categories)",
        input$p2_ts_metric,
        input$p2_weather_metric,
        "Newey-West HAC (Bartlett kernel, automatic bandwidth, calendar-month grid)",
        "bonferroni",
        "5",
        "10"
      )
    )
    
    list(
      Page_Status = audit_checks,
      Monthly_Weather_CIs = monthly_weather_ci,
      Time_Series_CIs = monthly_time_series_ci,
      Weather_Binomial_CIs = weather_binomial_ci,
      Weather_Classification = weather_classification,
      Export_Settings = export_settings
    )
  })
  
  output$download_page_status <- downloadHandler(
    filename = function() {
      loc_tag <- gsub(
        "[^A-Za-z0-9]+",
        "_",
        paste(p2_selected_locs(), collapse = "_vs_")
      )
      loc_tag <- substr(loc_tag, 1, 60)
      paste0(
        "Page_Status_",
        loc_tag,
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
  # PAGE 3 SERVER: STATEWIDE COUNTY SEVERE RATE MAP -> COUNTY STREET ZONES
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
  
  # Sync Page 3 Date Range when Year changes
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
  
  # Filtered accidents by Page 3 Year & Date Range
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
  
  # Statewide county-level severe accident rate summary for selected dates
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
      select(GEOID, census_name, population) %>%
      left_join(agg, by = "census_name") %>%
      mutate(
        total_accidents   = replace_na(total_accidents, 0L),
        severe_accidents  = replace_na(severe_accidents, 0L),
        severe_rate       = ifelse(total_accidents > 0, severe_accidents / total_accidents, 0),
        qualifies_rate    = total_accidents >= 20,
        # Color scale uses severe_rate only for counties with >= 20 total crashes
        severe_rate_map   = ifelse(qualifies_rate, severe_rate, NA_real_)
      )
    
    m_cnty    <- max(1, sum(cnty_df$total_accidents > 0))
    alpha_adj <- get_adjusted_alpha(m_cnty, "bonferroni", alpha = 0.05)
    
    cnty_df %>%
      mutate(
        rate_ci_low  = calc_binom_ci(severe_accidents, total_accidents, "lower", conf_level = 1 - alpha_adj),
        rate_ci_high = calc_binom_ci(severe_accidents, total_accidents, "upper", conf_level = 1 - alpha_adj)
      ) %>%
      arrange(desc(qualifies_rate), desc(severe_rate), desc(severe_accidents))
  })
  
  # Localized Street Zones (~1 km segments) inside the selected county
  p3_street_zone_summary <- reactive({
    req(input$p3_county != "ALL_VA")
    df_raw <- p3_date_filtered_acc() %>%
      filter(census_name == input$p3_county, !is.na(Street), Street != "")
    
    if (nrow(df_raw) == 0) return(tibble())
    
    # Group by Street + 0.01-degree (~1 km) coordinate grid to pinpoint specific zones
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
    
    # Require at least 3 crashes in a zone (fallback to all zones if rural county has few crashes)
    filtered <- grouped %>% filter(total_accidents >= 3)
    if (nrow(filtered) == 0) {
      filtered <- grouped
    }
    
    # Rank by Severe Accident Count (Volume) and keep Top 30 zones
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
      "Where Are Severe Crashes Most Concentrated Across Virginia? (Click a Locality to Zoom In)"
    } else {
      paste0("Priority Road Corridors in ", input$p3_county, ": Top 30 Severe Crash Zones (~1 km)")
    }
  })
  
  output$p3_map_subtitle <- renderText({
    if (input$p3_county == "ALL_VA") {
      "Localities with at least 20 crashes are shaded by their share of severe crashes so small and large communities can be compared fairly. Low-volume localities (<20 crashes) appear in gray. Click any locality to locate its highest-crash corridors."
    } else {
      paste0(
        "Showing the Top 30 localized (~1 km) street zones in ", input$p3_county,
        ". Circle color = Severe Crash Count; Circle size = Total Crashes in zone. Click 'Zoom Out to Full Virginia Map' to return."
      )
    }
  })
  
  output$p3_table_title <- renderText({
    if (input$p3_county == "ALL_VA") {
      "Virginia Localities Ranked by Severe Crash Share"
    } else {
      paste0("Top Priority Street Corridors in ", input$p3_county)
    }
  })
  
  output$p3_table_footer <- renderText({
    if (input$p3_county == "ALL_VA") {
      "Lists each locality's crash totals, severe crash share, and likely range (Bonferroni simultaneous 95% binomial CI). Localities with at least 20 crashes are ranked first."
    } else {
      "Lists each ~1 km street zone's severe crashes, total crashes, severe share, and likely range (Bonferroni simultaneous 95% binomial CI)."
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
            select(GEOID, total_accidents, severe_accidents, severe_rate, qualifies_rate, severe_rate_map, rate_ci_low, rate_ci_high),
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
            display_name, " | Severe Share: ", percent(severe_rate, accuracy = 0.1),
            " (", comma(severe_accidents), " severe / ", comma(total_accidents), " total)",
            ifelse(qualifies_rate, "", " [Low volume: <20 crashes]")
          ),
          popup = ~paste0(
            "<b>Locality: </b>", display_name, "<br>",
            "<b>Severe Crash Share: </b>", percent(severe_rate, accuracy = 0.1),
            ifelse(!is.na(rate_ci_low), paste0(" (Likely Range / 95% CI: ", percent(rate_ci_low, 0.1), "–", percent(rate_ci_high, 0.1), ")"), ""), "<br>",
            "<b>Severe Crashes: </b>", comma(severe_accidents), "<br>",
            "<b>Total Crashes: </b>", comma(total_accidents), "<br>",
            "<b>Volume Status: </b>", ifelse(qualifies_rate, "20+ crashes", "Under 20 crashes (low volume)")
          )
        )
      
      if (input$p3_county == "ALL_VA") {
        m <- m %>%
          fitBounds(lng1 = -83.68, lat1 = 36.54, lng2 = -75.24, lat2 = 39.47) %>%
          addLegend(
            position  = "bottomright",
            pal       = va_pal,
            values    = rate_domain,
            title     = "Locality Severe Share (%)<br><small>(Gray = <20 crashes)</small>",
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
                "<b>Zone Severe Share: </b>", percent(severe_rate, accuracy = 0.1),
                " (Likely Range / 95% CI: ", percent(rate_ci_low, 0.1), "–", percent(rate_ci_high, 0.1), ")"
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
        "Statewide Virginia Severe Crash Share Map (localities with at least 20 crashes). ",
        "Highest severe crash share is in ", c1$census_name, " at ", percent(c1$severe_rate, 0.1),
        " (", comma(c1$severe_accidents), " severe out of ", comma(c1$total_accidents), " total crashes). ",
        if (!is.null(c2)) paste0("Second highest is ", c2$census_name, " at ", percent(c2$severe_rate, 0.1), ".") else ""
      )
      render_a11y_box(msg, input$a11y_show_text)
    } else {
      df <- p3_street_zone_summary()
      if (nrow(df) == 0) return(NULL)
      z1 <- df %>% slice(1)
      z2 <- if (nrow(df) >= 2) df %>% slice(2) else NULL
      msg <- paste0(
        "Corridor hotspot summary for ", input$p3_county, ". ",
        "Top priority street zone is ", z1$Street_Zone, " with ",
        comma(z1$severe_accidents), " severe crashes out of ", comma(z1$total_accidents),
        " total crashes (", percent(z1$severe_rate, 0.1), " severe share). ",
        if (!is.null(z2)) paste0("Second highest zone is ", z2$Street_Zone, " with ", comma(z2$severe_accidents), " severe crashes.") else ""
      )
      render_a11y_box(msg, input$a11y_show_text)
    }
  })
  
  output$p3_street_table <- renderDT({
    if (input$p3_county == "ALL_VA") {
      df <- p3_statewide_county_rates() %>%
        filter(total_accidents > 0) %>%
        mutate(Volume_Note = ifelse(qualifies_rate, "20+ Crashes", "<20 Crashes (Low Volume)")) %>%
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
          "Statewide Locality Severe Crash Share Table (with Simultaneous 95% CIs)"
        ),
        colnames = c(
          "Locality", "Total Crashes", "Severe Crashes",
          "Severe Share (%)", "Range Low (95% CI)", "Range High (95% CI)", "Crash Volume"
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
          paste0("Priority Street Corridor Table for ", input$p3_county, " (with Simultaneous 95% CIs)")
        ),
        colnames = c(
          "Street & Localized Zone (~1 km)", "Severe Crashes", "Total Crashes",
          "Severe Share (%)", "Range Low (95% CI)", "Range High (95% CI)"
        ),
        options = list(pageLength = 12, scrollX = TRUE)
      ) %>%
        formatPercentage(c("severe_rate", "rate_ci_low", "rate_ci_high"), digits = 1) %>%
        formatCurrency(c("severe_accidents", "total_accidents"), currency = "", interval = 3, mark = ",", digits = 0)
    }
  })
}

shinyApp(ui = ui, server = server)