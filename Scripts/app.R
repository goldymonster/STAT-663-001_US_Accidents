# ==============================================================================
# VIRGINIA TRAFFIC SAFETY EXECUTIVE DASHBOARD
# Office of the Governor | Highway Safety & Infrastructure Analytics
# Authors: Matthew Goldenberg & Maria Wood (STAT 663)
# ==============================================================================
# Required packages:
# install.packages(c("shiny", "bslib", "tidyverse", "lubridate", "plotly",
#                    "leaflet", "sf", "tigris", "DT", "scales", "bsicons"))

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
# Multiple comparisons adjustment (Bonferroni, Sidak, or Unadjusted)
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

# Exact Binomial CI helper (for overall rates and time series)
calc_binom_ci <- function(x, n, bound = c("lower", "upper"), conf_level = 0.95) {
  bound <- match.arg(bound)
  idx <- ifelse(bound == "lower", 1, 2)
  conf_level <- pmin(pmax(conf_level, 0.50), 0.999999)
  map2_dbl(x, n, function(k, total) {
    if (is.na(k) || is.na(total) || total <= 0) return(NA_real_)
    binom.test(k, total, conf.level = conf_level)$conf.int[idx]
  })
}

# Exact Poisson CI helper (for monthly severe accident counts)
calc_poisson_ci <- function(k_vec, bound = c("lower", "upper"), conf_level = 0.95) {
  bound <- match.arg(bound)
  idx <- ifelse(bound == "lower", 1, 2)
  conf_level <- pmin(pmax(conf_level, 0.50), 0.999999)
  map_dbl(k_vec, function(k) {
    if (is.na(k) || k < 0) return(NA_real_)
    poisson.test(as.integer(round(k)), conf.level = conf_level)$conf.int[idx]
  })
}

# Standard Textbook Confidence Interval Formula (Mean +/- t * SE):
# Enforces a minimum number of qualifying months (min_months) so n is always large enough
# for the t-distribution to be well-behaved and centered on the simple monthly average.
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
  
  # Require at least min_months (default 10 months) so every CI has sufficient n
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
            plotlyOutput("p1_county_plot", height = "390px")
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
            plotlyOutput("p1_weather_plot", height = "390px")
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
            plotlyOutput("p1_ci_plot", height = "430px"),
            DTOutput("p1_ci_table")
          )
        ),
        card_footer(
          class = "text-muted small",
          "Red dot = Point Estimate (Monthly Average or Overall Rate); Blue horizontal bar = Simultaneous 95% Confidence Interval adjusted for multiple comparisons."
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
      
      layout_column_wrap(
        width = 1/2,
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            textOutput("p2_ts_title")
          ),
          card_body(
            plotlyOutput("p2_ts_plot", height = "390px")
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
            plotlyOutput("p2_weather_plot", height = "390px")
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
            plotlyOutput("p2_ci_plot", height = "420px"),
            DTOutput("p2_ci_table")
          )
        ),
        card_footer(
          class = "text-muted small",
          "How to read this chart: For each weather type with at least n qualifying months, the red dot is the simple monthly average (Mean) and the blue horizontal bar is the Simultaneous 95% Confidence Interval (Mean ± t * SE)."
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
          " Tip: Click any county directly on the Virginia map to zoom in and inspect its highest-risk streets, or click 'Zoom Out to Full Virginia Map' above to see the whole Commonwealth."
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
            leafletOutput("p3_map", height = "580px")
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
            "Includes simultaneous Bonferroni-adjusted 95% binomial confidence intervals for street-level severity rates."
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
      geom_col(fill = exec_navy, alpha = 0.9, width = 0.68) +
      coord_flip() +
      labs(x = NULL, y = metric_label) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(face = "bold", color = "#1A202C")
      )
    
    if (input$p1_metric == "severe_rate") {
      p <- p + scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.08)))
    } else {
      p <- p + scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(0, 0.08)))
    }
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
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
      geom_col(fill = exec_teal, alpha = 0.9, width = 0.68) +
      coord_flip() +
      scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.08))) +
      labs(
        x = NULL,
        y = "Severe Accidents (% of Total in Weather Condition)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(face = "bold", color = "#1A202C")
      )
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
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
    
    # width = 0 prevents ggplotly from rendering oversized vertical end-caps that merge across rows
    p <- ggplot(df, aes(x = reorder(Group, Mean_Est), y = Mean_Est)) +
      geom_errorbar(
        aes(ymin = CI_Lower, ymax = CI_Upper),
        width = 0, color = exec_teal, linewidth = 2.4
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
        color = exec_crimson, size = 3.4
      ) +
      coord_flip() +
      labs(x = NULL, y = y_lab) +
      theme_minimal(base_size = 12) +
      theme(axis.text.y = element_text(face = "bold"))
    
    if (is_pct) {
      p <- p + scale_y_continuous(labels = percent_format())
    } else {
      p <- p + scale_y_continuous(labels = comma_format())
    }
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  output$p1_ci_table <- renderDT({
    df <- p1_ci_data()
    is_pct <- input$p1_ci_type %in% c("CIPIrate", "SARWC")
    
    tbl <- df %>%
      select(Group, N_Units, Mean_Est, CI_Lower, CI_Upper)
    
    dt <- datatable(
      tbl,
      rownames = FALSE,
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
  
  output$p2_ts_title <- renderText({
    paste0("Monthly Severe Accidents & Simultaneous 95% Confidence Band: ", input$p2_county)
  })
  
  output$p2_weather_title <- renderText({
    grp_label <- ifelse(input$p2_weather_grouping == "Weather_Category", "Core Weather Categories", "Weather Conditions")
    paste0("Top ", grp_label, " Associated with Severe Accidents (Plot b): ", input$p2_county)
  })
  
  output$p2_ts_plot <- renderPlotly({
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
    
    ts_df <- ts_base %>%
      mutate(
        pois_low    = calc_poisson_ci(severe_accidents, "lower", conf_level = c_level),
        pois_high   = calc_poisson_ci(severe_accidents, "upper", conf_level = c_level),
        severe_rate = ifelse(total_accidents > 0, severe_accidents / total_accidents, NA_real_),
        rate_low    = calc_binom_ci(severe_accidents, total_accidents, "lower", conf_level = c_level),
        rate_high   = calc_binom_ci(severe_accidents, total_accidents, "upper", conf_level = c_level)
      )
    
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
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  # Monthly Weather Table for Selected County:
  # Only keeps months with >= input$p2_min_monthly_acc (default 5 accidents in that month)
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
    
    # Filter for months with at least `p2_min_monthly_acc` accidents (fallback to >= 2 for tiny counties)
    qual <- monthly_all %>% filter(accidents >= input$p2_min_monthly_acc)
    if (nrow(qual) < 5) {
      qual <- monthly_all %>% filter(accidents >= 2)
    }
    if (nrow(qual) < 3) {
      qual <- monthly_all
    }
    qual
  })
  
  # Overall Weather Summary for Selected County (for Plot b and Bottom Table)
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
      geom_col(fill = "#483D8B", alpha = 0.9, width = 0.65) +
      coord_flip() +
      labs(x = NULL, y = y_lab) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(face = "bold", color = "#1A202C")
      )
    
    if (is_rate) {
      p <- p + scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.08)))
    } else {
      p <- p + scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(0, 0.08)))
    }
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  # Page 2 Simple Monthly CI Calculation (Mean +/- t * SE across qualifying months)
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
    
    # width = 0 removes vertical end-caps so CIs never collide across adjacent weather categories
    p <- ggplot(df, aes(x = reorder(Group, Mean_Est), y = Mean_Est)) +
      geom_errorbar(
        aes(ymin = CI_Lower, ymax = CI_Upper),
        width = 0, color = exec_teal, linewidth = 2.4
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
        color = exec_crimson, size = 3.4
      ) +
      coord_flip() +
      labs(x = NULL, y = y_lab) +
      theme_minimal(base_size = 12) +
      theme(axis.text.y = element_text(face = "bold"))
    
    if (is_pct) {
      p <- p + scale_y_continuous(labels = percent_format())
    } else {
      p <- p + scale_y_continuous(labels = comma_format())
    }
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  output$p2_ci_table <- renderDT({
    df <- p2_ci_data()
    is_pct <- (input$p2_weather_metric == "severe_rate")
    
    tbl <- df %>%
      select(Group, N_Units, Mean_Est, CI_Lower, CI_Upper)
    
    dt <- datatable(
      tbl,
      rownames = FALSE,
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