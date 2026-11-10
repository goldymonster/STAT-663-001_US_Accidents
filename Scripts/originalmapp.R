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
# Avoid hardcoded setwd() paths so the project runs from a clean checkout (M2/M4)
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

# Read data; treat empty strings as NA so blank weather/street cells are caught
acc_raw <- read.csv(existing_path, na.strings = c("", "NA"), stringsAsFactors = FALSE)

# Clean and standardize variables
acc <- acc_raw %>%
  mutate(
    # Robust timestamp parsing (handles both ISO8601 'T/Z' and standard formats)
    Start_Time = ymd_hms(Start_Time, tz = "UTC", quiet = TRUE),
    End_Time   = ymd_hms(End_Time, tz = "UTC", quiet = TRUE),
    Year       = year(Start_Time),
    month_year = floor_date(Start_Time, unit = "month"),
    
    # Severity classification (Severity 3 & 4 = Severe)
    severe     = Severity >= 3,
    
    # Standardize GEOID as 5-character string for exact tigris spatial joins
    GEOID      = str_pad(as.character(GEOID), width = 5, side = "left", pad = "0"),
    
    # Clean text fields
    Weather_Condition = str_squish(Weather_Condition),
    Street            = str_squish(Street),
    census_name       = str_squish(census_name)
  ) %>%
  filter(!is.na(census_name), !is.na(Severity), !is.na(Year))

# Pre-compute static county population rankings across all VA localities in data
county_pop_ranks <- acc %>%
  group_by(GEOID, census_name) %>%
  summarise(population = first( na.omit(population) ), .groups = "drop") %>%
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

# Load Virginia county/independent city boundaries once via tigris
va_boundaries <- tryCatch({
  counties(state = "VA", cb = TRUE, year = 2022, progress_bar = FALSE) %>%
    st_transform(4326)
}, error = function(e) {
  NULL
})

# Exact Clopper-Pearson 95% Binomial Confidence Interval helper (vectorized)
calc_binom_ci <- function(x, n, bound = c("lower", "upper")) {
  bound <- match.arg(bound)
  idx <- ifelse(bound == "lower", 1, 2)
  map2_dbl(x, n, function(k, total) {
    if (is.na(k) || is.na(total) || total <= 0) return(NA_real_)
    binom.test(k, total)$conf.int[idx]
  })
}

# Exact Garwood 95% Poisson Confidence Interval helper for monthly counts
calc_poisson_ci <- function(k_vec, bound = c("lower", "upper")) {
  bound <- match.arg(bound)
  idx <- ifelse(bound == "lower", 1, 2)
  map_dbl(k_vec, function(k) {
    if (is.na(k) || k < 0) return(NA_real_)
    poisson.test(as.integer(round(k)))$conf.int[idx]
  })
}

# Colorblind-safe executive palette (Okabe-Ito & Commonwealth Navy/Gold/Crimson)
exec_navy    <- "#0F2042"
exec_crimson <- "#9E1B32"
exec_teal    <- "#0072B2"
exec_amber   <- "#D55E00"
exec_slate   <- "#4A5568"

# ==============================================================================
# 2. USER INTERFACE (bslib Modern Executive Layout)
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
        width = 310,
        selectInput(
          "p1_pop_tier",
          "1. Locality Population Tier:",
          choices = c(
            "Top 5 in Population"    = "top5",
            "Middle 5 in Population" = "mid5",
            "Bottom 5 in Population" = "bot5",
            "Top 10 in Population"   = "top10",
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
          "3. Primary Display Metric:",
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
          "4. Uncertainty / Interval View:",
          choices = c(
            "Monthly Severe Ratio 95% CI & PI (CIPI)"      = "CIPI",
            "Monthly Severe Rate 95% CI & PI (CIPIrate)"   = "CIPIrate",
            "Weather Severe Rate 95% Binomial CI (SARWC)"  = "SARWC"
          ),
          selected = "CIPI"
        ),
        sliderInput(
          "p1_min_weather_n",
          "Min. Accidents per Weather Condition:",
          min = 5, max = 100, value = 20, step = 5
        ),
        hr(),
        tags$small(
          class = "text-muted",
          bs_icon("info-circle"),
          " Severe accidents are defined as Severity levels 3 and 4 (major traffic delay impact). ",
          "Population denominators use 2023 U.S. Census Bureau county/city estimates."
        )
      ),
      
      # KPI Value Boxes
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
      
      # Row 1 Charts: County Comparison + Weather Severity
      layout_column_wrap(
        width = 1/2,
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "County Comparison by Selected Metric & Population Tier"
          ),
          card_body(
            plotlyOutput("p1_county_plot", height = "400px")
          ),
          card_footer(
            class = "text-muted small",
            "Bars show point estimates; error bars on rates show exact 95% Clopper-Pearson binomial confidence intervals."
          )
        ),
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Weather Conditions Most Associated with Severe Accidents"
          ),
          card_body(
            plotlyOutput("p1_weather_plot", height = "400px")
          ),
          card_footer(
            class = "text-muted small",
            "Filtered dynamically by the selected Population Tier and Year above. Error bars indicate 95% binomial CIs."
          )
        )
      ),
      
      # Row 2: Grouped Confidence & Prediction Intervals (CIPI / CIPIrate / SARWC)
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-dark text-white",
          "Uncertainty & Volatility Analysis: Confidence and Prediction Intervals"
        ),
        card_body(
          layout_column_wrap(
            width = 1/2,
            plotlyOutput("p1_ci_plot", height = "420px"),
            DTOutput("p1_ci_table")
          )
        ),
        card_footer(
          class = "text-muted small",
          "Note on Uncertainty: Thick blue bars show 95% Confidence Intervals (uncertainty in the mean); thin gray bars show 95% Prediction Intervals (month-to-month volatility bounds, clamped at [0, 100%]). Zero-accident months are treated as 0 for per-capita ratios but excluded (NA) from conditional severity rates."
        )
      )
    )
  ),
  
  # ============================================================================
  # PAGE 2: COUNTY DEEP-DIVE & TEMPORAL DYNAMICS
  # ============================================================================
  nav_panel(
    title = "2. County Deep-Dive & Time Series",
    icon = bs_icon("graph-up"),
    layout_sidebar(
      sidebar = sidebar(
        title = "County Selection",
        width = 310,
        selectInput(
          "p2_county",
          "Select Virginia Locality:",
          choices = all_counties,
          selected = if ("Fairfax County" %in% all_counties) "Fairfax County" else all_counties[1]
        ),
        radioButtons(
          "p2_ts_metric",
          "Time Series Metric:",
          choices = c(
            "Monthly Severe Accident Count (with 95% Poisson CI)" = "count",
            "Monthly Severe Rate % (with 95% Binomial CI)"        = "rate"
          ),
          selected = "count"
        ),
        radioButtons(
          "p2_weather_metric",
          "County Weather Ranking Metric (Plot b):",
          choices = c(
            "Severe Rate (% of Accidents in Weather that are Severe)" = "severe_rate",
            "Severe Ratio (Severe Accidents per 100k Residents)"      = "severe_ratio"
          ),
          selected = "severe_rate"
        ),
        sliderInput(
          "p2_min_weather_n",
          "Min. Accidents per Weather Condition in County:",
          min = 1, max = 50, value = 10, step = 1
        ),
        hr(),
        tags$small(
          class = "text-muted",
          bs_icon("lightbulb"),
          " Statistical Note: Ranking weather within a single county by 'Severe Ratio' (severe / population) is proportional to raw volume because county population is constant. Use 'Severe Rate' to identify high-risk weather conditions relative to occurrence."
        )
      ),
      
      # Row 1: Monthly Time Series with CIs + Worst Weather Conditions (Plot b)
      layout_column_wrap(
        width = 1/2,
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            textOutput("p2_ts_title")
          ),
          card_body(
            plotlyOutput("p2_ts_plot", height = "410px")
          ),
          card_footer(
            class = "text-muted small",
            "Shaded ribbon represents exact 95% Poisson confidence intervals (for monthly counts) or 95% Clopper-Pearson binomial intervals (for monthly severity rates)."
          )
        ),
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            textOutput("p2_weather_title")
          ),
          card_body(
            plotlyOutput("p2_weather_plot", height = "410px")
          ),
          card_footer(
            class = "text-muted small",
            "Interactive Plotly chart (Plot 'b') showing top weather conditions and 95% confidence intervals for the selected locality."
          )
        )
      ),
      
      # Row 2: Detailed Summary Table of Rates, Ratios, and Counts
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-dark text-white",
          "Detailed Breakdown of Counts, Severe Rates, and Per-Capita Ratios by Weather & Year"
        ),
        card_body(
          DTOutput("p2_summary_table")
        )
      )
    )
  ),
  
  # ============================================================================
  # PAGE 3: GEOSPATIAL & STREET-LEVEL HOTSPOT MAP
  # ============================================================================
  nav_panel(
    title = "3. Geospatial & Street Hotspot Map",
    icon = bs_icon("geo-alt-fill"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Geospatial Controls",
        width = 310,
        selectInput(
          "p3_county",
          "1. Select Virginia Locality:",
          choices = all_counties,
          selected = if ("Fairfax County" %in% all_counties) "Fairfax County" else all_counties[1]
        ),
        radioButtons(
          "p3_map_mode",
          "2. Map Display Layer:",
          choices = c(
            "Street-Level Hotspots (Grouped by Street)" = "streets",
            "Individual Severe Accident Points"         = "points"
          ),
          selected = "streets"
        ),
        radioButtons(
          "p3_street_metric",
          "3. Street Ranking / Color Metric:",
          choices = c(
            "Severe Accident Count (Volume Hotspot)"       = "severe_accidents",
            "Severe Accident Rate (% Severe on Street)"    = "severe_rate",
            "Severe Accidents per 100k County Residents"   = "severe_ratio"
          ),
          selected = "severe_accidents"
        ),
        sliderInput(
          "p3_min_street_n",
          "Min. Total Accidents on Street:",
          min = 1, max = 50, value = 5, step = 1
        ),
        sliderInput(
          "p3_top_n_streets",
          "Number of Top Streets to Display on Map:",
          min = 5, max = 100, value = 25, step = 5
        ),
        hr(),
        tags$small(
          class = "text-muted",
          bs_icon("pin-map-fill"),
          " Circle size encodes accident volume on that street; color encodes the selected severity metric using the colorblind-safe Viridis/Plasma scale."
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
            padding = 0,
            leafletOutput("p3_map", height = "560px")
          ),
          card_footer(
            class = "text-muted small",
            "County boundary matched via official 5-digit Census GEOID to prevent Fairfax County vs. Fairfax City boundary collisions."
          )
        ),
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-dark text-white",
            "Highest-Risk Streets in Selected Locality (Grouped by Street)"
          ),
          card_body(
            DTOutput("p3_street_table")
          ),
          card_footer(
            class = "text-muted small",
            "Click any row or inspect the map markers for exact street-level counts, severe rates, and 95% binomial confidence intervals."
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
  # PAGE 1 REACTIVE DATA
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
  
  # County-level summary for Page 1
  p1_county_summary <- reactive({
    df <- p1_filtered_acc()
    
    # Keep all selected tier counties visible even if 0 accidents occurred in a given year
    base_counties <- county_pop_ranks %>%
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
        severe_ratio     = (severe_accidents / population) * 100000,
        rate_ci_low      = calc_binom_ci(severe_accidents, total_accidents, "lower"),
        rate_ci_high     = calc_binom_ci(severe_accidents, total_accidents, "upper")
      )
  })
  
  # KPI Outputs
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
  
  # Page 1 Plot 1: County Bar / Interval Chart
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
      geom_col(fill = exec_navy, alpha = 0.9, width = 0.7)
    
    if (input$p1_metric == "severe_rate") {
      p <- p +
        geom_errorbar(
          aes(ymin = rate_ci_low, ymax = rate_ci_high),
          width = 0.25, color = exec_amber, linewidth = 0.8
        ) +
        scale_y_continuous(labels = percent_format())
    } else {
      p <- p + scale_y_continuous(labels = comma_format())
    }
    
    p <- p +
      coord_flip() +
      labs(x = NULL, y = metric_label) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(face = "bold", color = "#1A202C")
      )
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  # Page 1 Plot 2: Weather Conditions Associated with Severe Accidents
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
      slice_max(severe_percent, n = 10) %>%
      mutate(
        lower_ci = calc_binom_ci(severe_accidents, total_accidents, "lower"),
        upper_ci = calc_binom_ci(severe_accidents, total_accidents, "upper")
      )
    df
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
          "95% Binomial CI: [", percent(lower_ci, accuracy = 0.1), ", ",
          percent(upper_ci, accuracy = 0.1), "]<br>",
          "Severe / Total: ", comma(severe_accidents), " / ", comma(total_accidents)
        )
      )
    ) +
      geom_col(fill = exec_teal, alpha = 0.85, width = 0.65) +
      geom_errorbar(
        aes(ymin = lower_ci, ymax = upper_ci),
        width = 0.25, color = "#1A202C", linewidth = 0.7
      ) +
      coord_flip() +
      scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.1))) +
      labs(
        x = NULL,
        y = "Severe Accidents (% of Total in Weather Condition)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(face = "bold")
      )
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  # Page 1 Plot 3 & Table: Grouped CIs (CIPI, CIPIrate, SARWC)
  p1_monthly_variance <- reactive({
    df <- p1_filtered_acc() %>% filter(!is.na(month_year))
    validate(need(nrow(df) > 0, "No monthly records available for this filter selection."))
    
    # Expand across the observed monthly sequence in the filtered data
    all_months <- seq(min(df$month_year), max(df$month_year), by = "1 month")
    
    monthly <- df %>%
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
        # Per-capita ratio is validly 0 when 0 severe accidents occur in a month
        severe_ratio = (severe_accidents / population) * 100000,
        # Statistical Correction: Conditional rate P(Severe | Accident) is NA (not 0) when 0 accidents occur
        severe_rate  = ifelse(accidents == 0, NA_real_, severe_accidents / accidents)
      )
    monthly
  })
  
  p1_ci_data <- reactive({
    if (input$p1_ci_type == "SARWC") {
      df <- p1_weather_summary() %>%
        transmute(
          Group    = Weather_Condition,
          N_Units  = total_accidents,
          Mean_Est = severe_percent,
          CI_Lower = lower_ci,
          CI_Upper = upper_ci,
          PI_Lower = NA_real_,
          PI_Upper = NA_real_
        )
      return(df)
    }
    
    monthly <- p1_monthly_variance()
    target_col <- ifelse(input$p1_ci_type == "CIPI", "severe_ratio", "severe_rate")
    upper_cap  <- ifelse(input$p1_ci_type == "CIPI", Inf, 1.0)
    
    res <- monthly %>%
      group_by(census_name) %>%
      summarise(
        pop      = first(population),
        N_Units  = sum(!is.na(.data[[target_col]])),
        Mean_Est = mean(.data[[target_col]], na.rm = TRUE),
        variance = var(.data[[target_col]], na.rm = TRUE),
        stdev    = sqrt(variance),
        SE       = stdev / sqrt(N_Units),
        t_val    = ifelse(N_Units > 1, qt(0.975, df = N_Units - 1), NA_real_),
        # Clamp lower bounds at 0 and rate upper bounds at 1.0
        CI_Lower = pmax(0, Mean_Est - (t_val * SE)),
        CI_Upper = pmin(upper_cap, Mean_Est + (t_val * SE)),
        PI_Lower = pmax(0, Mean_Est - (t_val * stdev * sqrt(1 + (1 / N_Units)))),
        PI_Upper = pmin(upper_cap, Mean_Est + (t_val * stdev * sqrt(1 + (1 / N_Units)))),
        .groups  = "drop"
      ) %>%
      arrange(desc(pop))
    
    if (input$p1_pop_tier == "all") {
      res <- res %>% slice_head(n = 15)
    }
    
    res %>% rename(Group = census_name)
  })
  
  output$p1_ci_plot <- renderPlotly({
    df <- p1_ci_data()
    validate(need(nrow(df) > 0, "Insufficient data to compute intervals."))
    
    is_pct <- input$p1_ci_type %in% c("CIPIrate", "SARWC")
    y_lab  <- switch(
      input$p1_ci_type,
      "CIPI"     = "Monthly Severe Accidents per 100k Residents",
      "CIPIrate" = "Monthly Severe Accident Rate (% of Accidents)",
      "SARWC"    = "Severe Accident Rate by Weather Condition (%)"
    )
    
    p <- ggplot(df, aes(x = reorder(Group, Mean_Est), y = Mean_Est))
    
    # Plot Prediction Intervals (if applicable) as thin gray background bars
    if (input$p1_ci_type != "SARWC") {
      p <- p + geom_errorbar(
        aes(ymin = PI_Lower, ymax = PI_Upper),
        width = 0.15, color = "gray65", linewidth = 0.9
      )
    }
    
    p <- p +
      geom_errorbar(
        aes(ymin = CI_Lower, ymax = CI_Upper),
        width = 0, color = exec_teal, linewidth = 2.4
      ) +
      geom_point(
        aes(
          text = paste0(
            "<b>", Group, "</b><br>",
            "Estimate: ", ifelse(is_pct, percent(Mean_Est, 0.1), round(Mean_Est, 2)), "<br>",
            "95% CI: [", ifelse(is_pct, percent(CI_Lower, 0.1), round(CI_Lower, 2)), ", ",
            ifelse(is_pct, percent(CI_Upper, 0.1), round(CI_Upper, 2)), "]",
            ifelse(
              !is.na(PI_Lower),
              paste0(
                "<br>95% PI: [",
                ifelse(is_pct, percent(PI_Lower, 0.1), round(PI_Lower, 2)), ", ",
                ifelse(is_pct, percent(PI_Upper, 0.1), round(PI_Upper, 2)), "]"
              ),
              ""
            )
          )
        ),
        color = exec_crimson, size = 3.2
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
      select(Group, N_Units, Mean_Est, CI_Lower, CI_Upper, PI_Lower, PI_Upper)
    
    dt <- datatable(
      tbl,
      rownames = FALSE,
      colnames = c("Locality / Condition", "N (Months/Acc)", "Mean", "95% CI Low", "95% CI High", "95% PI Low", "95% PI High"),
      options = list(pageLength = 8, dom = "tip", scrollX = TRUE)
    )
    
    if (is_pct) {
      dt <- dt %>% formatPercentage(c("Mean_Est", "CI_Lower", "CI_Upper", "PI_Lower", "PI_Upper"), digits = 1)
    } else {
      dt <- dt %>% formatRound(c("Mean_Est", "CI_Lower", "CI_Upper", "PI_Lower", "PI_Upper"), digits = 2)
    }
    dt
  })
  
  # ----------------------------------------------------------------------------
  # PAGE 2 REACTIVE DATA: COUNTY DEEP-DIVE
  # ----------------------------------------------------------------------------
  p2_county_acc <- reactive({
    acc %>% filter(census_name == input$p2_county)
  })
  
  output$p2_ts_title <- renderText({
    paste0("Monthly Severe Accidents & 95% Confidence Band: ", input$p2_county)
  })
  
  output$p2_weather_title <- renderText({
    paste0("Top Weather Conditions Associated with Severe Accidents: ", input$p2_county)
  })
  
  # Page 2 Plot 1: Monthly Time Series with Exact 95% CIs
  output$p2_ts_plot <- renderPlotly({
    df <- p2_county_acc() %>% filter(!is.na(month_year))
    validate(need(nrow(df) > 0, "No time-series data available for this locality."))
    
    all_months <- seq(min(df$month_year), max(df$month_year), by = "1 month")
    
    ts_df <- df %>%
      group_by(month_year) %>%
      summarise(
        total_accidents  = n(),
        severe_accidents = sum(severe, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      complete(month_year = all_months, fill = list(total_accidents = 0L, severe_accidents = 0L)) %>%
      mutate(
        # Exact 95% Poisson CI for monthly counts
        pois_low  = calc_poisson_ci(severe_accidents, "lower"),
        pois_high = calc_poisson_ci(severe_accidents, "upper"),
        # Exact 95% Binomial CI for monthly severe rate
        severe_rate = ifelse(total_accidents > 0, severe_accidents / total_accidents, NA_real_),
        rate_low    = calc_binom_ci(severe_accidents, total_accidents, "lower"),
        rate_high   = calc_binom_ci(severe_accidents, total_accidents, "upper")
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
              "95% Poisson CI: [", round(pois_low, 1), ", ", round(pois_high, 1), "]<br>",
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
              "95% Binomial CI: [", percent(rate_low, 0.1), ", ", percent(rate_high, 0.1), "]<br>",
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
  
  # Page 2 Plot 2: Worst Weather Conditions for Selected County (Plot b)
  p2_weather_data <- reactive({
    df <- p2_county_acc() %>%
      filter(!is.na(Weather_Condition), Weather_Condition != "") %>%
      group_by(Weather_Condition) %>%
      summarise(
        accidents            = n(),
        severe_accidents     = sum(severe, na.rm = TRUE),
        not_severe_accidents = sum(!severe, na.rm = TRUE),
        population           = first(population),
        severe_ratio         = (severe_accidents / population) * 100000,
        severe_rate          = severe_accidents / accidents,
        not_severe_rate      = not_severe_accidents / accidents,
        rate_ci_low          = calc_binom_ci(severe_accidents, accidents, "lower"),
        rate_ci_high         = calc_binom_ci(severe_accidents, accidents, "upper"),
        .groups = "drop"
      )
    df
  })
  
  output$p2_weather_plot <- renderPlotly({
    df <- p2_weather_data() %>%
      filter(accidents >= input$p2_min_weather_n) %>%
      slice_max(.data[[input$p2_weather_metric]], n = 8)
    
    validate(
      need(nrow(df) > 0, "No weather conditions in this locality meet the minimum accident threshold. Lower the slider on the left.")
    )
    
    is_rate <- input$p2_weather_metric == "severe_rate"
    y_lab   <- ifelse(
      is_rate,
      "Severe Accident Rate (% of Weather Accidents)",
      "Severe Accidents per 100,000 Residents (Severe Ratio)"
    )
    
    p <- ggplot(
      df,
      aes(
        x = reorder(Weather_Condition, .data[[input$p2_weather_metric]]),
        y = .data[[input$p2_weather_metric]],
        text = paste0(
          "<b>", Weather_Condition, "</b><br>",
          "Severe Rate: ", percent(severe_rate, accuracy = 0.1),
          " (95% CI: ", percent(rate_ci_low, 0.1), "–", percent(rate_ci_high, 0.1), ")<br>",
          "Severe Ratio (per 100k): ", round(severe_ratio, 2), "<br>",
          "Severe Accidents: ", comma(severe_accidents), "<br>",
          "Total Accidents: ", comma(accidents)
        )
      )
    ) +
      geom_col(fill = "#483D8B", alpha = 0.9, width = 0.65)
    
    if (is_rate) {
      p <- p +
        geom_errorbar(
          aes(ymin = rate_ci_low, ymax = rate_ci_high),
          width = 0.22, color = exec_amber, linewidth = 0.8
        ) +
        scale_y_continuous(labels = percent_format())
    } else {
      p <- p + scale_y_continuous(labels = comma_format())
    }
    
    p <- p +
      coord_flip() +
      labs(x = NULL, y = y_lab) +
      theme_minimal(base_size = 12) +
      theme(axis.text.y = element_text(face = "bold"))
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  # Page 2 Table: Complete Breakdown of Rates & Counts
  output$p2_summary_table <- renderDT({
    df <- p2_weather_data() %>%
      arrange(desc(severe_accidents)) %>%
      select(
        Weather_Condition, accidents, severe_accidents, not_severe_accidents,
        severe_rate, rate_ci_low, rate_ci_high, severe_ratio
      )
    
    datatable(
      df,
      rownames = FALSE,
      colnames = c(
        "Weather Condition", "Total Accidents", "Severe Accidents",
        "Non-Severe Accidents", "Severe Rate (%)", "95% CI Low",
        "95% CI High", "Severe Ratio (per 100k)"
      ),
      options = list(pageLength = 10, scrollX = TRUE)
    ) %>%
      formatPercentage(c("severe_rate", "rate_ci_low", "rate_ci_high"), digits = 1) %>%
      formatRound("severe_ratio", digits = 2) %>%
      formatCurrency(c("accidents", "severe_accidents", "not_severe_accidents"), currency = "", interval = 3, mark = ",", digits = 0)
  })
  
  # ----------------------------------------------------------------------------
  # PAGE 3 REACTIVE DATA: GEOSPATIAL & STREET HOTSPOTS
  # ----------------------------------------------------------------------------
  p3_county_acc <- reactive({
    acc %>%
      filter(
        census_name == input$p3_county,
        !is.na(Start_Lat),
        !is.na(Start_Lng)
      )
  })
  
  # Street-level aggregation for Page 3
  p3_street_summary <- reactive({
    df <- p3_county_acc() %>%
      filter(!is.na(Street), Street != "") %>%
      group_by(Street) %>%
      summarise(
        total_accidents  = n(),
        severe_accidents = sum(severe, na.rm = TRUE),
        population       = first(population),
        severe_rate      = severe_accidents / total_accidents,
        severe_ratio     = (severe_accidents / population) * 100000,
        rate_ci_low      = calc_binom_ci(severe_accidents, total_accidents, "lower"),
        rate_ci_high     = calc_binom_ci(severe_accidents, total_accidents, "upper"),
        lat              = median(Start_Lat, na.rm = TRUE),
        lng              = median(Start_Lng, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(total_accidents >= input$p3_min_street_n) %>%
      arrange(desc(.data[[input$p3_street_metric]]), desc(total_accidents))
    
    df
  })
  
  output$p3_map_title <- renderText({
    paste0("Geospatial Street Hotspot Map: ", input$p3_county)
  })
  
  output$p3_map <- renderLeaflet({
    df_pts <- p3_county_acc()
    validate(need(nrow(df_pts) > 0, "No geocoded accident records found for this locality."))
    
    # Match boundary via exact 5-digit Census GEOID (avoids Fairfax County vs. Fairfax city collision)
    target_geoid <- unique(df_pts$GEOID)[1]
    boundary_poly <- NULL
    if (!is.null(va_boundaries) && !is.na(target_geoid)) {
      boundary_poly <- va_boundaries %>% filter(GEOID == target_geoid)
    }
    
    m <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron)
    
    if (!is.null(boundary_poly) && nrow(boundary_poly) > 0) {
      m <- m %>%
        addPolygons(
          data = boundary_poly,
          fill = TRUE,
          fillColor = exec_navy,
          fillOpacity = 0.05,
          color = exec_navy,
          weight = 3,
          opacity = 0.9,
          group = "County Boundary"
        )
    }
    
    if (input$p3_map_mode == "streets") {
      streets_df <- p3_street_summary() %>%
        slice_head(n = input$p3_top_n_streets)
      
      validate(
        need(nrow(streets_df) > 0, "No streets meet the minimum accident threshold. Lower the 'Min. Total Accidents on Street' slider.")
      )
      
      pal <- colorNumeric(
        palette = "plasma",
        domain = streets_df[[input$p3_street_metric]],
        reverse = TRUE
      )
      
      # Scale marker radius smoothly between 6 and 20 pixels based on total accidents on street
      max_acc <- max(streets_df$total_accidents, 1)
      radii   <- 6 + 14 * sqrt(streets_df$total_accidents / max_acc)
      
      m <- m %>%
        addCircleMarkers(
          data = streets_df,
          lng = ~lng,
          lat = ~lat,
          radius = radii,
          fillColor = ~pal(streets_df[[input$p3_street_metric]]),
          color = "#1A202C",
          weight = 1.2,
          fillOpacity = 0.85,
          popup = ~paste0(
            "<b>Street: </b>", Street, "<br>",
            "<b>Locality: </b>", input$p3_county, "<br>",
            "<b>Severe Accidents: </b>", comma(severe_accidents), "<br>",
            "<b>Total Accidents: </b>", comma(total_accidents), "<br>",
            "<b>Severe Rate: </b>", percent(severe_rate, accuracy = 0.1),
            " (95% CI: ", percent(rate_ci_low, 0.1), "–", percent(rate_ci_high, 0.1), ")<br>",
            "<b>Severe Ratio (per 100k): </b>", round(severe_ratio, 2)
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = streets_df[[input$p3_street_metric]],
          title = switch(
            input$p3_street_metric,
            "severe_accidents" = "Severe Count",
            "severe_rate"      = "Severe Rate",
            "severe_ratio"     = "Severe / 100k"
          ),
          opacity = 0.85
        )
    } else {
      # Individual severe accident points (capped at 2,500 points for smooth browser rendering)
      sev_pts <- df_pts %>%
        filter(severe) %>%
        slice_head(n = 2500)
      
      validate(need(nrow(sev_pts) > 0, "No severe accidents recorded in this locality."))
      
      m <- m %>%
        addCircleMarkers(
          data = sev_pts,
          lng = ~Start_Lng,
          lat = ~Start_Lat,
          radius = 4,
          color = exec_crimson,
          fillOpacity = 0.65,
          stroke = FALSE,
          popup = ~paste0(
            "<b>Street: </b>", Street, "<br>",
            "<b>Severity: </b>", Severity, "<br>",
            "<b>Weather: </b>", Weather_Condition, "<br>",
            "<b>Time: </b>", Start_Time
          )
        )
    }
    
    m
  })
  
  output$p3_street_table <- renderDT({
    df <- p3_street_summary() %>%
      select(
        Street, total_accidents, severe_accidents,
        severe_rate, rate_ci_low, rate_ci_high, severe_ratio
      )
    
    datatable(
      df,
      rownames = FALSE,
      colnames = c(
        "Street / Highway", "Total Acc.", "Severe Acc.",
        "Severe Rate (%)", "95% CI Low", "95% CI High", "Severe per 100k"
      ),
      options = list(pageLength = 10, scrollX = TRUE)
    ) %>%
      formatPercentage(c("severe_rate", "rate_ci_low", "rate_ci_high"), digits = 1) %>%
      formatRound("severe_ratio", digits = 2)
  })
}

shinyApp(ui = ui, server = server)