
# ============================================================
# VIRGINIA TRAFFIC ACCIDENT DASHBOARD
# Pages:
# 1. Overall Summary
# 2. Individual County Analysis
# 3. Virginia Accident Map
# ============================================================


# 1. INSTALL AND LOAD PACKAGES
# ============================================================

packages <- c(
  "shiny",
  "bslib",
  "bsicons",
  "tidyverse",
  "lubridate",
  "DT",
  "leaflet",
  "leaflet.extras",
  "sf",
  "tigris",
  "scales"
)

missing_packages <- packages[
  !packages %in% installed.packages()[, "Package"]
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(packages, library, character.only = TRUE))


# 2. PREPARE DATA
# ============================================================

# Assumes your original dataset is called acc
#setwd("C:/Users/mgold/Desktop/ClassMaterial/STAT663 Data Visualization/STAT-663-001_US_Accidents/Scripts")
acc<- read.csv("../Data/Virginia_Accidents_With_Pop.csv")
acc <- acc %>%
  mutate(severe = Severity >= 3)

acc_dash <- acc %>%
  mutate(
    census_name = as.character(census_name),
    
    date = parse_date_time(
      as.character(Start_Time),
      orders = c(
        "ymd HMS", "ymd HM",
        "mdy HMS", "mdy HM"
      ),
      tz = "UTC"
    ),
    
    year = lubridate::year(date),
    
    month = floor_date(date, "month"),
    
    severe = case_when(
      tolower(as.character(severe)) %in%
        c("true", "1", "yes") ~ TRUE,
      
      tolower(as.character(severe)) %in%
        c("false", "0", "no") ~ FALSE,
      
      TRUE ~ NA
    ),
    
    population = as.numeric(population),
    
    Weather_Condition = as.character(Weather_Condition),
    
    Street = as.character(Street),
    
    Start_Lat = as.numeric(Start_Lat),
    
    Start_Lng = as.numeric(Start_Lng)
  ) %>%
  filter(
    !is.na(census_name),
    census_name != ""
  )


# 3. COUNTY POPULATION GROUPS
# ============================================================

county_population <- acc_dash %>%
  group_by(census_name) %>%
  summarise(
    population = if (all(is.na(population))) {
      NA_real_
    } else {
      first(na.omit(population))
    },
    .groups = "drop"
  ) %>%
  filter(
    !is.na(population),
    population > 0
  ) %>%
  arrange(population)


top5 <- county_population %>%
  slice_max(
    population,
    n = 5,
    with_ties = FALSE
  ) %>%
  pull(census_name)


bottom5 <- county_population %>%
  slice_min(
    population,
    n = 5,
    with_ties = FALSE
  ) %>%
  pull(census_name)


middle_start <- floor(
  (nrow(county_population) - 5) / 2
) + 1

middle5 <- county_population %>%
  slice(middle_start:(middle_start + 4)) %>%
  pull(census_name)


available_years <- sort(
  unique(na.omit(acc_dash$year))
)


# 4. CONFIDENCE INTERVAL FUNCTIONS
# ============================================================

wilson_ci <- function(x, n) {
  
  z <- qnorm(0.975)
  
  p <- x / n
  
  denominator <- 1 + z^2 / n
  
  center <- (
    p + z^2 / (2 * n)
  ) / denominator
  
  margin <- z * sqrt(
    p * (1 - p) / n +
      z^2 / (4 * n^2)
  ) / denominator
  
  tibble(
    lower = pmax(0, center - margin),
    upper = pmin(1, center + margin)
  )
}


poisson_ci <- function(x) {
  
  tibble(
    lower = ifelse(
      x == 0,
      0,
      0.5 * qchisq(0.025, 2 * x)
    ),
    
    upper = 0.5 * qchisq(
      0.975,
      2 * (x + 1)
    )
  )
}


# 5. VIRGINIA MAP DATA
# ============================================================

options(tigris_use_cache = TRUE)

va_counties <- tigris::counties(
  state = "VA",
  cb = TRUE,
  year = 2024
) %>%
  st_transform(4326) %>%
  mutate(
    census_key = str_to_lower(
      str_remove(
        NAME,
        regex("\\s+(county|city)$")
      )
    )
  )


county_map_data <- acc_dash %>%
  mutate(
    census_key = str_to_lower(
      str_remove(
        census_name,
        regex("\\s+(county|city)$")
      )
    )
  ) %>%
  group_by(census_key) %>%
  summarise(
    total_accidents = n(),
    severe_accidents = sum(severe %in% TRUE),
    known_severity = sum(!is.na(severe)),
    severe_rate = ifelse(
      known_severity > 0,
      severe_accidents / known_severity,
      NA_real_
    ),
    .groups = "drop"
  )


va_map_data <- va_counties %>%
  left_join(
    county_map_data,
    by = "census_key"
  )


# 6. USER INTERFACE
# ============================================================

ui <- navbarPage(
  
  title = "Virginia Traffic Accident Dashboard",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
  
  # ==========================================================
  # PAGE 1: OVERALL SUMMARY
  # ==========================================================
  
  tabPanel(
    "Overall Summary",
    
    sidebarLayout(
      
      sidebarPanel(
        
        h4("Dashboard Filters"),
        
        selectInput(
          "population_group",
          "County Population Group:",
          
          choices = c(
            "Top 5 Population" = "top",
            "Middle 5 Population" = "middle",
            "Bottom 5 Population" = "bottom"
          )
        ),
        
        selectInput(
          "year",
          "Select Year:",
          
          choices = c(
            "All Years" = "all",
            setNames(
              as.character(available_years),
              as.character(available_years)
            )
          ),
          
          selected = "all"
        ),
        
        width = 3
      ),
      
      mainPanel(
        
        h3("Overall Accident Statistics"),
        
        uiOutput("summary_cards"),
        
        hr(),
        
        h4("County Statistics"),
        
        DTOutput("overall_table"),
        
        hr(),
        
        h4("Weather Conditions Associated with Severe Accidents"),
        
        plotOutput(
          "overall_weather",
          height = "400px"
        ),
        
        hr(),
        
        h4("Grouped Confidence Interval Comparisons"),
        
        selectInput(
          "ci_plot_type",
          "Select Plot Type:",
          
          choices = c(
            "Grouped Bar Chart" = "bar",
            "Point and Confidence Interval" = "point",
            "Faceted Comparison" = "facet"
          )
        ),
        
        plotOutput(
          "overall_ci",
          height = "500px"
        )
      )
    )
  ),
  
  
  # ==========================================================
  # PAGE 2: INDIVIDUAL COUNTY ANALYSIS
  # ==========================================================
  
  tabPanel(
    "County Analysis",
    
    sidebarLayout(
      
      sidebarPanel(
        
        h4("County Filters"),
        
        selectInput(
          "county",
          "Select County:",
          
          choices = sort(
            unique(acc_dash$census_name)
          )
        ),
        
        selectInput(
          "county_year",
          "Select Year:",
          
          choices = c(
            "All Years" = "all",
            setNames(
              as.character(available_years),
              as.character(available_years)
            )
          ),
          
          selected = "all"
        ),
        
        width = 3
      ),
      
      mainPanel(
        
        h3("Individual County Analysis"),
        
        h4("Monthly Severe Accident Trends"),
        
        plotOutput(
          "county_ts",
          height = "400px"
        ),
        
        hr(),
        
        h4("Weather Conditions Associated with Severe Accidents"),
        
        plotOutput(
          "county_weather",
          height = "400px"
        ),
        
        hr(),
        
        h4("County Accident Statistics"),
        
        DTOutput("county_table")
      )
    )
  ),
  
  
  # ==========================================================
  # PAGE 3: VIRGINIA MAP
  # ==========================================================
  
  tabPanel(
    "Virginia Map",
    
    h3("Virginia Traffic Accident Map"),
    
    p(
      "Explore accident frequency across Virginia counties."
    ),
    
    leafletOutput(
      "va_map",
      height = "550px"
    ),
    
    hr(),
    
    h3("Individual County Accident Map"),
    
    selectInput(
      "map_county",
      "Select County:",
      
      choices = sort(
        unique(acc_dash$census_name)
      )
    ),
    
    radioButtons(
      "map_type",
      "Select Map Visualization:",
      
      choices = c(
        "Street Accident Circles" = "circles",
        "Accident Heat Map" = "heat"
      ),
      
      selected = "circles",
      inline = TRUE
    ),
    
    leafletOutput(
      "county_map",
      height = "600px"
    ),
    
    hr(),
    
    h4("Street-Level Accident Statistics"),
    
    DTOutput("street_table")
  )
  
)


# 7. SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  # ==========================================================
  # PAGE 1: OVERALL SUMMARY
  # ==========================================================
  
  selected_counties <- reactive({
    
    switch(
      input$population_group,
      "top" = top5,
      "middle" = middle5,
      "bottom" = bottom5
    )
    
  })
  
  
  overall_data <- reactive({
    
    df <- acc_dash %>%
      filter(
        census_name %in% selected_counties()
      )
    
    if (input$year != "all") {
      df <- df %>%
        filter(year == as.numeric(input$year))
    }
    
    df
    
  })
  
  
  overall_summary <- reactive({
    
    overall_data() %>%
      group_by(census_name) %>%
      summarise(
        
        total_accidents = n(),
        
        severe_accidents = sum(
          severe %in% TRUE
        ),
        
        known_severity = sum(
          !is.na(severe)
        ),
        
        .groups = "drop"
        
      ) %>%
      left_join(
        county_population,
        by = "census_name"
      ) %>%
      mutate(
        
        severe_rate = ifelse(
          known_severity > 0,
          severe_accidents / known_severity,
          NA_real_
        ),
        
        severe_ratio =
          severe_accidents / population * 100000
        
      )
    
  })
  
  
  # SUMMARY CARDS
  
  output$summary_cards <- renderUI({
    
    df <- overall_summary()
    
    total_acc <- sum(df$total_accidents)
    
    total_severe <- sum(df$severe_accidents)
    
    total_known <- sum(df$known_severity)
    
    total_population <- sum(df$population)
    
    total_rate <- ifelse(
      total_known > 0,
      total_severe / total_known,
      NA_real_
    )
    
    total_ratio <- ifelse(
      total_population > 0,
      total_severe / total_population * 100000,
      NA_real_
    )
    
    fluidRow(
      
      column(
        4,
        value_box(
          title = "Total Accidents",
          value = comma(total_acc),
          showcase = bs_icon("car-front"),
          theme = "primary"
        )
      ),
      
      column(
        4,
        value_box(
          title = "Severe Accident Rate",
          value = percent(
            total_rate,
            accuracy = 0.01
          ),
          showcase = bs_icon("exclamation-triangle"),
          theme = "danger"
        )
      ),
      
      column(
        4,
        value_box(
          title = "Severe Ratio per 100,000 Residents",
          value = round(total_ratio, 2),
          showcase = bs_icon("people"),
          theme = "success"
        )
      )
    )
    
  })
  
  
  # OVERALL TABLE
  
  output$overall_table <- renderDT({
    
    df <- overall_summary() %>%
      transmute(
        
        County = census_name,
        
        Population = comma(population),
        
        `Total Accidents` = comma(
          total_accidents
        ),
        
        `Severe Accidents` = comma(
          severe_accidents
        ),
        
        `Severe Rate` = percent(
          severe_rate,
          accuracy = 0.01
        ),
        
        `Severe Ratio per 100,000` = round(
          severe_ratio,
          2
        )
        
      )
    
    datatable(
      df,
      options = list(
        pageLength = 5,
        searching = FALSE,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
    
  })
  
  
  # OVERALL WEATHER PLOT
  
  output$overall_weather <- renderPlot({
    
    weather_data <- overall_data() %>%
      filter(
        !is.na(Weather_Condition),
        Weather_Condition != "",
        !is.na(severe)
      ) %>%
      group_by(Weather_Condition) %>%
      summarise(
        
        total_accidents = n(),
        
        severe_accidents = sum(severe),
        
        severe_rate =
          severe_accidents / total_accidents,
        
        .groups = "drop"
        
      ) %>%
      filter(
        total_accidents >= 5
      ) %>%
      slice_max(
        severe_rate,
        n = 10,
        with_ties = FALSE
      ) %>%
      mutate(
        Weather_Condition = reorder(
          Weather_Condition,
          severe_rate
        )
      )
    
    validate(
      need(
        nrow(weather_data) > 0,
        "Not enough weather observations."
      )
    )
    
    ggplot(
      weather_data,
      aes(
        x = Weather_Condition,
        y = severe_rate
      )
    ) +
      
      geom_col(fill = "steelblue") +
      
      coord_flip() +
      
      scale_y_continuous(
        labels = percent_format()
      ) +
      
      labs(
        title = "Severe Accident Rate by Weather Condition",
        x = "Weather Condition",
        y = "Severe Accident Rate"
      ) +
      
      theme_minimal(base_size = 13)
    
  })
  
  
  # OVERALL CONFIDENCE INTERVAL PLOT
  
  output$overall_ci <- renderPlot({
    
    df <- overall_summary() %>%
      filter(known_severity > 0)
    
    validate(
      need(
        nrow(df) > 0,
        "No observations available."
      )
    )
    
    rate_ci <- wilson_ci(
      df$severe_accidents,
      df$known_severity
    )
    
    ratio_ci <- poisson_ci(
      df$severe_accidents
    )
    
    plot_data <- bind_rows(
      
      tibble(
        County = df$census_name,
        Metric = "Severe Rate",
        Value = df$severe_rate,
        Lower = rate_ci$lower,
        Upper = rate_ci$upper
      ),
      
      tibble(
        County = df$census_name,
        Metric = "Severe Ratio per 100,000",
        Value = df$severe_ratio,
        Lower = ratio_ci$lower /
          df$population * 100000,
        Upper = ratio_ci$upper /
          df$population * 100000
      )
      
    )
    
    p <- ggplot(
      plot_data,
      aes(
        x = County,
        y = Value
      )
    )
    
    if (input$ci_plot_type == "bar") {
      
      p <- p +
        geom_col(
          fill = "steelblue",
          width = 0.7
        ) +
        geom_errorbar(
          aes(
            ymin = Lower,
            ymax = Upper
          ),
          width = 0.2
        )
      
    } else if (input$ci_plot_type == "point") {
      
      p <- p +
        geom_point(
          size = 3,
          color = "darkblue"
        ) +
        geom_errorbar(
          aes(
            ymin = Lower,
            ymax = Upper
          ),
          width = 0.2
        )
      
    } else {
      
      p <- p +
        geom_col(
          fill = "steelblue",
          width = 0.7
        ) +
        geom_errorbar(
          aes(
            ymin = Lower,
            ymax = Upper
          ),
          width = 0.2
        )
      
    }
    
    p +
      
      facet_wrap(
        ~Metric,
        scales = "free_y"
      ) +
      
      labs(
        title = "Severe Accident Rate and Ratio by County",
        x = "County",
        y = "Value"
      ) +
      
      theme_minimal(base_size = 12) +
      
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
    
  })
  
  
  # ==========================================================
  # PAGE 2: INDIVIDUAL COUNTY ANALYSIS
  # ==========================================================
  
  county_data <- reactive({
    
    df <- acc_dash %>%
      filter(
        census_name == input$county
      )
    
    if (input$county_year != "all") {
      df <- df %>%
        filter(
          year == as.numeric(input$county_year)
        )
    }
    
    df
    
  })
  
  
  # MONTHLY SEVERE ACCIDENT TIME SERIES
  
  output$county_ts <- renderPlot({
    
    df <- county_data() %>%
      filter(
        !is.na(month),
        !is.na(severe)
      ) %>%
      group_by(month) %>%
      summarise(
        severe_accidents = sum(severe),
        .groups = "drop"
      )
    
    validate(
      need(
        nrow(df) > 0,
        "No data available for this county and year."
      )
    )
    
    df <- df %>%
      complete(
        month = seq.Date(
          min(month),
          max(month),
          by = "month"
        ),
        fill = list(
          severe_accidents = 0
        )
      )
    
    ci <- poisson_ci(
      df$severe_accidents
    )
    
    df <- bind_cols(
      df,
      ci
    )
    
    ggplot(
      df,
      aes(
        x = month,
        y = severe_accidents
      )
    ) +
      
      geom_ribbon(
        aes(
          ymin = lower,
          ymax = upper
        ),
        fill = "steelblue",
        alpha = 0.15
      ) +
      
      geom_line(
        color = "steelblue",
        linewidth = 1
      ) +
      
      geom_point(
        color = "darkblue",
        size = 2
      ) +
      
      geom_errorbar(
        aes(
          ymin = lower,
          ymax = upper
        ),
        width = 10,
        alpha = 0.5
      ) +
      
      scale_x_date(
        date_labels = "%b %Y",
        date_breaks = "2 months"
      ) +
      
      labs(
        title = paste(
          "Monthly Severe Accidents:",
          input$county
        ),
        subtitle = "95% Exact Poisson Confidence Intervals",
        x = "Month",
        y = "Severe Accidents"
      ) +
      
      theme_minimal(base_size = 13) +
      
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
    
  })
  
  
  # COUNTY WEATHER PLOT
  
  output$county_weather <- renderPlot({
    
    weather_data <- county_data() %>%
      filter(
        !is.na(Weather_Condition),
        Weather_Condition != "",
        !is.na(severe)
      ) %>%
      group_by(Weather_Condition) %>%
      summarise(
        
        total_accidents = n(),
        
        severe_accidents = sum(severe),
        
        severe_rate =
          severe_accidents / total_accidents,
        
        .groups = "drop"
        
      ) %>%
      filter(
        total_accidents >= 5
      ) %>%
      slice_max(
        severe_rate,
        n = 10,
        with_ties = FALSE
      ) %>%
      mutate(
        Weather_Condition = reorder(
          Weather_Condition,
          severe_rate
        )
      )
    
    validate(
      need(
        nrow(weather_data) > 0,
        "Not enough weather observations."
      )
    )
    
    ggplot(
      weather_data,
      aes(
        x = Weather_Condition,
        y = severe_rate
      )
    ) +
      
      geom_col(
        fill = "darkorange"
      ) +
      
      coord_flip() +
      
      scale_y_continuous(
        labels = percent_format()
      ) +
      
      labs(
        title = paste(
          "Weather Conditions:",
          input$county
        ),
        x = "Weather Condition",
        y = "Severe Accident Rate"
      ) +
      
      theme_minimal(base_size = 13)
    
  })
  
  
  # COUNTY STATISTICS TABLE
  
  output$county_table <- renderDT({
    
    df <- county_data()
    
    total <- nrow(df)
    
    severe_total <- sum(
      df$severe %in% TRUE
    )
    
    known <- sum(
      !is.na(df$severe)
    )
    
    pop <- county_population %>%
      filter(
        census_name == input$county
      ) %>%
      pull(population)
    
    pop <- if (length(pop) > 0) {
      pop[1]
    } else {
      NA_real_
    }
    
    result <- tibble(
      
      County = input$county,
      
      Population = pop,
      
      `Total Accidents` = total,
      
      `Severe Accidents` = severe_total,
      
      `Severe Rate` = ifelse(
        known > 0,
        severe_total / known,
        NA_real_
      ),
      
      `Severe Ratio per 100,000` = ifelse(
        !is.na(pop) && pop > 0,
        severe_total / pop * 100000,
        NA_real_
      )
      
    ) %>%
      mutate(
        
        Population = comma(Population),
        
        `Total Accidents` = comma(
          `Total Accidents`
        ),
        
        `Severe Accidents` = comma(
          `Severe Accidents`
        ),
        
        `Severe Rate` = percent(
          `Severe Rate`,
          accuracy = 0.01
        ),
        
        `Severe Ratio per 100,000` = round(
          `Severe Ratio per 100,000`,
          2
        )
        
      )
    
    datatable(
      result,
      options = list(
        searching = FALSE,
        paging = FALSE,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
    
  })
  
  
  # ==========================================================
  # PAGE 3: VIRGINIA MAP
  # ==========================================================
  
  # STATEWIDE VIRGINIA MAP
  
  output$va_map <- renderLeaflet({
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = va_map_data$total_accidents,
      na.color = "#D3D3D3"
    )
    
    leaflet(va_map_data) %>%
      addProviderTiles(
        providers$CartoDB.Positron
      ) %>%
      addPolygons(
        
        fillColor = ~pal(total_accidents),
        
        fillOpacity = 0.7,
        
        color = "white",
        
        weight = 1,
        
        popup = ~paste0(
          "<b>", NAME, "</b><br>",
          "Total Accidents: ",
          comma(total_accidents), "<br>",
          "Severe Accidents: ",
          comma(severe_accidents), "<br>",
          "Severe Rate: ",
          percent(severe_rate, accuracy = 0.01)
        ),
        
        highlightOptions = highlightOptions(
          weight = 3,
          color = "black",
          bringToFront = TRUE
        )
        
      ) %>%
      addLegend(
        pal = pal,
        values = ~total_accidents,
        title = "Total Accidents",
        position = "bottomright"
      )
    
  })
  
  
  # STREET-LEVEL DATA
  
  street_data <- reactive({
    
    acc_dash %>%
      filter(
        census_name == input$map_county,
        !is.na(Street),
        Street != "",
        !is.na(Start_Lat),
        !is.na(Start_Lng),
        between(Start_Lat, 36, 40),
        between(Start_Lng, -84, -74)
      ) %>%
      group_by(Street) %>%
      summarise(
        
        total_accidents = n(),
        
        severe_accidents = sum(
          severe %in% TRUE
        ),
        
        known_severity = sum(
          !is.na(severe)
        ),
        
        severe_rate = ifelse(
          known_severity > 0,
          severe_accidents / known_severity,
          NA_real_
        ),
        
        latitude = mean(
          Start_Lat,
          na.rm = TRUE
        ),
        
        longitude = mean(
          Start_Lng,
          na.rm = TRUE
        ),
        
        .groups = "drop"
        
      ) %>%
      filter(
        total_accidents >= 5,
        is.finite(latitude),
        is.finite(longitude)
      ) %>%
      arrange(
        desc(severe_rate)
      )
    
  })
  
  
  # INDIVIDUAL COUNTY MAP
  
  output$county_map <- renderLeaflet({
    
    streets <- street_data()
    
    selected_key <- str_to_lower(
      str_remove(
        input$map_county,
        regex("\\s+(county|city)$")
      )
    )
    
    boundary <- va_counties %>%
      filter(
        census_key == selected_key
      )
    
    m <- leaflet() %>%
      addProviderTiles(
        providers$CartoDB.Positron
      )
    
    if (nrow(boundary) > 0) {
      
      m <- m %>%
        addPolygons(
          data = boundary,
          fill = FALSE,
          color = "black",
          weight = 2
        )
      
    }
    
    if (nrow(streets) > 0) {
      
      if (input$map_type == "heat") {
        
        m <- m %>%
          leaflet.extras::addHeatmap(
            
            lng = streets$longitude,
            
            lat = streets$latitude,
            
            intensity = streets$total_accidents,
            
            blur = 20,
            
            radius = 15
            
          )
        
      } else {
        
        pal <- colorNumeric(
          palette = "YlOrRd",
          domain = streets$severe_rate
        )
        
        m <- m %>%
          addCircleMarkers(
            
            data = streets,
            
            lng = ~longitude,
            
            lat = ~latitude,
            
            radius = ~sqrt(total_accidents) * 2,
            
            fillColor = ~pal(severe_rate),
            
            fillOpacity = 0.8,
            
            color = "black",
            
            weight = 1,
            
            popup = ~paste0(
              "<b>", Street, "</b><br>",
              "Total Accidents: ",
              comma(total_accidents), "<br>",
              "Severe Accidents: ",
              comma(severe_accidents), "<br>",
              "Severe Rate: ",
              percent(severe_rate, accuracy = 0.01)
            )
            
          ) %>%
          addLegend(
            pal = pal,
            values = streets$severe_rate,
            title = "Severe Accident Rate",
            position = "bottomright",
            labFormat = labelFormat(
              transform = function(x) x * 100,
              suffix = "%"
            )
          )
        
      }
      
    }
    
    if (nrow(boundary) > 0) {
      
      bounds <- st_bbox(boundary)
      
      m <- m %>%
        fitBounds(
          lng1 = bounds["xmin"],
          lat1 = bounds["ymin"],
          lng2 = bounds["xmax"],
          lat2 = bounds["ymax"]
        )
      
    }
    
    m
    
  })
  
  
  # STREET STATISTICS TABLE
  
  output$street_table <- renderDT({
    
    df <- street_data() %>%
      mutate(
        
        `Severe Rate` = percent(
          severe_rate,
          accuracy = 0.01
        ),
        
        `Total Accidents` = comma(
          total_accidents
        ),
        
        `Severe Accidents` = comma(
          severe_accidents
        )
        
      ) %>%
      select(
        Street,
        `Total Accidents`,
        `Severe Accidents`,
        `Severe Rate`
      )
    
    datatable(
      df,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
    
  })
  
}


# 8. RUN DASHBOARD
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
