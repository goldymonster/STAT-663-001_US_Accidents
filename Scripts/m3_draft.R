
setwd("C:/Users/mgold/Desktop/ClassMaterial/STAT663 Data Visualization/STAT-663-001_US_Accidents/Scripts")
acc<- read.csv("../Data/Virginia_Accidents_With_Pop.csv")
acc <- acc %>%
  mutate(severe = Severity >= 3)
weather_summary <- acc %>%
  filter(!is.na(Weather_Condition)) %>%
  group_by(Weather_Condition) %>%
  summarise(
    total_accidents = n(),
    severe_accidents = sum(severe, na.rm = TRUE),
    severe_percent = severe_accidents / total_accidents,
    .groups = "drop"
  ) %>%
  filter(total_accidents >= 20) %>%
  slice_max(severe_percent, n = 10)

weather_summary <- weather_summary %>%
  mutate(
    lower_ci = map2_dbl(
      severe_accidents, total_accidents,
      ~ binom.test(.x, .y)$conf.int[1]
    ),
    upper_ci = map2_dbl(
      severe_accidents, total_accidents,
      ~ binom.test(.x, .y)$conf.int[2]
    )
  )



SARWC <- ggplot(
  weather_summary,
  aes(
    x = reorder(Weather_Condition, severe_percent),
    y = severe_percent
  )
) +
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    width = 0.2,
    color = "gray40"
  ) +
  geom_point(
    size = 3,
    color = "steelblue"
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Severe Accident Rate by Weather Condition",
    subtitle = "Points represent rates; lines represent 95% confidence intervals",
    x = "Weather Condition",
    y = "Percentage of Accidents Classified as Severe"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 9)
  )


county_summary_p <- acc %>%
  filter(!is.na(census_name)) %>%
  group_by(census_name) %>%
  summarise(
    severe_accidents = sum(severe == TRUE, na.rm = TRUE),
    population = first(population),
    severe_ratio = (severe_accidents / population) * 100000,
    .groups = "drop"
  ) %>%
  arrange(desc(severe_ratio))


top_5_counties <- county_summary_p %>%
  slice_max(severe_ratio, n = 5) %>%
  pull(census_name)


county_monthly <- acc %>%
  filter(
    census_name %in% top_5_counties,
    severe == TRUE,
    !is.na(Start_Time)
  ) %>%
  mutate(
    month = floor_date(as.POSIXct(Start_Time), "month")
  ) %>%
  group_by(census_name, month) %>%
  summarise(
    severe_accidents = n(),
    .groups = "drop"
  )


ts <- ggplot(
  county_monthly,
  aes(x = month, y = severe_accidents, color = census_name)
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Monthly Severe Traffic Accidents in Top 5 Virginia Counties",
    subtitle = "Counties selected by highest severe accident-to-population ratios",
    x = "Month",
    y = "Number of Severe Accidents",
    color = "County"
  ) +
  scale_x_datetime(
    date_labels = "%b %Y",
    date_breaks = "3 months"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )






SARWC

county_summary <- acc %>%
  filter(!is.na(County)) %>%
  group_by(County) %>%
  summarise(
    severe_accidents = sum(severe == TRUE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(severe_accidents))


library(leaflet)
library(tigris)
library(sf)

ff_accidents <- acc %>%
  filter(
    census_name == "Fairfax County",
    Severity >= 3,
    !is.na(Start_Lat),
    !is.na(Start_Lng)
  )

fairfax_boundary <- counties(
  state = "VA",
  cb = TRUE,
  year = 2024
) %>%
  filter(NAME == "Fairfax County") %>%
  st_transform(4326)

map<- leaflet(ff_accidents) %>%
  addTiles(
    urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
    attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
  ) %>%
  addPolygons(
    data = fairfax_boundary,
    fill = FALSE,
    color = "black",
    weight = 3,
    opacity = 1,
    group = "County Boundary"
  ) %>%
  addCircleMarkers(
    lng = ~Start_Lng,
    lat = ~Start_Lat,
    radius = 3,
    color = "red",
    fillOpacity = 0.6,
    stroke = FALSE,
    popup = ~paste(
      "County:", County,
      "<br>Severity:", Severity,
      "<br>Weather:", Weather_Condition,
      "<br>Time:", Start_Time
    )
  )

map