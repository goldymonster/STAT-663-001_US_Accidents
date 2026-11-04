dfva_joined <- read_csv("Data/Virginia_Accidents_With_Pop.csv")

acc<-dfva_joined

acc <- acc %>% mutate(severe = Severity >= 3)

county_summary <- acc %>%
  filter(!is.na(County)) %>%
  group_by(County) %>%
  summarise(
    severe_accidents = sum(severe == TRUE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(severe_accidents))

county_summary %>%
  slice_head(n = 5) %>%
  ggplot(
    aes(x = reorder(County, severe_accidents),
        y = severe_accidents)
  ) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(
    title = "Top 5 Virginia Counties by Severe Accident Counts",
    x = "County",
    y = "Number of Severe Accidents"
  ) 



weather_summary <- acc %>%
  filter(!is.na(Weather_Condition)) %>%
  group_by(Weather_Condition) %>%
  summarise(
    total_accidents = n(),
    severe_accidents = sum(severe, na.rm = TRUE),
    severe_percent = severe_accidents / total_accidents * 100,
    .groups = "drop"
  ) %>%
  filter(total_accidents >= 30) %>%
  slice_max(severe_percent, n = 10)

ggplot(
  weather_summary,
  aes(x = reorder(Weather_Condition, severe_percent),
      y = severe_percent)
) +
  geom_col(fill = "darkslateblue") +
  coord_flip() +
  labs(
    title = "Top Weather Conditions Associated with Severe Accidents",
    x = "Weather Condition",
    y = "Severe Accidents (%)"
  )









acc %>%
  filter(!is.na(Weather_Condition)) %>%
  count(Weather_Condition, sort = TRUE) %>%
  slice_head(n = 12) %>%
  ggplot(
    aes(x = reorder(Weather_Condition, n), y = n)
  ) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Most Common Weather Conditions During Accidents",
    x = "Weather Condition",
    y = "Number of Accidents"
  )

weather_summary2 <- acc %>%
  filter(
    !is.na(Weather_Condition),
    !is.na(Severity)
  ) %>%
  group_by(Weather_Condition) %>%
  summarise(
    accidents = n(),
    severe_percent = mean(Severity >= 3) * 100,
    .groups = "drop"
  ) %>%
  filter(accidents >= 100) %>%
  arrange(desc(severe_percent))

weather_summary2 %>%
  ggplot(
    aes(
      x = reorder(Weather_Condition, severe_percent),
      y = severe_percent
    )
  ) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(
    title = "Percentage of Severe Accidents by Weather",
    subtitle = "Conditions with at least 100 recorded accidents",
    x = "Weather Condition",
    y = "Severe Accidents (%)"
  )

# Just Fairfax
weather_summary_f <- acc %>%
  filter(!is.na(Weather_Condition)) %>%
  filter(County == 'Fairfax County') %>%
  group_by(Weather_Condition) %>%
  summarise(
    total_accidents = n(),
    severe_accidents = sum(severe, na.rm = TRUE),
    severe_percent = severe_accidents / total_accidents * 100,
    .groups = "drop"
  ) %>%
  slice_max(severe_percent, n = 10)

ggplot(
  weather_summary_f,
  aes(x = reorder(Weather_Condition, severe_percent),
      y = severe_percent)
) +
  geom_col(fill = "darkslateblue") +
  coord_flip() +
  labs(
    title = "Top Weather Conditions Associated with Severe Accidents for Fairfax",
    x = "Weather Condition",
    y = "Severe Accidents (%)"
  )

# With Pop

county_summary_p <- acc %>%
  filter(!is.na(County)) %>%
  group_by(County) %>%
  summarise(
    severe_accidents = sum(severe == TRUE, na.rm = TRUE),
    population = first(population),
    severe_ratio = severe_accidents / population,
    .groups = "drop"
  ) %>%
  arrange(desc(severe_accidents))

county_summary_p %>%
  slice_head(n = 5) %>%
  ggplot(
    aes(x = reorder(County, severe_ratio),
        y = severe_ratio)
  ) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(
    title = "Top 5 Virginia Counties by Severe Accident to Population Ratio",
    x = "County",
    y = "Severe Accident to Population Ratio"
  ) 

# Time series

#pooled counts plotted
monthly_accidents <- acc %>%
  filter(!is.na(Start_Time)) %>%
  mutate(
    month = floor_date(as.POSIXct(Start_Time), unit = "month")
  ) %>%
  filter(severe == TRUE) %>%
  count(month)

ggplot(monthly_accidents, aes(x = month, y = n)) +
  geom_line(color = "darkslateblue", linewidth = 1) +
  geom_point(color = "darkslateblue", size = 2) +
  labs(
    title = "Monthly Severe Traffic Accidents in Virginia",
    x = "Month",
    y = "Number of Severe Accidents"
  ) +
  scale_x_datetime(date_labels = "%b %Y",
                   date_breaks = "3 months") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#top 5 counts with counts plotted
top_5 <- acc %>%
  filter(!is.na(County), severe == TRUE) %>%
  count(County, sort = TRUE) %>%
  slice_head(n = 5) %>%
  pull(County)

county_monthly <- acc %>%
  filter(
    County %in% top_5,
    severe == TRUE,
    !is.na(Start_Time)
  ) %>%
  mutate(
    month = floor_date(as.POSIXct(Start_Time), "month")
  ) %>%
  count(month, County)

ggplot(county_monthly,
       aes(x = month, y = n, color = County)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Monthly Severe Accidents Across Top 5 Counties",
    subtitle = "Counties selected by highest severe accident counts",
    x = "Month",
    y = "Number of Severe Accidents",
    color = "County"
  ) 


#top 5 ratios with ratios plotted
county_summary_p <- acc %>%
  filter(!is.na(County)) %>%
  group_by(County) %>%
  summarise(
    severe_accidents = sum(severe == TRUE, na.rm = TRUE),
    population = first(population),
    severe_ratio = (severe_accidents / population) * 100000,
    .groups = "drop"
  ) %>%
  arrange(desc(severe_ratio))

top_5_counties <- county_summary_p %>%
  slice_max(severe_ratio, n = 5) %>%
  pull(County)


county_monthly <- acc %>%
  filter(
    County %in% top_5_counties,
    severe == TRUE,
    !is.na(Start_Time)
  ) %>%
  mutate(
    month = floor_date(as.POSIXct(Start_Time), "month")
  ) %>%
  group_by(County, month) %>%
  summarise(
    severe_accidents = n(),
    population = first(population),
    severe_ratio = (severe_accidents / population) * 100000,
    .groups = "drop"
  )


ggplot(
  county_monthly,
  aes(x = month, y = severe_ratio, color = County)
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Monthly Severe Accidents to 100k Residents Ratio",
    subtitle = "Counties selected by highest severe accident-to-population ratios",
    x = "Month",
    y = "Severe Accidents per 100k Residents",
    color = "County"
  ) +
  scale_x_datetime(date_labels = "%b %Y",
                   date_breaks = "3 months") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#top 5 ratios with counts plotted
top_5_counties <- county_summary_p %>%
  slice_max(severe_ratio, n = 5) %>%
  pull(County)


county_monthly <- acc %>%
  filter(
    County %in% top_5_counties,
    severe == TRUE,
    !is.na(Start_Time)
  ) %>%
  mutate(
    month = floor_date(as.POSIXct(Start_Time), "month")
  ) %>%
  group_by(County, month) %>%
  summarise(
    severe_accidents = n(),
    .groups = "drop"
  )


ggplot(
  county_monthly,
  aes(x = month, y = severe_accidents, color = County)
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

#top 5 county pop



# different county name
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


ggplot(
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
