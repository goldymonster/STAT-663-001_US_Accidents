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


