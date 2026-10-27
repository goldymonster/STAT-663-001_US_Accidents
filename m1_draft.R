library(tidyverse)


acc <- us %>% filter(State == 'VA')

summary(acc)

table(acc$Severity)
summary(acc$`Precipitation(in)`)


acc <- acc %>% mutate(severe = Severity >= 3)

acc %>%
  filter(!is.na(Severity)) %>%
  count(Severity) %>%
  ggplot(aes(x = factor(Severity), y = n)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Virginia Car Accidents by Severity",
    x = "Accident Severity",
    y = "Number of Accidents")


acc %>%
  summarise(
    total_accidents = n(),
    severe_accidents = sum(severe, na.rm = TRUE),
    severe_percent = mean(severe, na.rm = TRUE) * 100
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

weather_summary <- acc %>%
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

weather_summary %>%
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

road_features <- c(
  "Junction",
  "Crossing",
  "Traffic_Signal",
  "Stop",
  "Railway",
  "Traffic_Calming",
  "Roundabout",
  "Give_Way",
  "Amenity",
  "Bump",
  "No_Exit",
  "Turning_Loop",
  "Station"
)

road_summary <- acc %>%
  select(Severity, all_of(road_features)) %>%
  pivot_longer(
    cols = all_of(road_features),
    names_to = "road_feature",
    values_to = "present"
  ) %>%
  filter(!is.na(present), !is.na(Severity)) %>%
  group_by(road_feature, present) %>%
  summarise(
    accidents = n(),
    severe_percent = mean(Severity >= 3) * 100,
    .groups = "drop"
  )

ggplot(
  road_summary,
  aes(
    x = road_feature,
    y = severe_percent,
    fill = factor(present)
  )
) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Accident Severity by Road Feature",
    x = "Road Feature",
    y = "Severe Accidents (%)",
    fill = "Feature Present"
  )



county_summary <- acc %>%
  filter(!is.na(County)) %>%
  group_by(County) %>%
  summarise(
    accidents = n(),
    severe_accidents = sum(severe, na.rm = TRUE),
    severe_percent = mean(severe, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(accidents))

county_summary %>%
  slice_head(n = 15) %>%
  ggplot(
    aes(x = reorder(County, accidents), y = accidents)
  ) +
  geom_col(fill = "darkslateblue") +
  coord_flip() +
  labs(
    title = "Virginia Counties with the Most Recorded Accidents",
    x = "County",
    y = "Number of Accidents"
  ) 




