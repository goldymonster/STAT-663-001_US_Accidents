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



ggplot(
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


county_summary <- acc %>%
  filter(!is.na(County)) %>%
  group_by(County) %>%
  summarise(
    severe_accidents = sum(severe == TRUE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(severe_accidents))