library(tidyverse)  
library(data.table)
library(sf)
library(tigris)
library(lubridate) 
options(tigris_use_cache = TRUE)

setwd("C:/Users/mgold/Desktop/ClassMaterial/STAT663 Data Visualization/STAT-663-001_US_Accidents/Scripts")
dfva_joined <- read.csv("../Data/Virginia_Accidents_With_Pop.csv")
str(dfva_joined)

# Format timestamps
dfva_joined$Start_Time <- as.POSIXct(
  dfva_joined$Start_Time,
  format = "%Y-%m-%dT%H:%M:%OSZ",
  tz = "UTC"
)
dfva_joined$End_Time <- as.POSIXct(
  dfva_joined$End_Time,
  format = "%Y-%m-%dT%H:%M:%OSZ",
  tz = "UTC"
)

## General Summary Table
county_summary_p <- dfva_joined %>%
  mutate(severe = Severity >= 3) %>%
  filter(!is.na(census_name)) %>%
  group_by(census_name) %>%
  summarise(
    severe_accidents = sum(severe == TRUE, na.rm = TRUE),
    not_severe_accidents = sum(severe == FALSE, na.rm = TRUE),
    population = first(population),
    severe_ratio = (severe_accidents / population) * 100000,
    accidents = n(),
    severe_rate = severe_accidents/accidents,
    not_severe_rate = not_severe_accidents/accidents,
    .groups = "drop"
  ) %>%
  arrange(desc(population)) %>%
  slice_max(population, n = 10)

knitr::kable(county_summary_p, digits = 3)

variancecounty <- dfva_joined %>%
  mutate(
    severe = Severity >= 3, 
    month_year = floor_date(Start_Time, "month") 
  ) %>%
  filter(!is.na(census_name)) %>%
  group_by(census_name, month_year) %>%
  summarise(
    severe_accidents = sum(severe == TRUE, na.rm = TRUE),
    not_severe_accidents = sum(severe == FALSE, na.rm = TRUE),
    population = first(population),
    accidents = n(),
    .groups = "drop"
  ) %>%
  complete(census_name, month_year, fill = list(severe_accidents = 0, not_severe_accidents = 0, accidents = 0)) %>%
  group_by(census_name) %>%
  fill(population, .direction = "downup") %>%
  ungroup() %>%
  mutate(
    severe_ratio = (severe_accidents / population) * 100000,
    severe_rate = ifelse(accidents == 0, 0, severe_accidents / accidents)
  ) %>% arrange(desc(population), month_year)

# Top 10 counties by population
top10_counties <- variancecounty %>%
  group_by(census_name) %>%
  summarise(population = first(population), .groups = "drop") %>%
  slice_max(population, n = 10) %>%
  pull(census_name)

# Boxplot
variancecounty %>%
  filter(census_name %in% top10_counties) %>%
  ggplot(aes(
    x = reorder(census_name, severe_ratio),
    y = severe_ratio
  )) +
  geom_boxplot() +
  labs(
    title = "Monthly Severe Accident Rate Distributions\nTop 10 Virginia Counties by Population",
    x = "County",
    y = "Severe Accidents per 100,000 Residents"
  ) +
  coord_flip() +
  theme_minimal()

CIPI <- variancecounty %>%
  group_by(census_name) %>%
  summarize(
    pop = first(population),
    n = sum(!is.na(severe_ratio) & !is.nan(severe_ratio)), 
    mean = mean(severe_ratio, na.rm = TRUE),
    variance = var(severe_ratio, na.rm = TRUE),
    stdev = sqrt(variance),
    SE = stdev / sqrt(n),
    t_val = qt(0.975, df = n - 1), 
    ci_lower = mean - (t_val * SE),
    ci_upper = mean + (t_val * SE),
    pi_lower = mean - (t_val * stdev * sqrt(1 + (1/n))),
    pi_upper = mean + (t_val * stdev * sqrt(1 + (1/n))),
    .groups = "drop"
  ) %>%
  arrange(desc(pop)) %>%
  slice_head(n = 10)

knitr::kable(CIPI, digits = 3)

interval_chart <- ggplot(CIPI, aes(x = reorder(census_name, mean), y = mean)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0, color = "steelblue", linewidth = 2) +
  geom_point(color = "firebrick", size = 3) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Traffic Accident Ratios: Averages and Volatility",
    subtitle = "95% Confidence Intervals of the top 10 Virginia Counties in Population",
    x = NULL,
    y = "Accidents per 100,000 Residents"
  )

interval_chart


## Now doing for rate
CIPIrate <- variancecounty %>%
  group_by(census_name) %>%
  summarize(
    pop = first(population),
    n = sum(!is.na(severe_rate) & !is.nan(severe_rate)), 
    mean = mean(severe_rate, na.rm = TRUE),
    variance = var(severe_rate, na.rm = TRUE),
    stdev = sqrt(variance),
    SE = stdev / sqrt(n),
    t_val = qt(0.975, df = n - 1), 
    ci_lower = mean - (t_val * SE),
    ci_upper = mean + (t_val * SE),
    pi_lower = mean - (t_val * stdev * sqrt(1 + (1/n))),
    pi_upper = mean + (t_val * stdev * sqrt(1 + (1/n))),
    .groups = "drop"
  ) %>%
  arrange(desc(pop)) %>%
  slice_head(n = 10)

knitr::kable(CIPIrate, digits = 3)

interval_chart <- ggplot(CIPIrate, aes(x = reorder(census_name, mean), y = mean)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0, color = "steelblue", linewidth = 2) +
  geom_point(color = "firebrick", size = 3) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Traffic Accident Rates: Averages and Volatility",
    subtitle = "95% Confidence Intervals of the top 10 Virginia Counties in Population",
    x = NULL,
    y = "Severe Accident Rate (Proportion of Total)"
  )

interval_chart
