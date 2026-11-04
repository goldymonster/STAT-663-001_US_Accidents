library(tidyverse)  
library(data.table)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)
dfva_joined <- read.csv("../Data/Virginia_Accidents_With_Pop.csv")
str(dfva_joined)
setwd("C:/Users/mgold/Desktop/ClassMaterial/STAT663 Data Visualization/STAT-663-001_US_Accidents/Scripts")

dfva_joined$Start_Time<- as.POSIXct(
  dfva_joined$Start_Time,
  format = "%Y-%m-%dT%H:%M:%OSZ",
  tz = "UTC"
)
dfva_joined$End_Time <- as.POSIXct(
  dfva_joined$End_Time,
  format = "%Y-%m-%dT%H:%M:%OSZ",
  tz = "UTC")

## General Summary Table
county_summary_p <- dfva_joined %>%
  mutate(severe = Severity >= 3, year = year(Start_Time), month = month(Start_Time)) %>%
  filter(!is.na(census_name)) %>%
  group_by(census_name) %>%
  summarise(
    severe_accidents = sum(severe == TRUE, na.rm = TRUE),
    population = first(population),
    severe_ratio = (severe_accidents / population) * 100000,
    .groups = "drop"
  ) %>%
  arrange(desc(population))%>%
  slice_max(population, n=10)
knitr::kable(county_summary_p, digits = 3)

#Variance Table
variancecounty <- dfva_joined %>%
  mutate(severe = Severity >= 3, year = year(Start_Time), month = month(Start_Time)) %>%
  filter(!is.na(census_name)) %>%
  mutate(year_month = paste(year, month, sep = "-")) %>%
  group_by(census_name, year_month) %>%
  summarise(
    severe_accidents = sum(severe == TRUE, na.rm = TRUE),
    population = first(population),
    severe_ratio = (severe_accidents / population) * 100000,
    .groups = "drop"
  ) %>%
  arrange(desc(population))

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
    x = reorder(census_name, severe_ratio, median),
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
