# Heat map code
library(maps)
library(ggplot2)
library(dplyr)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(fastDummies)
library(recipes) 
library(forcats)
library(mapproj)
library(tidyverse)

# Prepping dataset for heatmaps
analysis_df <- read_csv("data/SVI_HealthcareAccess_dataset.csv")

ohio_map <- map_data("county") %>% 
  filter(region == "ohio") %>% 
  mutate(county = toupper(subregion))

map_df <- ohio_map %>% 
  left_join(analysis_df, by = "county")

# Heat map: Ohio SVI score
ggplot(map_df, aes(long, lat, group = group, fill = x2018_overall_svi_score)) +
  geom_polygon(color = "white", size = 0.2) +
  coord_map() +
  scale_fill_viridis_c(option = "rocket",  direction = -1) +
  theme_void() +
  labs(fill = "SVI", title = "Ohio Social Vulnerability Index (SVI)")

# Heat map: Total population
ggplot(map_df, aes(long, lat, group = group, fill = total_population)) +
  geom_polygon(color = "white", linewidth = 0.3) +
  coord_map() +
  scale_fill_viridis_c(option = "rocket", direction = -1) +
  theme_void() +
  labs(
    title = "Total Population by Ohio County",
    fill = "Population"
  )

# Heat map: Socioeconomic Score
ggplot(map_df, aes(long, lat, group = group, fill = socioeconomic_score)) +
  geom_polygon(color = "white", size = 0.2) +
  coord_map() +
  scale_fill_viridis_c(option = "rocket",  direction = -1) +
  theme_void() +
  labs(fill = "SES", title = "Socioeconomic score")


# Heat map: Housing Type & Transportation
ggplot(map_df, aes(long, lat, group = group, fill = housing_type_transportation)) +
  geom_polygon(color = "white", size = 0.2) +
  coord_map() +
  scale_fill_viridis_c(option = "rocket",  direction = -1) +
  theme_void() +
  labs(fill = "Housing type/Transportation", title = "Housing Type & Transportation")

# Heat map: Household Composition & Disability Vulnerability
ggplot(map_df, aes(long, lat, group = group, fill = house_composition_disability_score)) +
  geom_polygon(color = "white", linewidth = 0.2) +
  coord_map() +
  scale_fill_viridis_c(option = "rocket", direction = -1) +
  theme_void() +
  labs(
    title =  "Household Composition & Disability Vulnerability\nby Ohio County",
    fill = "Housing/Disability/Theme Score",
  )

# Heat map: Primary Care Physician Rate
ggplot(map_df, aes(long, lat, group = group, fill = `Primary Care Physicians Rate`)) +
  geom_polygon(color = "white", linewidth = 0.2) +
  coord_map() +
  scale_fill_viridis_c(option = "rocket", direction = -1) +
  theme_void() +
  labs(
    title =  "Primary Care Physician Rate by Ohio County",
    fill = "PCP Rate per n/1000",
  )
