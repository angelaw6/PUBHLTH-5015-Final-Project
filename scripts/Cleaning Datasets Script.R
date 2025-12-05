library(dplyr)
library(tidyverse)
library(readxl)
library(readr)
library(janitor)

# SVI Dataset 
county_raw = read_csv("data/SVI_County_Export.csv", show_col_types = FALSE) |>
  clean_names()

# Cleaning SVI Dataset
county_svi = county_raw |>
  separate(location, into = c("county_name", "state"), sep = ", ") |>
  mutate(
    county = county_name |> 
      str_remove(" County") |>
      str_to_upper()
  )
prep = function(df) {
  df |>
    mutate(across(where(is.character), as.factor)) |>
    mutate(across(where(is.factor), ~fct_explicit_na(.x, na_level = "Missing"))) |>
    mutate(across(where(is.numeric), ~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)))
}

county_svi_simple = prep(county_svi)
county_svi_simple$county_name <- gsub(" County$", "", county_svi_simple$county_name)
county_svi_simple$fips <- sprintf("%05d", county_svi_simple$fips)


# Healthcare Access dataset
excel_sheets("data/2025 County Health Rankings Ohio Data - v3.xlsx")
main <- read_excel("data/2025 County Health Rankings Ohio Data - v3.xlsx",
                   sheet = "Select Measure Data", skip = 1)

# Process datast (extract primary care physician info)
healthcare_vars <- c(
  "FIPS", "State", "County",  # keep these for identification
  
  "# Primary Care Physicians",
  "Primary Care Physicians Rate",
  "Primary Care Physicians Ratio",
  "National Z-Score...90"
)
healthcare_access_data <- main[, healthcare_vars]
healthcare_access_data <- healthcare_access_data[-1, ]

# Join both datasets based on FIPS
merged_data = healthcare_access_data %>% 
  left_join(county_svi_simple, by = c("FIPS" = "fips"))

head(merged_data)
print(colnames(merged_data))
write_csv(merged_data, "data/SVI_HealthcareAccess_dataset.csv")
