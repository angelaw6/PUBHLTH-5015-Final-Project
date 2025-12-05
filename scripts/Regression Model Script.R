library(ggplot2)
library(dplyr)
library(tidyverse)
library(corrplot)
library(car)
library(MASS)
library(olsrr)
library(ggrepel)

#Prep dataset
merged_data <- read_csv("data/SVI_HealthcareAccess_dataset.csv")
head(merged_data)
print(colnames(merged_data))

# EDA 
# Correlation Matrix
numeric_vars <- dplyr::select(merged_data, where(is.numeric))
cor_matrix = cor(numeric_vars, use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color", tl.cex = 0.7)

# Linear Regression Model
full_mod = lm(
  `Primary Care Physicians Rate` ~ 
    total_population +
    housing_units +
    occupied_households +
    socioeconomic_score +
    below_poverty +
    unemployed +
    income +
    no_hs_diploma +
    house_composition_disability_score +
    age_65_or_older +
    age_17_or_under +
    civilian_with_a_disability +
    single_parent_household +
    minority_status_language +
    minority +
    speak_english_less_than_well +
    housing_type_transportation+
    multi_unit_structure +
    mobile_homes +
    crowding +
    no_vehicle +
    group_quarters,
  data = merged_data
)
summary(full_mod)

# Full model Visualizations
# Residuals plots
par(mfrow = c(2,2))
plot(full_mod)

# PCP Prediction plot
merged_data$pred_full <- predict(full_mod)

ggplot(merged_data,
       aes(x = pred_full, y = `Primary Care Physicians Rate`)) +
  geom_point(alpha = 0.7, color = "darkred") +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(
    title = "Predicted vs Actual Primary Care Physician Rate (Full Model)",
    x = "Predicted",
    y = "Actual"
  ) +
  theme_minimal()

# PCP vs SVI score
ggplot(merged_data, aes(x = x2018_overall_svi_score, y = `Primary Care Physicians Rate`)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  geom_text_repel(data = subset(merged_data, `Primary Care Physicians Rate` > quantile(`Primary Care Physicians Rate`, 0.95)),
                  aes(label = county_name), size = 3, color = "black") +
  labs(
    title = "Primary Care Physician Rate vs Social Vulnerability Index",
    x = "Social Vulnerability Index (SVI)",
    y = "Primary Care Physicians Rate"
  ) +
  theme_minimal()

# Model selection : Backwards Stepwise Regression
step_mod = stepAIC(full_mod, direction = "backward")

# Final Model
summary(step_mod)