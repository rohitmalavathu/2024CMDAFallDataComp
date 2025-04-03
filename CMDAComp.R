#imported librarys
library(readr)
library(ggplot2)
library(randomForest)
library(patchwork)
library(pdp)
library(formattable)
library(dplyr)
library(caret)
library(gt)

#dataset imports
waste_data <- read_csv("waste_data.csv")
adata <- read_csv("alzheimer.csv")
hdata <- read_csv("HEPC.csv")
h2data <- read_csv("HEP.csv")

waste_data$e_waste_percent <- waste_data$special_waste_e_waste_tons_year / waste_data$total_msw_generated_tons_year * 100

waste_data$e_waste_per_capita <- waste_data$special_waste_e_waste_tons_year / waste_data$population_population_number_of_people

waste_data$food_organic_waste_per_capita <- ((waste_data$composition_food_organic_waste_percent / 100) * waste_data$total_msw_generated_tons_year) / waste_data$population_population_number_of_people

waste_data$glass_per_capita <- ((waste_data$composition_glass_percent / 100) * waste_data$total_msw_generated_tons_year) / waste_data$population_population_number_of_people

waste_data$metal_per_capita <- ((waste_data$composition_metal_percent / 100) * waste_data$total_msw_generated_tons_year) / waste_data$population_population_number_of_people

waste_data$other_per_capita <- ((waste_data$composition_other_percent / 100) * waste_data$total_msw_generated_tons_year) / waste_data$population_population_number_of_people

waste_data$paper_cardboard_per_capita <- ((waste_data$composition_paper_cardboard_percent / 100) * waste_data$total_msw_generated_tons_year) / waste_data$population_population_number_of_people

waste_data$plastic_per_capita <- ((waste_data$composition_plastic_percent / 100) * waste_data$total_msw_generated_tons_year) / waste_data$population_population_number_of_people

waste_data$total_per_capita <- waste_data$total_msw_generated_tons_year / waste_data$population_population_number_of_people

merged_data <- merge(waste_data, adata, by.x = "country_name", by.y = "country")

merged_data <- merge(merged_data, hdata, by = "country_name")

merged_data <- merge(merged_data, h2data, by = "country_name")

merged_data <- na.omit(merged_data)

exclude_columns <- c("country_name", "iso3c", "region_id", "income_id")

corr_subset <- merged_data %>%
  select(-one_of(exclude_columns))

corr_matrix <- cor(corr_subset)

corr_of_interest <- corr_matrix[, "AlzheimersRates_PrevalenceCases_RatePer100k_2021"]

remove_outliers <- function(data) {
  data_cleaned <- data
  for (col in names(data_cleaned)) {
    if (is.numeric(col)) {
      Q1 <- quantile(data_cleaned[[col]], 0.25, na.rm = TRUE)
      Q3 <- quantile(data_cleaned[[col]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      data_cleaned <- data_cleaned[data_cleaned[[col]] >= lower_bound & data_cleaned[[col]] <= upper_bound, ]
      }
    }
  return(data_cleaned)
}

merged_data_mod <- remove_outliers(merged_data)

merged_data_mod <- subset(merged_data_mod, select = -special_waste_e_waste_tons_year)

corr_data_subset <- merged_data_mod[, !names(merged_data_mod) %in% c("iso3c", "region_id", "country_name", "income_id", "AlzheimersRates_PrevalenceCasesUpperValue_RatePer100k_2021", "AlzheimersRates_PrevalenceCasesLowerValue_RatePer100k_2021")]

corr_matrix <- cor(corr_data_subset)

corr_of_interest <- corr_matrix[, "AlzheimersRates_PrevalenceCases_RatePer100k_2021"]

y_var <- "AlzheimersRates_PrevalenceCases_RatePer100k_2021"

variables <- names(corr_of_interest)
correlation_values <- as.numeric(corr_of_interest)
p_values <- numeric(length(variables))

# Calculate p-values using cor.test for each variable
for (i in seq_along(variables)) {
  x_var <- variables[i]
  
  if (x_var != y_var) {
    test_result <- cor.test(merged_data_mod[[y_var]], merged_data_mod[[x_var]])
    p_values[i] <- test_result$p.value
  } else {
    p_values[i] <- NA  # Assign NA for self-correlation
  }
}

correlation_table <- data.frame(
  Variable = variables,
  Correlation = correlation_values,
  P_Value = p_values
)

correlation_table <- correlation_table %>%
  filter(Variable != "AlzheimersRates_PrevalenceCases_RatePer100k_2021")

correlation_table <- correlation_table %>%
  mutate(Variable = recode(Variable,
                           "gdp_per_capita" = "GDP Per Capita",
                           "composition_food_organic_waste_percent" = "% Food Organic Waste",
                           "composition_glass_percent" = "% Glass",
                           "composition_metal_percent" = "% Metal",
                           "composition_other_percent" = "% Other",
                           "composition_paper_cardboard_percent" = "% Paper Cardboard",
                           "composition_plastic_percent" = "% Plastic",
                           "population_population_number_of_people" = "Population",
                           "total_msw_generated_tons_year" = "Total MSW",
                           "e_waste_percent" = "% E-Waste",
                           "e_waste_per_capita" = "E-Waste Per Capita",
                           "food_organic_waste_per_capita" = "Food Organic Waste Per Capita",
                           "glass_per_capita" = "Glass Per Capita",
                           "metal_per_capita" = "Metal Per Capita",
                           "other_per_capita" = "Other Per Capita",
                           "paper_cardboard_per_capita" = "Paper Cardboard Per Capita",
                           "plastic_per_capita" = "Plastic Per Capita",
                           "total_per_capita" = "Total Per Capita",
                           "health_expenditure_per_capita" = "Health Expenditure Per Capita",
                           "health_expenditure_percent" = "% of GDP: Health Expenditure"))

formatted_table <- correlation_table %>%
  gt() %>%
  fmt_number(
    columns = c(Correlation, P_Value),
    decimals = 3
  ) %>%
  data_color(
    columns = Correlation,
    colors = scales::col_numeric(
      palette = c("red", "blue", "red"),
      domain = c(-1, 1)
    )
  ) %>%
  fmt(
    columns = P_Value,
    fns = function(x) {
      ifelse(x < 0.001, "< 0.001", format(round(x, 3), nsmall = 3))
    }
  ) %>%
  data_color(
    columns = P_Value,
    colors = scales::col_numeric(
      palette = c("red", "blue"),
      domain = c(0, 1)
    )
  ) %>%
  cols_label(
    Variable = "Indicator Variable",
    Correlation = "Correlation",
    P_Value = "P-Value"
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>%
  tab_header(
    title = "Correlation Table with P-Values"
  ) %>%
  opt_table_font(
    font = "Arial"
  )

formatted_table

p1 <- ggplot(merged_data_mod, aes(x=composition_food_organic_waste_percent, y=AlzheimersRates_PrevalenceCases_RatePer100k_2021)) +
  geom_point() +
  xlab("Composition Food Organic Waste (%)") +
  ylab("Alzheimer's Rates Per 100k") +
  geom_smooth(method=lm, se=FALSE) +
  annotate("text", 
           x = 67.5, 
           y = 2250, 
           label = paste0("Corr = ", as.character(round(correlation_table[2, 2], 3))), 
           size = 4, 
           color = "black", 
           fontface = "bold")

p2 <- ggplot(merged_data_mod, aes(x=composition_glass_percent, y=AlzheimersRates_PrevalenceCases_RatePer100k_2021)) +
  geom_point() +
  xlab("Composition Glass (%)") +
  ylab("Alzheimer's Rates Per 100k") +
  geom_smooth(method=lm, se=FALSE) +
  annotate("text", 
           x = 17.5, 
           y = 1000, 
           label = paste0("Corr = ", as.character(round(correlation_table[3, 2], 3))), 
           size = 4, 
           color = "black", 
           fontface = "bold")

p3 <- ggplot(merged_data_mod, aes(x=composition_paper_cardboard_percent, y=AlzheimersRates_PrevalenceCases_RatePer100k_2021)) +
  geom_point() +
  xlab("Composition Paper Cardboard (%)") +
  ylab("Alzheimer's Rates Per 100k") +
  geom_smooth(method=lm, se=FALSE) +
  annotate("text", 
           x = 40, 
           y = 900, 
           label = paste0("Corr = ", as.character(round(correlation_table[6, 2], 3))), 
           size = 4, 
           color = "black", 
           fontface = "bold")

p4 <- ggplot(merged_data_mod, aes(x=e_waste_percent, y=AlzheimersRates_PrevalenceCases_RatePer100k_2021)) +
  geom_point() +
  xlab("Composition E-Waste (%)") +
  ylab("Alzheimer's Rates Per 100k") +
  geom_smooth(method=lm, se=FALSE) +
  annotate("text", 
           x = 10, 
           y = 850, 
           label = paste0("Corr = ", as.character(round(correlation_table[10, 2], 3))), 
           size = 4, 
           color = "black", 
           fontface = "bold")

(p1 | p2) /
(p3 | p4) + plot_annotation(title = "Scatter Plot of Waste Indicators (Percent of Total Waste)")

p5 <- ggplot(merged_data_mod, aes(x=paper_cardboard_per_capita, y=AlzheimersRates_PrevalenceCases_RatePer100k_2021)) +
  geom_point() +
  xlab("Paper Cardboard") +
  ylab("Alzheimer's Rates Per 100k") +
  geom_smooth(method=lm, se=FALSE) +
  annotate("text", 
           x = 0.25, 
           y = 500, 
           label = paste0("Corr = ", as.character(round(correlation_table[16, 2], 3))), 
           size = 3, 
           color = "black", 
           fontface = "bold")

p6 <- ggplot(merged_data_mod, aes(x=glass_per_capita, y=AlzheimersRates_PrevalenceCases_RatePer100k_2021)) +
  geom_point() +
  xlab("Glass") +
  ylab("Alzheimer's Rates Per 100k") +
  geom_smooth(method=lm, se=FALSE) +
  annotate("text", 
           x = 0.0625, 
           y = 500, 
           label = paste0("Corr = ", as.character(round(correlation_table[13, 2], 3))), 
           size = 3, 
           color = "black", 
           fontface = "bold")

p7 <- ggplot(merged_data_mod, aes(x=e_waste_per_capita, y=AlzheimersRates_PrevalenceCases_RatePer100k_2021)) +
  geom_point() +
  xlab("E-Waste") +
  ylab("Alzheimer's Rates Per 100k") +
  geom_smooth(method=lm, se=FALSE) +
  annotate("text", 
           x = 0.04, 
           y = 500, 
           label = paste0("Corr = ", as.character(round(correlation_table[11, 2], 3))), 
           size = 3, 
           color = "black", 
           fontface = "bold")

p8 <- ggplot(merged_data_mod, aes(x=plastic_per_capita, y=AlzheimersRates_PrevalenceCases_RatePer100k_2021)) +
  geom_point() +
  xlab("Plastic") +
  ylab("Alzheimer's Rates Per 100k") +
  geom_smooth(method=lm, se=FALSE) +
  annotate("text", 
           x = 0.11, 
           y = 2000, 
           label = paste0("Corr = ", as.character(round(correlation_table[17, 2], 3))), 
           size = 3, 
           color = "black", 
           fontface = "bold")

p9 <- ggplot(merged_data_mod, aes(x=other_per_capita, y=AlzheimersRates_PrevalenceCases_RatePer100k_2021)) +
  geom_point() +
  xlab("Other") +
  ylab("Alzheimer's Rates Per 100k") +
  geom_smooth(method=lm, se=FALSE) +
  annotate("text", 
           x = 0.2, 
           y = 2500, 
           label = paste0("Corr = ", as.character(round(correlation_table[15, 2], 3))), 
           size = 3, 
           color = "black", 
           fontface = "bold")

p10 <- ggplot(merged_data_mod, aes(x=total_per_capita, y=AlzheimersRates_PrevalenceCases_RatePer100k_2021)) +
  geom_point() +
  xlab("Total Municipal Solid Waste") +
  ylab("Alzheimer's Rates Per 100k") +
  geom_smooth(method=lm, se=FALSE) +
  annotate("text", 
           x = 0.85, 
           y = 500, 
           label = paste0("Corr = ", as.character(round(correlation_table[18, 2], 3))), 
           size = 3, 
           color = "black", 
           fontface = "bold")

(p5 | p6 | p7) /
(p8 | p9 | p10) + plot_annotation(title = "Scatter Plot of Waste Indicators (Per Capita)")

p11 <- ggplot(merged_data_mod, aes(x=gdp_per_capita, y=AlzheimersRates_PrevalenceCases_RatePer100k_2021)) +
  geom_point() +
  xlab("GDP Per Capita") +
  ylab("Alzheimer's Rates Per 100k") +
  geom_smooth(method=lm, se=FALSE) +
  annotate("text", 
           x = 80000, 
           y = 2250, 
           label = paste0("Corr = ", as.character(round(correlation_table[1, 2], 3))), 
           size = 4, 
           color = "black", 
           fontface = "bold")

p12 <- ggplot(merged_data_mod, aes(x=health_expenditure_per_capita, y=AlzheimersRates_PrevalenceCases_RatePer100k_2021)) +
  geom_point() +
  xlab("Health Expenditure Per Capita") +
  ylab("Alzheimer's Rates Per 100k") +
  geom_smooth(method=lm, se=FALSE) +
  annotate("text", 
           x = 1250, 
           y = 2500, 
           label = paste0("Corr = ", as.character(round(correlation_table[20, 2], 3))), 
           size = 4, 
           color = "black", 
           fontface = "bold")

p13 <- ggplot(merged_data_mod, aes(x=health_expenditure_percent, y=AlzheimersRates_PrevalenceCases_RatePer100k_2021)) +
  geom_point() +
  xlab("Health Expenditure (% of GDP)") +
  ylab("Alzheimer's Rates Per 100k") +
  geom_smooth(method=lm, se=FALSE) +
  annotate("text", 
           x = 5, 
           y = 2000, 
           label = paste0("Corr = ", as.character(round(correlation_table[19, 2], 3))), 
           size = 4, 
           color = "black", 
           fontface = "bold")

(p11 | p13) / 
(p12) + plot_annotation(title = "Scatter Plot of Economic Indicators")

set.seed(123)

cv_results <- train(
  AlzheimersRates_PrevalenceCases_RatePer100k_2021 ~ 
    gdp_per_capita + 
    composition_food_organic_waste_percent + 
    composition_glass_percent + 
    composition_paper_cardboard_percent + 
    e_waste_percent +
    e_waste_per_capita +
    glass_per_capita +
    other_per_capita +
    paper_cardboard_per_capita +
    plastic_per_capita +
    total_per_capita +
    health_expenditure_per_capita +
    health_expenditure_percent, 
  data = merged_data,
  method = "rf",
  trControl = trainControl(method = "cv", number = 10),
  metric = "RMSE"
)

mean_rmse <- mean(cv_results$resample$RMSE)
mean_mae <- mean(cv_results$resample$MAE)
mean_r_squared <- mean(cv_results$resample$Rsquared)

# Calculate Adjusted R-squared
n <- nrow(merged_data)  # Total number of observations
p <- length(cv_results$finalModel$forest$independent.variables)  # Number of predictors (features)
adjusted_r2_values <- 1 - ((1 - cv_results$resample$Rsquared) * (n - 1)) / (n - p - 1)
mean_adjusted_r2 <- mean(adjusted_r2_values)

# Create a gt table
metric_table <- data.frame(
  Metric = c("RMSE", "MAE", "Adjusted R-squared"),
  Value = c(mean_rmse, mean_mae, mean_adjusted_r2)
)

# Convert to gt table
metric_table_gt <- metric_table %>%
  gt() %>%
  tab_header(
    title = "Model Performance Metrics"
  ) %>%
  cols_label(
    Metric = "Metric",
    Value = "Value"
  ) %>%
  fmt_number(
    columns = vars(Value),
    decimals = 3
  ) %>%
  opt_table_font(
    font = "Arial"
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  )

# Print the table
metric_table_gt

importance_df <- data.frame(
  Feature = rownames(importance_values$importance),
  Importance = importance_values$importance$Overall  # Correct column name for importance
)

importance_df$Normalized_Importance <- (importance_df$Importance - min(importance_df$Importance)) / 
  (max(importance_df$Importance) - min(importance_df$Importance))

importance_df <- importance_df %>%
  mutate(Feature = recode(Feature,
                           "gdp_per_capita" = "GDP Per Capita",
                           "composition_food_organic_waste_percent" = "% Food Organic Waste",
                           "composition_glass_percent" = "% Glass",
                           "composition_metal_percent" = "% Metal",
                           "composition_other_percent" = "% Other",
                           "composition_paper_cardboard_percent" = "% Paper Cardboard",
                           "composition_plastic_percent" = "% Plastic",
                           "population_population_number_of_people" = "Population",
                           "total_msw_generated_tons_year" = "Total MSW",
                           "e_waste_percent" = "% E-Waste",
                           "e_waste_per_capita" = "E-Waste Per Capita",
                           "food_organic_waste_per_capita" = "Food Organic Waste Per Capita",
                           "glass_per_capita" = "Glass Per Capita",
                           "metal_per_capita" = "Metal Per Capita",
                           "other_per_capita" = "Other Per Capita",
                           "paper_cardboard_per_capita" = "Paper Cardboard Per Capita",
                           "plastic_per_capita" = "Plastic Per Capita",
                           "total_per_capita" = "Total Per Capita",
                           "health_expenditure_per_capita" = "Health Expenditure Per Capita",
                           "health_expenditure_percent" = "% of GDP: Health Expenditure"))

ggplot(importance_df, aes(x = reorder(Feature, Normalized_Importance), y = Normalized_Importance)) +
  geom_bar(stat = "identity", fill = "black") +
  coord_flip() +  # Flip the axes for better readability
  ggtitle("Normalized Feature Importance Plot") +
  xlab("Indicators") +
  ylab("Normalized Importance")

pdp_plot1 <- partial(cv_results$finalModel, pred.var = "health_expenditure_per_capita", 
                     train = merged_data)
pdp_plot2 <- partial(cv_results$finalModel, pred.var = "e_waste_per_capita", 
                     train = merged_data)

gg_pdp_plot1 <- ggplot(pdp_plot1, aes(x = health_expenditure_per_capita, y = yhat)) +
  geom_line() +
  xlab("Health Expenditure Per Capita") +
  ylab("Predicted Alzheimer's Rate") +
  ggtitle("PDP for Health Expenditure Per Capita")

gg_pdp_plot2 <- ggplot(pdp_plot2, aes(x = e_waste_per_capita, y = yhat)) +
  geom_line() +
  xlab("Composition E-Waste Per Capita") +
  ylab("Predicted Alzheimer's Rate") +
  ggtitle("PDP for Composition E-Waste Per Capita")

gg_pdp_plot1 / gg_pdp_plot2 + plot_annotation(title = "Partial Dependence Plot of Most Important Waste Indicator and Economic Indicator")

ice_plot_data <- partial(cv_results$finalModel, 
                         pred.var = "health_expenditure_per_capita", 
                         type = "regression", 
                         ice = TRUE, 
                         plot = FALSE, 
                         train = merged_data)  # plot = FALSE to return the data instead of plotting

ice_plot_data <- as.data.frame(ice_plot_data)

ggplot(ice_plot_data, aes(x = health_expenditure_per_capita, y = yhat)) +
  geom_line(aes(group = yhat.id), alpha = 0.3, color = "black") +  # Individual ICE lines
  stat_summary(fun = mean, geom = "line", color = "red", size = 1) +  # Mean line
  labs(title = "Individual Conditional Expectation (ICE) Plot: Health Expenditure Per Capita",
       x = "Health Expenditure Per Capita",
       y = "Predicted Alzheimer's Rate") +
  theme_minimal()

ice_plot_data2 <- partial(cv_results$finalModel, 
                         pred.var = "e_waste_per_capita", 
                         type = "regression", 
                         ice = TRUE, 
                         plot = FALSE, 
                         train = merged_data)  # plot = FALSE to return the data instead of plotting

ice_plot_data2 <- as.data.frame(ice_plot_data2)

ggplot(ice_plot_data2, aes(x = e_waste_per_capita, y = yhat)) +
  geom_line(aes(group = yhat.id), alpha = 0.3, color = "black") +  # Individual ICE lines
  stat_summary(fun = mean, geom = "line", color = "red", size = 1) +  # Mean line
  labs(title = "Individual Conditional Expectation (ICE) Plot: E-Waste Per Capita",
       x = "E-Waste Per Capita",
       y = "Predicted Alzheimer's Rate") +
  theme_minimal()