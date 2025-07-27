# Load necessary libraries for data manipulation, regression modeling, and table generation
library(dplyr)
library(tidyr)
library(readr)
library(psych)
library(fastDummies)
library(ggplot2)
library(broom)
library(here)
library(stargazer)
library(lmtest) # For Durbin-Watson and Breusch-Pagan tests
library(nortest) # For Anderson-Darling test for large datasets

# --- Utility Functions ---
# Source utility functions for saving tables and plots
if (!exists("save_table")) {
  original_wd <- getwd()
  setwd(here())
  
  ensure_output_dirs <- function() {
    output_dirs <- c(here("output", "figures"), here("output", "tables"), here("output", "reports"))
    for (dir in output_dirs) {
      if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE, showWarnings = FALSE)
        message(paste("Created directory:", dir))
      }
    }
  }
  
  setwd(original_wd)
}

# --- Data Loading and Preprocessing ---
# Load the pre-processed and analysis-ready dataset
analysis_dataset <- readRDS(here("processed-data", "analysis_ready_dataset.rds"))

# Create dummy variables explicitly with fastDummies
# Assuming gender, urban_rural, work_type are categorical; adjust names if different
analysis_dataset <- analysis_dataset %>%
  dummy_cols(select_columns = c("gender_f", "urban_rural_f", "work_type_f"), 
             remove_first_dummy = TRUE, # Avoid multicollinearity
             remove_selected_columns = TRUE) # Remove original categorical columns

# save Preproccessed data
saveRDS(analysis_dataset, file = here("processed-data", "preprocessed-dataset.rds"))
print("Analysis-ready dataset saved as 'preprocessed-dataset.rds'")

# Print column names to verify dummy variables
print("=== Dataset Columns After Dummy Coding ===")
print(colnames(analysis_dataset))

print("=== STARTING REGRESSION ANALYSIS ===")
print(paste("Sample size:", nrow(analysis_dataset)))

# =============================================================================
# REGRESSION MODELING
# =============================================================================
# Objective: To model the relationship between the independent variables and the
# dependent variable, 'responsible_social_contribution'. We will build a series of models
# to test the relative importance of academic vs. alternative predictors.
# Note: Replace dummy variable names with actual names from your dataset after dummy_cols

attach(analysis_dataset)

# --- Model 1(a): Academic Performance Only ---

model1a <- lm(responsible_social_contribution ~ academic_performance + control_predictor_trad +
                gender_f_Female + urban_rural_f_Rural + work_type_f_Business, data = analysis_dataset)

# --- Model 1(b): Academic Performance Only --- (Across Demographic Factors)
model1b <- lm(responsible_social_contribution ~ academic_performance + control_predictor_trad, data = analysis_dataset)

# --- Model 2(a): relation_with_school Only ---
model2a <- lm(responsible_social_contribution ~ relation_with_school +
                gender_f_Female + urban_rural_f_Rural + work_type_f_Business, data = analysis_dataset)

# --- Model 2(b): relation_with_school Only --- (Across Demographic Factors)
model2b <- lm(responsible_social_contribution ~ relation_with_school, data = analysis_dataset)

# --- Model 3(a): child_relation_with_school Only ---
model3a <- lm(responsible_social_contribution ~ child_relation_with_school +
                gender_f_Female + urban_rural_f_Rural + work_type_f_Business, data = analysis_dataset)

# --- Model 3(b): child_relation_with_school Only --- (Across Demographic Factors)
model3b <- lm(responsible_social_contribution ~ child_relation_with_school, data = analysis_dataset)

# --- Model 4(a): school_type Only ---
model4a <- lm(responsible_social_contribution ~ school_type +
                gender_f_Female + urban_rural_f_Rural + work_type_f_Business, data = analysis_dataset)

# --- Model 4(b): school_type Only --- (Across Demographic Factors)
model4b <- lm(responsible_social_contribution ~ school_type, data = analysis_dataset)

# --- Model 5(a): house_environment Only ---
model5a <- lm(responsible_social_contribution ~ house_environment +
                gender_f_Female + urban_rural_f_Rural + work_type_f_Business, data = analysis_dataset)

# --- Model 5(b): house_environment Only --- (Across Demographic Factors)
model5b <- lm(responsible_social_contribution ~ house_environment, data = analysis_dataset)

# --- Model 6(a): Combined Model ---
model6a <- lm(responsible_social_contribution ~ relation_with_school + child_relation_with_school + school_type + house_environment +
                gender_f_Female + urban_rural_f_Rural + work_type_f_Business, data = analysis_dataset)

# --- Model 6(b): Combined Model --- (Across Demographic Factors)
model6b <- lm(responsible_social_contribution ~ relation_with_school + child_relation_with_school + school_type + house_environment, data = analysis_dataset)

# --- Model 7(a): Combined Model ---
model7a <- lm(responsible_social_contribution ~ alternative_factors +
                gender_f_Female + urban_rural_f_Rural + work_type_f_Business, data = analysis_dataset)

# --- Model 7(b): Combined Model --- (Across Demographic Factors)
model7b <- lm(responsible_social_contribution ~ alternative_factors, data = analysis_dataset)

# --- Model 8(a): Combined Model ---
model8a <- lm(responsible_social_contribution ~ academic_performance +
                relation_with_school + child_relation_with_school + school_type + house_environment +
                control_predictor_trad + gender_f_Female + urban_rural_f_Rural + work_type_f_Business, data = analysis_dataset)

# --- Model 8(b): Combined Model --- (Across Demographic Factors)
model8b <- lm(responsible_social_contribution ~ academic_performance +
                relation_with_school + child_relation_with_school + school_type + house_environment +
                control_predictor_trad, data = analysis_dataset)

# --- Model 9(a): Combined Model ---
model9a <- lm(responsible_social_contribution ~ academic_performance + alternative_factors +
                control_predictor_trad + gender_f_Female + urban_rural_f_Rural + work_type_f_Business, data = analysis_dataset)

# --- Model 9(b): Combined Model --- (Across Demographic Factors)
model9b <- lm(responsible_social_contribution ~ academic_performance + alternative_factors +
                control_predictor_trad, data = analysis_dataset)

print("=== MODELS CREATED SUCCESSFULLY ===")

# --- Model 10: Final Model of Best Fit ---
best_model = lm(responsible_social_contribution ~ academic_performance + house_environment +
                  control_predictor_trad + gender_f_Female + urban_rural_f_Rural + work_type_f_Business, 
                data = analysis_dataset)

# Extract model summary information
model_summary <- summary(best_model)

# Get actual coefficient names and create labels mapping
coef_names <- rownames(model_summary$coefficients)

# Create a data frame with coefficients and statistics
results_df <- data.frame(
  Variable = rownames(model_summary$coefficients),
  Coefficient = model_summary$coefficients[, "Estimate"],
  Std_Error = model_summary$coefficients[, "Std. Error"],
  t_value = model_summary$coefficients[, "t value"],
  p_value = model_summary$coefficients[, "Pr(>|t|)"],
  Significance = ifelse(model_summary$coefficients[, "Pr(>|t|)"] < 0.001, "***",
                        ifelse(model_summary$coefficients[, "Pr(>|t|)"] < 0.01, "**",
                               ifelse(model_summary$coefficients[, "Pr(>|t|)"] < 0.05, "*", "")))
)

# Add model statistics
model_stats <- data.frame(
  Variable = c("R-squared", "Adj. R-squared", "F-statistic", "Observations"),
  Coefficient = c(model_summary$r.squared, model_summary$adj.r.squared, 
                  model_summary$fstatistic[1], nobs(best_model)),
  Std_Error = NA,
  t_value = NA,
  p_value = NA,
  Significance = ""
)

# Combine results
final_result <- rbind(results_df, model_stats)

#...

print("=== MODEL OF BEST FIT CREATED SUCCESSFULLY ===")

detach(analysis_dataset)

# =============================================================================
# MODEL DIAGNOSTICS
# =============================================================================

# Custom VIF function in base R
calculate_vif <- function(model) {
  predictors <- names(coef(model))[-1] # Exclude intercept
  if (length(predictors) <= 1) return(NULL) # VIF not meaningful for single predictor
  vif_values <- numeric(length(predictors))
  names(vif_values) <- predictors
  for (i in seq_along(predictors)) {
    formula <- as.formula(paste(predictors[i], "~", paste(predictors[-i], collapse = "+")))
    temp_model <- lm(formula, data = model$model)
    r_squared <- summary(temp_model)$r.squared
    vif_values[i] <- 1 / (1 - r_squared)
  }
  return(vif_values)
}

# Function to create and save Q-Q and residuals vs. fitted plots
create_diagnostic_plots <- function(model, model_name) {
  # Residuals vs. Fitted
  resid_plot <- ggplot(data = data.frame(fitted = fitted(model), residuals = residuals(model)), 
                       aes(x = fitted, y = residuals)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = paste("Residuals vs. Fitted:", model_name), x = "Fitted Values", y = "Residuals") +
    theme_minimal()
  
  # Q-Q Plot
  qq_plot <- ggplot(data = data.frame(residuals = residuals(model)), 
                    aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line() +
    labs(title = paste("Q-Q Plot:", model_name), x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
  
  # Save plots
  ggsave(here("output", "figures", paste0("resid_", model_name, ".png")), resid_plot, width = 5, height = 4)
  ggsave(here("output", "figures", paste0("qq_", model_name, ".png")), qq_plot, width = 5, height = 4)
  
  return(list(resid_plot = resid_plot, qq_plot = qq_plot))
}

perform_diagnostics <- function(model, model_name) {
  # Linearity: Ramsey RESET test
  reset_test <- resettest(model)
  linearity_check <- paste(case_when(
    reset_test$p.value >= 0.1 ~ "Linear",
    reset_test$p.value >= 0.05 ~ "Almost Linear",
    reset_test$p.value >= 0.01 ~ "Nearly Linear",
    TRUE ~ "Non-Linear"
  ), as.character(reset_test$p.value))
  
  # Independence: Durbin-Watson test
  dw_test <- dwtest(model)
  dw_result <- paste0("DW = ", round(dw_test$statistic, 2), ", p = ", round(dw_test$p.value, 3))
  
  # Homoscedasticity: Breusch-Pagan test
  bp_test <- bptest(model)
  bp_result <- paste0("BP = ", round(bp_test$statistic, 2), ", p = ", round(bp_test$p.value, 3))
  
  # Normality: Use Anderson-Darling for large datasets, Shapiro-Wilk for smaller ones
  n <- length(residuals(model))
  if (n > 5000) {
    norm_test <- ad.test(residuals(model))
    norm_result <- paste0("AD = ", round(norm_test$statistic, 2), ", p = ", round(norm_test$p.value, 3))
    norm_note <- "Anderson-Darling used (n > 5000)"
  } else if (n >= 3) {
    norm_test <- shapiro.test(residuals(model))
    norm_result <- paste0("W = ", round(norm_test$statistic, 2), ", p = ", round(norm_test$p.value, 3))
    norm_note <- "Shapiro-Wilk used"
  } else {
    norm_result <- "NA"
    norm_note <- "Sample size < 3"
  }
  
  # Multicollinearity: VIF (using custom function)
  vif_result <- "NA"
  vif_values <- calculate_vif(model)
  if (!is.null(vif_values)) {
    if (any(vif_values > 5)) {
      vif_result <- "High"
    } else {
      vif_result <- "Low"
    }
  }
  
  # Outliers: Cook's distance
  cooks_d <- cooks.distance(model)
  outliers_result <- paste0(sum(cooks_d > 1), " obs > 1")
  
  # Exogeneity: Check correlation between residuals and predictors
  exog_result <- "NA"
  predictors <- names(coef(model))[-1]
  if (length(predictors) > 0) {
    correlations <- sapply(predictors, function(pred) {
      cor(residuals(model), model$model[[pred]], use = "complete.obs")
    })
    exog_result <- if (any(abs(correlations) > 0.3)) "Potential Endogeneity" else "No Issues"
  }
  
  # Recommendations
  recommendations <- c()
  if (reset_test$p.value < 0.05) recommendations <- c(recommendations, "Consider non-linear terms or transformations.")
  if (dw_test$p.value < 0.05) recommendations <- c(recommendations, "Consider robust standard errors for autocorrelation.")
  if (bp_test$p.value < 0.05) recommendations <- c(recommendations, "Consider robust standard errors for heteroscedasticity.")
  if (grepl("p = [0.]+[0-4]", norm_result)) recommendations <- c(recommendations, "Residuals not normal. With large N, this is less critical, but see Q-Q plot.")
  if (vif_result == "High") recommendations <- c(recommendations, "High multicollinearity detected. Check predictor correlations.")
  if (sum(cooks_d > 1) > 0) recommendations <- c(recommendations, "Significant outliers detected. Investigate influential points.")
  if (exog_result == "Potential Endogeneity") recommendations <- c(recommendations, "Potential endogeneity detected. Consider instrumental variables or model respecification.")
  
  if (length(recommendations) == 0) recommendations <- "None"
  else recommendations <- paste(recommendations, collapse = " ")
  
  # Create and save diagnostic plots
  create_diagnostic_plots(model, model_name)
  
  # Create a summary data frame
  diagnostics_df <- data.frame(
    Model = model_name,
    Linearity = linearity_check,
    Independence = dw_result,
    Homoscedasticity = bp_result,
    Normality = norm_result,
    Normality_Note = norm_note,
    Multicollinearity = vif_result,
    Outliers = outliers_result,
    Exogeneity = exog_result,
    Recommendations = recommendations
  )
  
  return(diagnostics_df)
}

# List of all models
model_list <- list(
  model1a = model1a, model1b = model1b,
  model2a = model2a, model2b = model2b,
  model3a = model3a, model3b = model3b,
  model4a = model4a, model4b = model4b,
  model5a = model5a, model5b = model5b,
  model6a = model6a, model6b = model6b,
  model7a = model7a, model7b = model7b,
  model8a = model8a, model8b = model8b,
  model9a = model9a, model9b = model9b,
  best_model = best_model
)

# Perform diagnostics for all models
diagnostics_results <- lapply(names(model_list), function(name) {
  perform_diagnostics(model_list[[name]], name)
})

# Combine results into a single table
diagnostics_table <- do.call(rbind, diagnostics_results)

# Print and save the diagnostics table
print("=== MODEL DIAGNOSTICS ===")
print(diagnostics_table)
write_csv(diagnostics_table, here("output", "tables", "regression_diagnostics.csv"))

# =============================================================================
# MODEL SUMMARIES & SAVING
# =============================================================================
# Save model summaries as CSV for each model
for (name in names(model_list)) {
  
  # Extract model summary information
  model_summary <- summary(model_list[[name]])
  
  # Get actual coefficient names and create labels mapping
  coef_names <- rownames(model_summary$coefficients)
  
  # Create a data frame with coefficients and statistics
  results_df <- data.frame(
    Variable = rownames(model_summary$coefficients),
    Coefficient = model_summary$coefficients[, "Estimate"],
    Std_Error = model_summary$coefficients[, "Std. Error"],
    t_value = model_summary$coefficients[, "t value"],
    p_value = model_summary$coefficients[, "Pr(>|t|)"],
    Significance = ifelse(model_summary$coefficients[, "Pr(>|t|)"] < 0.001, "***",
                          ifelse(model_summary$coefficients[, "Pr(>|t|)"] < 0.01, "**",
                                 ifelse(model_summary$coefficients[, "Pr(>|t|)"] < 0.05, "*", "")))
  )
  
  # Add model statistics
  model_stats <- data.frame(
    Variable = c("R-squared", "Adj. R-squared", "F-statistic", "Observations"),
    Coefficient = c(model_summary$r.squared, model_summary$adj.r.squared, 
                    model_summary$fstatistic[1], nobs(model_list[[name]])),
    Std_Error = NA,
    t_value = NA,
    p_value = NA,
    Significance = ""
  )
  
  # Combine results
  final_results <- rbind(results_df, model_stats)
  
  # Save as CSV
  write.csv(final_results, 
            file = here("output", "tables", paste0(name, "_summary.csv")),
            row.names = FALSE)
}

# Print summaries for all models
for (name in names(model_list)) {
  cat("\n")
  cat("====================================================================\n")
  cat("                        MODEL SUMMARY:", name, "\n")
  cat("====================================================================\n")
  print(summary(model_list[[name]]))
}

# Save the R model objects for later use
saveRDS(model_list, file = here("processed-data", "regression-models.rds"))

# Create a proper CSV-formatted table for all models
model_summaries <- bind_rows(
  lapply(names(model_list), function(name) {
    tidy(model_list[[name]], conf.int = TRUE) %>%
      mutate(Model = name)
  }),
  .id = "id"
) %>%
  select(Model, everything(), -id)

# Save as proper CSV
write_csv(model_summaries, here("output", "tables", "regression_models_summary.csv"))

print("=== REGRESSION ANALYSIS COMPLETE ===")
print("Regression models, diagnostics, and summary table have been saved.")
