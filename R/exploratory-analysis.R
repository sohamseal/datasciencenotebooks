library(dplyr)
library(tidyr)
library(readr)
library(psych)
library(fastDummies)
library(ggplot2)
library(ggcorrplot)
library(here)

# Function to ensure output directories exist
ensure_output_dirs <- function() {
  output_dirs <- c(
    here("output", "figures"),
    here("output", "tables"),
    here("output", "reports")
  )
  
  for (dir in output_dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      message(paste("Created directory:", dir))
    }
  }
}

# Helper function to save plots with standardized naming and formats
save_plot <- function(obj, name, width = 10, height = 8, dpi = 300) {
  # Ensure output directory exists
  ensure_output_dirs()
  
  # Clean the name (remove special characters, replace spaces with underscores)
  clean_name <- gsub("[^A-Za-z0-9_-]", "_", name)
  clean_name <- gsub("_{2,}", "_", clean_name)  # Replace multiple underscores with single
  clean_name <- gsub("^_|_$", "", clean_name)   # Remove leading/trailing underscores
  
  # Define file paths
  png_path <- here("output", "figures", paste0(clean_name, ".png"))
  
  # Save as PNG
  ggsave(filename = png_path, plot = obj, width = width, height = height, 
         dpi = dpi, units = "in", device = "png")
  
  message(paste("Plot saved as:"))
  message(paste("  PNG:", png_path))
  
  return(list(png = png_path))
}

# Helper function to save tables with standardized naming and formats
save_table <- function(df, name, include_rownames = FALSE) {
  # Ensure output directory exists
  ensure_output_dirs()
  
  # Clean the name (remove special characters, replace spaces with underscores)
  clean_name <- gsub("[^A-Za-z0-9_-]", "_", name)
  clean_name <- gsub("_{2,}", "_", clean_name)  # Replace multiple underscores with single
  clean_name <- gsub("^_|_$", "", clean_name)   # Remove leading/trailing underscores
  
  # Define file paths
  csv_path <- here("output", "tables", paste0(clean_name, ".csv"))
  
  # Save as CSV
  write_csv(df, csv_path)
  
  message(paste("Table saved as:"))
  message(paste("  CSV:", csv_path))
  
  return(list(csv = csv_path))
}

# function to trim outliers
trim_outliers_iqr <- function(data, variable_name) {
  Q1 <- quantile(data[[variable_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[variable_name]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val
  
  data %>%
    filter(!!sym(variable_name) >= lower_bound, !!sym(variable_name) <= upper_bound)
}

# Q-Q Plot Functions
qq_plot <- function(data, column_name){
  ggplot(data = data, 
         aes_string(sample = column_name)) +
    stat_qq() +
    stat_qq_line() +
    labs(title = paste("Q-Q Plot:", tools::toTitleCase(gsub("_", " ", column_name))), 
         x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
}

# Load analysis dataset
structured_dataset = readRDS(here("processed-data", "structured-dataset.rds"))

# Create an sample analysis table for the report
sample_analysis_dataset = (
  structured_dataset %>%
    select(
      responsible_social_contribution,
      academic_performance,
      relation_with_school,
      child_relation_with_school,
      school_type,
      house_environment,
      alternative_factors
    )
)

save_table(head(sample_analysis_dataset), "sample_analysis_table")

# Q-Q Plots
qq_plot(sample_analysis_dataset, "responsible_social_contribution")
qq_plot(sample_analysis_dataset, "academic_performance")
qq_plot(sample_analysis_dataset, "relation_with_school")
qq_plot(sample_analysis_dataset, "child_relation_with_school")
qq_plot(sample_analysis_dataset, "school_type")
qq_plot(sample_analysis_dataset, "house_environment")
qq_plot(sample_analysis_dataset, "alternative_factors")

# Taking only the interquartile range of the dataset as Q-Q plots show non normality in tails
analysis_dataset <- structured_dataset %>%
   trim_outliers_iqr("responsible_social_contribution")  %>%
  trim_outliers_iqr("alternative_factors")

# Q-Q Plots
qq_plot(analysis_dataset, "responsible_social_contribution")
qq_plot(analysis_dataset, "academic_performance")
qq_plot(analysis_dataset, "relation_with_school")
qq_plot(analysis_dataset, "child_relation_with_school")
qq_plot(analysis_dataset, "school_type")
qq_plot(analysis_dataset, "house_environment")
qq_plot(analysis_dataset, "alternative_factors")

# Attach dataset to make variables available directly
attach(analysis_dataset)

# =============================================================================
# DESCRIPTIVE STATISTICS
# =============================================================================

# Key composites and controls for analysis
key_variables <- c(
  "responsible_social_contribution", "academic_performance", "alternative_factors",
  "relation_with_school", "child_relation_with_school", "school_type", 
  "house_environment", "control_predictor_trad"
)

# Descriptive statistics using psych::describe()
descriptive_stats <- analysis_dataset %>%
  select(all_of(key_variables)) %>%
  psych::describe() %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable") # Convert row names to a column

# Save descriptive statistics
save_table(descriptive_stats, "descriptive_statistics_psych")

# =============================================================================
# VISUALIZATION OF DESCRIPTIVE STATISTICS
# =============================================================================
# Objective: To provide a visual overview of the central tendency and variability of key variables.

# Calculate means and standard deviations for plotting
summary_data <- analysis_dataset %>%
  select(all_of(key_variables)) %>%
  summarise_all(list(mean = mean, sd = sd), na.rm = TRUE) %>%
  pivot_longer(everything(), names_to = "Statistic", values_to = "Value") %>%
  separate(Statistic, into = c("Variable", "Type"), sep = "_(?=[^_]*$)") %>%
  pivot_wider(names_from = Type, values_from = Value)

# Create the dot plot with error bars
descriptive_plot <- ggplot(summary_data, aes(x = mean, y = reorder(Variable, mean))) +
  geom_point(size = 4, color = "darkblue") +
  geom_errorbarh(aes(xmin = mean - sd, xmax = mean + sd), height = 0.2, color = "gray60") +
  labs(title = "Mean and Standard Deviation of Key Variables",
       x = "Mean (with +/- 1 SD error bars)",
       y = "Variable") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"))

# Save the plot
save_plot(descriptive_plot, "descriptive_statistics_plot")

# =============================================================================
# CORRELATION MATRIX
# =============================================================================

# Select numeric predictors and outcome for correlation matrix
numeric_vars <- analysis_dataset %>%
  select(all_of(key_variables)) %>%
  select_if(is.numeric)

# Calculate correlation matrix
cor_matrix <- cor(numeric_vars, use = "complete.obs")

# Save correlation matrix as table
cor_df <- as.data.frame(cor_matrix)
cor_df$Variable <- rownames(cor_df)
cor_df <- cor_df %>% select(Variable, everything())
save_table(cor_df, "correlation_matrix")

# Create correlation heatmap
cor_plot <- ggcorrplot(cor_matrix, 
                      hc.order = TRUE, 
                      type = "lower",
                      lab = TRUE,
                      lab_size = 3,
                      title = "Correlation Matrix of Key Variables",
                      ggtheme = ggplot2::theme_minimal())

# Save correlation plot
save_plot(cor_plot, "correlation_heatmap")

# =============================================================================
# DISTRIBUTION PLOTS
# =============================================================================

# Get numeric variables for distribution plots
numeric_composites <- c("responsible_social_contribution", "academic_performance", 
                       "alternative_factors", "relation_with_school", 
                       "child_relation_with_school", "school_type", 
                       "house_environment", "control_predictor_trad")

# Filter to only include variables that exist and are numeric
available_numeric <- numeric_composites[numeric_composites %in% names(analysis_dataset)]
available_numeric <- available_numeric[sapply(analysis_dataset[available_numeric], is.numeric)]

# 1. Histograms + density for each composite
for (var in available_numeric) {
  hist_plot <- ggplot(analysis_dataset, aes_string(x = var)) +
    geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.7, fill = "skyblue", color = "black") +
    geom_density(color = "red", size = 1) +
    labs(title = paste("Distribution of", tools::toTitleCase(gsub("_", " ", var))),
         x = tools::toTitleCase(gsub("_", " ", var)),
         y = "Density") +
    theme_minimal()
  
  save_plot(hist_plot, paste0("histogram_", var))
}

# 2. Boxplots
for (var in available_numeric) {
  # Boxplot by gender
  box_gender <- ggplot(analysis_dataset, aes_string(x = "gender_f", y = var, fill = "gender_f")) +
    geom_boxplot(alpha = 0.7) +
    labs(title = paste("Distribution of", tools::toTitleCase(gsub("_", " ", var)), "by Gender"),
         x = "Gender",
         y = tools::toTitleCase(gsub("_", " ", var))) +
    theme_minimal() +
    theme(legend.position = "none")
  
  save_plot(box_gender, paste0("boxplot_", var, "_by_gender"))
  
  # Boxplot by urban/rural
  box_urban <- ggplot(analysis_dataset, aes_string(x = "urban_rural_f", y = var, fill = "urban_rural_f")) +
    geom_boxplot(alpha = 0.7) +
    labs(title = paste("Distribution of", tools::toTitleCase(gsub("_", " ", var)), "by Location"),
         x = "Location",
         y = tools::toTitleCase(gsub("_", " ", var))) +
    theme_minimal() +
    theme(legend.position = "none")
  
  save_plot(box_urban, paste0("boxplot_", var, "_by_location"))
  
  # Boxplot by work type
  box_urban <- ggplot(analysis_dataset, aes_string(x = "work_type_f", y = var, fill = "work_type_f")) +
    geom_boxplot(alpha = 0.7) +
    labs(title = paste("Distribution of", tools::toTitleCase(gsub("_", " ", var)), "by Work Type"),
         x = "Work Type",
         y = tools::toTitleCase(gsub("_", " ", var))) +
    theme_minimal() +
    theme(legend.position = "none")
  
  save_plot(box_urban, paste0("boxplot_", var, "_by_work_type"))
}

# 3. Scatter plots of each predictor vs. outcome with simple linear trend
outcome_var <- "responsible_social_contribution"
predictor_vars <- available_numeric[available_numeric != outcome_var]

for (var in predictor_vars) {
  scatter_plot <- ggplot(analysis_dataset, aes_string(x = var, y = outcome_var)) +
    geom_point(alpha = 0.6, color = "steelblue") +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    labs(title = paste("Relationship between", tools::toTitleCase(gsub("_", " ", var)), 
                      "and", tools::toTitleCase(gsub("_", " ", outcome_var))),
         x = tools::toTitleCase(gsub("_", " ", var)),
         y = tools::toTitleCase(gsub("_", " ", outcome_var))) +
    theme_minimal()
  
  save_plot(scatter_plot, paste0("scatter_", var, "_vs_", outcome_var))
}

# =============================================================================
# EXTENDED BIVARIATE ANALYSIS: HISTOGRAMS BY CATEGORICAL FACTORS
# =============================================================================
# Objective: To visualize the distribution of key continuous variables across different categorical groups.

# Define the continuous variables to plot
continuous_vars_for_hist <- c(
  "responsible_social_contribution", 
  "academic_performance", 
  "relation_with_school", 
  "child_relation_with_school", 
  "school_type", 
  "house_environment"
)

# Define the categorical variables
categorical_vars_for_hist <- c(
  "gender_f", 
  "urban_rural_f", 
  "work_type_f"
)

for (cont_var in continuous_vars_for_hist) {
  for (cat_var in categorical_vars_for_hist) {
    # Create histogram with density, filled by categorical variable
    hist_plot_faceted <- ggplot(analysis_dataset, aes_string(x = cont_var, fill = cat_var)) +
      geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.7, position = "identity", color = "black") +
      geom_density(alpha = 0.2) +
      labs(title = paste("Distribution of", tools::toTitleCase(gsub("_", " ", cont_var)), "by", tools::toTitleCase(gsub("_", " ", cat_var))),
           x = tools::toTitleCase(gsub("_", " ", cont_var)),
           y = "Density") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    save_plot(hist_plot_faceted, paste0("histogram_", cont_var, "_by_", cat_var))
  }
}

# =============================================================================
# GROUP SUMMARIES
# =============================================================================

# Group summaries by gender and urban_rural
group_summaries <- analysis_dataset %>%
  group_by(work_type_f) %>%
  summarise(
    across(all_of(available_numeric), 
           list(mean = ~round(mean(.x, na.rm = TRUE), 3),
                sd = ~round(sd(.x, na.rm = TRUE), 3)),
           .names = "{.col}_{.fn}"),
    n = n(),
    .groups = "drop"
  )

# Save group summaries
save_table(group_summaries, "group_summaries_by_work_type")

# Additional summary by gender only
gender_summaries <- analysis_dataset %>%
  group_by(gender_f) %>%
  summarise(
    across(all_of(available_numeric), 
           list(mean = ~round(mean(.x, na.rm = TRUE), 3),
                sd = ~round(sd(.x, na.rm = TRUE), 3)),
           .names = "{.col}_{.fn}"),
    n = n(),
    .groups = "drop"
  )

save_table(gender_summaries, "group_summaries_by_gender")

# Additional summary by location only
location_summaries <- analysis_dataset %>%
  group_by(urban_rural_f) %>%
  summarise(
    across(all_of(available_numeric), 
           list(mean = ~round(mean(.x, na.rm = TRUE), 3),
                sd = ~round(sd(.x, na.rm = TRUE), 3)),
           .names = "{.col}_{.fn}"),
    n = n(),
    .groups = "drop"
  )

save_table(location_summaries, "group_summaries_by_location")

# Detach dataset to avoid masking issues
detach(analysis_dataset)

print("=== EXPLORATORY ANALYSIS COMPLETE ===")
print("All tables and figures saved to /output/ directory")

# Save analysis-ready dataset
saveRDS(analysis_dataset, file = here("processed-data", "analysis_ready_dataset.rds"))
print("Analysis-ready dataset saved as 'analysis-ready-dataset.rds'")

# end