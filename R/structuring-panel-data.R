# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(psych)
library(here) # automatically sets working directory

extracted_panel_data = readRDS(here("processed-data", "extracted-panel-data.rds"))

# ============================================================================
# PREPARE DATA FOR ANALYSIS
# ============================================================================

# Create analysis-ready dataset with standardized variables
analysis_dataset <- extracted_panel_data %>%
  # STEP 1: Create all new factor variables using mutate()
  mutate(
    gender_f = factor(XRO3, levels = c(1, 2), labels = c("Male", "Female")),
    urban_rural_f = factor(URBAN, levels = c(1, 0), labels = c("Urban", "Rural")),
    business_f = factor(WKBUSINESS, levels = c(0, 1, 2, 3, 4), labels = c("No Business", "No Business", "No Business", "Business", "Business")),
    salary_f = factor(WKSALARY, levels = c(0, 1, 2, 3, 4), labels = c("No Salary", "No Salary", "No Salary", "Salary", "Salary")),
    farm_f = factor(WKFARM, levels = c(0, 1, 2, 3, 4), labels = c("No farm", "No farm", "No farm", "farm", "farm")),
    animal_f = factor(WKANIMAL, levels = c(0, 1, 2, 3, 4), labels = c("No animal", "No animal", "No animal", "animal", "animal")),
    work_type_f = case_when(
      WKSALARY >= 3 ~ "Salary",
      WKBUSINESS >= 3 ~ "Business", 
      WKFARM >= 3 ~ "Farm",
      WKANIMAL >= 3 ~ "Animal"
    ) %>% factor(levels = c("Animal", "Farm", "Business", "Salary"))
  ) %>%
  # STEP 2: Select the final set of desired columns, including the new factors
  select(
    responsible_social_contribution,
    
    academic_performance,
      completed_years,
      secondary_class,
      reading_score,
      math_score,
      writing_level,
      scholarship,
      degree_class,
    
    alternative_factors,
      relation_with_school,
        enjoy_school, # Test enjoy school
        teacher_nice, # Test teacher not nice (reverse coded)
        school_hrs, # School Hrs/week
        homework_hrs, # Homework Hrs/week
      child_relation_with_school,
        ch_teacher,
        ch_average_student,
        ch_school_enjoyment,
        ch_resilience,
        ch_ever_praised,
      school_type,
      house_environment,
    
    control_predictor_trad,
    
    # Now include the new factor variables we just created in mutate()
    gender_f,
    urban_rural_f,
    work_type_f
  ) %>%
  # STEP 3: Filter out rows where ANY of the selected columns have an NA
  filter(!if_any(everything(), is.na))

# Save analysis-ready dataset
saveRDS(analysis_dataset, file = here("processed-data", "structured-dataset.rds"))
print("Analysis-ready dataset saved as 'structured-dataset.rds'")

print("=== DATA PREPARATION COMPLETE ===")
print("Ready for econometric analysis!")
