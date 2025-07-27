# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(psych)
library(here) # automatically sets working directory

panel_data = readRDS(here("processed-data", "raw-panel-data.rds"))

# Extracting necessary columns
necessary_cols = c(
    
    "HHBASE", # unique id for household
    "HHSPLITID", # unique id for split household
    "IDPERSON", # Person id, unique 12[IHDS2] or 11[IHDS1] byte string
    "PBASE", # multisurvey Person id for each household
    "XRO5", # age in 2005
    "RO5", # age in 2012
    "URBAN", # rural-urban Census 2001 for IHDS-I; 2011 for IHDS-II
    "XRO3", # Sex - revised
    
    "WKANYPLUS", # work participation (farm business w|s animal) 2012
    "XED4", # Educ: Attended school 2005
    
    "WKBUSINESS", # work business 2012
    "WKSALARY", # work wage salary 2012
    "WKFARM", # work farm 2012
    "WKANIMAL", # work animal 2012
    
    # **Dependent Variable: Responsible Social Contribution**
    
    # **Happiness & Well-being**
    
    "TO2Y", # smoke, alcohol, yesno
    "TO3", # Smoke cigarettes [IHDS2 only]
    "TO4", # Smoke bidis or hukkah [IHDS2 only]
    
    # **Health**
    
    "BAZ", # BMI for age zscore from zanthro(US) months>=24
    
    "MB5", # High BP (lifestyle diseases)
    "MB6", # Heart disease (lifestyle diseases)
    "MB7", # Diabetes (lifestyle diseases)
    "MB14", # Mental illness (lifestyle diseases)
    "MB15", # STD or AIDS (diseases for lack of hygiene)
    "MB4", # Tuberculosis (diseases for lack of hygiene)
    "SM8", # Diarrhoea with blood (diseases for lack of hygiene)
    
    "MB24", # Days hosp
    "SM17", # Days hosp
    
    "MB2Y", # yesno disease
    "MB19", # yesno Treatment
    "MB25", # Cost Dr/hosp 
    "MB27", # Cost Medicine
    "MB29", # Med Insurance Rs [IHDS2 only] 
    
    "SM2Y", # yesno sick [IHDS2 only] 
    "SM12", # yesno Treatment 
    "SM18", # Dr/hosp Rs 
    "SM20", # Medicine Rs 
    "SM22", # Med Insurance Rs [IHDS2 only] 
    
    # **Social Responsibility**
    
    "AN6", # Animal care: Frequency 
    "AN5Y", # Animal care work (reverse coded)
    
    # **Productive Contribution**
    
    "RO7", # Primary Activity Status [IHDS2 only] 
    "WKEARNPLUS", # Earnings est 
    "XWKEARN", # sum ag nonag salary farm animal business 2005
    "INCOME", # Annual individual income 2012
    "XINCOME", # HH Annual income Rs2012
    "COTOTAL", # Annual consumption expenditure
    "INCBENEFITS", # all govt benefits Rs 
    "POOR2", # Poverty 2012 Tendulkar cutoffs yesno (reverse coding required)
    "WKDAYS", # work days /year (farm, business, wage|salary) >= 200
    
    # **Independent Variables**
    
    "ED6", # Educ: Completed Years, never,<1=0 
    "ED8", # Educ: secondary class 
    "XTA8B", # Test reading score 
    "XTA9B", # Test math score 
    "XTA10B", # Test Writing level [IHDS1 values] 
    "XCS24Y", # Scholarship 
    "ED13", # Educ: Degree class
    # CONTROL:  (hh income 2005)
    "XWKEARNPLUS", # Earnings est.: sum w|s farm business animal
    "XED6", # Educ: Completed Years
    
    # **Alternative Predictors**
    
    # 1# **Relation with school**
    
    "XTA5", # Test enjoy school 
    "XTA6", # Test teacher not nice 
    "XCS10", # School Hrs/week 
    "XCS11", # Homework Hrs/week
    "XED7", # Educ: Ever repeated 
    # CONSTRAINT: 
    "XTA3", # test attended school
    
    "XCH2", # Child: School enrollment 
    "XCH15", # Child: Average student 
    "XCH16", # Child: School enjoyment 
    "XCH18", # Child: Ever praised 
    "XCH17", # Child: # Repeats
    "XCH19", # Child: Ever beaten 
    "XCH9", # Child: Fair(unfair) Teacher
    "XCH10", # Child: Good Teacher
    "XCH11", # Child: Biased Teacher
    "ED6", # Educ: Completed Years, never,<1=0
    # CONSTRAINT: 
    "XCH1Y", # Child: EdHe questions
    
    # 2# **School type**
    
    "XCS4", # School type
    "XCS25", # School fees
    "XCS12", # Pvt Tuition Hrs/week
    
    # 3# **House environment**
    
    "HHEDUCF", # Highest female adult educ [max=15] 
    "HHEDUCM", # Highest male adult educ [max=15] 
    "HHLITERATE" # Any adult (or head) in hh literate
  )

# ============================================================================
# CONSTRUCT COMPOSITE MEASURES
# ============================================================================

# 1.1 DEPENDENT VARIABLE(y): Responsible Social Contribution
construct_dependent_variable <- function(data) {
  data %>%
    mutate(
      # Happiness & Well-being (z scale)
      happiness_wellbeing = (
        as.vector(scale(1 - TO2Y)) +  # smoke, alcohol, yesno (reverse coded)
        as.vector(scale(5 - TO3)) +  # Smoke cigarettes (reverse coded)
        as.vector(scale(5 - TO4)) # Smoke bidis or hukkah (reverse coded)
      ) / 3,
      
      # Health (z scale)
        
        baz_indicator = coalesce(BAZ, 0),  # BMI normalized
          
        # --- Lifestyle Diseases (Z-Scaled) ---
        lifestyle_diseases = coalesce(as.vector(scale({
          # 1. Convert relevant columns to binary (0/1) using pmin(.x, 1). NA remains NA.
          binary_lifestyle_diseases <- 
            select(., c("MB5", "MB6", "MB7", "MB14")) %>%
            mutate(across(everything(), ~ pmin(.x, 1, na.rm = FALSE)))
            
          # 2. Count the number of diseases by summing binaries (na.rm=TRUE handles NAs in sum)
          num_lifestyle_diseases <- 
            rowSums(binary_lifestyle_diseases, na.rm = TRUE)
            
          all_original_lifestyle_na <-
            rowSums(is.na(
              select(., c("MB5", "MB6", "MB7", "MB14")))) == length(c("MB5", "MB6", "MB7", "MB14"))
          num_lifestyle_diseases[all_original_lifestyle_na] <- 0
            
          # 4. Reverse code the count (Max possible lifestyle diseases = 4)
          length(c("MB5", "MB6", "MB7", "MB14")) - num_lifestyle_diseases
        })), 0),
        
        # --- Hygiene-related Diseases (Z-Scaled) ---
        hygiene_diseases = coalesce(as.vector(scale({
          # 1. Convert relevant columns to binary using pmin(.x, 1). NA remains NA.
          binary_hygiene_diseases <- select(., c("MB15", "MB4", "SM8")) %>%
            mutate(across(everything(), ~ pmin(.x, 1, na.rm = FALSE)))
          
          # 2. Count the number of diseases
          num_hygiene_diseases <- rowSums(binary_hygiene_diseases, na.rm = TRUE)
          
          # 3. Refine NA handling for the count
          all_original_hygiene_na <- 
            rowSums(is.na(
              select(., c("MB15", "MB4", "SM8")))) == length(c("MB15", "MB4", "SM8"))
          num_hygiene_diseases[all_original_hygiene_na] <- NA_real_
          
          # 4. Reverse code the count (Max possible hygiene diseases = 3)
          length(c("MB15", "MB4", "SM8")) - num_hygiene_diseases
        })), 0),
        
        # healthy days
        healthy_days = coalesce(as.vector(scale({
          # 1. Sum the days in hospital. If either MB24 or SM17 is NA, their sum will be NA.
          days_in_hospital <- MB24 + SM17
            
          # 2. Get the maximum observed total days in hospital across the entire column.
          # This serves as our 'Max_Old_Value' for reverse coding.
          max_observed_days = max(days_in_hospital, na.rm = TRUE)
          min_observed_days = min(days_in_hospital, na.rm = TRUE)
            
          # 3. Apply reverse coding: (Max observed days - Current total days)
          # Less days in hospital should mean a higher social contribution score.
          (max_observed_days + min_observed_days) - days_in_hospital
        })), 0),
          
          # --- New: Healthcare Utilization (Z-Scaled) ---
        healthcare_utilization = ({
          mb_utilization_component <- case_when(
            MB2Y == 1 ~ (coalesce(as.vector(scale(MB19)), 0) + 
                         coalesce(as.vector(scale(MB25)), 0) + 
                         coalesce(as.vector(scale(MB27)), 0))/3,
            MB2Y == 0 ~ 0,
            TRUE ~ 0
          )
          
          sm_utilization_component <- case_when(
            SM2Y == 1 ~ (coalesce(as.vector(scale(SM12)), 0) + 
                         coalesce(as.vector(scale(SM18)), 0) + 
                         coalesce(as.vector(scale(SM20)), 0))/3,
            SM2Y == 0 ~ 0,
            TRUE ~ 0
          )
            
          # Combine both components. Sum will be NA if either component is NA.
          (mb_utilization_component + sm_utilization_component)/2
        }),
      
      health = (
        
        baz_indicator * 0.2 +
        lifestyle_diseases * 0.1 +
        hygiene_diseases * 0.1 +
        healthy_days * 0.2 +
        healthcare_utilization * 0.2
          
      ),
      
      # Animal Care (z scale)
      animal_care = {
        
        an6_scaled <- as.vector(scale(.data$AN6))
        an5y_scaled <- as.vector(scale(.data$AN5Y))
        
        base_weight <- 0.15
        additional_weight <- 0.35
        
        effective_weight <- case_when(
          .data$WKANIMAL <= 2 ~ base_weight,
          .data$WKANIMAL > 2 ~ additional_weight,
          TRUE ~ 0
        )
        
        weighted_sum <- (an6_scaled * effective_weight) + (an5y_scaled * effective_weight)
        
        coalesce(weighted_sum, 0)
      },
      
      # Productive Contribution (z scale)
      
      productive_contribution = ({
        productive_contr <- case_when(
            (RO7 <= 10 & WKDAYS >= 100 & XINCOME != 0) ~ (
              coalesce(as.vector(scale(WKEARNPLUS)), 0) + # Work Earnings est 2012
              coalesce(as.vector(scale(INCOME)), 0) + # Annual individual income 2012
              coalesce(as.vector(scale(XINCOME)), 0) + # HH Annual income Rs2012
              coalesce(as.vector(scale({COTOTAL * INCOME/XINCOME})), 0) # Annual individual consumption expenditure
              ) / 4,
            TRUE ~ 0
          )
        productive_contr
        }),
      
      # Overall Responsible Social Contribution (weighted average)
      responsible_social_contribution = (
        happiness_wellbeing * 0.20 +
        health * 0.30 +
        animal_care * 0.20 +
        productive_contribution * 0.30
      )
    )
}

# 4.2 INDEPENDENT VARIABLES(X_0): Traditional Predictors
construct_traditional_predictors <- function(data) {
  data %>%
    mutate(
      
      # Individual components (z scale)
      completed_years = coalesce(as.vector(scale(ED6)), 0),
      secondary_class = coalesce(as.vector(scale({
        secondary_cl = case_when(
            ED8 == "I" ~ 3,
            ED8 == "II" ~ 2,
            ED8 == "III" ~ 1,
            TRUE ~ 0
          )
        secondary_cl
        })), 0),
      reading_score = coalesce(as.vector(scale(XTA8B)), 0),
      math_score = coalesce(as.vector(scale(XTA9B)), 0),
      writing_level = coalesce(as.vector(scale(XTA10B)), 0), 
      scholarship = coalesce(as.vector(scale(XCS24Y)), 0),
      degree_class = coalesce(as.vector(scale({
        degree_cl = case_when(
            ED13 == "I" ~ 3,
            ED13 == "II" ~ 2,
            ED13 == "III" ~ 1,
            TRUE ~ 0
          )
        degree_cl
        })), 0),
      control_predictor_trad = 
        (coalesce(as.vector(scale(XWKEARNPLUS)), 0) + # hh income 2005
         coalesce(as.vector(scale(XED6)), 0) # completed years 2005
         ) / 2,
      
      # Academic Performance Composite (weighted average)
      academic_performance = (
        
        secondary_class * 0.2 +
        degree_class * 0.2 +
        scholarship * 0.2 +
        reading_score * 0.1 +
        writing_level * 0.1 + 
        math_score * 0.1 +
        completed_years * 0.1
        
      )
    )
}

# 4.3 ALTERNATIVE PREDICTORS(X_1): Alternative School-life Factors
construct_alternative_predictors <- function(data) {
  data %>%
    mutate(
      # 1# Relation with school (z scale)
      
      enjoy_school = coalesce(as.vector(scale(XTA5)), 0), # Test enjoy school
      teacher_nice = coalesce(as.vector(scale(4 - XTA6)), 0), # Test teacher not nice (reverse coded)
      school_hrs = coalesce(as.vector(scale(XCS10)), 0), # School Hrs/week
      homework_hrs = coalesce(as.vector(scale(XCS11)), 0), # Homework Hrs/week
      
      relation_with_school = (
        
        enjoy_school * 0.35 + # Test enjoy school
        teacher_nice * 0.35 + # Test teacher not nice (reverse coded)
        school_hrs * 0.20 + # School Hrs/week
        homework_hrs * 0.10 # Homework Hrs/week
        ),
      
      # 2# Child's relation with school (z scale)
      
      ch_teacher = (
        coalesce(as.vector(scale(XCH9)), 0) + # Child: Fair(unfair) Teacher
        coalesce(as.vector(scale(5 - XCH10)), 0) + # Child: Good Teacher (reverse coded)
        coalesce(as.vector(scale(4 - XCH11)), 0) # Child: Biased Teacher (reverse coded)
      ) / 3,
      ch_average_student = coalesce(as.vector(scale(XCH15)), 0),  # Child: Average student
      ch_school_enjoyment = coalesce(as.vector(scale(XCH16)), 0), # Child: School enjoyment
      ch_resilience = (
        coalesce(as.vector(scale(ED6)), 0) + # Educ: Completed Years
        coalesce(as.vector(scale(XCH17)), 0) + # Child: # Repeats
        coalesce(as.vector(scale(XCH19)), 0) # Child: Ever beaten
      ) / 3,
      ch_ever_praised = coalesce(as.vector(scale(XCH18)), 0), # Child: Ever praised
      
      child_relation_with_school = (
        
        ch_teacher +
        ch_average_student +
        ch_school_enjoyment +
        ch_resilience +
        ch_ever_praised
        
      ) / 5,
      
      # 3# School type (z scale)
      
      school_type = coalesce(as.vector(scale(XCS4)), 0), # School type
      school_fees = coalesce(as.vector(scale(XCS25)), 0), # School fees
      
      school_type = (
        school_type + # School type
        school_fees # School fees
      ) / 2,
      
      # 4# House environment (z scale)
      
      highest_education_female = coalesce(as.vector(scale(HHEDUCF)), 0), # Highest female adult educ [max=15]
      highest_education_male = coalesce(as.vector(scale(HHEDUCM)), 0), # Highest male adult educ [max=15]
      any_adult_literate = coalesce(as.vector(scale(HHLITERATE)), 0), # Any adult (or head) in hh literate
      
      house_environment = (
        
        highest_education_female + # Highest female adult educ [max=15]
        highest_education_male + # Highest male adult educ [max=15]
        any_adult_literate # Any adult (or head) in hh literate
        
      ) / 3,

      alternative_factors = (
        
        relation_with_school +
        child_relation_with_school +
        school_type +
        house_environment
        
      ) / 4
      
    )
}

extracted_panel_data <- panel_data %>%
  select(
    all_of(necessary_cols)
  )

# Construct all composite measures
extracted_panel_data <- extracted_panel_data %>%
  construct_dependent_variable() %>%
  construct_traditional_predictors() %>%
  construct_alternative_predictors() 

extracted_panel_data <- extracted_panel_data %>%
  # participation (farm business w|s animal) 2012
  filter(WKANYPLUS != 0) %>%
  # Educ: Attended school 2005
  filter(XED4 == 1)

# save
saveRDS(extracted_panel_data, file = here("processed-data", "extracted-panel-data.rds"))
print("Extracted and cleaned panel dataset saved as 'extracted-panel-data.rds'")
