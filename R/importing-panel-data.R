
# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(psych)
library(here) # automatically sets working directory
library(tools) # For file_path_sans_ext()

# ============================================================================
# STEP 1: LOAD AND PREPARE DATA
# ============================================================================

# Define the path to the raw IHDS data
raw_data_dir <- here("data", "ihds-data", "data")
data_files <- list.files(path = raw_data_dir, full.names = TRUE)

data <- list()

for (file_path in data_files) {
  data_name <- tools::file_path_sans_ext(basename(file_path))
  message(paste("  - Importing:", basename(file_path)))
  df <- read.delim(file_path, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
  data[[data_name]] <- df
}

# ============================================================================
# CREATE PANEL DATASET
# ============================================================================

# Create panel dataset using tracking information
panel_data <- data[["panel_individual"]]

# ============================================================================
# STEP 3: SAVE PANEL DATASET
# ============================================================================

saveRDS(panel_data, file = here("processed-data", "raw-panel-data.rds"))
print("Raw panel dataset saved as 'raw-panel-data.rds'")
# end