required_packages <- c(
  "dplyr",
  "tidyr",
  "readr",
  "ggplot2",
  "psych",
  "skimr",
  "ggcorrplot",
  "here",
  "tools",
  "rmarkdown",
  "viridis",
  "stargazer",
  "ggthemes",
  "pandoc",
  "tinytex"
)

# Function to check, install, and load packages
install_and_load_packages <- function(packages) {
  for (package_name in packages) {
    if (!require(package_name, character.only = TRUE)) {
      message(paste("Installing package:", package_name))
      install.packages(package_name, dependencies = TRUE)
      # Try loading again after installation
      if (!require(package_name, character.only = TRUE)) {
        stop(paste("Failed to install and load package:", package_name, ". Please install manually."))
      }
    }
  }
}

# Set CRAN mirror
options(repos = c(CRAN = "https://cran.r-project.org"))

# Call the function to install and load all required packages
install_and_load_packages(required_packages)

source(here("R","importing-panel-data.R"))

source(here("R","extracting-panel-data.R"))

source(here("R","structuring-panel-data.R"))

source(here("R","exploratory-analysis.R"))

source(here("R","regression-analysis.R"))

# Knitting the main rmarkdown file

rmd_file <- here("reports", "empirical-study-report.Rmd")
render(input = rmd_file)

#end