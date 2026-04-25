################################################################################
## Project: Policy experiment aversion: Survey and embedded randomized trial 
## conducted during a national debate about a real-life policy 
## experiment on the effectiveness of an in-work tax credit
##
## Script written by Ingeborg Hess Elgersma
################################################################################

#Installing relevant packages
packages <- list("here"       = "1.0.2", 
                 "tidyverse"  = "2.0.0", 
                 "data.table" = "1.16.4",
                 "glue"       = "1.8.0",
                 "survey"     = "4.4-8",
                 "gt"         = "1.0.0",
                 "gtsummary"  = "2.4.0",
                 "mice"       = "3.18.0",
                 "janitor"    = "2.2.1")

# Function to check, install, and load packages
library(remotes)
install_and_load <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    version <- packages[[pkg]]
    install_version(pkg, version, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

# Apply the function to each package
invisible(lapply(names(packages), install_and_load))
rm(packages)

# Suppress col type messages from read_csv2.
options(readr.show_col_types = FALSE)

# Create results directory if it does not exist
if (!dir.exists(here("results"))) {
  dir.create(here("results"))
}

######################## Main analysis ####################

# `trim` controls weight trimming in table1.R / table3.R. Set FALSE for the
# main analysis; a second pass with trim=TRUE below produces the sensitivity
# analysis (Tables S5/S6).
trim <- FALSE

# Table 1
source(here("table1.R"))

# Table 2
source(here("table2.R"))

# Table 3
source(here("table3.R"))
######################## Sensitivity analysis #################

# Supplementary Table S2 (full education / voting-intention breakdown)
source(here("tableS2.R"))

# Supplementary Table S3 (sample average treatment effect, SATE)
source(here("tableS3.R"))

# Re-run Tables 1 and 3 with trimmed calibration weights (5th/95th percentile)
# to produce Tables S4 and S5.
trim <- TRUE
source(here("table1.R"))
source(here("table3.R"))

#Results found in results folder
