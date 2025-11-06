################################################################################
## Project: Policy experiment aversion: Survey and embedded randomized trial 
## conducted during a national debate about a real-life policy 
## experiment on the effectiveness of an in-work tax credit
##
## Script written by Ingeborg Hess Elgersma
##
## This script produces table 2 in http://dx.doi.org/10.2139/ssrn.5408584
################################################################################

#Installing relevant packages
packages <- c("here", 
              "tidyverse", 
              "data.table",
              "glue",
              "survey",
              "gt",
              "gtsummary",
              "mice")

# Function to check, install, and load packages
install_and_load <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE)
  }
}

# Apply the function to each package
invisible(lapply(packages, install_and_load))
rm(packages)


######################## Analysis ############################################

# Table 1

source(here("table1.R"))

# Table 2

source(here("table2.R"))


# Table 3

source(here("table3.R"))



#Results found in results folder


