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
packages <- c("tidyverse", 
              "data.table",
              "glue",
              "survey",
              "gt",
              "gtsummary",
              "pxweb",
              "PxWebApiData")

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


# Setting up the working directory
user <- "INEL"
setwd(paste0("C:/Users/", user,"/Folkehelseinstituttet/Centre for Epidemic Interventions Research - Dokumenter/Projects/Message Lab/Attitudes towards trials initiated by the government/Sideeksperiment/analyse/analysis_R"))
