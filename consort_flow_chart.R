
#Invited
print("Invited (n=4323)")

#Consented
source(here("data_cleaning.R"))
consented <- clean_data()
print(glue::glue("Consented (n={nrow(consented)})"))

#Randomized
print(glue::glue("Randomized (n={nrow(consented)})"))

#Allocated to treatment
print(glue::glue("Allocated to treatment (n={nrow(consented[treatment==1])})"))

#Allocated to control
print(glue::glue("Allocated to control (n={nrow(consented[treatment==0])})"))

#Analysis
source("weigh_data_2.R")
d <- raked_data_survey(trimweights = F)
full <- d[[1]]

print(glue::glue("Analysed primary outcome (treatment) (n={nrow(full[treatment==1])})"))
print(glue::glue("Analysed primary outcome (control) (n={nrow(full[treatment==0])})"))

print(glue::glue("Excluded because of missing data (treatment) (n={nrow(consented[treatment==1])-nrow(full[treatment==1])})"))
print(glue::glue("Excluded because of missing data (control) (n={nrow(consented[treatment==0])-nrow(full[treatment==0])})"))

