
raked_data_survey <- function(trimweights) {
  library(survey)
  library(data.table)
  library(tidyverse)
  source("data_cleaning.R")
  
  # Getting the cleaned dataset
  ds <- clean_data()
  
  # Retieving the population targets
  source("produce_target.R")
  targets_list <- produce_targets()
  
  sex <- targets_list[["sex_target"]]
  age <- targets_list[["age_target"]]
  edu1 <- targets_list[["edu_target"]]
  party2 <- targets_list[["polls_target"]]
  
  #Setting edu1 value 6 to edu_level 4
  ds[, edu_level := if_else(is.na(edu_level), 4, edu_level)]
  
  # Grouping the lowest education categories into one category as there are two
  # Few observations in the lowest education group
  ds[, edu1 := if_else(edu1 == 1, 2, edu1)]
  ds[, sex := if_else(sex == 4, 3, sex)]
  
  ds <- ds[!is.na(sex)][!is.na(edu1)][!is.na(party2)][!is.na(age)][!is.na(heard_about)]

  ds$id <- 1:nrow(ds)
  
  set.seed(1234)
  #Creating weights for each group separately
  
  #Staring with the intervention group
  ds_treated <-ds[treatment==1]
  
  #Extracting the proportion in the sample who answered "I don't know" or 
  # "I don't want to say" to the question about gender
  prop.sex_sample <- nrow(ds_treated[sex == 3]) / nrow(ds_treated)
  
  #Adjusting the population proportions to account for the missing gender category
  freq_and_prop_gender3 <- data.table(sex = 3, prop =prop.sex_sample)
  
  #Getting the frequencies used in raking by multiplying proportion with total in intervention arm
  #freq_and_prop_gender3 <- freq_and_prop_gender3[, Freq := nrow(ds_treated) *prop][, c("sex", "Freq")]
  sex.treated<- rbind(sex[, c("sex", "prop")], freq_and_prop_gender3)
  sex.treated <- sex.treated[, prop := prop / sum(prop)]
  sex.treated <- as.data.frame(sex.treated[, Freq := nrow(ds_treated) *prop][, c("sex", "Freq")])
  
  #Extracting the proportion in the sample who answered "I don't know" or 
  # "I don't want to say" to the question about education
  prop.edu6_sample <- nrow(ds_treated[edu1 == 6]) / nrow(ds_treated)
  freq_and_prop_edu6 <- data.table(edu1 = 6, prop =prop.edu6_sample)
  edu1.treated<- rbind(edu1[, c("edu1", "prop")], freq_and_prop_edu6)
  edu1.treated <- edu1.treated[, edu1 := ifelse(edu1 == 1, 2, edu1)]
  #Adjusting proportions to sum to 1
  edu1.treated <- edu1.treated[, .(n = sum(prop)), by= "edu1"][, prop := n/sum(n)]
  edu1.treated <- edu1.treated[, Freq := nrow(ds_treated) * prop][, c("edu1", "Freq")]
  
  
  #Calculating frequencies to be used in raking based on total in intervention arm
  age.treated <- as.data.frame(age[, Freq:= nrow(ds_treated)*prop][, c("age", "Freq")])
  party2.treated <- as.data.frame(party2[, Freq:= nrow(ds_treated)*prop][, c("party2", "Freq")])
  
  #Setting up the survey design without raked weights
  ds_treated_survey_design_unweighted <- svydesign(
    ids = ~ 1, # No clustering assumed
    data = ds_treated
  )
  
  #Raking
  ds_treated_raked <- rake(design = ds_treated_survey_design_unweighted,
                         sample.margins = list(~sex, ~age, ~edu1, ~party2),
                         population.margins = list(sex.treated, age.treated, edu1.treated, party2.treated))
  #Extracting weights
  ds_treated$weight_survey_raked <- weights(ds_treated_raked)
  
  if(trimweights==T) {
    ds_treated_raked.trim <- trimWeights(ds_treated_raked, 
                                       lower=quantile(ds_treated$weight_survey_raked, c(.05)), 
                                       upper=quantile(ds_treated$weight_survey_raked, c(.95)),
                                       strict=TRUE)
    ds_treated$weight_survey_raked <- weights(ds_treated_raked.trim)
    ds_treated_raked <- ds_treated_raked.trim
  }
  
  #Now repeating the procedure for the control group
  ds_untreated <-ds[treatment==0]
  
  #Extracting the proportion in the sample who answered "I don't know" or 
  # "I don't want to say" to the question about gender
  prop.sex_sample <- nrow(ds_untreated[sex == 3]) / nrow(ds_untreated)
  
  #Adjusting the population proportions to account for the missing gender category
  freq_and_prop_gender3 <- data.table(sex = 3, prop =prop.sex_sample)
  
  #Getting the frequencies used in raking by multiplying proportion with total in control arm

  sex.untreated<- rbind(sex[, c("sex", "prop")], freq_and_prop_gender3)
  sex.untreated <- sex.untreated[, prop := prop / sum(prop)]
  sex.untreated <- as.data.frame(sex.untreated[, Freq := nrow(ds_untreated) *prop][, c("sex", "Freq")])
  
  #Calculating frequencies to be used in raking based on total in the control arm
  age.untreated <- as.data.frame(age[, Freq:= nrow(ds_untreated)*prop][, c("age", "Freq")])
  party2.untreated <- as.data.frame(party2[, Freq:= nrow(ds_untreated)*prop][, c("party2", "Freq")])
  
  
  #Extracting the proportion in the sample who answered "I don't know" or 
  # "I don't want to say" to the question about education
  prop.edu6_sample <- nrow(ds_untreated[edu1 == 6]) / nrow(ds_untreated)
  freq_and_prop_edu6 <- data.table(edu1 = 6, prop =prop.edu6_sample)
  edu1.untreated<- rbind(edu1[, c("edu1", "prop")], freq_and_prop_edu6)
  edu1.untreated <- edu1.untreated[, edu1 := ifelse(edu1 == 1, 2, edu1)]
  #Adjusting proportions to sum to 1
  edu1.untreated <- edu1.untreated[, .(n = sum(prop)), by= "edu1"][, prop := n/sum(n)]
  edu1.untreated <- edu1.untreated[, Freq := nrow(ds_untreated) * prop][, c("edu1", "Freq")]
  
  
  
  
  #Setting up the survey design without raked weights
  ds_untreated_survey_design_unweighted <- svydesign(
    ids = ~ 1, # No clustering assumed
    data = ds_untreated
  )
  
  #Raking
  ds_untreated_raked <- rake(design = ds_untreated_survey_design_unweighted,
                           sample.margins = list(~sex, ~age, ~edu1, ~party2),
                           population.margins = list(sex.untreated, age.untreated, edu1.untreated, party2.untreated))
  #Extracting weights
  ds_untreated$weight_survey_raked <- weights(ds_untreated_raked)
  
  #Option to trim weights
  if(trimweights==T) {
    ds_untreated_raked.trim <- trimWeights(ds_untreated_raked, 
                                         lower=quantile(ds_untreated$weight_survey_raked, c(.05)), 
                                         upper=quantile(ds_untreated$weight_survey_raked, c(.95)),
                                         strict=TRUE)
    ds_untreated$weight_survey_raked <- weights(ds_untreated_raked.trim)
    
    ds_untreated_raked <- ds_untreated_raked.trim
  }
  

  #Combing the two datasets into one dataset
  ds <- rbind(ds_treated, ds_untreated)
  
  to_return <-list(ds, ds_treated_raked, ds_untreated_raked)

  
  return(to_return)
  
}
