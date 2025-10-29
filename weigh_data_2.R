
raked_data_survey <- function(trimweights=F) {
  library(survey)
  library(data.table)
  source("data_cleaning.R")
  
  ds <- clean_data()
  source("produce_target.R")
  targets_list <- produce_targets()
  
  sex <- targets_list[["sex_target"]]
  age <- targets_list[["age_target"]]
  edu1 <- targets_list[["edu_target"]]
  party2 <- targets_list[["polls_target"]]
  
  ds[, edu1 := if_else(edu1 == 6, NA, edu1)]
  ds[, edu1 := if_else(edu1 == 1, 2, edu1)]
  ds[, sex := if_else(sex == 4, 3, sex)]
  ds <- ds[!is.na(sex)][!is.na(edu1)][!is.na(party2)][!is.na(age)][!is.na(hort)]
  
  ds$id <- 1:nrow(ds)
  
  set.seed(1966)
  #Creating weights for each group separately
  ds_treated <-ds[treatment==1]
  
  prop.sex_sample <- nrow(ds_treated[sex == 3]) / nrow(ds_treated)
  sex.treated<- as.data.frame(rbind(sex[, c("sex", "prop")], data.table(sex = 3, prop =
                                                                       prop.sex_sample))[, prop := prop / sum(prop)][, Freq := nrow(ds_treated) *
                                                                                                                       prop][, c("sex", "Freq")])
  age.treated <- as.data.frame(age[, Freq:= nrow(ds_treated)*prop][, c("age", "Freq")])
  edu1.treated<- edu1[, edu1 := ifelse(edu1 == 1, 2, edu1)][, .(n = sum(n)), by = edu1][, prop := n /sum(n)][, Freq := nrow(ds_treated) * prop][, c("edu1", "Freq")]
  
  party2.treated <- as.data.frame(party2[, Freq:= nrow(ds_treated)*prop][, c("party2", "Freq")])
  
  
  ds_treated_survey_design_unweighted <- svydesign(
    ids = ~ 1, # No clustering assumed
    data = ds_treated
  )
  
  ds_treated_raked <- rake(design = ds_treated_survey_design_unweighted,
                         sample.margins = list(~sex, ~age, ~edu1, ~party2),
                         population.margins = list(sex.treated, age.treated, edu1.treated, party2.treated))
  ds_treated$weight_survey_raked <- weights(ds_treated_raked)
  
  if(trimweights==T) {
    ds_treated_raked.trim <- trimWeights(ds_treated_raked, 
                                       lower=quantile(ds_treated$weight_survey_raked, c(.05)), 
                                       upper=quantile(ds_treated$weight_survey_raked, c(.95)),
                                       strict=TRUE)
    ds_treated$weight_survey_raked <- weights(ds_treated_raked.trim)
  }
  

  ds_untreated <-ds[treatment==0]
  prop.sex_sample <- nrow(ds_untreated[sex == 3]) / nrow(ds_untreated)
  sex.untreated<- as.data.frame(rbind(sex[, c("sex", "prop")], data.table(sex = 3, prop =
                                                                          prop.sex_sample))[, prop := prop / sum(prop)][, Freq := nrow(ds_untreated) *
                                                                                                                          prop][, c("sex", "Freq")])
  age.untreated <- as.data.frame(age[, Freq:= nrow(ds_untreated)*prop][, c("age", "Freq")])
  edu1.untreated<- edu1[, edu1 := ifelse(edu1 == 1, 2, edu1)][, .(n = sum(n)), by = edu1][, prop := n /sum(n)][, Freq := nrow(ds_untreated) * prop][, c("edu1", "Freq")]
  
  party2.untreated <- as.data.frame(party2[, Freq:= nrow(ds_untreated)*prop][, c("party2", "Freq")])
  
  
  ds_untreated_survey_design_unweighted <- svydesign(
    ids = ~ 1, # No clustering assumed
    data = ds_untreated
  )
  
  ds_untreated_raked <- rake(design = ds_untreated_survey_design_unweighted,
                           sample.margins = list(~sex, ~age, ~edu1, ~party2),
                           population.margins = list(sex.untreated, age.untreated, edu1.untreated, party2.untreated))
  ds_untreated$weight_survey_raked <- weights(ds_untreated_raked)
  #summary(ds_untreated$weight_survey_raked)
  #quantile(ds_untreated$weight_survey_raked, c(.05, 0.95))
  if(trimweights==T) {
    ds_untreated_raked.trim <- trimWeights(ds_untreated_raked, 
                                         lower=quantile(ds_untreated$weight_survey_raked, c(.05)), 
                                         upper=quantile(ds_untreated$weight_survey_raked, c(.95)),
                                         strict=TRUE)
    ds_untreated$weight_survey_raked <- weights(ds_untreated_raked.trim)
  }
  

  
  ds <- rbind(ds_treated, ds_untreated)

  #generaldesigneffect(ds$weight_raked)
  
  return(ds)
  
}
