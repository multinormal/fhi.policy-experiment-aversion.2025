################################################################################
## Project: Policy experiment aversion: Survey and embedded randomized trial 
## conducted during a national debate about a real-life policy 
## experiment on the effectiveness of an in-work tax credit
##
## Script written by Ingeborg Hess Elgersma
##
## This script produces the sensitivity analysis in
## table 1 in http://dx.doi.org/10.2139/ssrn.5408584
################################################################################

# Using the mice package
library(mice)

# In this sensitivity analysis we reanalyse the primary outcome, but instead of
# assuming that missing party data reflects non-participation, we impute
# party affiliation based on other non-missing variables

sensitivty_analysis <- function() {
  source(here("data_cleaning.R"))
  
  #Extract the cleaned dataset pre imputation
  ds <- clean_data()
  
  # Party
  print(glue::glue("{round(nrow(ds[party2 == 11]) / nrow(ds)*100, 1)}% of the respondends did not know which party to vote for if the election was tomorrow"))
  
  #Impute first and then compute weights
  ds_copy <- ds[treatment == 0]
  ds_copy[, party2 := if_else(party2 == 11, NA, party2)] #Respondents who do not know what to vote
  ds_copy[, edu1 := if_else(edu1 == 6, NA, edu1)]
  ds_copy[, sex := if_else(sex == 4, 3, sex)]
  ds_copy <- ds_copy[, c("age", "sex", "edu1", "party2", "randomize_worst")]
  
  #Imputing
  imputations <- mice(
    ds_copy,
    m = 50,
    printFlag = F,
    maxit = 20,
    seed = 1234
  )
  
  # Getting the population targets
  source(here("produce_target.R"))
  targets_list <- produce_targets()
  
  #Looping through all imputations
  est <- se <- lower_bound <- upper_bound <- var <- est_unweighted<- eff_ss <- NULL
  
  for (i in 1:imputations$m) {
    #Extract dataset
    imputation <- complete(imputations, i)
    #imputation <- complete(imputations, 1)
    
    #Prepare for raking by extracting population margins
    sex <- targets_list[["sex_target"]]
    age <- targets_list[["age_target"]]
    edu1 <- targets_list[["edu_target"]]
    party2 <- targets_list[["polls_target"]][party2 != 11][, .(prop = prop /
                                                                 sum(prop))][, party2 := .I]
    
    prop.sex_sample <- nrow(data.table(imputation)[sex == 3]) / nrow(imputation)
    sex.pop <- as.data.frame(rbind(sex[, c("sex", "prop")], data.table(sex = 3, prop =
                                                                         prop.sex_sample))[, prop := prop / sum(prop)][, Freq := nrow(imputation) *
                                                                                                                         prop][, c("sex", "Freq")])
    age.pop <- as.data.frame(age[, Freq := nrow(imputation) * prop][, c("age", "Freq")])
    edu1.pop <- edu1[, edu1 := ifelse(edu1 == 1, 2, edu1)][, .(n = sum(n)), by = edu1][, prop := n /sum(n)][, Freq := nrow(imputation) * prop][, c("edu1", "Freq")]
    edu1.pop <- as.data.frame(edu1.pop)
    party2.pop <- as.data.frame(party2[, Freq := nrow(imputation) * prop][, c("party2", "Freq")])
    
    #Create survey object
    nonraked <- svydesign(
      ids = ~ 1,
      data = imputation,
      weights = ~ 1
    )
    
    # Rake
    raked_survey <- rake(
      design = nonraked,
      sample.margins = list( ~ sex, ~ age, ~ edu1, ~
                               party2),
      population.margins = list(sex.pop, age.pop, edu1.pop, party2.pop)
    )
    
    if (trim == T) {
      raked_survey_trim <- trimWeights(raked_survey, 
                                  lower=quantile(weights(raked_survey), c(.05)), 
                                  upper=quantile(weights(raked_survey), c(.95)),
                                  strict=TRUE)
      raked_survey <- raked_survey_trim
    }
    
    #Calculate proportion with CI for one imputation
    prop_ci <- suppressWarnings(
      svyciprop(~ randomize_worst, raked_survey, method = "lo")
    )
    prop_ci_unweighted <- suppressWarnings(
      svyciprop(~ randomize_worst, nonraked, method = "lo")
    )
    
    #Effective sample size
    w <- weights(raked_survey)
    eff_ss_this <- sum(w)^2 / sum(w^2)
    
    #Extract estimates
    est_unweighted <- c(est_unweighted, as.vector(prop_ci_unweighted))
    est <- c(est, as.vector(prop_ci))
    se <- c(se, as.vector(sqrt(attr(prop_ci, "var"))))
    var <- c(var, as.vector(attr(prop_ci, "var")))
    eff_ss <- c(eff_ss, eff_ss_this)
  }
  
  # Pooling the results
  est_pool <- mean(est)
  est_pool_unweighted <- mean(est_unweighted)
  eff_ss_pooled <- mean(eff_ss)
  between_var <- var(est)
  within_var <- mean(se^2)
  se_pool <- sqrt(within_var + between_var + between_var / imputations$m)
  
  # Calculating the confidence interval
  z <- qnorm(1 - (1 - 0.95) / 2)
  ci_lower <- est_pool - z * se_pool
  ci_upper <- est_pool + z * se_pool
  
  
  output <- c(as.character(nrow(ds_copy)), 
              as.character(round(eff_ss_pooled,0)), 
              as.character(round(est_pool_unweighted*100, 1)), 
              as.character(round(est_pool*100, 1)),
              glue::glue("{round(ci_lower*100, 1)} to {round(ci_upper*100, 1)}"))

  return(output)
}
