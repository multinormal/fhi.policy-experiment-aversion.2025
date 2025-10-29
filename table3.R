################################################################################
## Project: Policy experiment aversion: Survey and embedded randomized trial 
## conducted during a national debate about a real-life policy 
## experiment on the effectiveness of an in-work tax credit
##
## Script written by Ingeborg Hess Elgersma
##
## This script produces table 2 in http://dx.doi.org/10.2139/ssrn.5408584
################################################################################


source("weigh_data_2.R")
ds <- raked_data_survey() 
ds$one <- 1
ds[, edu3:= case_when(edu1== 2 ~ 1,
                      edu1== 3 ~ 2, 
                      edu1 %in% 4:5 ~3)]
table(ds$edu3, ds$treatment, ds$randomize_worst)
survey_design <- svydesign(
  ids = ~ 1, # No clustering assumed
  data = ds,
  weights = ~ weight_survey_raked
)


################# Primary outcome

model_rr <- svyglm(randomize_worst~treatment, design=survey_design, 
                                family=quasibinomial(link="log")) 
results_rr <- broom::tidy(model_rr, conf.int = TRUE, exponentiate = T)

model_rd <- svyglm(randomize_worst~treatment, design=survey_design, 
                   family=quasibinomial(link="identity")) 
results_rd <- broom::tidy(model_rd, conf.int = TRUE)
p_values <- if(results_rr$p.value[results_rr$term == 'treatment']>=0.001) {
  round(results_rr$p.value[results_rr$term == 'treatment'], 3)
} else{
  "<0.001"
}

#Weighted counts
subset_design <- subset(survey_design, treatment==0)
w <- weights(subset_design)
eff_n <- sum(w)^2 / sum(w^2)
primary_c <- svytable(~randomize_worst, subset_design, Ntotal = eff_n)

subset_design <- subset(survey_design, treatment==1)
w <- weights(subset_design)
eff_n <- sum(w)^2 / sum(w^2)
primary_i <- svytable(~randomize_worst, subset_design, Ntotal = eff_n)

primary_outcome <- tibble(Gr = "Policy experiment aversion",
unweighted_control = glue::glue("{nrow(ds[treatment == 0 & randomize_worst == 1])} ({round(nrow(ds[treatment == 0 & randomize_worst == 1])/nrow(ds[treatment == 0])*100, 1)}%)"),
weighted_control = glue::glue(("{round(primary_c[2],0)} ({round(primary_c[2]/sum(primary_c)*100,1)}%)")),
unweighted_intervention = glue::glue("{nrow(ds[treatment == 1 & randomize_worst == 1])} ({round(nrow(ds[treatment == 1 & randomize_worst == 1])/nrow(ds[treatment == 1])*100, 1)}%)"),
weighted_intervention = glue::glue(("{round(primary_i[2],0)} ({round(primary_i[2]/sum(primary_i)*100,1)}%)")),
rd = glue::glue("{round(results_rd$estimate[results_rd$term == 'treatment'], 2)} [{round(results_rd$conf.low[results_rd$term == 'treatment'], 2)} to {round(results_rd$conf.high[results_rd$term == 'treatment'], 2)}]"), 
rr = glue::glue("{round(results_rr$estimate[results_rr$term == 'treatment'], 2)} [{round(results_rr$conf.low[results_rr$term == 'treatment'], 2)} to {round(results_rr$conf.high[results_rr$term == 'treatment'], 2)}]"), 
p_values = p_values                        
)


################# Secondary outcome

model_rr <- svyglm(just_diff_treatment_2~treatment, design=survey_design, 
                   family=quasibinomial(link="log")) 
results_rr <- broom::tidy(model_rr, conf.int = TRUE, exponentiate = T)
model_rd <- svyglm(just_diff_treatment_2~treatment, design=survey_design, 
                   family=quasibinomial(link="identity")) 
results_rd <- broom::tidy(model_rd, conf.int = TRUE)
p_values <- if(results_rr$p.value[results_rr$term == 'treatment']>=0.001) {
  round(results_rr$p.value[results_rr$term == 'treatment'], 3)
} else{
  "<0.001"
}

#Weighted counts
subset_design <- subset(survey_design, treatment==0)
w <- weights(subset_design)
eff_n <- sum(w)^2 / sum(w^2)
secondary_c <- svytable(~just_diff_treatment_2, subset_design, Ntotal = eff_n)

subset_design <- subset(survey_design, treatment==1)
w <- weights(subset_design)
eff_n <- sum(w)^2 / sum(w^2)
secondary_i <- svytable(~just_diff_treatment_2, subset_design, Ntotal = eff_n)

secondary_outcome <- tibble(Gr = "Differential treatment justifiability",
                          unweighted_control = glue::glue("{nrow(ds[treatment == 0 & just_diff_treatment_2 == 1])} ({round(nrow(ds[treatment == 0 & just_diff_treatment_2 == 1])/nrow(ds[treatment == 0])*100, 1)}%)"),
                          weighted_control = glue::glue(("{round(secondary_c[2],0)} ({round(secondary_c[2]/sum(secondary_c)*100,1)}%)")),
                          unweighted_intervention = glue::glue("{nrow(ds[treatment == 1 & just_diff_treatment_2 == 1])} ({round(nrow(ds[treatment == 1 & just_diff_treatment_2 == 1])/nrow(ds[treatment == 1])*100, 1)}%)"),
                          weighted_intervention = glue::glue(("{round(secondary_i[2],0)} ({round(secondary_i[2]/sum(secondary_i)*100,1)}%)")),
                          rd = glue::glue("{round(results_rd$estimate[results_rd$term == 'treatment'], 2)} [{round(results_rd$conf.low[results_rd$term == 'treatment'], 2)} to {round(results_rd$conf.high[results_rd$term == 'treatment'], 2)}]"), 
                          rr = glue::glue("{round(results_rr$estimate[results_rr$term == 'treatment'], 2)} [{round(results_rr$conf.low[results_rr$term == 'treatment'], 2)} to {round(results_rr$conf.high[results_rr$term == 'treatment'], 2)}]"), 
                          p_values = p_values                        
)


################# Secondary outcome 2

model_rr <- svyglm(randomize_worst_floor ~treatment, design=survey_design, 
                   family=quasibinomial(link="log")) 
results_rr <- broom::tidy(model_rr, conf.int = TRUE, exponentiate = T)

model_rd <- svyglm(randomize_worst_floor ~treatment, design=survey_design, 
                   family=quasibinomial(link="identity")) 
results_rd <- broom::tidy(model_rd, conf.int = TRUE)
p_values <- if(results_rr$p.value[results_rr$term == 'treatment']>=0.001) {
  round(results_rr$p.value[results_rr$term == 'treatment'], 3)
} else{
  "<0.001"
}

#Weighted counts
subset_design <- subset(survey_design, treatment==0)
w <- weights(subset_design)
eff_n <- sum(w)^2 / sum(w^2)
secondary2_c <- svytable(~randomize_worst_floor , subset_design, Ntotal = eff_n)

subset_design <- subset(survey_design, treatment==1)
w <- weights(subset_design)
eff_n <- sum(w)^2 / sum(w^2)
secondary2_i <- svytable(~randomize_worst_floor, subset_design, Ntotal = eff_n)

secondary2_outcome <- tibble(Gr = "Inferred experiment aversion",
                            unweighted_control = glue::glue("{nrow(ds[treatment == 0 & randomize_worst_floor == 1])} ({round(nrow(ds[treatment == 0 & randomize_worst_floor == 1])/nrow(ds[treatment == 0])*100, 1)}%)"),
                            weighted_control = glue::glue(("{round(secondary2_c[2],0)} ({round(secondary2_c[2]/sum(secondary2_c)*100,1)}%)")),
                            unweighted_intervention = glue::glue("{nrow(ds[treatment == 1 & randomize_worst_floor == 1])} ({round(nrow(ds[treatment == 1 & randomize_worst_floor == 1])/nrow(ds[treatment == 1])*100, 1)}%)"),
                            weighted_intervention = glue::glue(("{round(secondary2_i[2],0)} ({round(secondary2_i[2]/sum(secondary2_i)*100,1)}%)")),
                            rd = glue::glue("{round(results_rd$estimate[results_rd$term == 'treatment'], 2)} [{round(results_rd$conf.low[results_rd$term == 'treatment'], 2)} to {round(results_rd$conf.high[results_rd$term == 'treatment'], 2)}]"), 
                            rr = glue::glue("{round(results_rr$estimate[results_rr$term == 'treatment'], 2)} [{round(results_rr$conf.low[results_rr$term == 'treatment'], 2)} to {round(results_rr$conf.high[results_rr$term == 'treatment'], 2)}]"), 
                            p_values = p_values                        
)
#Function to calculate risk difference and risk ratios for subgroups

subgroup_rr_rd <- function(var) {
  subgr <- rd <- rd_low <- rd_high <- rr <- rr_low <- rr_high <- NULL
  values <- sort(unique(ds[[var]]))
  
  for (value in values) {
    subset_design <- subset(survey_design, survey_design$variables[[var]] == value)
    
    # RR model
    model_rr <- svyglm(randomize_worst ~ treatment,
                       design = subset_design,
                       family = quasibinomial(link = "log"))
    results_rr <- broom::tidy(model_rr, conf.int = TRUE, exponentiate = TRUE)
    rr <- c(rr, results_rr$estimate[results_rr$term == "treatment"])
    rr_low <- c(rr_low, results_rr$conf.low[results_rr$term == "treatment"])
    rr_high <- c(rr_high, results_rr$conf.high[results_rr$term == "treatment"])
    
    # RD model
    model_rd <- try(svyglm(randomize_worst ~ treatment,
                           design = subset_design,
                           family = quasibinomial(link = "identity")), silent = TRUE)
    if (!inherits(model_rd, "try-error")) {
      results_rd <- broom::tidy(model_rd, conf.int = TRUE)
      rd <- c(rd, results_rd$estimate[results_rd$term == "treatment"])
      rd_low <- c(rd_low, results_rd$conf.low[results_rd$term == "treatment"])
      rd_high <- c(rd_high, results_rd$conf.high[results_rd$term == "treatment"])
    } else {
      rd <- c(rd, NA); rd_low <- c(rd_low, NA); rd_high <- c(rd_high, NA)
    }
    
    subgr <- c(subgr, value)
  }
  
  return(tibble(subgr, rd, rd_low, rd_high, rr, rr_low, rr_high))
}

#Function to calculate p-value for interactions
all_interactions <- function(var) {
  
  interaction_formula <- as.formula(paste("randomize_worst ~ treatment *", var))
  main_formula <- as.formula(paste("randomize_worst ~ treatment +", var))
                                   
  model_interaction <- svyglm(interaction_formula, design=survey_design, 
                            family=quasibinomial(link="log")) 
  model_main <- svyglm(main_formula, design=survey_design, 
                        family=quasibinomial(link="log")) 
  results_interaction <- anova(model_main, model_interaction)
  if (as.vector(results_interaction$p)>=0.001) {
    printable <- round(as.vector(results_interaction$p), 3)
  } else {
    printable <- "<0.001"
  }
   
  return(printable) 
  }


#Function to calculate weighted and unweighted counts
counts_deff_subgroup <- function(var, gr) {
  values <- sort(unique(survey_design$variables[[var]]))
  
  subgroup <- unweighted_count <- percent_unweighted <- weighted_count <- effective_n <- percent_weighted <- c()
  
  for (value in values) {
    subset_design <- subset(
      survey_design,
      survey_design$variables[[var]] == value &
        survey_design$variables$treatment == gr &
        survey_design$variables$randomize_worst == 1
    )
    
    subset_total <- subset(
      survey_design,
      survey_design$variables[[var]] == value &
        survey_design$variables$treatment == gr
    )
    
    if (nrow(subset_design$variables) == 0 ||
        nrow(subset_total$variables) == 0) {
      subgroup <- c(subgroup, value)
      
      # Unweighted
      dat <- ds[treatment == gr & randomize_worst == 1]
      n <- sum(dat[[var]] == value)
      N <- sum(ds[treatment == gr][[var]] == value)
      perc <- ifelse(N == 0, 0, n / N * 100)
      
      unweighted_count <- c(unweighted_count, n)
      percent_unweighted <- c(percent_unweighted, perc)
      
      # Weighted
      weighted_count <- c(weighted_count, 0)
      effective_n <- c(effective_n, 0)
      percent_weighted <- c(percent_weighted, 0)
      
      next
    }
    
    
    # Unweighted counts
    dat <- ds[treatment == gr & randomize_worst == 1]
    n <- sum(dat[[var]] == value)
    N <- sum(ds[treatment == gr][[var]] == value)
    perc <- n / N * 100
    
    unweighted_count <- c(unweighted_count, n)
    percent_unweighted <- c(percent_unweighted, perc)
    
    # Weighted count
    wt <- tryCatch({
      svytotal( ~ one, design = subset_design)
    }, error = function(e)
      NA)
    weighted_count <- c(weighted_count, wt[1])
    
    # Effective sample size
    w <- weights(subset_design)
    eff_n <- sum(w)^2 / sum(w^2)
    effective_n <- c(effective_n, eff_n)
    
    # Weighted percent
    wt_total <- tryCatch({
      svytotal( ~ one, design = subset_total)
    }, error = function(e)
      NA)
    pct <- tryCatch({
      100 * wt[1] / wt_total[1]
    }, error = function(e)
      NA)
    percent_weighted <- c(percent_weighted, pct)
    
    subgroup <- c(subgroup, value)
  }
  return(
    tibble(
      subgroup ,
      unweighted_count,
      percent_unweighted,
      weighted_count,
      effective_n,
      percent_weighted
    )
  )
}

make_subgroup_tbl <- function(var) {

res_treated <- counts_deff_subgroup(var, 1)
res_untreated <- counts_deff_subgroup(var, 0)
res_regression <- subgroup_rr_rd(var)


unweighted_control <- glue::glue("{res_untreated$unweighted_count} ({round(res_untreated$percent_unweighted,1)}%)")
unweighted_intervention <- glue::glue("{res_treated$unweighted_count} ({round(res_treated$percent_unweighted,1)}%)")
weighted_control <- glue::glue("{round(res_untreated$effective_n ,0)} ({round(res_untreated$percent_weighted,1)}%)")
weighted_intervention <- glue::glue("{round(res_treated$effective_n ,0)} ({round(res_treated$percent_weighted,1)}%)")
rd <- glue::glue("{round(res_regression$rd, 2)} [{round(res_regression$rd_low ,2)} to {round(res_regression$rd_high ,2)}]")
rr <- glue::glue("{round(res_regression$rr, 2)} [{round(res_regression$rr_low ,2)} to {round(res_regression$rr_high ,2)}]")
return(tibble(unweighted_control, weighted_control, unweighted_intervention, weighted_intervention, rd, rr))
}
rownames_table3 <- read_csv("data/rownames_table3.txt")
tbl_subgr <- make_subgroup_tbl("age") %>%
  rbind(make_subgroup_tbl("sex"))%>%
  rbind(make_subgroup_tbl("edu3")) %>%
  rbind(make_subgroup_tbl("party1")) %>%
  rbind(make_subgroup_tbl("hort"))

p_vals <- tibble(p_values = c(all_interactions("age"), rep("", 4), all_interactions("sex"), rep("", 2), all_interactions("edu3"), rep("", 2), all_interactions("party1"), rep("", 4), all_interactions("hort"), rep("", 2)))

tbl3 <- cbind(rownames_table3, tbl_subgr, p_vals)
tbl3 <- primary_outcome %>%
  rbind(secondary_outcome) %>%
  rbind(secondary2_outcome) %>%
  rbind(tibble(Gr = "", unweighted_control = "", weighted_control = "", unweighted_intervention = "",weighted_intervention = "", rd = "", rr = "", p_values = "p-value for interaction")) %>%
  rbind(tbl3)
  
table3 <-tbl3%>%
  gt() %>%
  cols_label(
    Gr = "",
    # Remove "Gr" label
    unweighted_control  = "Unweighted",
    weighted_control  = "Weighted",
    unweighted_intervention   = "Unweighted",
    weighted_intervention  = "Weighted",
    rd = "Risk difference",
    rr = "Risk ratio",
    p_values = "p-value"
  ) %>%
  tab_spanner(label = "Control Participants \n (No explanatory text)",
              columns = c(unweighted_control, weighted_control)) %>%
  tab_spanner(label = "Intervention Participants \n (Explanatory text)",
              columns = c(unweighted_intervention, weighted_intervention)) %>%
  # tab_spanner(label = "Non-swifties",
  #             columns = c(unweighted_control, weighted_control)) %>%
  # tab_spanner(label = "Swifties",
  #             columns = c(unweighted_intervention, weighted_intervention)) %>%
  tab_row_group(group = "Had heard about the proposal", rows = 21:23) %>%
  tab_row_group(group = "Voting intention", rows = 16:20) %>%
  tab_row_group(group = "Education level", rows =13:15) %>%
  tab_row_group(group = "Gender", rows = 10:12) %>%
  tab_row_group(group = "Age group", rows = 5:9) %>%
  tab_row_group(group = "Subgroup analyses for primary outcome", rows = 4) %>%
  tab_row_group(group = "Secondary outcomes", rows = 2:3) %>%
  tab_row_group(group = "Primary outcome", rows = 1) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups()) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_spanners()) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(rows=4, columns=8)) %>%
  cols_align(align = "center",
             columns = c(unweighted_control, weighted_control, unweighted_intervention, weighted_intervention, rr, rd, p_values)) %>%
  cols_align(align = "right", columns = c(Gr))%>%
  tab_footnote(
    footnote = "Unweighted counts and percentages of participants in each arm. These reflect the sample as recruited and are not adjusted to match the population of people residing in Norway",
    locations = cells_column_labels(columns = c("unweighted_control", "unweighted_intervention"))
  )%>%
  tab_footnote(
    footnote = "Weighted percentages, using post-stratification weights to match the sample with the population of people residing in Norway.",
    locations = cells_column_labels(columns = c("weighted_control", "weighted_intervention"))
  )%>%
  tab_footnote(
    footnote = "The risk ratios are re-expressed as adjusted differences, accounting for uncertainty in the risks in the control arm as well as the risk ratios.",
    locations = cells_column_labels(columns = c("rd"))
  )%>%
  tab_footnote(
    footnote = "Population average treatment effects (PATEs) estimated using weighted, unadjusted generalized linear models (binomial family, log link).",
    locations = cells_column_labels(columns = c("rr"))
  )%>%
  tab_footnote(
    footnote = "Two sided tests of the null hypothesis of no treatment effect.",
    locations = cells_column_labels(columns = c("p_values"))
  ) %>%
  tab_footnote(
    footnote = "Two-sided tests of interaction terms in weighted generalized linear models, testing the null hypothesis of equality of treatment effects across subgroup levels. All estimates are presented with two-sided Wald-type 95% confidence intervals",
    locations = cells_body(rows=4, columns=8))
gtsave(table3, "results/table3.html")
