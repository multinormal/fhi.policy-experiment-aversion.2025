################################################################################
## Project: Policy experiment aversion: Survey and embedded randomized trial 
## conducted during a national debate about a real-life policy 
## experiment on the effectiveness of an in-work tax credit
##
## Script written by Ingeborg Hess Elgersma
##
## This script produces table 3 in http://dx.doi.org/10.2139/ssrn.5408584
################################################################################


source(here("weigh_data_2.R"))
d <- raked_data_survey(trimweights=trim)
ds <- d[[1]]
survey_design_intervention <- d[[2]]
survey_design_control <- d[[3]]

ds$one <- 1

survey_design <- svydesign(
  ids = ~ 1, # No clustering assumed
  data = ds,
  weights = ~ weight_survey_raked
)


############### Analysis of primary and secondary outcomes #####################

#Helper functions

# Helper function for formatting percentages
fmt_pct <- function(n, total) {
  glue::glue("{n} ({round(n/total*100, 1)}%)")
}

fmt_ci <- function(est, low, high, decimals = 2) {
  glue::glue("{round(est, decimals)} [{round(low, decimals)} to {round(high, decimals)}]")
}
# Helper function for weighted counts
get_weighted_counts <- function(design, treatment_val, outcome_var) {
  subset_design <- subset(design, treatment == treatment_val)
  w <- weights(subset_design)
  eff_n <- sum(w)^2 / sum(w^2)
  formula <- as.formula(paste0("~", outcome_var))
  svytable(formula, subset_design, Ntotal = eff_n)
}

#Analyse each primary and secondary outcomes
analyze_outcome <- function(outcome_var, outcome_label, survey_design, ds) {
  # Create formula
  formula <- as.formula(paste(outcome_var, "~ treatment"))
  
  # Fit models
  model_rr <- svyglm(formula, 
                     design = survey_design, 
                     family = quasibinomial(link = "log"))
  model_rd <- svyglm(formula, 
                     design = survey_design, 
                     family = quasibinomial(link = "identity"))
  
  # Extract results
  results_rr <- broom::tidy(model_rr, conf.int = TRUE, exponentiate = TRUE)
  results_rd <- broom::tidy(model_rd, conf.int = TRUE)
  
  # Get treatment effects
  rr_treatment <- results_rr[results_rr$term == 'treatment', ]
  rd_treatment <- results_rd[results_rd$term == 'treatment', ]
  
  p_values <- ifelse(rr_treatment$p.value >= 0.001, 
                     round(rr_treatment$p.value, 3), 
                     "<0.001")
  
  # Get weighted counts
  counts_c <- get_weighted_counts(survey_design, 0, outcome_var)
  counts_i <- get_weighted_counts(survey_design, 1, outcome_var)
  
  # Build result table
  tibble(
    Gr = outcome_label,
    unweighted_control = fmt_pct(
      ds[treatment == 0 & get(outcome_var) == 1, .N],
      ds[treatment == 0, .N]
    ),
    weighted_control = fmt_pct(
      round(counts_c[2], 0),
      sum(counts_c)
    ),
    unweighted_intervention = fmt_pct(
      ds[treatment == 1 & get(outcome_var) == 1, .N],
      ds[treatment == 1, .N]
    ),
    weighted_intervention = fmt_pct(
      round(counts_i[2], 0),
      sum(counts_i)
    ),
    rd = fmt_ci(rd_treatment$estimate, rd_treatment$conf.low, rd_treatment$conf.high),
    rr = fmt_ci(rr_treatment$estimate, rr_treatment$conf.low, rr_treatment$conf.high),
    p_values = p_values
  )
}

# Row 1-3 in table 3
primary_outcome <- analyze_outcome(
  outcome_var = "randomize_worst",
  outcome_label = "Policy experiment aversion",
  survey_design = survey_design,
  ds = ds
)

secondary_outcome <- analyze_outcome(
  outcome_var = "just_diff_treatment_2",
  outcome_label = "Differential treatment justifiability",
  survey_design = survey_design,
  ds = ds
)

secondary2_outcome <- analyze_outcome(
  outcome_var = "randomize_worst_floor",
  outcome_label = "Inferred experiment aversion",
  survey_design = survey_design,
  ds = ds
)

########################## Subgroup analysis ###################################

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
# Function to format subgroup table
make_subgroup_tbl <- function(var) {
  res_treated <- counts_deff_subgroup(var, 1)
  res_untreated <- counts_deff_subgroup(var, 0)
  res_regression <- subgroup_rr_rd(var)
  
  
  unweighted_control <- glue::glue(
    "{res_untreated$unweighted_count} ({round(res_untreated$percent_unweighted,1)}%)"
  )
  unweighted_intervention <- glue::glue("{res_treated$unweighted_count} ({round(res_treated$percent_unweighted,1)}%)")
  weighted_control <- glue::glue(
    "{round(res_untreated$effective_n ,0)} ({round(res_untreated$percent_weighted,1)}%)"
  )
  weighted_intervention <- glue::glue("{round(res_treated$effective_n ,0)} ({round(res_treated$percent_weighted,1)}%)")
  rd <- glue::glue(
    "{round(res_regression$rd, 2)} [{round(res_regression$rd_low ,2)} to {round(res_regression$rd_high ,2)}]"
  )
  rr <- glue::glue(
    "{round(res_regression$rr, 2)} [{round(res_regression$rr_low ,2)} to {round(res_regression$rr_high ,2)}]"
  )
  return(
    tibble(
      unweighted_control,
      weighted_control,
      unweighted_intervention,
      weighted_intervention,
      rd,
      rr
    )
  )
}

###################### Merging and formating table 3  ##########################

rownames_table3 <- read_csv(here("data", "rownames_table3.txt"))
tbl_subgr <- make_subgroup_tbl("age") %>%
  rbind(make_subgroup_tbl("sex"))%>%
  rbind(make_subgroup_tbl("edu_level")) %>%
  rbind(make_subgroup_tbl("party1")) %>%
  rbind(make_subgroup_tbl("hort"))

p_vals <- tibble(p_values = c(all_interactions("age"), rep("", 4), all_interactions("sex"), rep("", 2), all_interactions("edu_level"), rep("", 2), all_interactions("party1"), rep("", 4), all_interactions("hort"), rep("", 2)))

subgroup_part <- cbind(rownames_table3, tbl_subgr, p_vals)
tbl3 <- primary_outcome %>%
  rbind(secondary_outcome) %>%
  rbind(secondary2_outcome) %>%
  rbind(tibble(Gr = "", unweighted_control = "", weighted_control = "", unweighted_intervention = "",weighted_intervention = "", rd = "", rr = "", p_values = "p-value for interaction")) %>%
  rbind(subgroup_part)
  
table3 <-tbl3%>%
  gt() %>%
  cols_label(
    Gr = "",
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



if (trim ==F) {
  gtsave(table3, here("results", "table1.html"))
} else {
  gtsave(table3, here("results", "tableS3B.html"))
}


