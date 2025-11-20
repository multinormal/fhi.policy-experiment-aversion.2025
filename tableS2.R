################################################################################
## Project: Policy experiment aversion: Survey and embedded randomized trial 
## conducted during a national debate about a real-life policy 
## experiment on the effectiveness of an in-work tax credit
##
## Script written by Ingeborg Hess Elgersma
##
## This script produces results as in supplementary table S2 for
## calculating the sample average treatment effect (SATE) http://dx.doi.org/10.2139/ssrn.5408584
################################################################################

source(here("weigh_data_2.R"))
d <- raked_data_survey()
ds <- d[[1]]

############### Analysis of primary and secondary outcomes #####################

#Helper functions

# Helper function for formatting percentages
fmt_pct <- function(n, total) {
  glue::glue("{n} ({round(n/total*100, 1)}%)")
}

fmt_ci <- function(est, low, high, decimals = 2) {
  glue::glue("{round(est, decimals)} [{round(low, decimals)} to {round(high, decimals)}]")
}

#Analyse each primary and secondary outcomes
analyze_outcome <- function(outcome_var, outcome_label, data) {
  # Create formula
  formula <- as.formula(paste(outcome_var, "~ treatment"))
  
  # Fit models
  model_rr <- glm(formula, 
                  data = data, 
                  family = binomial(link = "log"))
  model_rd <- glm(formula, 
                  data = data, 
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

  
  # Build result table
  tibble(
    Gr = outcome_label,
    unweighted_control = fmt_pct(
      data[treatment == 0 & get(outcome_var) == 1, .N],
      data[treatment == 0, .N]
    ),
    unweighted_intervention = fmt_pct(
      data[treatment == 1 & get(outcome_var) == 1, .N],
      data[treatment == 1, .N]
    ),
    rd = fmt_ci(rd_treatment$estimate, rd_treatment$conf.low, rd_treatment$conf.high),
    rr = fmt_ci(rr_treatment$estimate, rr_treatment$conf.low, rr_treatment$conf.high),
    p_values = p_values
  )
}

# Row 1-3 in table S2
primary_outcome <- analyze_outcome(
  outcome_var = "randomize_worst",
  outcome_label = "Policy experiment aversion",
  data = ds
)

secondary_outcome <- analyze_outcome(
  outcome_var = "just_diff_treatment_2",
  outcome_label = "Differential treatment justifiability",
  data = ds
)

secondary2_outcome <- analyze_outcome(
  outcome_var = "randomize_worst_floor",
  outcome_label = "Inferred experiment aversion",
  data = ds
)


## Subgroups


#Function to calculate p-value for interactions
all_interactions <- function(var) {
  
  interaction_formula <- as.formula(paste("randomize_worst ~ treatment *", var))
  main_formula <- as.formula(paste("randomize_worst ~ treatment +", var))
  
  model_interaction <- glm(interaction_formula, data=ds, 
                           family=binomial(link="log")) 
  model_main <- glm(main_formula, data=ds, 
                    family=binomial(link="log")) 
  results_interaction <- anova(model_main, model_interaction)
  
  if (as.vector(results_interaction[2,5])<=0.001) {
    printable <- "<0.001"
  } else {
    printable <- round(as.vector(results_interaction[2,5]), 3)
  }
  
  return(printable) 
}


subgroup_rr_rd <- function(var) {
  subgr <- rd <- rd_low <- rd_high <- rr <- rr_low <- rr_high <- NULL
  values <- sort(unique(ds[[var]]))
  
  for (value in values) {
    subset_data <- subset(ds, ds[[var]] == value)
    
    # RR model
    model_rr <- try(glm(randomize_worst ~ treatment,
                    data = subset_data,
                    family = binomial(link = "log")), silent = TRUE)
    results_rr <- try(broom::tidy(model_rr, conf.int = TRUE, exponentiate = TRUE), silent = TRUE)
    
    if (!inherits(model_rr, "try-error")&!inherits(results_rr, "try-error")) {
      #results_rr <- broom::tidy(model_rr, conf.int = TRUE, exponentiate = TRUE)
      rr <- c(rr, results_rr$estimate[results_rr$term == "treatment"])
      rr_low <- c(rr_low, results_rr$conf.low[results_rr$term == "treatment"])
      rr_high <- c(rr_high, results_rr$conf.high[results_rr$term == "treatment"])
    } else {
      rr <- c(rr, NA); rr_low <- c(rr_low, NA); rr_high <- c(rr_high, NA)
    }
    
    # RD model
    model_rd <- try(glm(randomize_worst ~ treatment,
                        data = subset_data,
                        family = binomial(link = "identity")), silent = TRUE)
    if (!inherits(model_rd, "try-error")) {
      results_rd <- try(broom::tidy(model_rd, conf.int = TRUE), silent = TRUE)
      if (!inherits(results_rd, "try-error")) {
        rd <- c(rd, results_rd$estimate[results_rd$term == "treatment"])
        rd_low <- c(rd_low, results_rd$conf.low[results_rd$term == "treatment"])
        rd_high <- c(rd_high, results_rd$conf.high[results_rd$term == "treatment"])
      }
      else {
        p1 <- mean(subset_data[treatment==1, randomize_worst])
        p2 <- mean(subset_data[treatment==0, randomize_worst])
        
        # Calculate risk difference
        risk_difference <- p1 - p2
        
        # Calculate standard error of the risk difference
        se_rd <- sqrt( (p1 * (1 - p1) / nrow(subset_data[treatment==1])) + (p2 * (1 - p2) / nrow(subset_data[treatment==0])) )
        
        # Set Z-score for 95% confidence interval
        z_score <- qnorm(0.975) # For a 95% CI, 0.975 for two-sided
        
        # Calculate confidence interval
        lower_ci <- risk_difference - (z_score * se_rd)
        upper_ci <- risk_difference + (z_score * se_rd)
        
        rd <- c(rd, risk_difference)
        rd_low <- c(rd_low, lower_ci)
        rd_high <- c(rd_high, upper_ci)
      }
      
    } else {
      rd <- c(rd, NA); rd_low <- c(rd_low, NA); rd_high <- c(rd_high, NA)
    }
    
    subgr <- c(subgr, value)
  }
  
  return(tibble(subgr, rd, rd_low, rd_high, rr, rr_low, rr_high))
}


#Function to calculate weighted and unweighted counts
counts_deff_subgroup <- function(var, gr) {
  values <- sort(unique(ds[[var]]))
  
  subgroup <- unweighted_count <- percent_unweighted <- c()
  
  for (value in values) {
    
      # Unweighted
      dat <- ds[treatment == gr & randomize_worst == 1]
      n <- sum(dat[[var]] == value)
      N <- sum(ds[treatment == gr][[var]] == value)
      perc <- ifelse(N == 0, 0, n / N * 100)
      
      unweighted_count <- c(unweighted_count, n)
      percent_unweighted <- c(percent_unweighted, perc)
      
    
    subgroup <- c(subgroup, value)
  }
  return(
    tibble(
      subgroup ,
      unweighted_count,
      percent_unweighted
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
  rd <- glue::glue(
    "{round(res_regression$rd, 2)} [{round(res_regression$rd_low ,2)} to {round(res_regression$rd_high ,2)}]"
  )
  rr <- glue::glue(
    "{round(res_regression$rr, 2)} [{round(res_regression$rr_low ,2)} to {round(res_regression$rr_high ,2)}]"
  )
  return(
    tibble(
      unweighted_control,
      unweighted_intervention,
      rd,
      rr
    )
  )
}


###################### Merging and formating table S2  ##########################

rownames_tableS2 <- read_csv(here("data", "rownames_table3.txt"))
tbl_subgr <- make_subgroup_tbl("age") %>%
  rbind(make_subgroup_tbl("sex"))%>%
  rbind(make_subgroup_tbl("edu_level")) %>%
  rbind(make_subgroup_tbl("party1")) %>%
  rbind(make_subgroup_tbl("hort"))
p_vals <- tibble(p_values = c(all_interactions("age"), rep("", 4), all_interactions("sex"), rep("", 2), all_interactions("edu_level"), rep("", 2), all_interactions("party1"), rep("", 4), all_interactions("hort"), rep("", 2)))

subgroup_part <- cbind(rownames_tableS2, tbl_subgr, p_vals)
tblS2 <- primary_outcome %>%
  rbind(secondary_outcome) %>%
  rbind(secondary2_outcome) %>%
  rbind(tibble(Gr = "", unweighted_control = "", unweighted_intervention = "", rd = "", rr = "", p_values = "p-value for interaction")) %>%
  rbind(subgroup_part)

tableS2 <-tblS2%>%
  gt() %>%
  cols_label(
    Gr = "",
    unweighted_control  = "Control Participants \n (No explanatory text)",
    unweighted_intervention   = "Intervention Participants \n (Explanatory text)",
    rd = "Risk difference",
    rr = "Risk ratio",
    p_values = "p-value"
  ) %>%
  tab_spanner(label = "Sample average treatment effect (SATE)",
              columns = c(rd, rr, p_values)) %>%
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
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(rows=4, columns=6)) %>%
  cols_align(align = "center",
             columns = c(unweighted_control, unweighted_intervention, rr, rd, p_values)) %>%
  cols_align(align = "right", columns = c(Gr))%>%
  tab_footnote(
    footnote = "Two sided tests of the null hypothesis of no treatment effect.",
    locations = cells_column_labels(columns = c("p_values"))
  ) %>%
  tab_footnote(
    footnote = "Two-sided tests of interaction terms in weighted generalized linear models, testing the null hypothesis of equality of treatment effects across subgroup levels. All estimates are presented with two-sided Wald-type 95% confidence intervals",
    locations = cells_body(rows=4, columns=6))


gtsave(table3, here("results", "tableS2.html"))
