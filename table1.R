################################################################################
## Project: Policy experiment aversion: Survey and embedded randomized trial 
## conducted during a national debate about a real-life policy 
## experiment on the effectiveness of an in-work tax credit
##
## Script written by Ingeborg Hess Elgersma
##
## This script produces table 1 in http://dx.doi.org/10.2139/ssrn.5408584
################################################################################


source(here("weigh_data_2.R"))
d <- raked_data_survey()
ds <- d[[1]]


# In table 1 we report results from the survey analysis. We use the survey design
# object from the control group
control_raked <- d[[3]]
control <- ds[treatment ==0]

# Calculating the sample size
ss <- nrow(control)

# Calculating the effective sample size
w <- weights(control_raked)
eff_ss <- sum(w)^2 / sum(w^2)

# Function to calculate row values in table 1
fun_table_1_row <- function(var) {
  var_formula <- reformulate(var)
  #Getting the unweighted proportion
  unweighted_prop <- round(mean(control[[var]])*100,1)
  #Getting the weighted proportion and with confidence interval
  prop_ci <- svyciprop(var_formula,control_raked, method= "lo")
  weighted_prop <- round(as.vector(prop_ci)*100, 1)
  lower_bound <- round(as.vector(attr(prop_ci, "ci")[1])*100, 1)
  upper_bound <- round(as.vector(attr(prop_ci, "ci")[2])*100, 1)
  ci <- glue("{lower_bound} to {upper_bound}")
  output <- c(as.character(ss), as.character(round(eff_ss,0)), as.character(unweighted_prop), as.character(weighted_prop), ci)
  return(output)
}


#Primary analysis: Policy experiment  aversion
primary <- fun_table_1_row("randomize_worst")

#Sensitivity analysis: Policy experiment aversion
source("sensitivity_analysis_missingparty.R")
sensitivity <- sensitivty_analysis()

#Secondary analysis: Inferred experiment aversion
secondary_inferred <- fun_table_1_row("randomize_worst_floor")

#Secondary analysis: Experiment preference
secondary_exprmnt_pref <- fun_table_1_row("exprmnt_pref")

#Secondary analysis: Favours implementation
secondary_fav_impl <- fun_table_1_row("favours_impl")

#Secondary analysis: Favours non-implementation
secondary_fav_notimpl <- fun_table_1_row("favours_notimpl")

#Secondary analysis: Favours experimentation
secondary_fav_exprmnt <- fun_table_1_row("favours_exprmnt")

#Secondary analysis: Justifiably of differential treatment
secondary_just_diff_trt <- fun_table_1_row("just_diff_treatment_2")

#Combing results in one table
tab_primary_secondary <- rbind(primary, sensitivity, secondary_inferred, secondary_exprmnt_pref, secondary_fav_impl, secondary_fav_notimpl, secondary_fav_exprmnt, secondary_just_diff_trt)
colnames(tab_primary_secondary) <- c("ss", "eff_ss", "unweighted_prop", "weighted_prop", "ci")
tab <- data.frame(var = rownames(tab_primary_secondary), tab_primary_secondary)
tab$var <- "Overall population"


# Function to calculate row values in table 1 for subgroups

fun_table_1_row_subgroup <- function(var, lev) {
  # Unweighted numbers
  data <- control[control[[var]]%in%lev]
  # Calculates N each group
  ss <- nrow(data)
  # Calculates effective N each group

  unweighted_prop <- round(mean(data$randomize_worst)*100,1)
  
  # Weighted counts and percentages
  survey_design_subgroup <- subset(
    control_raked,
    control_raked$variables[[var]] %in% lev)
  
  # Calculating the effective sample size in each group
  w <- weights(survey_design_subgroup) 
  eff_ss <- sum(w)^2 / sum(w^2)
  
  # Calculating the weighted proportions and confidence interval
  prop_ci <- svyciprop(~randomize_worst,survey_design_subgroup, method= "lo")
  weighted_prop <- round(as.vector(prop_ci)*100, 1)
  lower_bound <- round(as.vector(attr(prop_ci, "ci")[1])*100, 1)
  upper_bound <- round(as.vector(attr(prop_ci, "ci")[2])*100, 1)
  ci <- glue("{lower_bound} to {upper_bound}")
  output <- c(as.character(ss), as.character(round(eff_ss,0)), as.character(unweighted_prop), as.character(weighted_prop), ci)
  return(output)
}
subgr_tab <- c("Age group", rep("", 5)) %>%
  rbind(c("<20 years", fun_table_1_row_subgroup("age", 1))) %>%
  rbind(c("20-35 years", fun_table_1_row_subgroup("age", 2)))%>%
  rbind(c("36-50 years", fun_table_1_row_subgroup("age", 3))) %>%
  rbind(c("51-70 years", fun_table_1_row_subgroup("age", 4))) %>%
  rbind(c(">70 years", fun_table_1_row_subgroup("age", 5))) %>%
  rbind(c("Gender", rep("", 5))) %>%
  rbind(c("Female", fun_table_1_row_subgroup("sex", 1))) %>%
  rbind(c("Male", fun_table_1_row_subgroup("sex", 2))) %>%
  rbind(c("Do not know", fun_table_1_row_subgroup("sex", 3))) %>%
  rbind(c("Education level", rep("", 5))) %>% 
  rbind(c("Low", fun_table_1_row_subgroup("edu_level", 1))) %>%
  rbind(c("Middle", fun_table_1_row_subgroup("edu_level", 2)))%>%
  rbind(c("High", fun_table_1_row_subgroup("edu_level", 3)))%>%
  rbind(c("Voting intention", rep("", 5))) %>% 
  rbind(c("Labor Party (Arbeiderpartiet)", fun_table_1_row_subgroup("party1", 1))) %>%
  rbind(c("Conservative Party (H\u00F8yre) ", fun_table_1_row_subgroup("party1", 2)))%>%
  rbind(c("Progress Party (Fremskrittspartiet)", fun_table_1_row_subgroup("party1", 3)))%>%
  rbind(c("Other party", fun_table_1_row_subgroup("party1", 4))) %>%
  rbind(c("Do not know", fun_table_1_row_subgroup("party1", 5))) %>%
  rbind(c("Heard about the proposal", rep("", 5))) %>% 
  rbind(c("Yes", fun_table_1_row_subgroup("heard_about", 1))) %>%
  rbind(c("No / Do not know", fun_table_1_row_subgroup("heard_about", 2:3)))

colnames(subgr_tab) <- c("var", "ss", "eff_ss", "unweighted_prop", "weighted_prop", "ci")
tab2 <- data.frame(subgr_tab)          

tab <- tab %>%
  rbind(tab2)



table1 <-gt(tab) %>%
  cols_label(
    var = "",
    ss = "Sample Size",
    eff_ss = "Effective Sample Size",
    unweighted_prop = "Unweighted",
    weighted_prop = "Estimate",
    ci = "95% CI"
  ) %>%
  tab_row_group(group = "Subgroup analyses: Experiment aversion", rows = 9:30) %>%
  tab_row_group(group = "Secondary analysis: Justifiability of differential treatment", rows = 8) %>%
  tab_row_group(group = "Secondary analysis: Favours experimentation", rows = 7) %>%
  tab_row_group(group = "Secondary analysis: Favours non-implementation", rows = 6) %>%
  tab_row_group(group = "Secondary analysis: Favours implementation", rows = 5) %>%
  tab_row_group(group = "Secondary analysis: Experiment preference", rows = 4) %>%
  tab_row_group(group = "Secondary analysis: Inferred experiment aversion", rows = 3) %>%
  tab_row_group(group = "Sensitivity analysis: Policy experiment aversion", rows = 2) %>%
  tab_row_group(group = "Primary analysis: Policy experiment aversion", rows = 1) %>%
  tab_style(style = cell_text(weight = "bold"), locations = list(cells_row_groups(), cells_column_labels(), cells_column_spanners(), cells_body(rows= c(9, 15, 19, 23, 29), columns=1))) %>%
  cols_align(align = "center",
             columns = c(ss, eff_ss, unweighted_prop, weighted_prop, ci)) %>%
  cols_align(align = "right", columns = c(var)) %>%
  tab_footnote(
    footnote = "Effective sample size reflects design effects from weighting",
    locations = cells_column_labels(columns = c("eff_ss"))
  ) %>%
  tab_footnote(
    footnote = "Unweighted complete-case estimates not matched to the population of people residing in Norway",
    locations = cells_column_labels(columns = c("unweighted_prop")))%>%
  tab_footnote(
    footnote = "Estimates matched to the population of people residing in Norway",
    locations = cells_column_labels(columns = c("weighted_prop")))%>%
  tab_footnote(
    footnote = "Primary and sensitivity analyses differ in treatment of missing/uncertain voting intention responses",
    locations = cells_row_groups(groups=2))%>%
  tab_footnote(
    footnote = "Secondary analysis accounts for potential floor effects.",
    locations = cells_row_groups(groups=3))%>%
  tab_footnote(
    footnote = "The proportion that agrees or strongly agrees with the statement",
    locations = cells_row_groups(groups=4:8))%>%
  tab_footnote(
    footnote = "Complete results for all categories available in supplementary variables",
    locations = cells_body(rows=19, columns=1))%>%
  tab_footnote(
    footnote = "Results presented for the three parties with highest polling support as of June 2025; complete results for all parties will be available in supplementary materials.",
    locations = cells_body(rows=23, columns=1))

gtsave(table1, here("results", "table1.html"))






