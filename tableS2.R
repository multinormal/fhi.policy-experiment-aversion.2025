################################################################################
## Project: Policy experiment aversion: Survey and embedded randomized trial 
## conducted during a national debate about a real-life policy 
## experiment on the effectiveness of an in-work tax credit
##
## Script written by Ingeborg Hess Elgersma
##
## This script produces results as in table 1 for
## all categories as specified in http://dx.doi.org/10.2139/ssrn.5408584
################################################################################

#Retrieves the function to calculate row values in table 1 for subgroups
source(here("table1.R")) 

subgr_add <- c("Education level", rep("", 4)) %>%
  rbind(c("No complete education / Primary", fun_table_1_row_subgroup("edu1", 2))) %>%
  rbind(c("Upper secondary", fun_table_1_row_subgroup("edu1", 3))) %>%
  rbind(c("Vocational college", fun_table_1_row_subgroup("edu1", 4))) %>%
  rbind(c("University or university college", fun_table_1_row_subgroup("edu1", 5))) %>%
  rbind(c("Prefer not to answer or do not know", fun_table_1_row_subgroup("edu1", 6))) %>%
  rbind(c("Voting intention", rep("", 4))) %>%
  # Parties listed left-to-right by Chapel Hill Expert Survey 2024 score, matching manuscript Table S2
  rbind(c("R\u00F8dt (0.6)",                              fun_table_1_row_subgroup("party2", 9))) %>%
  rbind(c("Sosialistisk Venstreparti (1.8)",             fun_table_1_row_subgroup("party2", 4))) %>%
  rbind(c("Arbeiderpartiet (3.7)",                       fun_table_1_row_subgroup("party2", 1))) %>%
  rbind(c("Milj\u00F8partiet de Gr\u00F8nne (3.8)",      fun_table_1_row_subgroup("party2", 8))) %>%
  rbind(c("Senterpartiet (5.1)",                         fun_table_1_row_subgroup("party2", 5))) %>%
  rbind(c("Venstre (5.8)",                               fun_table_1_row_subgroup("party2", 7))) %>%
  rbind(c("Kristelig Folkeparti (5.8)",                  fun_table_1_row_subgroup("party2", 6))) %>%
  rbind(c("H\u00F8yre (6.6)",                            fun_table_1_row_subgroup("party2", 2))) %>%
  rbind(c("Fremskrittspartiet (7.8)",                    fun_table_1_row_subgroup("party2", 3))) %>%
  rbind(c("Other parties",                               fun_table_1_row_subgroup("party2", 10))) %>%
  rbind(c("Do not know",                                 fun_table_1_row_subgroup("party2", 11))) %>%
  rbind(c("Heard about the proposal", rep("", 4))) %>%
  rbind(c("Yes",         fun_table_1_row_subgroup("heard_about", 1))) %>%
  rbind(c("No",          fun_table_1_row_subgroup("heard_about", 2))) %>%
  rbind(c("Do not know", fun_table_1_row_subgroup("heard_about", 3)))

colnames(subgr_add) <- c("var", "ss", "eff_ss", "uncalibrated_prop", "calibrated_est_ci")

tableS2 <- gt(as.data.frame(subgr_add)) %>%
  cols_label(
    var = "",
    ss = "Sample Size",
    eff_ss = "Effective Sample Size",
    uncalibrated_prop = "Uncalibrated Estimate (%)",
    calibrated_est_ci = "Calibrated Estimate (%, 95% CI)"
  ) %>%
  tab_row_group(group = "Subgroup analyses: Experiment aversion", rows = 1:nrow(as.data.frame(subgr_add))) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_row_groups(),
      cells_column_labels(),
      cells_column_spanners(),
      # Subgroup header rows: 1 = Education level, 7 = Voting intention, 19 = Heard about
      cells_body(rows = c(1, 7, 19), columns = 1)
    )
  ) %>%
  cols_align(align = "center",
             columns = c(ss, eff_ss, uncalibrated_prop, calibrated_est_ci)) %>%
  cols_align(align = "right", columns = c(var)) %>%
  tab_footnote(
    footnote = "Effective sample size reflects design effects from calibration, using Kish's effective sample size formula.",
    locations = cells_column_labels(columns = c("eff_ss"))
  ) %>%
  tab_footnote(
    footnote = "Uncalibrated complete case estimates not matched to the population of people residing in Norway.",
    locations = cells_column_labels(columns = c("uncalibrated_prop"))
  ) %>%
  tab_footnote(
    footnote = "Calibrated estimates matched to the population of people residing in Norway.",
    locations = cells_column_labels(columns = c("calibrated_est_ci"))
  ) %>%
  tab_footnote(
    footnote = paste0(
      "Values in parentheses: general left\u2013right position from the Chapel Hill ",
      "Expert Survey 2024 on a scale from 0 (extreme left) to 10 (extreme right), ",
      "rounded to one decimal. Parties are listed from left to right."
    ),
    locations = cells_body(rows = 7, columns = 1)
  )

gtsave(tableS2, here("results", "tableS2.html"))
