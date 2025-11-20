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

subgr_add <- c("Education level", rep("", 5)) %>% 
  rbind(c("No complete education / Primary	", fun_table_1_row_subgroup("edu1", 2))) %>%
  rbind(c("Upper secondary", fun_table_1_row_subgroup("edu1", 3)))%>%
  rbind(c("Vocational college", fun_table_1_row_subgroup("edu1", 4)))%>%
  rbind(c("University or university college", fun_table_1_row_subgroup("edu1", 5)))%>%
  rbind(c("Prefer not to answer or do not know", fun_table_1_row_subgroup("edu1", 6)))%>%
  rbind(c("Voting intention", rep("", 5))) %>% 
  rbind(c("Labor Party (Arbeiderpartiet)", fun_table_1_row_subgroup("party2", 1))) %>%
  rbind(c("Conservative Party (H\u00F8yre) ", fun_table_1_row_subgroup("party2", 2)))%>%
  rbind(c("Progress Party (Fremskrittspartiet)", fun_table_1_row_subgroup("party2", 3)))%>%
  rbind(c("Socialist Left Party (Sosialistisk Venstreparti)", fun_table_1_row_subgroup("party2", 4))) %>%
  rbind(c("Centre Party (Senterpartiet) ", fun_table_1_row_subgroup("party2", 5)))%>%
  rbind(c("Christian Democrats (Kristelig Folkeparti)", fun_table_1_row_subgroup("party2", 6)))%>%
  rbind(c("Liberal Party (Venstre) ", fun_table_1_row_subgroup("party2", 7)))%>%
  rbind(c("Green Party (Milj\u00F8partiet De Gr\u00F8nne)", fun_table_1_row_subgroup("party2", 8))) %>%
  rbind(c("Red Party (R\u00F8dt)", fun_table_1_row_subgroup("party2", 9)))%>%
  rbind(c("Other parties", fun_table_1_row_subgroup("party2", 10))) %>%
  rbind(c("Do not know", fun_table_1_row_subgroup("party2", 11))) %>%
  rbind(c("Heard about the proposal", rep("", 5))) %>% 
  rbind(c("Yes", fun_table_1_row_subgroup("heard_about", 1))) %>%
  rbind(c("No", fun_table_1_row_subgroup("heard_about", 2)))%>%
  rbind(c("Do not know", fun_table_1_row_subgroup("heard_about", 3)))

colnames(subgr_add) <- c("var", "ss", "eff_ss", "unweighted_prop", "weighted_prop", "ci")

tableS1 <-gt(as.data.frame(subgr_add)) %>%
  cols_label(
    var = "",
    ss = "Sample Size",
    eff_ss = "Effective Sample Size",
    unweighted_prop = "Unweighted",
    weighted_prop = "Estimate",
    ci = "95% CI"
  ) %>%
  tab_row_group(group = "Subgroup analyses: Experiment aversion", rows = 1:nrow(as.data.frame(subgr_add))) %>%
  tab_style(style = cell_text(weight = "bold"), locations = list(cells_row_groups(), cells_column_labels(), cells_column_spanners(), cells_body(rows= c(1, 7, 19), columns=1))) %>%
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
    locations = cells_column_labels(columns = c("weighted_prop")))

gtsave(table1, here("results", "tableS1.html"))
