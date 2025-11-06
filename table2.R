################################################################################
## Project: Policy experiment aversion: Survey and embedded randomized trial 
## conducted during a national debate about a real-life policy 
## experiment on the effectiveness of an in-work tax credit
##
## Script written by Ingeborg Hess Elgersma
##
## This script produces table 2 in http://dx.doi.org/10.2139/ssrn.5408584
################################################################################

source(here("weigh_data_2.R"))
d <- raked_data_survey()
ds <- d[[1]]
survey_design_intervention <- d[[2]]
survey_design_control <- d[[3]]
# 

#This function summarises unweighted counts and and percentages for each value
summarize_var_unweighted <- function(dt, var) {
  dt[, .(text = glue::glue("{.N} ({round(.N / nrow(dt) * 100, 1)})%")), by = var]
}

#This function does this for each variable and each value
summarize_var_unweighted <- function(dt, var) {
  vals <- c()
  if(var =="sex") {
    for(i in 1:3){
      vals[i] <- glue::glue("{nrow(dt[sex==i])} ({round(nrow(dt[sex==i])/ nrow(dt) * 100, 1)}%)")
    }
  }
  if(var =="age") {
    for(i in 1:5){
      vals[i] <- glue::glue("{nrow(dt[age==i])} ({round(nrow(dt[age==i])/ nrow(dt) * 100, 1)}%)")
    }
  }
  if(var =="edu1") {
    for(i in 2:6){
      vals[i-1] <- glue::glue("{nrow(dt[edu1==i])} ({round(nrow(dt[edu1==i])/ nrow(dt) * 100, 1)}%)")
    }
  }
  if(var =="party1") {
    for(i in 1:5){
      vals[i] <- glue::glue("{nrow(dt[party1==i])} ({round(nrow(dt[party1==i])/ nrow(dt) * 100, 1)}%)")
    }
  }
  if(var =="heard_about") {
    for(i in 1:3){
      vals[i] <- glue::glue("{nrow(dt[heard_about==i])} ({round(nrow(dt[heard_about==i])/ nrow(dt) * 100, 1)}%)")
    }
  }
  return(vals)
}


unweighted_col <- function(data) {
  unweighted_column <- c(
    nrow(data),
    summarize_var_unweighted(data, "age"),
    summarize_var_unweighted(data, "sex"),
    summarize_var_unweighted(data, "edu1"),
    "-",
    summarize_var_unweighted(data, "party1"),
    summarize_var_unweighted(data, "heard_about")
    
  )
  return(unweighted_column)
}

# Columns weighted
weighted_col <- function(survey_design) {
  w <- weights(survey_design)
  eff_n <- sum(w)^2 / sum(w^2)
  
  summarize_n_weighted <- function(var) {
    var_formula <- reformulate(var)
    op <- c()
    tbl <- svytable(
      var_formula,
      design = survey_design
    )
    for (i in 1:length(tbl)) {
      op[i] <- glue::glue(
       "{round(tbl[i] / (nrow(data)) * 100, 1)}%"
      )
      # op[i] <- glue::glue(
      #  "{round(tbl[i],0)} ({round(tbl[i] / (nrow(data) / generaldesigneffect(data$weight_raked)) * 100, 1)}%)"
      # )
    }
    return(op)
  }
  
  
  weighted_column <- c(
    round(eff_n, 0),
    summarize_n_weighted(data, "age"),
    summarize_n_weighted(data, "sex"),
    summarize_n_weighted(data, "edu1"),
    "-",
    summarize_n_weighted(data, "party1"),
    summarize_n_weighted(data, "heard_about")
    
  )
  return(weighted_column)
}

rownames_table2 <- read_csv(here("data", "rownames_table2.txt"))

tbl <- data.table(rownames_table2, 
                  unweighted_col_c =unweighted_col(ds[treatment==0]), 
                  weighted_col_c = weighted_col(survey_design_control), 
                  unweighted_col_t =unweighted_col(ds[treatment==1]), 
                  weighted_col_t = weighted_col(survey_design_intervention)) 
table2 <- tbl %>%
  gt() %>%
  cols_label(
    Gr = "",
    # Remove "Gr" label
    unweighted_col_c = "Unweighted",
    weighted_col_t = "Weighted",
    unweighted_col_t = "Unweighted",
    weighted_col_c = "Weighted"
  ) %>%
  tab_spanner(label = "Control Participants \n (No explanatory text)",
              columns = c(unweighted_col_c, weighted_col_c)) %>%
  tab_spanner(label = "Intervention Participants \n (Explanatory text)",
              columns = c(unweighted_col_t, weighted_col_t)) %>%
  tab_row_group(group = "Had heard about the proposal", rows = 20:22) %>%
  tab_row_group(group = "Voting intention", rows = 15:19) %>%
  tab_row_group(group = "Education level", rows = 10:14) %>%
  tab_row_group(group = "Gender", rows = 7:9) %>%
  tab_row_group(group = "Age group", rows = 2:6) %>%
  tab_row_group(group = "All participants", rows = 1) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups()) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_spanners()) %>%
  fmt_number(
    columns = c(unweighted_col_c, weighted_col_c, unweighted_col_t, weighted_col_t),
    decimals = 1,
    use_seps = FALSE
  ) %>%
  cols_align(align = "center",
             columns = c(unweighted_col_c, weighted_col_c, unweighted_col_t, weighted_col_t)) %>%
  cols_align(align = "right", columns = c(Gr))%>%
  tab_footnote(
    footnote = "Unweighted counts and percentages of participants in each arm. These reflect the sample as recruited and are not adjusted to match the population of people residing in Norway",
    locations = cells_column_labels(columns = c("unweighted_col_t", "unweighted_col_c"))
  )%>%
  tab_footnote(
    footnote = "Weighted percentages, using post-stratification weights to match the sample with the population of people residing in Norway.",
    locations = cells_column_labels(columns = c("weighted_col_t", "weighted_col_c"))
  )
gtsave(table2, here("results", "table2.html"))
