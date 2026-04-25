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
d <- raked_data_survey(trimweights = FALSE)
ds <- d[[1]]
survey_design_intervention <- d[[2]]
survey_design_control <- d[[3]]


#This function summarises uncalibrated counts and and percentages for each value
summarize_var_uncalibrated <- function(dt, var) {
  vals <- c()
  if (var == "sex") {
    for (i in 1:3) {
      vals[i] <- glue::glue("{nrow(dt[sex==i])} ({round(nrow(dt[sex==i])/ nrow(dt) * 100, 1)}%)")
    }
  }
  if (var == "age") {
    for (i in 1:5) {
      vals[i] <- glue::glue("{nrow(dt[age==i])} ({round(nrow(dt[age==i])/ nrow(dt) * 100, 1)}%)")
    }
  }
  if (var == "edu1") {
    for (i in 2:6) {
      vals[i - 1] <- glue::glue("{nrow(dt[edu1==i])} ({round(nrow(dt[edu1==i])/ nrow(dt) * 100, 1)}%)")
    }
  }
  if (var == "party1") {
    for (i in 1:5) {
      vals[i] <- glue::glue("{nrow(dt[party1==i])} ({round(nrow(dt[party1==i])/ nrow(dt) * 100, 1)}%)")
    }
  }
  if (var == "heard_about") {
    for (i in 1:3) {
      vals[i] <- glue::glue("{nrow(dt[heard_about==i])} ({round(nrow(dt[heard_about==i])/ nrow(dt) * 100, 1)}%)")
    }
  }
  return(vals)
}


uncalibrated_col <- function(data) {
  uncalibrated_column <- c(
    nrow(data),
    summarize_var_uncalibrated(data, "age"),
    summarize_var_uncalibrated(data, "sex"),
    summarize_var_uncalibrated(data, "edu1"),
    summarize_var_uncalibrated(data, "party1"),
    summarize_var_uncalibrated(data, "heard_about")
  )
  return(uncalibrated_column)
}

# Columns calibrated
calibrated_col <- function(survey_design) {
  w <- weights(survey_design)
  eff_n <- sum(w)^2 / sum(w^2)
  
  summarize_n_calibrated <- function(var) {
    var_formula <- reformulate(var)
    op <- c()
    tbl <- svytable(var_formula, design = survey_design)
    for (i in 1:length(tbl)) {
      op[i] <- glue::glue(
        "{round((tbl[i] / (sum(tbl)))*eff_n, 0)} ({round(tbl[i] / (sum(tbl)) * 100, 1)}%)"
      )
    }
    return(op)
  }
  
  calibrated_column <- c(
    round(eff_n, 0),
    summarize_n_calibrated("age"),
    summarize_n_calibrated("sex"),
    summarize_n_calibrated("edu1"),
    summarize_n_calibrated("party1"),
    summarize_n_calibrated("heard_about")
  )
  return(calibrated_column)
}

rownames_table2 <- read_csv(here("data", "rownames_table2.txt"))

tbl <- data.table(
  rownames_table2,
  uncalibrated_col_c = uncalibrated_col(ds[treatment == 0]),
  calibrated_col_c = calibrated_col(survey_design_control),
  uncalibrated_col_t = uncalibrated_col(ds[treatment == 1]),
  calibrated_col_t = calibrated_col(survey_design_intervention)
)

table2 <- tbl %>%
  gt() %>%
  cols_label(
    Gr = "",
    uncalibrated_col_c = "Uncalibrated",
    calibrated_col_c = "Calibrated",
    uncalibrated_col_t = "Uncalibrated",
    calibrated_col_t = "Calibrated"
  ) %>%
  tab_spanner(label = "Control Participants (No explanatory text)",
              columns = c(uncalibrated_col_c, calibrated_col_c)) %>%
  tab_spanner(label = "Intervention Participants (Explanatory text)",
              columns = c(uncalibrated_col_t, calibrated_col_t)) %>%
  tab_row_group(group = "Had heard about the proposal", rows = 20:22) %>%
  tab_row_group(group = "Voting intention", rows = 15:19) %>%
  tab_row_group(group = "Education level", rows = 10:14) %>%
  tab_row_group(group = "Gender", rows = 7:9) %>%
  tab_row_group(group = "Age group", rows = 2:6) %>%
  tab_row_group(group = "All participants", rows = 1) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups()) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_spanners()) %>%
  cols_align(
    align = "center",
    columns = c(uncalibrated_col_c, calibrated_col_c, uncalibrated_col_t, calibrated_col_t)
  ) %>%
  cols_align(align = "right", columns = c(Gr)) %>%
  tab_footnote(
    footnote = paste0(
      "Uncalibrated counts and percentages of participants in each arm. These reflect ",
      "the sample as recruited and are not adjusted to match the population of people ",
      "residing in Norway."
    ),
    locations = cells_column_labels(columns = c("uncalibrated_col_t", "uncalibrated_col_c"))
  ) %>%
  tab_footnote(
    footnote = paste0(
      "Calibrated counts and percentages, using post-stratification weights to match ",
      "the sample with the population of people residing in Norway. Calibrated counts ",
      "reflect design effects from calibration, using Kish's effective sample size ",
      "formula and are rounded to the nearest integer."
    ),
    locations = cells_column_labels(columns = c("calibrated_col_t", "calibrated_col_c"))
  ) %>%
  tab_footnote(
    footnote = paste0(
      "Values in parentheses: general left\u2013right position from the Chapel Hill ",
      "Expert Survey 2024 on a scale from 0 (extreme left) to 10 (extreme right), ",
      "rounded to one decimal."
    ),
    locations = cells_row_groups(groups = "Voting intention")
  )

gtsave(table2, here("results", "table2.html"))
