
produce_targets <- function() {

  
  #Age and sex
  ApiData("http://data.ssb.no/api/v0/en/table/07459",
          returnMetaFrames = TRUE)
  age_sex_ssb <- ApiData(
    "http://data.ssb.no/api/v0/en/table/07459",
    Region = c("0"),
    Kjonn = c(2, 1),
    Tid = "2025",
    Alder = c(18:85)
  )
  age_sex_ssb <- as.data.table(age_sex_ssb[2])[, age := as.numeric(dataset.Alder)][, age_gr :=
                                                                                     cut(
                                                                                       age,
                                                                                       breaks = c(-Inf, 19, 35, 50, 70, Inf),
                                                                                       labels = c("1", "2", "3", "4", "5"),
                                                                                       right = TRUE
                                                                                     )][, !c("age")][, age := as.numeric(age_gr)][, sex := if_else(dataset.Kjonn ==
                                                                                                                                                     2, 1, 2)]
  
  age_target <- age_sex_ssb[, .(n = sum(as.numeric(dataset.value))), by = age][, prop := n /
                                                                                 sum(n)]
  sex_target <- age_sex_ssb[, .(n = sum(as.numeric(dataset.value))), by = sex][, prop := n /
                                                                                 sum(n)]
  
  
  
  # Education
  ApiData("http://data.ssb.no/api/v0/en/table/08921",
          returnMetaFrames = TRUE)
  edu_ssb <- ApiData(
    "http://data.ssb.no/api/v0/en/table/08921",
    UtdanNivaa = c("01", "02a", "11", "03a", "04a", "09a"),
    Region = c("0"),
    Kjonn = "Both sexes",
    Tid = "2024",
    Alder = c("00000"),
    ContentsCode = c("Personer")
  )
  edu_ssb <- as.data.table(edu_ssb[2])[, edu1 := case_when(
    dataset.UtdanNivaa == "09a" ~ 1,
    dataset.UtdanNivaa ==
      "01" ~ 2,
    dataset.UtdanNivaa ==
      "02a" ~ 3,
    dataset.UtdanNivaa ==
      "11" ~ 4,
    dataset.UtdanNivaa %in% c("03a", "04a") ~ 5
  )]
  edu_target <- edu_ssb[, .(n = sum(as.numeric(dataset.value))), by = edu1][, prop := n /
                                                                              sum(n)][order(edu1)]
  
  
  
  
  # Party
  poll_of_polls <- read.csv2(
    "data/poll_of_polls.csv"
  )
  poll_of_polls <- as.data.table(poll_of_polls)[X == "Mai '25"]
  poll_of_polls <- melt(
    poll_of_polls,
    id.vars = "X",
    variable.name = "Party",
    value.name = "Oppslutning_seter"
  )
  
  poll_of_polls[, `:=` (polling_percent = as.numeric(gsub(
    ",", ".", sub(" .*", "", Oppslutning_seter)
  )),
  seats = as.numeric(gsub(
    "[^0-9]", "", sub(".*\\(", "", Oppslutning_seter)
  )))]
  
  #Adjusting for turnout last election
  polls_target <- data.table(party2 = c(1:11), prop = c((poll_of_polls$polling_percent *
                                                           0.772) / 100, 1 - 0.772))
  targets <- list(age_target, sex_target,edu_target, polls_target)
  names(targets) <- c("age_target", "sex_target","edu_target", "polls_target")
  return(targets)
}
