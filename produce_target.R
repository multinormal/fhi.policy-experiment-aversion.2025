
produce_targets <- function() {

  #The following commented code shows how data on population age and sex 
  #distributions were retrieved from Statistics Norway (SSB). For reproducibility,
  #the data has been stored in a csv file in the data folder.
  
  # #Age and sex
  # ApiData("http://data.ssb.no/api/v0/en/table/07459",
  #         returnMetaFrames = TRUE)
  # age_sex_ssb <- ApiData(
  #   "http://data.ssb.no/api/v0/en/table/07459",
  #   Region = c("0"),
  #   Kjonn = c(2, 1),
  #   Tid = "2025",
  #   Alder = c(18:85)
  # )
  # 
  # age_sex_ssb <- as.data.table(age_sex_ssb[2])
  # write.csv2(age_sex_ssb, "data/age_sex_ssb.csv", row.names = F)
  
  
  age_sex_ssb<- as.data.table(read.csv2(here("data", "age_sex_ssb.csv")))
  
  #Making age (dataset.Alder) a numeric variable
  age_sex_ssb <- age_sex_ssb[, dataset.Alder := as.numeric(dataset.Alder)]
  
  #Cutting age into groups based on grouped age in the survey
  age_sex_ssb <- age_sex_ssb[, age_gr :=
                               cut(
                                 dataset.Alder,
                                 breaks = c(-Inf, 19, 35, 50, 70, Inf),
                                 labels = c("1", "2", "3", "4", "5"),
                                 right = TRUE
                               )]
  
  #Making age variable into a numeric variable - variable class need to match
  #when raking
  age_sex_ssb <- age_sex_ssb[, age := as.numeric(age_gr)]
  
  #Recoding sex (dataset.Kjonn) to match the survey coding of sex
  age_sex_ssb <- age_sex_ssb[, sex := if_else(dataset.Kjonn ==2, 1, 2)]
  
  #Creating the target values (marginal proportions) by summing 
  #population frequency by age group and then calculate the proportion by
  #dividing by the full population
  age_target <- age_sex_ssb[, .(n = sum(as.numeric(dataset.value))), by = age][, prop := n /
                                                                                 sum(n)]
  sex_target <- age_sex_ssb[, .(n = sum(as.numeric(dataset.value))), by = sex][, prop := n /
                                                                                 sum(n)]
  
  
  #The following commented code shows how data on education were 
  #retrieved from Statistics Norway (SSB). For reproducibility,
  #the data has been stored in a csv file in the data folder.

  # edu_ssb <- ApiData(
  #   "http://data.ssb.no/api/v0/en/table/08921",
  #   UtdanNivaa = c("01", "02a", "11", "03a", "04a", "09a"),
  #   Region = c("0"),
  #   Kjonn = "Both sexes",
  #   Tid = "2024",
  #   Alder = c("00000"),
  #   ContentsCode = c("Personer")
  # )
  # write.csv2(edu_ssb[2], "data/edu_ssb.csv", row.names = F)
  
  
  edu_ssb<- as.data.table(read.csv2(here("data", "edu_ssb.csv")))
  
  #Recoding the education level so that it matches that of the survey
  edu_ssb <- edu_ssb[, edu1 := fcase(
    dataset.UtdanNivaa == "09a", 1,
    dataset.UtdanNivaa == "01", 2,
    dataset.UtdanNivaa == "02a", 3,
    dataset.UtdanNivaa == "11", 4,
    dataset.UtdanNivaa %in% c("03a", "04a"), 5
  )]
  
  #Creating the target values (marginal proportions) by summing 
  #population frequency by education group and then calculate the proportion by
  #dividing by the full population
  edu_target <- edu_ssb[, .(n = sum(as.numeric(dataset.value))), by = edu1][, prop := n /
                                                                              sum(n)][order(edu1)]
  
  
  # Party
  poll_of_polls <- read.csv2(
    here("data", "poll_of_polls.csv"))
  
  #Selecting polling data only from month of May 2025
  poll_of_polls <- as.data.table(poll_of_polls)[X == "Mai '25"]
  
  #From wide to long format
  poll_of_polls <- melt(
    poll_of_polls,
    id.vars = "X",
    variable.name = "Party",
    value.name = "Oppslutning_seter"
  )
  
  # Separating seats and percentage data into two variables
  poll_of_polls<- poll_of_polls[, polling_percent:=as.numeric(gsub(
    ",", ".", sub(" .*", "", Oppslutning_seter)
  ))]
  
  poll_of_polls<- poll_of_polls[, seats:=as.numeric(gsub(
    "[^0-9]", "", sub(".*\\(", "", Oppslutning_seter)
  ))]
  
  #Adjusting for turnout last election
  #Turnout was 77.2% last election
  #Adding the polling data for the 10 parties then adding a new row for
  #those abstaining
  polls_target <- data.table(party2 = c(1:11), prop = c((poll_of_polls$polling_percent *
                                                           0.772) / 100, 1 - 0.772))
  
  #Returning the targets as a list
  targets <- list(age_target, sex_target,edu_target, polls_target)
  names(targets) <- c("age_target", "sex_target","edu_target", "polls_target")
  return(targets)
}
