
clean_data <- function() {
  ds1 <- readxl::read_excel(
    "data/data1.xlsx"
  )
  ds1 <- janitor::clean_names(ds1)
  
  ds2 <- readxl::read_excel(
    "data/data2.xlsx")
  
  ds2 <- janitor::clean_names(ds2)
  names(ds1) <- c(
    names(ds1)[1:2],
    "hort",
    "innfore",
    "ikkeinnfore",
    "eksperimentere",
    "paastand",
    "alder",
    "kjonn",
    "utdanning",
    "parti",
    "answe_time_ms"
  )
  names(ds2) <- c(
    names(ds2)[1:2],
    "hort",
    "innfore",
    "ikkeinnfore",
    "eksperimentere",
    "paastand",
    "alder",
    "kjonn",
    "utdanning",
    "parti",
    "answe_time_ms"
  )
  ds1 <- as.data.table(ds1)[, treatment := 1]
  ds2 <- as.data.table(ds2)[, treatment := 0]
  
  
  ds <- as.data.table(rbind(ds1, ds2))[, `:=`(
    agree_randomize = case_when(
      eksperimentere == "Sterkt uenig" ~ 1,
      eksperimentere == "Uenig" ~ 2,
      eksperimentere == "N\u00F8ytral" ~ 3,
      eksperimentere == "Enig" ~ 4,
      eksperimentere == "Sterkt enig" ~ 5
    ),
    agree_imp = case_when(
      innfore == "Sterkt uenig" ~ 1,
      innfore == "Uenig" ~ 2,
      innfore == "N\u00F8ytral" ~ 3,
      innfore == "Enig" ~ 4,
      innfore == "Sterkt enig" ~ 5
    ),
    agree_notimpl = case_when(
      ikkeinnfore == "Sterkt uenig" ~ 1,
      ikkeinnfore == "Uenig" ~ 2,
      ikkeinnfore == "N\u00F8ytral" ~ 3,
      ikkeinnfore == "Enig" ~ 4,
      ikkeinnfore == "Sterkt enig" ~ 5
    ),
    just_diff_treatment = case_when(
      paastand == "Sterkt uenig" ~ 1,
      paastand == "Uenig" ~ 2,
      paastand == "N\u00F8ytral" ~ 3,
      paastand == "Enig" ~ 4,
      paastand == "Sterkt enig" ~ 5
    ),
    age = case_when(
      alder == "Under 20 \u00E5r" ~ 1,
      alder == "20-35 \u00E5r" ~ 2,
      alder == "36-50 \u00E5r" ~ 3,
      alder == "51-70 \u00E5r" ~ 4,
      alder == "Over 70 \u00E5r" ~ 5
    ),
    sex = case_when(
      kjonn == "Kvinne" ~ 1,
      kjonn == "Mann" ~ 2,
      kjonn == "Annet" ~ 3,
      kjonn == "\u00D8nsker ikke \u00E5 svare" ~ 4
    ),
    edu1 = case_when(
      utdanning == "Ingen fullf\u00F8rt utdanning" ~ 1,
      utdanning == "Grunnskole" ~ 2,
      utdanning == "Videreg\u00E5ende skole" ~ 3,
      utdanning == "Fagskole" ~ 4,
      utdanning == "Universitet- eller h\u00F8yskole" ~ 5,
      utdanning == "Jeg vil ikke svare/Jeg vet ikke" ~ 6
    ),
    party1 = case_when(
      parti == "Arbeiderpartiet" ~ 1,
      parti == "H\u00F8yre" ~ 2,
      parti == "Fremskrittspartiet" ~ 3,
      parti %in% c(
        "Andre partier",
        "Kristelig Folkeparti",
        "Milj\u00F8partiet De Gr\u00F8nne",
        "R\u00F8dt",
        "Senterpartiet",
        "Sosialistisk Venstreparti",
        "Venstre"
      ) ~ 4,
      parti == "Jeg vet ikke" ~ 5
    ),
    party2 = case_when(
      parti == "Arbeiderpartiet" ~ 1,
      parti == "H\u00F8yre" ~ 2,
      parti == "Fremskrittspartiet" ~ 3,
      parti == "Sosialistisk Venstreparti" ~ 4,
      parti == "Senterpartiet" ~ 5,
      parti == "Kristelig Folkeparti" ~ 6,
      parti == "Venstre" ~ 7,
      parti == "Milj\u00F8partiet De Gr\u00F8nne" ~ 8,
      parti == "R\u00F8dt" ~ 9,
      parti == "Andre partier" ~ 10,
      parti == "Jeg vet ikke" ~ 11 ## Feil men bare test
    ),
    heard_about = case_when(hort == "Ja" ~ 1, hort == "Nei" ~ 2, hort == "Vet ikke" ~ 3)
  )][, randomize_worst := if_else((agree_randomize < agree_imp) &
                                    (agree_randomize < agree_notimpl),
                                  1,
                                  0)]
  
  ds <- ds[, `:=`(randomize_worst_floor = if_else(agree_randomize == 1, 1, randomize_worst),
                  exprmnt_pref = if_else((agree_randomize > agree_imp) &
                                                   (agree_randomize > agree_notimpl),
                                                 1,
                                                 0),
                  just_diff_treatment_2= if_else(just_diff_treatment >= 4, 1, 0),
                  favours_impl = if_else(agree_imp >= 4, 1, 0),
                  favours_notimpl = if_else(agree_notimpl >= 4, 1, 0),
                  favours_exprmnt = if_else(agree_randomize >= 4, 1, 0),
                  edu_level=case_when(edu1 %in% c(1,2) ~ 1,
                                        edu1 %in% c(3) ~ 2,
                                        edu1 %in% c(4,5) ~ 3))] 
  #ds$edu1 <- factor(ds$edu1, levels = c(1, 2, 3, 4, 5))
  return(ds)
}
