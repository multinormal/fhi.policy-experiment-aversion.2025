
clean_data <- function() {
  ds1 <- readxl::read_excel(
    here("data", "data1.xlsx")
  )
  ds1 <- janitor::clean_names(ds1)
  
  ds2 <- readxl::read_excel(
    here("data", "data2.xlsx"))
  
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
  
  #English names and recoding variables
  ds <- as.data.table(rbind(ds1, ds2))[, `:=`(
    agree_randomize = fcase(
      eksperimentere == "Sterkt uenig", 1, #Strongly disagree
      eksperimentere == "Uenig", 2, #Disagree
      eksperimentere == "N\u00F8ytral", 3, #Neutral
      eksperimentere == "Enig", 4, #Agree
      eksperimentere == "Sterkt enig",5 #Strongly agree
    ),
    agree_imp = fcase(
      innfore == "Sterkt uenig", 1, #Strongly disagree
      innfore == "Uenig", 2, #Disagree
      innfore == "N\u00F8ytral", 3, #Neutral
      innfore == "Enig", 4, #Agree
      innfore == "Sterkt enig", 5 #Strongly agree
    ),
    agree_notimpl = fcase(
      ikkeinnfore == "Sterkt uenig", 1, #Strongly disagree
      ikkeinnfore == "Uenig", 2, #Disagree
      ikkeinnfore == "N\u00F8ytral", 3, #Neutral
      ikkeinnfore == "Enig", 4, #Agree
      ikkeinnfore == "Sterkt enig", 5 #Strongly agree
    ),
    just_diff_treatment = fcase(
      paastand == "Sterkt uenig", 1, #Strongly disagree
      paastand == "Uenig", 2, #Disagree
      paastand == "N\u00F8ytral", 3, #Neutral
      paastand == "Enig", 4, #Agree
      paastand == "Sterkt enig", 5 #Strongly agree
    ),
    age = fcase(
      alder == "Under 20 \u00E5r", 1, #Under 20
      alder == "20-35 \u00E5r", 2, #20-35
      alder == "36-50 \u00E5r", 3, #36-50
      alder == "51-70 \u00E5r", 4, #51-70
      alder == "Over 70 \u00E5r", 5 #Over 70
    ),
    sex = fcase(
      kjonn == "Kvinne", 1, #Female
      kjonn == "Mann", 2, #Male
      kjonn == "Annet", 3, #Other
      kjonn == "\u00D8nsker ikke \u00E5 svare", 4 #Do not want to respond
    ),
    edu1 = fcase(
      utdanning == "Ingen fullf\u00F8rt utdanning", 1, #No completed education
      utdanning == "Grunnskole", 2, #Primary education
      utdanning == "Videreg\u00E5ende skole", 3, #Secondary education
      utdanning == "Fagskole", 4, #Tertiary education
      utdanning == "Universitet- eller h\u00F8yskole", 5, #University
      utdanning == "Jeg vil ikke svare/Jeg vet ikke", 6 #Do not know / do not want to respond
    ),
    party1 = fcase(
      parti == "Arbeiderpartiet", 1, #Labour party (Arbeiderpartiet)
      parti == "H\u00F8yre", 2, #Conservative party (Hoyre)
      parti == "Fremskrittspartiet", 3, # Progress party (Fremskrittspartiet)
      parti %in% c(
        "Andre partier",
        "Kristelig Folkeparti",
        "Milj\u00F8partiet De Gr\u00F8nne",
        "R\u00F8dt",
        "Senterpartiet",
        "Sosialistisk Venstreparti",
        "Venstre"
      ), 4, #Other parties
      parti == "Jeg vet ikke", 5 #I do not know
    ),
    party2 = fcase(
      parti == "Arbeiderpartiet", 1,
      parti == "H\u00F8yre", 2,
      parti == "Fremskrittspartiet", 3,
      parti == "Sosialistisk Venstreparti", 4,
      parti == "Senterpartiet", 5,
      parti == "Kristelig Folkeparti", 6,
      parti == "Venstre", 7,
      parti == "Milj\u00F8partiet De Gr\u00F8nne", 8,
      parti == "R\u00F8dt", 9,
      parti == "Andre partier", 10,
      parti == "Jeg vet ikke", 11
    ),
    heard_about = fcase(hort == "Ja", 1, #Heard about propoasal
                        hort == "Nei",2,  #Not heard about proposal
                        hort == "Vet ikke", 3 #Do not know
                        )
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
                  edu_level=fcase(edu1 %in% c(1,2), 1, #Low education
                                        edu1 %in% c(3), 2, #Middle education
                                        edu1 %in% c(4,5), 3))] #High education


  return(ds)
}
