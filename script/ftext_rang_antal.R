# Skapa rangordnad lista med antal

ftext_rang_antal <- function(df, ant_jfr = 3, inledning1 = "", inledning2 = "", enhet1 = "", 
                             enhet2 = "", forts1 = "", ant_decimal = 0) {
  # Läs in funktion för att formatera tal med blanksteg som 1000-talavgränsare m.m.
  source("script/funcNr.R", encoding = 'UTF-8')
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(stringi)
  ant_jfr <- ant_jfr
  mening <-
    paste0("inledning1 ", '"', df[1, "enhet"], '"'," (", funcNr(df[1, "values"], ant_decimal), " enhet1), ")
  
  
  if (ant_jfr > 1) {
    if (ant_jfr > 2) {
      mening <- paste0(mening, "forts1 ")
    }
    for (i in 2:ant_jfr) {
      mening_ff <-
        paste0('"', df[i, "enhet"], '"', " (", funcNr(df[i, "values"], ant_decimal), " enhet2), ")
      mening <- paste0(mening, mening_ff)
    }
  }
  
  # Ersätt komma i slutet på mening med en punkt
  mening <- str_replace(mening, ", $", ".")
  # Ersätt komma före sista uppräkningen med " och "
  mening <- stringi::stri_replace_last_fixed(mening, "), ", ") och ")
  
  # Klistra in text
  mening <- str_replace(mening, "inledning1", inledning1)
  mening <- str_replace(mening, "enhet1", enhet1)
  mening <- str_replace(mening, "forts1", forts1)
  mening <- str_replace_all(mening, "enhet2", enhet2)
}


# TESTKOD
load("data/dfsys.RData")

lan_urval <- "Skåne län"


dfsys_bransch <- dfsys %>%
  filter(lan %in% lan_urval & ar == max(ar)) %>%
  select(lan, sni = naringsgren_sni_2007, ar, values) %>%
  group_by(lan, sni, ar) %>%
  summarise(values = sum(values, na.rm = TRUE)) %>%
  ungroup() %>%
  select(enhet = sni, values) %>%
  arrange(-values)

(ftext_rang_antal(df = dfsys_bransch,
                  ant_jfr = 3,
                  inledning1 = "Störst antal sysselsatta hade",
                  forts1 =  "följt av ",
                  enhet1 = "sysselsatta",
                  enhet2 = "sysselsatta",
                  ant_decimal = 0))
                      
