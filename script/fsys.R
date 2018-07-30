# Funktion som returnerar diagram över sysselsättningsutveckling per år och antalet
# sysselsatta per bransch senaste år

fsys <- function(lan_urval, ant_jfr = 1) {
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(scales)
  library(stringr)
  
  load("data/dfsys.RData")
  
  space <- function(x, ...) { 
    format(x, ..., big.mark = " ", scientific = FALSE, trim = TRUE)
  }
  
  dfsys_ar <- dfsys %>%
    filter(lan %in% lan_urval) %>%
    select(lan, ar, values) %>%
    group_by(lan, ar) %>%
    summarise(values = sum(values, na.rm = TRUE)) %>%
    ungroup()
  
  plot_sys_ar <- ggplot(dfsys_ar, aes(x = ar, y = values, colour = lan)) +
    geom_line() +
    scale_y_continuous(limits = c(0, NA), labels = space) +
    theme_bw() +
    labs(
      title = paste0("Sysselsättningsutveckling i ", lan_urval, " ", min(dfsys_ar$ar), " - ",
                     max(dfsys_ar$ar)),
      colour = NULL,
      x = "År",
      y = "Antal sysselsatta",
      caption = "Källa: SCB"
    ) +
    theme(legend.position="none")
  
  dfsys_bransch <- dfsys %>%
    filter(lan %in% lan_urval & ar == max(ar)) %>%
    select(lan, sni = naringsgren_sni_2007, ar, values) %>%
    group_by(lan, sni, ar) %>%
    summarise(values = sum(values, na.rm = TRUE)) %>%
    ungroup()
  
  plot_sys_bransch <- ggplot(dfsys_bransch, aes(x = reorder(sni, values), y = values)) +
    geom_bar(stat = "identity", fill = "red") +
    theme_bw() +
    scale_y_continuous(labels = space) +
    labs(
      title = str_wrap(paste0("Sysselsättning per bransch i ", lan_urval, " år ", max(dfsys_bransch$ar)), 30),
      colour = NULL,
      x = NULL,
      y = "Antal sysselsatta",
      caption = "Källa: SCB"
    ) +
    theme(legend.position="none") +
    coord_flip()
  

  
  # Skapa mening
  
  source("script/ftext_rang_antal.R", encoding = 'UTF-8')
  
  df <- dfsys_bransch %>%
    select(enhet = sni, values) %>%
    arrange(-values)
  
  txt_bransch <- ftext_rang_antal(df = df,
                    ant_jfr = ant_jfr,
                    inledning1 = "Störst antal sysselsatta hade",
                    forts1 =  "följt av ",
                    enhet1 = " sysselsatta",
                    enhet2 = " sysselsatta",
                    ant_decimal = 0)
  
  ldia_sys = list(plot_sys_ar = plot_sys_ar, plot_sys_bransch = plot_sys_bransch, txt_bransch = txt_bransch) 
  
  return(ldia_sys)
}
# test
fsys("Skåne län", ant_jfr = 2)
