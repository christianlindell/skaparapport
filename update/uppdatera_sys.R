library(pxweb)
library(dplyr)
library(tidyr)
library(janitor)


# ladda hem sysselsatt dagbef

dfsys <- 
  get_pxweb_data(url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207K/DagSNI07KonK",
                 dims = list(Region = c('*'),
                             SNI2007 = c('*'),
                             Kon = c('*'),
                             ContentsCode = c('*'),
                             Tid = c('*')),
                 clean = TRUE)

# Datacleaning

# Skapa variabelnamn som följer R-standard
dfsys <- janitor::clean_names(dfsys)

# Gör om factor till character
dfsys <- mutate_if(dfsys, is.factor, as.character)

# Gör om år till integer
dfsys$ar <- as.integer(dfsys$ar)

# Splitta upp regionkolumnen i en kolumn för län-kod och en länsnamnet
dfsys <- dfsys %>% 
  separate(region, 
           c("lan_kod", "lan"), 
           sep = "\\s", 
           extra = "merge")

# Sortera bort kommuner och riket
dfsys <- filter(dfsys, nchar(lan_kod) == 2 & lan != "Riket")

# Spara resultatet i data-mappen
save(dfsys, file = "data/dfsys.RData")

  
