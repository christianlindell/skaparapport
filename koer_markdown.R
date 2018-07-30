# SKRIPT FÖR ATT GENERERA RAPPORTER VILKA SPARAS I KATALOGEN "rapporter"

rm(list = ls()) # Rensar bort alla objekt som kan störa körningen

# Se till så att working directory är satt till "skapa_rapport v2".

## Packages
library(knitr)
library(rmarkdown)
library(readxl)


frensa_svenska_tecken <- function(txt) {
  txt <- gsub("Å|Ä", "A", txt)
  txt <- gsub("Ö", "O", txt)  
  txt <- gsub("å|ä", "a", txt)
  txt <- gsub("ö", "o", txt)
  return(txt)
}

# Skriptet öppnar en excelfil "lan.xlsx" som innehåller en tabell med alla
# länsnamn i riket. Skriptet itererar sig genom
# listan och skapar en html-fil för varje län I varje fil läggs dessutom in en ny meta-
# tag som anger user-agent (i annat fall anger Region Skåne default till IE 7 vilket innebär
# att RS-loggan i sidhuvudet och innehållsförteckningen inte syns). Förmodligen är det en
# inställning som IT-avdelningen i Region Skåne har gjort som inte berör i andra regioner.


newtxtuseragent <- paste0("</title>\n", "<meta http-equiv=", '"', "X-UA-Compatible", '"', " content=",
                          '"', "IE=edge,chrome=1", '"', ">")

regtab <- read_excel("data/lan.xlsx")



# Kollar om katalogen "rapporter" finns. Försöker skapa den annars.

folder <- "rapporter"
if (!file.exists(folder)) {
  warning(paste("Directory:", folder,
                "doesn't exist. Will create it"))
  fc <- try(dir.create(folder))
  if (inherits(fc, "try-error")) {
    stop("Failed to create directory '", folder,
         "'. Check path and permissions.", sep = "")
  }
}

fskaparapport <- function(studreg, filepath) {
  
  rmarkdown::render(
    input = "sysselsattning.Rmd",
    output_format = "html_document",
    encoding = 'UTF-8',
    output_file = frensa_svenska_tecken(
      paste0("Sysselsattning_", studreg, ".html")),
    output_dir = "rapporter"
  )
  # Nedanstående tre rader ändrar useragent, vilket måste göras i Region Skåne för inte 
  # all html ska köras i IE7-läge. Kan förmodligen tas bort i andra regioner.
  output_file <- paste(paste(readLines(filepath), collapse = "\n"), "\n")
  output_file <- sub("</title>", newtxtuseragent, output_file)
  cat(paste0(output_file, "\n"), file = filepath)
}


# for (i in 1:nrow(regtab)){
for (i in 1:2){
  print(paste0(i,"/", nrow(regtab)))
  studreg <- as.character(unlist(regtab[i, "lan"]))
  filepath <- file.path("rapporter", frensa_svenska_tecken(paste0("Sysselsattning_", 
                                                                        studreg, ".html")))
  fskaparapport(studreg, filepath)
}


