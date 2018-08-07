
# ----------------------------------------------
# Audrey Batzel
#
# 7/2/18

setwd('C:/local/gf/')
# ----------------------------------------------


# --------------------
# Set up R / install packages
rm(list=ls())

library(data.table)
library(stringr)
library(reshape2)
library(ggplot2)
library(stats)
library(Rcpp)
library(pdftools)
library(translate)
library(googleLanguageR)
# --------------------  


# ----------------------------------------------
# Overview - Files and Directories

# data directory
# when run on Unix, data directory needs to be set to /home/j (to run on the cluster), so set this here:
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = "C:/Users/abatzel/Documents/Audrey_PCE/"

# ----------------------------------------------

readAcronyms <- function(page){
  pg <- txt[page]
  pg <- strsplit(pg, "/n")
  
  pg <- as.data.frame(pg)
  pg <- as.data.table(pg)
  names(pg) <- c("list")
  
  pg <- colsplit(pg$list, " ", c("acronym", "meaning"))
  return(pg)
}

# ----------------------------------------------
download.file("http://www.nationalplanningcycles.org/sites/default/files/planning_cycle_repository/democratic_republic_of_congo/plan_strategique_national_2014-2017.pdf",
              "pdfs/plan_strategique_national_2014-2017.pdf", 
              mode= "wb")

txt <- pdf_text("pdfs/plan_strategique_national_2014-2017.pdf")
pages <- c(4, 5)

for (p in pages){
  newPage <- readAcronyms(p)
  if (exists("acronyms")) acronyms <- rbind(acronyms, newPage) else acronyms <- copy(newPage)
}

# ----------------------------------------------
download.file("https://www.measureevaluation.org/countries/democratic-republic-of-congo/rapport-annuel-de-activites-de-lutte-contre-le-paludisme-2016",
              "pdfs/Rapport annuel 2016 des activites de lutte contre le Paludisme version finale.pdf", 
              mode= "wb")

txt <- pdf_text("pdfs/Rapport annuel 2016 des activites de lutte contre le Paludisme version finale.pdf")

pages <- c(3, 4, 5)

for (p in pages){
  newPage <- readAcronyms(p)
  if (exists("acronyms")) acronyms <- rbind(acronyms, newPage) else acronyms <- copy(newPage)
}
# ----------------------------------------------
download.file("https://www.ilo.org/dyn/natlex/docs/ELECTRONIC/102956/124714/F-295779962/COD-102956.pdf",
              "pdfs/COD-102956.pdf", 
              mode= "wb")

txt <- pdf_text("pdfs/COD-102956.pdf")

pages <- c(4, 5)

for (p in pages){
  newPage <- readAcronyms(p)
  if (exists("acronyms")) acronyms <- rbind(acronyms, newPage) else acronyms <- copy(newPage)
}
# ----------------------------------------------
download.file("https://aidsfree.usaid.gov/sites/default/files/drc_hiv_guidelines_2016.pdf",
              "pdfs/drc_hiv_guidelines_2016.pdf", 
              mode= "wb")

txt <- pdf_text("pdfs/drc_hiv_guidelines_2016.pdf")

pages <- c(5)

for (p in pages){
  newPage <- readAcronyms(p)
  if (exists("acronyms")) acronyms <- rbind(acronyms, newPage) else acronyms <- copy(newPage)
}
# ----------------------------------------------


acronyms<- as.data.table(acronyms)

acronyms <- acronyms[acronym == "", acronym := NA]
acronyms$meaning <- gsub("/r", "", acronyms$meaning)

dt <- copy(acronyms)

for (row in 1:length(dt$acronym)){
  if (dt[row, is.na(acronym)]== TRUE )
  dt[row-1, newCol:= dt[row, meaning]] 
}

dt[!is.na(newCol), meaning:= paste(meaning, newCol)]

dt[,newCol:=NULL]
dt<- dt[!is.na(acronym)]

setorder(dt, acronym)
dt$meaning <- trimws(dt$meaning)
dt$acronym <- trimws(dt$acronym)
dt <- unique(dt)

dt<- dt[!acronym %in% c('Rapport', 'Acronymes', "LISTE")]
dt <- dt[!meaning %in% c("")]

dt <- dt[!mapply( grepl, "RDC 2014-2017", meaning)]

write.csv(dt, paste0(dir, "DRC_acronyms.csv"))
