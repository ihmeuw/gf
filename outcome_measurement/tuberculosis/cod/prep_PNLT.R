# ----------------------------------------------
# Audrey Batzel
#
setwd('C:/local/gf/')
# ----------------------------------------------


# --------------------
# Set up R / install packages
## -------------------
rm(list=ls())
library(data.table)
library(reshape2)
library(stringr)
library(RColorBrewer)
library(ggplot2)
library(lubridate)
library(readxl)
library(stats)
library(rlang)
library(zoo)
library(tidyr)
library(dplyr)
library(openxlsx)
# --------------------


# ----------------------------------------------
# Overview - Files and Directories
## ----------------------------------------------
# data directory
# file path where the files are stored
dir <- "J:/Project/Evaluation/GF/outcome_measurement/cod/National_TB_Program/"
dir_prepped <-"J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLT/"

# input file
file2018 <- "Synthèse Nationale T1 2018.xlsx"
file2017 <- 'Synthèse Nationale RDC 2017.xlsx'
pnlt_transl <- "PNLT_translations.xlsx"

# output file
pnlt_main <- "PNLT_prepped_data.csv"

## ----------------------------------------------
## load data from excel to visualize initial data
## ----------------------------------------------
dt2018 <- data.table(read_excel(paste0(dir, file2018), sheet= 'DEPISTAGE T1 018'))
dt2017 <- data.table(read_excel(paste0(dir, file2017), sheet= 'DEP T1 017'))

sheets2018 <- getSheetNames(paste0(dir, file2018))
sheets2017 <- getSheetNames(paste0(dir, file2017))

dt_transl <- data.table(read_excel(paste0(dir, pnlt_transl)))

# ----------------------------------------------
# get all files in a given year folder

getFiles <- function(year){
  files = list.files(paste0(dir, year))
  return(files)
}

for (year in 2014:2016){
  files <- getFiles(year)
  assign(paste0("files", year, sep=""), files)
}

# NOT SURE IF THE FOLLOWING WILL BE HELPFUL; it may be better to do it one sheet at a time?
# get sheets in each file
for(i in list_of_all_files){
  for(j in i){
    if ( !exists("sheets") ) sheets <- getSheetNames(paste0(dir, year, "/", j))
    currentFile <- getSheetNames(paste0(dir, year, "/", j))
    sheets <- append(currentFile, sheets)
  }
}
possible_sheet_names <- unique(sheets)
# ----------------------------------------------
## clean "DEPISTAGE T1" sheet
## ----------------------------------------------
## set first column to null 
dt2018[, X__1:=NULL]

## use dt_transl to set column names
## ignore translations that don't have unique columns in each dt
dt_transl<- dt_transl[is.na(ignore_in_col_naming)]
## set column names using "variable_in_code" column
colnames(dt2018) <- dt_transl$variable_in_code

## create a column for year and quarter
#  (automate this) use year from reading in
dt2018$year <- 2018

#  (automate this) if sheet is == T1, set quarter to 1 for example
dt2018$quarter <- 1

## remove any rows that are entirely NA 
dt2018 <- dt2018[!apply(is.na(dt2018), 1, all),]

## CLEAN DPS COLUMN
## change out special characters in DPS
## vector dictionary of special characters to regular characters
unwanted_array = list(    'S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                          'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                          'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                          'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                          'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )

dt2018$dps <- chartr(paste(names(unwanted_array), collapse=''),
                                      paste(unwanted_array, collapse=''),
                                      dt2018$dps)
dt2018$dps <- tolower(dt2018$dps)
dt2018$dps <- gsub(" ", "-", dt2018$dps)
dt2018$dps <- gsub("--", "-", dt2018$dps)

dps_list <- c("kwango", "kwilu", "mai-ndombe", "kongo-central-est", "kongo-central-ouest", "equateur", "mongala", "nord-ubangi", "sud-ubangi",                                       
              "tshuapa", "kasai", "lomami", "kasai-oriental", "sankuru", "haut-katanga", "haut-lomami", "lualaba", "tanganyika", "nord-kivu",
              "sud-kivu", "haut-uele", "bas-uele", "ituri", "tshopo", "kasai-central", "kinshasa", "maniema")

## remove other unncessary rows by only keeping dps in dps list
dt2018 <- dt2018[dps %in% dps_list]

## combine all years/quarters together

## export data (eventually -- all different kinds of sheets)
write.csv(dt2018, paste0(dir_prepped, pnlt_main))
