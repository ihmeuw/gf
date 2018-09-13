# ----------------------------------------------
# Audrey Batzel
# 8/1/18
# Prep PNLT data
#
setwd('C:/local/gf/')
# ----------------------------------------------


# --------------------
## Set up R / install packages
# -------------------
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
library(stringi)
# --------------------


# ----------------------------------------------
## Overview - Files and Directories
# ----------------------------------------------
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
pnlt_outcomes_17 <- "PNLT_case_outcomes_2017.csv"
# ----------------------------------------------


# ----------------------------------------------
## variables to use
# ----------------------------------------------
unwanted_array = list(    'S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                          'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                          'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                          'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                          'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )

dps_names <- c('kwango', 'kwilu', 'mai-ndombe', 'kongo-central-est', 'kongo-central-ouest', 'equateur', 'mongala', 'nord-ubangi', 'sud-ubangi', 'tshuapa', 'kasai', 'kasai-central', 
              'kasai-oriental', 'lomami', 'sankuru', 'haut-katanga', 'haut-lomami', 'lualaba', 'tanganyika', 'kinshasa', 'maniema', 'nord-kivu', 'sud-kivu', 'ituri', 'tshopo', 
              'bas-uele', 'haut-uele')
# ----------------------------------------------


# ----------------------------------------------
## load data from excel to visualize initial data
# ----------------------------------------------
# dt2018 <- data.table(read_excel(paste0(dir, file2018), sheet= 'DEPISTAGE T1 018'))
# dt2017 <- data.table(read_excel(paste0(dir, file2017), sheet= 'DEP T1 017'))

dt_transl <- data.table(read_excel(paste0(dir, pnlt_transl)))
# ----------------------------------------------


# ----------------------------------------------
## get all files in a given year folder
# ----------------------------------------------
getFiles <- function(year){
  files = list.files(paste0(dir, year))
  return(files)
}

# # get file names for a given year:
# files <- getFiles(year)
# 
# # clean file names to remove ones without data for cleaning:
# files_xlsx <- files[!grepl(".DOC", files)]
# files_xlsx <- files_xlsx[!grepl("NATIONAL", files_xlsx)]
# files_xlsx <- files_xlsx[!grepl("SUBMISSION_DATES", files_xlsx)]
# files_xlsx <- files_xlsx[!grepl("HEALTH_ZONES", files_xlsx)]
# files_xlsx <- files_xlsx[!grepl("INDICATORS_COLLECTED", files_xlsx)]
#-----------------------------------------------


# ----------------------------------------------
## get all sheets in a given file
# ----------------------------------------------
getSheets <- function(file, year){
  sheets <- getSheetNames(paste0(dir, year, "/", file))
  
  # # determine what quarter the file is from:  --- NOT RELEVANT FOR 2017/2018
  # if (grepl("T1", file)){
  #   quarter= "T1"
  # }else if (grepl("T2", file)){
  #   quarter= "T2"
  # }else if (grepl("T3", file)){
  #   quarter= "T3"
  # }else if (grepl("T4", file)){
  #   quarter= "T4"
  # }
  # return(sheets)
}
## NOTE: three types of sheets to clean:  DEP, AGE, and EVAL
# ----------------------------------------------


#-----------------------------------------------
## Prep 2017 and 2018 data -- first focus
#-----------------------------------------------
files18 <- getFiles(2018)
files18 <- files18[!grepl("~", files18)]
files17 <- getFiles(2017)
files17 <- files17[!grepl("~", files17)]

# there is just one file in each year folder for 2018/2017
sheets18 <- getSheets(files18[1], 2018)
sheets17 <- getSheets(files17[1], 2017)

# 2017:
# SET UP:
  sheets <- sheets17
  # EVAL SHEETS:
    # get just eval sheets for each quarter
    sheets_eval <- sheets[grepl("EVAL", sheets)]
    sheets_eval <- sheets_eval[!grepl("SYN", sheets_eval)]
    
    # make a data table of sheet properties
    dt_sheets_eval <- as.data.table(sheets_eval)
    setnames(dt_sheets_eval, "sheets_eval", "sheet_name")
    dt_sheets_eval[, c("sheet_type", "TB_type"):= transpose(stri_split_fixed(sheet_name, " ", 2))]
    dt_sheets_eval[, year:= lapply(strsplit(TB_type, " "), tail, 1)]
    dt_sheets_eval[, TB_type := gsub(paste0(" ", year), "", TB_type), by="sheet_name"]
    dt_sheets_eval[, quarter:= lapply(strsplit(TB_type, " "), tail, 1)]
    dt_sheets_eval[, TB_type := gsub(paste0(" ", quarter), "", TB_type), by="sheet_name"]
    
  # DEP SHEETS
    # get just the DEP sheets from sheets
    sheets_dep <- sheets[grepl("DEP", sheets)]
    sheets_dep <- sheets_dep[!grepl("SYNTH", sheets_dep)]
    
  # AGE SHEETS
    # get just the EVAL sheets from sheets
    sheets_age <- sheets[grepl("AGE", sheets)]
    sheets_age <- sheets_age[!grepl("SYN", sheets_age)]
    sheets_age <- sheets_age[!grepl("STNTH", sheets_age)]
    
    # make a data table of sheet properties
    dt_sheets_age <- as.data.table(sheets_age)
    setnames(dt_sheets_age, "sheets_age", "sheet_name")
    dt_sheets_age[, c("sheet_type", "TB_type"):= transpose(stri_split_fixed(sheet_name, " ", 2))]
    dt_sheets_age[, year:= lapply(strsplit(TB_type, " "), tail, 1)]
    dt_sheets_age[, TB_type := gsub(paste0(" ", year), "", TB_type), by="sheet_name"]
    dt_sheets_age[, quarter:= lapply(strsplit(TB_type, " "), tail, 1)]
    dt_sheets_age[, TB_type := gsub(paste0(" ", quarter), "", TB_type), by="sheet_name"]
#---------------------------------------------
    
    
#---------------------------------------------   
# CLEAN:
    # EVAL SHEETS:
    year=2017
    file=file2017[1]
    i = 1
    
    for (s in sheets_eval[1:length(sheets_eval)]){
      
      dt <- data.table(read_excel(paste0(dir, year, "/", file), sheet= s))
      
      # remove rows at the top up until the header row
      setnames(dt, colnames(dt)[1], "col1")
      index <- grep("CPLT", dt$col1 )
      
      dt <- dt[-c(1:(index-1))]
      
      # remove columns of percentages
      cols <- !is.na( dt[1,] )
      cols <- colnames(dt)[cols]
      
      dt <- dt[, cols, with=FALSE]
      
      # remove rows that are entirely NA
      rows_to_remove <- apply(dt, 1, function(x) all(is.na(x)))
      dt <- dt[!rows_to_remove, ]
      # remove rows where col1 is na
      dt <- dt[!is.na(col1)]
      
      # remove total rows (sometime has "RDC")
      dt <- dt[!grepl("TOTAL", col1)]
      dt <- dt[!grepl("RDC", col1)]
      
      # set column names to be header row:
      colnames(dt) <- as.character(dt[1,])
      
      # remove header row in row 1
      dt <- dt[-1, ]
      
      ##----------------------------------
      # clean column names:
      colnames(dt) <- tolower(colnames(dt))
    
      # Setnames <- function(x, old, new, allow.absent.cols=F) {
      #   if (!allow.absent.cols) {
      #     setnames(x, old, new)
      #   } else {
      #     old.intersect <- intersect(old, names(x))
      #     common.indices <- old %in% old.intersect
      #     new.intersect <- new[common.indices]
      #     setnames(x, old.intersect, new.intersect)
      #   }
      # }
      setnames(dt, grep('cplt', colnames(dt)), 'dps')
      setnames(dt, grep('enreg', colnames(dt)), 'tot_cas_reg')
      
      for(n in c('guer')) if(any(grepl(n, names(dt)))) setnames(dt, grep(n, names(dt)), 'healed')
      if(!'healed' %in% names(dt)) print(paste0('In sheet, ', s, ', healed is not a column'))
      
      setnames(dt, grep('traitement termine', colnames(dt)), 'trt_complete')

      for(n in c('dece','dcd')) if(any(grepl(n, names(dt)))) setnames(dt, grep(n, names(dt)), 'died')
      if(!'died' %in% names(dt)) stop(paste0('In sheet, ', s, ', died is not a column'))
      
      for(n in c('echecs')) if(any(grepl(n, names(dt)))) setnames(dt, grep(n, names(dt)), 'trt_failed')
      if(!'trt_failed' %in% names(dt)) print(paste0('In sheet, ', s, ', trt_failed is not a column'))
      
      for(n in c('perdu','abandon', 'interruptions')) if(any(grepl(n, names(dt)))) setnames(dt, grep(n, names(dt)), 'lost_to_followup')
      if(!'lost_to_followup' %in% names(dt)) stop(paste0('In sheet, ', s, ', lost_to_followup is not a column'))
      
      setnames(dt, grep('transfer', colnames(dt)), 'transferred')
      
      for(n in c('total  evalue','total evalue', 'total cas evalues')) if(any(grepl(n, names(dt)))) setnames(dt, grep(n, names(dt)), 'cas_eval')
      if(!'cas_eval' %in% names(dt)) stop(paste0('In sheet, ', s, ', cas_eval is not a column'))
      
      for(n in c('non evalue')) if(any(grepl(n, names(dt)))) setnames(dt, grep(n, names(dt)), 'cas_not_eval')
      if(!'cas_not_eval' %in% names(dt)) stop(paste0('In sheet, ', s, ', cas_not_eval is not a column'))

      # clean DPS names
      dt$dps <- gsub(" ", "-", dt$dps)
      dt$dps <- gsub("--", "-", dt$dps)

      dt$dps <- chartr(paste(names(unwanted_array), collapse=''),
                           paste(unwanted_array, collapse=''),
                           dt$dps)
      
      # one case where this is different:
      
      dt <- dt[dps !="EQUATEUR"]
      dt <- dt[dps !="KASAI-ORIENTAL"]
      
      dt$dps <- tolower(dt$dps)
      
      dt[ dps == 'kasai-centre', dps:= 'kasai-central']
      
      dt <- dt[dps %in% dps_names]

      # add columns for quarter, year, and TB type
      dt[, sheet:= s]
      dt[, quarter:= dt_sheets_eval[sheet_name==s, quarter]]
      dt[, TB_type := dt_sheets_eval[sheet_name==s, TB_type]]
      dt[, data_year := dt_sheets_eval[sheet_name==s, year]]
      dt[, file_year := year]
      
      if (i==1){
        # if it's the first sheet, initialize the new dt
        outcomes <- dt
        # for subsequent sheets, rbind to that dt
      } else {
        outcomes <- rbindlist(list(outcomes, dt), use.names=TRUE, fill= TRUE)
      }
      print(s)
      i <- i + 1
    }
    
    write.csv(outcomes, file= paste0(dir_prepped, pnlt_outcomes_17))
    
# ----------------------------------------------

    
    
    
    
    
    
    
    
    
    
    
    

# ----------------------------------------------
## clean "EVAL"sheets
# ----------------------------------------------
# get just the EVAL sheets from sheets
sheets_eval <- sheets[grepl("EVAL", sheets)]
# grab just the EVAL sheets for the file quarter:
sheets_eval <- sheets_eval[grepl(quarter, sheets_eval)]

dt_sheets <- as.data.table(sheets_eval)
setnames(dt_sheets, "sheets_eval", "sheet_name")
dt_sheets[, c("sheet_type", "TB_type"):= transpose(stri_split_fixed(sheet_name, " ", 2))]
dt_sheets[, year:= lapply(strsplit(TB_type, " "), tail, 1)]
dt_sheets[, TB_type := gsub(paste0(" ", year), "", TB_type), by="sheet_name"]
dt_sheets[, quarter:= lapply(strsplit(TB_type, " "), tail, 1)]
dt_sheets[, TB_type := gsub(paste0(" ", quarter), "", TB_type), by="sheet_name"]

dps <- sub("COD_", "", file)
dps <- substring(dps, 0, regexpr("_NTCP", dps) - 1)

i<-1

for( s in sheets_eval ){
  
dt <- data.table(read_excel(paste0(dir, year, "/", file), sheet= s))
      # # This isn't always the case... seems to always start on the 6th row though?
      # # data starts below heading where it says "CSDT/ZS" -> get this row number
      # setnames(dt, colnames(dt)[1], "col1")
      # row <- dt[col1=="CSDT/ZS", which=TRUE]
      # rows_to_remove <- seq(1:(row-1))
      # dt <- dt[-c(rows_to_remove)]

dt <- dt[-c(1:5)]

# set column names
cols <- !is.na( dt[1, ] )
cols <- colnames(dt)[cols]
dt <- dt[, cols, with=FALSE]

colnames(dt) <- as.character(dt[1,])

# remove header rows and total rows
dt <- dt[-c(1, 2)]
setnames(dt, colnames(dt)[1], "col1")
dt <- dt[!grepl("TOTAL", col1)]

# remove rows where col1 is na
dt <- dt[!is.na(col1)]



# add columns for dps, quarter, year, and TB type
dt[, dps:= dps]
dt[, quarter:= quarter]
dt[, TB_type := dt_sheets[sheet_name==s, TB_type]]
dt[, data_year := dt_sheets[sheet_name==s, year]]
dt[, file_year := year]

if (i == 1){
  # if it's the first sheet, initialize the new dt
  case_outcomes <- dt
  # for subsequent sheets, rbind to that dt
} else {
  case_outcomes <- rbindlist(list(case_outcomes, dt), use.names=TRUE, fill= TRUE)
}
i = i + 1
}

setnames(case_outcomes, "col1", 'csdt_hz')
setnames(case_outcomes, "TOTAL CAS  ENREGISTRES", 'tot_cas_reg')
setnames(case_outcomes, "GUERIS", 'healed')
setnames(case_outcomes, "TRAITEMENTS ACHEVES", 'trt_complete')
setnames(case_outcomes, "DECEDES", 'died')
setnames(case_outcomes, "ECHECS", 'trt_failed')
setnames(case_outcomes, "INTERRUPTIONS", 'trt_interrupted')
setnames(case_outcomes, "TOTAL CAS EVALUES", 'tot_cas_eval')
setnames(case_outcomes, "TRANSFERES", 'transferred')
setnames(case_outcomes, "TOTAL  CAS NON EVALUES", 'cas_not_eval')

#colnames(dt) <- c('csdt_hz', 'tot_cas_reg', 'healed', 'trt_complete', 'died', 'trt_failed', 'trt_interrupted', 'tot_cas_eval', 'transferred', 'cas_not_eval' )
# ----------------------------------------------


# ----------------------------------------------
## clean "DEPISTAGE" or "DEP" sheet
# ----------------------------------------------
# get just the DEP sheets from sheets
sheet_dep <- sheets[grepl("DEP", sheets)]
# exclude sheet names that are synthesized or annual measures
sheet_dep <-  sheet_dep[grepl(quarter, sheet_dep)]
# ----------------------------------------------


# ----------------------------------------------
## clean "AGE"sheets
# ----------------------------------------------
# get just the EVAL sheets from sheets
sheets_age <- sheets[grepl("AGE", sheets)]
# grab just the EVAL sheets for the file quarter:
sheets_age <- sheets_age[grepl(quarter, sheets_age)]
# ----------------------------------------------









# loop through sheet names, reading in each and R binding it to a dt for that year
i = 1

for (s in dep_sheets){
  currentSheet <- data.table(read_excel(paste0(dir, file), sheet= s))
  currentSheet$quarter <- i
  # if it's the first sheet, initialize the new dt
  if (i == 1){
    dt <- currentSheet
  # for subsequent sheets, rbind to that dt
  } else {
    dt <- rbind(dt, currentSheet)
  }
  i = i + 1
}

return(dt)
}

dt$year <- year

# ----------------------------------------------
## set column names using excel doc matched with year
# ----------------------------------------------

if(year==2018){
  ## use dt_transl to set column names
  ## ignore translations that don't have unique columns in each dt
  dt_transl<- dt_transl[is.na(ignore_in_col_naming)]
  ## set column names using "variable_in_code" column
  col_names <- dt_transl$variable_in_code
} else if (year== 2017){
  col_names <-c('dps', 'pop_tot', 'pop_covered', 'tb_pres', 'micro_comp', 'micro_pos', 'xpert_comp', 'xpert_pos', 'ziehl_comp', 'ziehl_pos', 
                'cq_partic', 'tbp_confBac_new', 'tbp_confBac_relapse', 'tbp_confBac_outOfRelapse', 'tbp_confBac_children', 'tbp_confClinc_new', 
                'tbp_confClinc_relapse', 'tbp_confClinc_outOfRelapse', 'tbp_confClinc_children', 'tep_new', 'tep_relapse', 'tep_outOfRelapse', 
                'tep_children', 'treated_uncertain', 'tot_incCase', 'tot_case', 'tbPatient_hivTest', 'tbPatient_hivPos', 'tbPatient_hivPos_cotri', 
                'tbPatient_hivPos_art', 'plhiv_tbTest', 'plhiv_tbNotTest', 'plhiv_INH', 'child_livingWithTBCase', 'child_test', 'child_pos', 'child_INH', 
                'prisoners_cases', 'prisoners_treated', 'miners_cases', 'miners_treated', 'contact_cases', 'contact_treated', 'reco_oac_referred', 
                'reco_oac_supported', 'initial_1st_intention_children', 'initial_1st_intention_adults', 'retreat_1st_intention_children', 
                'retreat_1st_intention_adults', 'csdt_1', 'csdt_2', 'ignore_1', 'ignore_2', 'ignore_3', 'ignore_4', 'membership')
}

## set first column to null 
dt2018[, X__1:=NULL]

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
# use rbindlist(mylist, use.names = TRUE, fill= TRUE)

## export data (eventually -- all different kinds of sheets)
write.csv(dt2018, paste0(dir_prepped, pnlt_main))




##============NO LONGER NEEDED CODE==================

# # Make a data table with the file name info:
# dt_files <- as.data.table(files_xlsx)
# setnames(dt_files, "files_xlsx", "file_name")
# dt_files[, file_name2:= file_name]
# 
# dps <- sub("COD_", "", files_xlsx)
# dps <- sub("NTCP", ":", dps)
# dps <- substring(dps, 0, regexpr("_:", dps) - 1)

# for (year in 2014:2016){
#   files <- getFiles(year)
#   assign(paste0("files", year, sep=""), files)
# }

# # NOT SURE IF THE FOLLOWING WILL BE HELPFUL; it may be better to do it one sheet at a time?
# # get sheets in each file
# for(i in list_of_all_files){
#   for(j in i){
#     if ( !exists("sheets") ) sheets <- getSheetNames(paste0(dir, year, "/", j))
#     currentFile <- getSheetNames(paste0(dir, year, "/", j))
#     sheets <- append(currentFile, sheets)
#   }
# }
# possible_sheet_names <- unique(sheets)



