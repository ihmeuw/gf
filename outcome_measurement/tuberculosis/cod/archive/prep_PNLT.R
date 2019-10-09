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
prep_functions <- "./outcome_measurement/tuberculosis/cod/functions_prep_PNLT.R"
source(prep_functions)

# output file
pnlt_main <- "PNLT_prepped_data.csv"
pnlt_outcomes_17 <- "PNLT_case_outcomes_2017.csv"
pnlt_case_screening_17 <- "PNLT_case_screening_2017.csv"
pnlt_outcomes_18 <- "PNLT_case_outcomes_2018.csv"
pnlt_outcomes_18_tb_hiv <- "PNLT_case_outcomes_2018_TBHIV.csv"
pnlt_case_screening_18 <- "PNLT_case_screening_2018.csv"
# ----------------------------------------------


# ----------------------------------------------
## load excel sheet with variable translations
# ----------------------------------------------
dt_transl <- data.table(read_excel(paste0(dir, pnlt_transl)))
dt_transl$keep_2018 <- as.character(dt_transl$keep_2018)
dt_transl$keep_2017 <- as.character(dt_transl$keep_2017)
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

# SET UP:
make_dt_of_sheets <- function(sheets_year, type){  # NOTE: this doens't work for the format of 2018 sheets, might have to scrap it
  sheets_list <- sheets_year[grepl(type, sheets_year)]
  
  sheets_list <- sheets_list[!grepl("SYNTH", sheets_list)]
  sheets_list <- sheets_list[!grepl("ANNUEL", sheets_list)]
  sheets_list <- sheets_list[!grepl("SYN", sheets_list)]
  sheets_list <- sheets_list[!grepl("STNTH", sheets_list)]
  
  dt <- as.data.table(sheets_list)
  setnames(dt, "sheets_list", "sheet_name")
  dt$sheet_name <- trimws(dt$sheet_name)
  if (type== "EVAL" | type== "AGE"){
    dt[, c("sheet_type", "TB_type"):= transpose(stri_split_fixed(sheet_name, " ", 2))]
    dt[, year:= lapply(strsplit(TB_type, " "), tail, 1)]
    dt[, TB_type := gsub(paste0(" ", year), "", TB_type), by="sheet_name"]
    dt[, quarter:= lapply(strsplit(TB_type, " "), tail, 1)]
    dt[, TB_type := gsub(paste0(" ", quarter), "", TB_type), by="sheet_name"]
  } else {
    dt[, c("sheet_type", "quarter", "year") := tstrsplit(sheet_name, " ")]
  }
  return (dt)
}

### FOR 2017:
# make data tables of sheet properties
  dt_sheets_dep <- make_dt_of_sheets(sheets17, "DEP")
  dt_sheets_eval <- make_dt_of_sheets(sheets17, "EVAL")
  dt_sheets_age <- make_dt_of_sheets(sheets17, "AGE")
  
### FOR 2018
  sheets_dep <- sheets18[grepl("DEP", sheets18)]
  sheets_dep <- sheets_dep[grepl("T1", sheets_dep)]
  
  sheets_eval <- sheets18[grepl("EVAL", sheets18)]
  sheets_eval <- sheets_eval[grepl("T1", sheets_eval)]
  sheets_eval <- sheets_eval[1:2]
#---------------------------------------------
    
    
#---------------------------------------------   
# CLEAN EVAL SHEETS:
  # EDIT ~~~~~~~~~~~~~~~~~~~~~
  # run the user-written function sourced in
  case_outcomes <- clean_eval_sheets(dir, 2018, files18[1])
  year = 2018
  file = files18[1]
  s = sheets_eval[1]
  i <- 1
  
  for (s in sheets_eval[1:length(sheets_eval)]){
      
      dt <- data.table(read_excel(paste0(dir, year, "/", file), sheet= s))
      
      dt <- initial_clean(dt)
      
      # remove columns of percentages
      cols <- !is.na( dt[1,] )
      cols <- colnames(dt)[cols]
      
      dt <- dt[, cols, with=FALSE]
      
      # set column names to be header row:
      colnames(dt) <- as.character(dt[1,])
      
      # remove header row in row 1
      dt <- dt[-1, ]
      
      ##----------------------------------
      # clean column names:
      colnames(dt) <- tolower(colnames(dt))
      colnames(dt) <- chartr(paste(names(unwanted_array), collapse=''),
                       paste(unwanted_array, collapse=''),
                       colnames(dt))      
      
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

      for(n in c('transfer'))  if(any(grepl(n, names(dt)))) setnames(dt, grep(n, names(dt)), 'transferred')
      if(!'transferred' %in% names(dt)) print(paste0('In sheet, ', s, ', transferred is not a column'))
      
      for(n in c('total  evalue','total evalue', 'total cas evalues', 'total de cas')) if(any(grepl(n, names(dt)))) setnames(dt, grep(n, names(dt)), 'cas_eval')
      if(!'cas_eval' %in% names(dt)) stop(paste0('In sheet, ', s, ', cas_eval is not a column'))
      
      for(n in c('non evalue')) if(any(grepl(n, names(dt)))) setnames(dt, grep(n, names(dt)), 'cas_not_eval')
      if(!'cas_not_eval' %in% names(dt)) stop(paste0('In sheet, ', s, ', cas_not_eval is not a column'))
      
      dt <- clean_dps_names(dt)
      
      if(year == 2017) dt <- add_data_sheet_info(dt)
      if(year == 2018) {
        s <- trimws(s)
        dt[, sheet:= s]
        dt[, quarter:= 1]
        dt[, data_year:= year]
        dt[, file_year:= year]
      }
        
      if (i==1){
        # if it's the first sheet, initialize the new dt
        outcomes <- dt
        # for subsequent sheets, rbind to that dt
      } else {
        outcomes <- rbindlist(list(outcomes, dt), use.names=TRUE, fill= TRUE)
      }
      # print(s)
      i <- i + 1
    }
  
  # save the results as a csv
  write.csv(outcomes, file= paste0(dir_prepped, pnlt_outcomes_18_tb_hiv))
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
# ----------------------------------------------

# ---------------------------------------------   
# CLEAN DEP SHEETS:
file = files17[1]
year = 2017
sheets_dep <- sheets17[grepl("DEP", sheets17)]
sheets_dep <- sheets_dep[!grepl("SYN", sheets_dep)]
i = 1

for (s in sheets_dep) {
    dt <- data.table(read_excel(paste0(dir, year, "/", file), sheet= s))
    
    # basic tidying
    dt <- initial_clean(dt, year)
    
    # change column names
    if (year==2018) dt$col1 <- NULL
    
    col_names <- dt_transl[get(paste0("keep_", year))==1, variable_in_code]
    colnames(dt) <- col_names
    
    # finish cleaning up dps names/rows
    dt <- clean_dps_names(dt)
    
    # create columns for year/quarter/etc
    dt <- add_data_sheet_info(dt, dt_sheets_dep)
    
    if (year==2018) dt$quarter <- 1
    if (year==2018) dt$data_year <- 2018

    dt$quarter <- gsub("T", "", dt$quarter)

    # create date variable:
    dt[ file_year=="2017" & quarter=="1", date:= "2017-01-01"]
    dt[ file_year=="2017" & quarter=="2", date:= "2017-04-01"]
    dt[ file_year=="2017" & quarter=="3", date:= "2017-07-01"]
    dt[ file_year=="2017" & quarter=="4", date:= "2017-10-01"]
    dt[ file_year=="2018" & quarter=="1", date:= "2018-01-01"]    
    
    dt$date <- as.Date(dt$date)
    dt$quarter <- as.numeric(dt$quarter)
    dt$dps <- as.character(dt$dps)
    
    # create a dt to store/rbind each iteration of the loop
    if (i==1){
      # if it's the first sheet, initialize the new dt
      tb_screening_data <- dt
      # for subsequent sheets, rbind to that dt
    } else {
      tb_screening_data <- rbindlist(list(tb_screening_data, dt), use.names=TRUE, fill= TRUE)
    }
    print(s)
    i <- i + 1
}

write.csv(tb_screening_data, file= paste0(dir_prepped, pnlt_case_screening_17))
    









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



