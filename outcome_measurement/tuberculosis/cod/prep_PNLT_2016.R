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
setwd('C:/local/gf/')
source('./core/standardizeDPSnames_function.R')
# --------------------


# ----------------------------------------------
## Overview - Files and Directories
# ----------------------------------------------
# data directory
# file path where the files are stored
dir <- "J:/Project/Evaluation/GF/outcome_measurement/cod/National_TB_Program/"
dir_prepped <-"J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLT/"

# input files
pnlt_transl <- "PNLT_translations.xlsx"
prep_functions <- "./outcome_measurement/tuberculosis/cod/functions_prep_PNLT.R"
source(prep_functions)

# output files

# ----------------------------------------------


# ----------------------------------------------
## load excel sheet with variable translations
# ----------------------------------------------
dt_transl <- data.table(read_excel(paste0(dir, pnlt_transl)))
# ----------------------------------------------


# ----------------------------------------------
## 
# ----------------------------------------------
files <- getFiles(2016)

# prep T1 first?
files_t1 <- files[grep("T1", files)]

dt <- as.data.table(files_t1)
dt[, province := gsub("COD_", "", files_t1)]
dt[, c("province", "string"):= transpose(stri_split_fixed(province, "_", 2))]
dt[, c("province_part2", "string"):= transpose(stri_split_fixed(string, "_", 2))]
dt[, province:= paste(province, province_part2, sep="-")]
dt <- dt[, .(files_t1, province)]
dt[, province := gsub("-NTCP", "", province)]
dt[, province := gsub(" REPORT", "", province)]
dt[grepl("EAST", files_t1), province := paste0(province, "-est")]
dt[grepl("WEST", files_t1), province := paste0(province, "-ouest")]
dt[, province := tolower(province)]

dt <- dt[province %in% dps_names]
file_names <- copy(dt)
file_names <- file_names[files_t1 != "COD_KINSHASA_NTCP_REPORT_2016_T1_Y2017M06D06.XLS"]
# ----------------------------------------------


# ----------------------------------------------
## 
# ----------------------------------------------
year= '2016'
i = 1
f=file_names$files_t1[i]

for (f in file_names$files_t1[]){
  sheets <- getSheets(f, year)

  sheets <- sheets[grepl("DEP", sheets)]
  s <- sheets[grepl("T1", sheets)]  # should just be one sheet per loop cycle since just prepping 1 quarter
  
  if( f== "COD_HAUT_KATANGA_NTCP_REPORT_2016_T1_Y2017M06D06.XLSX") s= "TB S t1"
  if( f== "COD_HAUT_LOMAMI_NTCP_REPORT_2016_T1_Y2017M06D06.XLS") s = "DEP Trim 1 016"
  if( f== "COD_KASAI_CENTRAL_NTCP_REPORT_2016_T1_Y2017M06D06.XLS" | f== "COD_KASAI_NTCP_REPORT_2016_T1_Y2017M06D06.XLS") s = "DEP TRIM 1 2016"
  if( f== "COD_KONGO_CENTRAL_NTCP_REPORT_2016_T1_WEST_Y2017M06D06.XLSX" ) s= "DEP Q1 016"
  if( f== "COD_KWANGO_NTCP_REPORT_T1_Y2017M06D06.XLSX") s = "DEP 2016"
  if( f== "COD_KWILU_NTCP_REPORT_2016_T1_Y2017M06D06.XLSX") s = "DEPISTAGE 2016"
  
  dt <- data.table(read_excel(paste0(dir, year, "/", f), sheet= s)) 
  
  if( f== "COD_HAUT_UELE_NTCP_REPORT_2016_T1_Y2017M06D06.XLSX" | f== "COD_ITURI_NTCP_REPORT_2016_T1_Y2017M06D06.XLSX") {
    dt <- dt[, c(1:3, 25)]}
  
  if( f== "COD_BAS_UELE_NTCP_REPORT_2016_T1_Y2017M06D06.XLSX" | f== "COD_EQUATEUR_NTCP_REPORT_2016_T1_Y2017M06D06.XLSX" |
      f== "COD_KONGO_CENTRAL_NTCP_REPORT_2016_T1_EAST_Y2017M06D06.XLS"){
    dt <- dt[, c(2:4, 26)]}
  
  if( f== "COD_HAUT_KATANGA_NTCP_REPORT_2016_T1_Y2017M06D06.XLSX"){
    dt <- dt[, c(1:3, 22)]}
  
  if( f== "COD_HAUT_LOMAMI_NTCP_REPORT_2016_T1_Y2017M06D06.XLS"| f== "COD_KASAI_CENTRAL_NTCP_REPORT_2016_T1_Y2017M06D06.XLS" |
      f== "COD_KASAI_NTCP_REPORT_2016_T1_Y2017M06D06.XLS" | f== "COD_KONGO_CENTRAL_NTCP_REPORT_2016_T1_WEST_Y2017M06D06.XLSX"){
    dt <- dt[, c(1:3, 26)]}
  
  if( f== "COD_KASAI_ORIENTAL_NTCP_REPORT_2016_T1_Y2017M06D06.XLSX"){
    dt <- dt[, c(1:5, 27)]
    for (row in 8:nrow(dt)){
      if(dt[row, is.na(X__1)]) dt[row, X__1 := dt[row-1, X__1]]
    }
    dt$X__1 <- paste(dt$X__1, dt$X__2, sep= " ")
    dt <- dt[, c(1, 4:6)]}
  
  if (f== "COD_KINSHASA_NTCP_REPORT_2016_T1_Y2017M06D06.XLSX"){
    dt <- dt[, c(2:4, 25)]}
  
  if( f== "COD_KWANGO_NTCP_REPORT_T1_Y2017M06D06.XLSX"){
    dt <- dt[1:80, ]
    dt <- dt[, c(2:4, 21)]}
  
  if( f== "COD_KWILU_NTCP_REPORT_2016_T1_Y2017M06D06.XLSX"){
    dt <- dt[1:130, ]
    dt <- dt[, c(2:4, 21)]}
  
  colnames(dt) <- c("hz", "pop_tot", "pop_covered", "tot_case")
  
  # keep only rows of health zones
  dt$hz <- tolower(dt$hz)
  rows_to_keep <- grepl("zs", dt$hz)
  dt <- dt[rows_to_keep, ]
  
  dt[, dps:= file_names[files_t1 == f, province]]
  
  if (i==1){
    # if it's the first sheet, initialize the new dt
    cases <- dt
    # for subsequent sheets, rbind to that dt
  } else {
    cases <- rbindlist(list(cases, dt), use.names=TRUE, fill= TRUE)
  }
  print(i)
  i <- i + 1
}

cases$hz <- gsub("  ", " ", cases$hz)
cases <- cases[!grepl("^total zs$", cases$hz)]
cases <- cases[!grepl("^csdt/zs$", cases$hz)]
cases$quarter <- "1"
cases$year <- "2017"
# ----------------------------------------------





# ----- original code
setnames(dt, colnames(dt)[1], "col1")
setnames(dt, colnames(dt)[2], "col2")

index <- grep("CSDT", dt$col2)
index <- index[1]

# for haut-katanga, need this because subsetting to "index" removes these column names
cols_to_keep <- grep("des cas", dt[1,])
cols_to_keep <- cols_to_keep[1:2]

if (f== "COD_HAUT_KATANGA_NTCP_REPORT_2016_T1_Y2017M06D06.XLSX" | 
    f== "COD_HAUT_LOMAMI_NTCP_REPORT_2016_T1_Y2017M06D06.XLS") index <- grep("ZONES DE SANTE", dt$col1)

dt <- dt[-c(1:(index-1))]

# keep only certain columns:
colnames(dt) <- tolower(as.character(dt[1,]))

hz_col <- colnames(dt)[grepl("zs", colnames(dt))]
hz_col2 <- colnames(dt)[grepl("zones de sante", colnames(dt))]
pop_cols <- colnames(dt)[grepl("opulation", colnames(dt))]
cas_cols <- colnames(dt)[grepl("des cas", colnames(dt))]
other_cols <- NULL

if (f== "COD_HAUT_KATANGA_NTCP_REPORT_2016_T1_Y2017M06D06.XLSX") other_cols <- colnames(dt[, ..cols_to_keep])

dt <- dt[, c(hz_col, hz_col2, pop_cols, cas_cols, other_cols), with=FALSE]

colnames(dt)<- c("hz", "pop_total", "pop_covered", "total_inc_case", "total_all_case")


#---- another option: rename cols individually: (more manual way)
if( f== "COD_BAS_UELE_NTCP_REPORT_2016_T1_Y2017M06D06.XLSX" | f== "COD_EQUATEUR_NTCP_REPORT_2016_T1_Y2017M06D06.XLSX"){
  dt <- dt[, c(2:4, 14:26)]
  colnames(dt) <- c("hz", "pop_tot", "pop_covered", "tpp_new", "tpp_relapse", "tpp_notRelapse_trtFailure", "tpp_notRelapse_lostToFollowup",
                    "tpc_new", "tpc_relapse", "tpc_notRelapse", "tep_new", "tep_relapse", "tep_notRelapse", "treated_uncertain", "tot_incCase", "tot_case")}

if( f== "COD_HAUT_KATANGA_NTCP_REPORT_2016_T1_Y2017M06D06.XLSX"){
  dt <- dt[, c(1:3, 5:22)]
  colnames(dt) <- c("hz", "pop_tot", "pop_covered", "tpp_new", "tpp_relapse", "tpp_notRelapse_1", "tpp_notRelapse_2", "tpp_notRelapse_3",
                    "tpc_new", "tpc_relapse", "tpc_notRelapse_1", "tpc_notRelapse_2", "tpc_notRelapse_3",
                    "tep_new", "tep_relapse", "tep_notRelapse_1", "tep_notRelapse_2", "tep_notRelapse_3",
                    "treated_uncertain", "tot_incCase", "tot_case")}

if( f== "COD_HAUT_KATANGA_NTCP_REPORT_2016_T1_Y2017M06D06.XLSX"){
  dt <- dt[, c(1:3, 12:26)]
  colnames(dt) <- c("hz", "pop_tot", "pop_covered", "tpp_new", "tpp_relapse", "tpp_notRelapse_interruption", "tpp_notRelapse_trtFailure", 
                    "tpc_new", "tpc_relapse", "tpc_notRelapse_1", "tpc_notRelapse_2", "tpc_notRelapse_3",
                    "tep_new", "tep_relapse", "tep_notRelapse_1", "tep_notRelapse_2", "tep_notRelapse_3",
                    "treated_uncertain", "tot_incCase", "tot_case")}





