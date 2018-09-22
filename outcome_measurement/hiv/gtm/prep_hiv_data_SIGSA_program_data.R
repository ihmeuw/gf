# ----------------------------------------------
# Naomi Provost
# September 6, 2018
# Master code file for GTM HIV data cleaning - Program Data
# ----------------------------------------------
###### Set up R / install packages  ###### 
# ----------------------------------------------
rm(list=ls())
library(lubridate)
library(data.table)
library(readxl)
library(stats)
library(stringr)
library(rlang)
library(zoo)
library(raster)
library(rgeos)
library(ggplot2)
library(maptools)
library(RColorBrewer)

# ----------------------------------------------
#----------------------------------
# Set the directory to download the data
# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

#------------ Prep Data------------------------
#### Prep Data
# define main directory
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/HIV/SIGSA/')
prep_dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/prepped_data/')
outFile = paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/visualizations/SIGSA_hiv_test_program.pdf')
mapping_dir = paste0(root, '/Project/Evaluation/GF/mapping/gtm/')

program_sheet = "Program Data"
dt = data.table(read_excel(paste0(dir, "sigsa_file_list.xlsx"), sheet = program_sheet))
hiv_file_list = dt[title_name_english =="Screening"]

gf_data <- data.table(read_excel(paste0(dir,program_sheet, "/", hiv_file_list$file_name[5]), sheet=as.character(hiv_file_list$sheet_name[5])))

# tmpData <- prep_detailed_uga_budget(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]), 
#                                     file_list$start_date[i], file_list$qtr_number[i],
#                                     cashText, file_list$grant[i], 
#                                     file_list$disease[i], file_list$period[i],file_list$data_source[i])
# # 
# for(i in 1:length(hiv_file_list$file_name)){
#   gf_data <- data.table(read_excel(paste0(dir,program_sheet, "/", file_list$file_name[i]), sheet=as.character(file_list$sheet_name[i])))
# }
#   
                