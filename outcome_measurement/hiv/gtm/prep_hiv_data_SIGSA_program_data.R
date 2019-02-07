
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
library(tidyr)
library(readr)

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

fix_diacritics <- function(x) {
  replacement_chars = list('S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                           'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                           'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                           'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                           'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y')
  
  replace_me <- paste(names(replacement_chars), collapse='')
  replace_with <- paste(replacement_chars, collapse = '')
  return(chartr(replace_me, replace_with, x))
  
}

prep_screening <- function(gf_data, start_date){
  # Format Date
  date_df <- as.data.frame(t(gf_data[1]))
  date_dt = as.data.frame(t(fill(date_df,V1)))
  gf_data[1] = date_dt
  
  #Format Risk Group
  risk_df <- as.data.frame(t(gf_data[2]))
  risk_dt = as.data.frame(t(fill(risk_df,V1)))
  gf_data[2] = risk_dt
  
  gf_data = fill(gf_data, X__1)
  
  #Remove "TOTAL" to clean data table
  uglyRow = gf_data[1,]
  indexes = grep("TOTAL", uglyRow, invert = TRUE)
  gf_data = as.data.frame(gf_data)
  gf_total = gf_data[,indexes]
  gf_dt = as.data.table(gf_total)
  
  
  #Concatate first 3 rows to create unique identifer to melt on
  gf_names = gf_dt[1:3,]
  gf_names <- paste0(gf_names[1,], '-', gf_names[2,],'-', gf_names[3,])
  colnames(gf_dt) <- gf_names
  
  # Remove first 3 rows (unique identifer is now column name) and melt
  gf_dt <- gf_dt[4:nrow(gf_dt),]
  dt_melt <- melt(gf_dt, id = c('NA-NA-DAS', 'NA-NA-SERVICIO', 'NA-NA-DISTRITO'))
  
  # rename and seperate the values, update date variable
  colnames(dt_melt) <- c("DAS", 'SERVICIO', "DISTRITO", 'variable',"value")
  gf_data_cleaned = separate(dt_melt, variable, c("Date", "Group", "Status"), sep = "-")
  
  # The UAI table on the bottom of the page is formatted so these values show up-- removing them.
  gf_data_cleaned = gf_data_cleaned[gf_data_cleaned$DISTRITO!="TOTAL" & !gf_data_cleaned$value %in% c("ACEP","MTS", "PRE", "TRANS", "POST", "HSH", "VIH+", "P.VIH", "CONF."), ]
  gf_data_cleaned$value = as.numeric(gf_data_cleaned$value)
  
  gf_data_cleaned[,DISTRITO := gsub('·', "", DISTRITO)]
  gf_data_cleaned[,DISTRITO := gsub('\\.', "", DISTRITO)]
  gf_data_cleaned[,DISTRITO:= iconv(DISTRITO)]
  gf_data_cleaned[,DISTRITO:= str_squish(DISTRITO)]
  
  gf_data_cleaned$DISTRITO = fix_diacritics(gf_data_cleaned$DISTRITO)
  gf_data_cleaned$DISTRITO = toupper(trimws(gf_data_cleaned$DISTRITO))
  #gf_data_cleaned[,DISTRITO := gsub("CS DE", "",DISTRITO)]
  #gf_data_cleaned[,DISTRITO := gsub("CS", "",DISTRITO)]
  #gf_data_cleaned[,DISTRITO := gsub("CAP", "",DISTRITO)]
  #gf_data_cleaned[,DISTRITO := gsub("UAI DE", "",DISTRITO)]
  #gf_data_cleaned[,DISTRITO := gsub("UAI", "",DISTRITO)]
  

  # # 
  if(year(start_date) != 2014 & start_date != "2015-01-01"){
    gf_data_cleaned$Date = as.Date(as.numeric(gf_data_cleaned$Date),  origin = "1899-12-30")
  }else{ #FIX THIS for dates 2014 and the beginning of 2015
    gf_data_cleaned$Date = paste0(toupper(substr(gf_data_cleaned$Date, 1, 1)), tolower(substr(gf_data_cleaned$Date, 2, nchar(gf_data_cleaned$Date))))
    gf_data_cleaned$Date = paste0("01 ", gf_data_cleaned$Date)
    gf_data_cleaned$Date = parse_date(gf_data_cleaned$Date,"%d %B %y",locale=locale("es"))
    gf_data_cleaned$Date = as.Date(gf_data_cleaned$Date)    
  }
  
  return(gf_data_cleaned)
}

for(i in 1:length(hiv_file_list$file_name)){
  #iterate through file list
  prep_data <- data.table(read_excel(paste0(dir,program_sheet, "/", hiv_file_list$file_name[i]), sheet=as.character(hiv_file_list$sheet_name[i]), col_names = FALSE, col_types = "text", skip = 4))
  tmpData = prep_screening(prep_data, ymd(hiv_file_list$start_date[i]))
  if(i==1){
    resource_database = tmpData 
  } 
  if(i>1){
    resource_database = rbind(resource_database, tmpData, use.names=TRUE)}
}
 
# i = 2
# gf_data <- data.table(read_excel(paste0(dir,program_sheet, "/", hiv_file_list$file_name[i]), sheet=as.character(hiv_file_list$sheet_name[i]), col_names = FALSE, col_types = "text", skip = 4))
# start_date = hiv_file_list$start_date[i]



# Write csv & RDS to folderpath
write.csv(resource_database, paste0(prep_dir, "hiv_sigsa_data_prepped_PD_testing.csv"), row.names = FALSE, fileEncoding = "latin1" )
saveRDS(resource_database, paste0(prep_dir, "hiv_sigsa_data_prepped_PD_testing.rds"))

