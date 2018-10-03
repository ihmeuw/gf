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
hiv_file_list = dt[title_name_english == "UAI_treatment" | title_name_english == "Treatment"| title_name_english == "eligible_treatment"]

prep_treatment <- function(gf_data, start_date, file_type){
  # Format Date
  date_df <- as.data.frame(t(gf_data[1]))
  date_dt = as.data.frame(t(fill(date_df,V1)))
  gf_data[1] = date_dt
  
  #Format Table
  gf_data = fill(gf_data, X__1)
  
  #Remove "TOTAL" to clean data table
  uglyRow = gf_data[2,]
  indexes = grep("TOTAL", uglyRow, invert = TRUE)
  gf_data = as.data.frame(gf_data)
  gf_total = gf_data[,indexes]
  gf_total = gf_total[,-c(2, 4, length(gf_total))]
  gf_dt = as.data.table(gf_total)
  
  
  #Concatate first 2 rows to create unique identifer to melt on
  gf_names = gf_dt[1:2,]
  gf_names <- paste0(gf_names[1,], '-', gf_names[2,])
  colnames(gf_dt) <- gf_names
  
  # Remove first 2 rows (unique identifer is now column name) and melt
  gf_dt <- gf_dt[3:nrow(gf_dt),]
  colnames(gf_dt)[1] <- "Treatment"
  colnames(gf_dt)[2] <- "Group"
  dt_melt <- melt(gf_dt, id = c('Treatment', 'Group'))
  
  # rename and seperate the values, update date variable
  gf_data_cleaned = separate(dt_melt, variable, c("Date", "Facility"), sep = "-")
  
  if((file_type == 'UAI_treatment' & year(start_date) == 2015) | (file_type == "eligible_treatment" & year(start_date) == 2016)){
    gf_data_cleaned$Date = paste0(toupper(substr(gf_data_cleaned$Date, 1, 1)), tolower(substr(gf_data_cleaned$Date, 2, nchar(gf_data_cleaned$Date))))
    gf_data_cleaned$Date = paste0("01 ", gf_data_cleaned$Date, " ", year(start_date))
    gf_data_cleaned$Date = parse_date(gf_data_cleaned$Date,"%d %B %Y",locale=locale("es"))
    gf_data_cleaned$Date = as.Date(gf_data_cleaned$Date)
    
  }else{
    if(year(start_date) == 2014){
      gf_data_cleaned$Date = substr(gf_data_cleaned$Date, 6, nchar(gf_data_cleaned$Date))
      gf_data_cleaned$Date = paste0(toupper(substr(gf_data_cleaned$Date, 1, 1)), tolower(substr(gf_data_cleaned$Date, 2, nchar(gf_data_cleaned$Date))))
      gf_data_cleaned$Date = paste0("01 ", gf_data_cleaned$Date)
      gf_data_cleaned$Date = parse_date(gf_data_cleaned$Date,"%d %B %Y",locale=locale("es"))
      gf_data_cleaned$Date = as.Date(gf_data_cleaned$Date)
      
    }else if (start_date == "2015-01-01"){
      gf_data_cleaned$Date = paste0(toupper(substr(gf_data_cleaned$Date, 1, 1)), tolower(substr(gf_data_cleaned$Date, 2, nchar(gf_data_cleaned$Date))))
      gf_data_cleaned$Date = paste0("01 ", gf_data_cleaned$Date)
      gf_data_cleaned$Date = parse_date(gf_data_cleaned$Date,"%d %B %Y",locale=locale("es"))
      gf_data_cleaned$Date = as.Date(gf_data_cleaned$Date)
    }else{
      # Formats date for when numbers
      gf_data_cleaned$Date = as.Date(as.numeric(gf_data_cleaned$Date),  origin = "1899-12-30")
    }
  }
  
  
  gf_data_cleaned = gf_data_cleaned[toupper(Treatment) != "TOTAL"]
  gf_data_cleaned$Treatment = ifelse(gf_data_cleaned$Treatment == 'Total de abandonos / inactivos por diveresas causas (No muerte)', 'Total de abandonos/inactivos por diversas causas (no muerte)', 
                                     ifelse(gf_data_cleaned$Treatment == 'Pacientes en TAR Fallecidos en el mes del reporte', 'Pacientes en TAR fallecido en el mes del reporte',
                                            ifelse(gf_data_cleaned$Treatment == "Número de pacientes Referidos de otras Unidades de Atención Integral", "Número de pacientes trasladados a otras Unidades de Atención Integral", 
                                                   ifelse(gf_data_cleaned$Treatment == "Número de pacientes con VIH de la UAI que incian TARV","Número de pacientes con VIH de la UAI que inician TARV", 
                                                          ifelse(gf_data_cleaned$Treatment == "Número de pacientes trasladods a otras Unidades de Atencion Integral" | gf_data_cleaned$Treatment == "Número de pacientes Trasladados a otras Unidades de Atención Integral", "Número de pacientes trasladados a otras Unidades de Atención Integral",
                                                                 ifelse(gf_data_cleaned$Treatment == "Numero de pacientes con VIH de nuevo inreso a la UAI sin TARV","Número de pacientes con VIH de nuevo ingreso a la UAI sin TARV",
                                                                               ifelse(gf_data_cleaned$Treatment =="Total de pacientes con seguimiento clinico por VIH que han fallecido sin recibir TARV", "Total de pacientes con seguimiento clínico por VIH que han fallecido sin recibir TARV", 
                                                                                      ifelse(gf_data_cleaned$Treatment == "Total de pacientes que ya no asisten al seguimiento clínico por VIH y que no han iniciado TARV", "Total de pacientes que ya no asisten al seguimiento clinico por VIH y que no han iniciado TARV", gf_data_cleaned$Treatment))))))))
  
  gf_data_cleaned$value = as.numeric(gf_data_cleaned$value)
  
  return(gf_data_cleaned)
}


for(i in 1:length(hiv_file_list$file_name)){
  prep_data <- data.table(read_excel(paste0(dir,program_sheet, "/", hiv_file_list$file_name[i]), sheet=as.character(hiv_file_list$sheet_name[i]), col_names = FALSE, col_types = "text", skip = 1))
  tmpData = prep_treatment(prep_data, ymd(hiv_file_list$start_date[i]), hiv_file_list$title_name_english[i])
  
  if(i==1){
    resource_database = tmpData 
  } 
  if(i>1){
    resource_database = rbind(resource_database, tmpData, use.names=TRUE)}
}




# Write csv & RDS to folderpath
write.csv(resource_database, paste0(prep_dir, "hiv_sigsa_data_prepped_PD_treatment.csv"), row.names = FALSE, fileEncoding = "latin1")
saveRDS(resource_database, paste0(prep_dir, "hiv_sigsa_data_prepped_PD_treatment.rds"))

