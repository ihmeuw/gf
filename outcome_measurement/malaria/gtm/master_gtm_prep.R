# ----------------------------------------------
  # Audrey Batzel
  #
  # 4/5/18
  # Master prep file for GTM Malaria supply chain data 
# ----------------------------------------------
  ###### Set up R / install packages  ###### 
# ----------------------------------------------
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


# ----------------------------------------------
###### Call directories and load the prep data  ###### 
# ----------------------------------------------
  # file path where the files are stored
  local_dir <- "J:/Project/Evaluation/GF/outcome_measurement/gtm"
  
  antimalarials <- "J:/Project/Evaluation/GF/outcome_measurement/gtm/MALARIA/Antimalaricos/"
  
  # ----------------------------------------------
  ######For loop that appends each data file to our databese  ###### 
# ----------------------------------------------
  file_list <- data.table(read.csv(paste0(antimalarials, "prep_file_list.csv")))
  file_list$start_date <- ymd(file_list$start_date)
  file_list$file_name <- as.character(file_list$file_name)
  for(i in 1:length(file_list$file_name)){
    tmpData <- prep_malaria_data(antimalarials, file_list$file_name[i], file_list$sheet[i]
                                 , ymd(file_list$start_date[i]), file_list$period[i])
    if(i==1){
      antimal_database <- tmpData
    } else {
      antimal_database <- rbind(tmpData, antimal_database)
    }
    print(i)
  }
  

# ----------------------------------------------
###### Call directories and load the prep data  ###### 
  
# ----------------------------------------------
  
# sheets within each file to bind together (months) for each year 
    sheetnames12 <- c("ENE12", "FEB12", "MAR2012", "ABR12", "MAY12", "JUL12", "AGO12", "SEPT12", "OCT12", "NOV12", "DIC12")
    sheetnames13 <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Octubre"," Noviembre")
    sheetnames14 <- c("ABR", "MAY", "JUN","JUL", "AGO", "SEP", "OCT", "NOV", "DIC")
    sheetnames15 <- c("OCT15", "NOV15", "DIC15." )
    sheetnames16 <- c("ENE16", "FEB16", "MAR16", "ABR16", "MAY16", "JUN16")
    sheetnames17 <- c("Enero 2017", "Febrero 2017", "Abril 2017", "Mayo 2017", "Junio 2017") 
    
  # July 2012 - December 2012 column names (some order is reversed, names the same)
    columnNames2 <- c("das_code", "das", "Cloroquina250_previousBalance", "Cloroquina250_topLevelEntries", "Cloroquina250_deliveredToUser", 
                     "Cloroquina250_notDeliveredToUser", "Cloroquina250_realDemand", "Cloroquina250_readjustments", "Cloroquina250_nextMonthBalance", 
                     "Cloroquina250_existenceInBodega", "Cloroquina250_avgMonthlyDemand", "Cloroquina250_monthsAvailable", "Primaquina15_previousBalance", 
                     "Primaquina15_topLevelEntries", "Primaquina15_deliveredToUser", "Primaquina15_notDeliveredToUser", "Primaquina15_realDemand", 
                     "Primaquina15_readjustments", "Primaquina15_nextMonthBalance", "Primaquina15_existenceInBodega", "Primaquina15_avgMonthlyDemand", 
                     "Primaquina15_monthsAvailable", "Primaquina5_previousBalance", "Primaquina5_topLevelEntries", "Primaquina5_deliveredToUser", 
                     "Primaquina5_notDeliveredToUser", "Primaquina5_realDemand", "Primaquina5_readjustments", "Primaquina5_nextMonthBalance", 
                     "Primaquina5_existenceInBodega", "Primaquina5_avgMonthlyDemand", "Primaquina5_monthsAvailable")
  
  # January 2012 - May 2012 column names
    columnNames1 <- c("das_code", "das", "Primaquina15_previousBalance", "Primaquina15_topLevelEntries", "Primaquina15_deliveredToUser", "Primaquina15_notDeliveredToUser",
                      "Primaquina15_realDemand", "Primaquina15_readjustments", "Primaquina15_nextMonthBalance", "Primaquina15_existenceInBodega", "Primaquina15_avgMonthlyDemand", 
                      "Primaquina15_monthsAvailable", "Primaquina5_previousBalance", "Primaquina5_topLevelEntries", "Primaquina5_deliveredToUser", "Primaquina5_notDeliveredToUser", 
                      "Primaquina5_realDemand", "Primaquina5_readjustments", "Primaquina5_nextMonthBalance", "Primaquina5_existenceInBodega", "Primaquina5_avgMonthlyDemand", 
                      "Primaquina5_monthsAvailable", "Cloroquina250_previousBalance", "Cloroquina250_topLevelEntries", "Cloroquina250_deliveredToUser", "Cloroquina250_notDeliveredToUser",
                      "Cloroquina250_realDemand", "Cloroquina250_readjustments", "Cloroquina250_nextMonthBalance", "Cloroquina250_existenceInBodega", "Cloroquina250_avgMonthlyDemand", 
                      "Cloroquina250_monthsAvailable" )
# ----------------------------------------------
    

# ----------------------------------------------
  # Test that the output has the right number of rows
    if (nrow(dt)!=228) stop('Output data has wrong number of rows!')
# ----------------------------------------------            
    
    
    
    