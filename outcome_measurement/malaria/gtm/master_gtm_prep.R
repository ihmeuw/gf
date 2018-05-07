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
  local_dir <- "J:/Project/Evaluation/GF/outcome_measurement/gtm/"
  
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
  
antimal_database$amount <- as.numeric(antimal_database$amount)


get_antimalarial_types <- function(antimal){
  x <- "Antimoniato 10 ml"
  if(grepl("Cloroquina", antimal)){
    x <- "Cloroquina 250 mg"
  } else if (grepl("Primaquina15", antimal)){
    x <- "Primaquina 15 mg"
  } else if (grepl("Primaquina5",antimal)){
    x <- "Primaquina 5 mg"
  } else {
    x <- x
  }
  return(x)
}



antimal_database$drug_type <- mapply(get_antimalarial_types, antimal_database$antimalarial_input)

antimal_database$department <- gsub("\\*", "",antimal_database$department) 
antimal_database$department <- trimws(antimal_database$department, "r")
antimal_database[department=="Guate Nor Occidente"]$department <- "Guatemala Nor Occidente"
write.csv(antimal_database, paste0(local_dir, "prepped_data/", "antimalarial_prepped_data.csv"))

  