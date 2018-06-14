# ----------------------------------------------
# Audrey Batzel
#
# 3/6/18
# Prepping DRC PNLP 2014-2016 data for analaysis
# 5/1/18 
# Adding in new data from DRC PNLP that spans 2010-2017 and all provinces
# The current working directory should be set to the root of the repository
  setwd('C:/local/gf/')
# ----------------------------------------------


# --------------------
# Set up R / install packages
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
# --------------------


# ----------------------------------------------
# Overview - Files and Directories

# data directory
  # file path where the files are stored
  dir <- "J:/Project/Evaluation/GF/outcome_measurement/cod/National_Malaria_Program/"
  dir_prepped <-"J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP"
    
  # csv of file names for importing
  PNLP_files <- read.csv(paste0(dir, "PNLP_file_names.csv"), fileEncoding = "latin1")
  
  # prep_data() function
  prep_data <-"./outcome_measurement/malaria/cod/prep_data.R"
  source(prep_data)

# output files 
  # (output to prepped_data folder within cod folder)
  
  # file path: J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/", fileName , ".csv"))
  
  # file names: 
    # COD_PNLP_Data_Indicators_long - prepped data.table object, appended data from 2014-2016
    # COD_PNLP_Data_Indicators_wide
    # COD_PNLP_Data_Interventions_long - prepped data.table object, appended data from 2014-2016
    # COD_PNLP_Data_Interventions_wide
# ----------------------------------------------
  
  
# ----------------------------------------------
# Load data - to visualize and work through cleaning to make sure function will work
  # example
  # cod_mdata_KIN16 <- data.table(read_excel(paste0(dir,"/", PNLP_files$File.Names.[1], '.xls'), sheet= "KIN"))

  # new data example:
    #dt <- data.table(read_excel(paste0(dir, "/", PNLP_files$File.Names[9], ".xls"), sheet= 'KIN'))
# ----------------------------------------------

  
# ----------------------------------------------
# Use a loop to run prep_data() on each of the three data sheets for the three years
  # for which we have the data.
  
  # variables needed:
  i <- 1
  index <- 4
  
 for(index in 4:11) {
   
   if (index==11){
     sheetnames <- excel_sheets(paste0(dir, PNLP_files$File.Names[index], ".xlsx"))
     sheetnames = sheetnames[!sheetnames %in% 'INPUTRDC']
   } else{
     sheetnames <- excel_sheets(paste0(dir, PNLP_files$File.Names[index], ".xls"))
     sheetnames = sheetnames[!sheetnames %in% 'INPUTRDC']
   }

    for(s in sheetnames) {

      if (index==11){
          dt <- data.table(read_excel(paste0(dir, PNLP_files$File.Names[index], ".xlsx"), sheet= s))
      } else{
          dt <- data.table(read_excel(paste0(dir, PNLP_files$File.Names[index], ".xls"), sheet= s))
      }
      
        currentSheet <- prep_data(dt, s, index)
     
      # to show where it is breaking if there is an error
        print(s)
        print(nrow(currentSheet))
        
      # to figure out which rows are missing from the prepped data that shouldn't be
        if (s=="NK"|s=="MN"){
          healthzone<-dt[["X__1"]]
        } else {
          healthzone<-dt[["X__2"]]
        }
        
        healthzone = unique(healthzone)
        healthzone = tolower(healthzone)
        healthzoneprep <- currentSheet[["health_zone"]]
        healthzoneprep <- unique(healthzoneprep)
        missing_hz <- healthzone[!healthzone %in% healthzoneprep]
        print(missing_hz)

        for (h in healthzoneprep){
          if ( nrow(currentSheet[health_zone==h, ]) != 12){
            print( h )
            print( nrow(currentSheet[health_zone==h,]) )
          }
        }
        
        # to compare missing health zones between two years
        # hz_missing_2011 <- hzNK2014[!hzNK2014 %in% hzNK2011]
        # print(hz_missing_2011)
        # hz_missing_2014 <- hzNK2011[!hzNK2011 %in% hzNK2014]
        # print(hz_missing_2014)
        
      # need if statement to distinguish first sheet, and then
      # add to the first sheet with subsequent ones with rbind()
      if (i==1) fullData <- currentSheet
      if (i>1) fullData <- rbind(fullData, currentSheet, fill=TRUE)
      i <- i+1
    }
   print("Checking to see if date is NA anywhere...")
   print(fullData[is.na(date), c(1:8)])
  }
# ----------------------------------------------
  
  
# ----------------------------------------------
# Test that the output has the right number of rows
  if (nrow(fullData)!=49320) stop('Output data has wrong number of rows!')
# ----------------------------------------------

  
# ----------------------------------------------
# compare health zones and dps between years to make sure names are consistent
# ---------------------------------------------- 
  hz2010 <- unique(fullData[year==2010, health_zone])
  hz2011 <- unique(fullData[year==2011, health_zone])
  hz2012 <- unique(fullData[year==2012, health_zone])
  hz2013 <- unique(fullData[year==2013, health_zone])
  hz2014 <- unique(fullData[year==2014, health_zone])
  hz2015 <- unique(fullData[year==2015, health_zone])
  hz2016 <- unique(fullData[year==2016, health_zone])
  hz2017 <- unique(fullData[year==2017, health_zone])
  
  hz_missing <- hz2017[!hz2017 %in% hz2016]
  print(hz_missing)
  hz_missing_2017 <- hz2016[!hz2016 %in% hz2017]
  print(hz_missing_2017)
  

# ----------------------------------------------  
# ----------------------------------------------  
  fullData$dps <- tolower(fullData$dps)
  
  dps2010 <- unique(fullData[year==2010, dps])
  dps2011 <- unique(fullData[year==2011, dps])
  dps2012 <- unique(fullData[year==2012, dps])
  dps2013 <- unique(fullData[year==2013, dps])
  dps2014 <- unique(fullData[year==2014, dps])
  dps2015 <- unique(fullData[year==2015, dps])
  dps2016 <- unique(fullData[year==2016, dps])
  dps2017 <- unique(fullData[year==2017, dps])
  
  dps_missing <- dps2017[!dps2017 %in% dps2010]
  print(dps_missing)
  dps_missing_2017 <- dps2010[!dps2010 %in% dps2017]
  print(dps_missing_2017)
  
# ----------------------------------------------   
  
  
  
  #unique(fullData[health_zone=="Kirotshe", year])
# ---------------------------------------------- 
  # export fullData
  write.csv(fullData, paste0("J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP_2010to2017_prepped.csv"))
# ----------------------------------------------     
