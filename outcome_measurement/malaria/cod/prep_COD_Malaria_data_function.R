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
    
  # csv of file names for importing data
  PNLP_files <- read.csv(paste0(dir, "PNLP_file_names.csv"), fileEncoding = "latin1")
  
  # prep_data() function
  prep_data <-"./outcome_measurement/malaria/cod/prep_data.R"
  source(prep_data)

# output files 
  dir_prepped <-"J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/"
  # outputs the data as a file called: "PNLP_2010to2017_prepped.csv"
# ----------------------------------------------
  
  
# ----------------------------------------------
# Load data - to visualize what the raw data looks like in R, and to work through cleaning process to make sure function will work
  # example from older version of data files:
    # cod_mdata_KIN16 <- data.table(read_excel(paste0(dir,"/", PNLP_files$File.Names.[1], '.xls'), sheet= "KIN"))

  # new data example:
    #dt <- data.table(read_excel(paste0(dir, "/", PNLP_files$File.Names[9], ".xls"), sheet= 'KIN'))
# ----------------------------------------------

  
# ----------------------------------------------
# Use a loop to run prep_data() on each of the three data sheets for the three years
  # for which we have the data.
  
  # set-up variables needed:
  i <- 1
  index <- 4  # index is set to 4 because we will not include the first three files in the PNLP_files df
              # these are older versions of the 2014-2016 data.

# loop through each file in the df PNLP_files
 for(index in 4:11) {
   
   # this if statement just accounts for a different file type used in one of the excel sheets, and then
   # sets the sheetnames to be the sheetnames in each file
   if (index==11){
     sheetnames <- excel_sheets(paste0(dir, PNLP_files$File.Names[index], ".xlsx"))
     sheetnames = sheetnames[!sheetnames %in% 'INPUTRDC']
   } else{
     sheetnames <- excel_sheets(paste0(dir, PNLP_files$File.Names[index], ".xls"))
     sheetnames = sheetnames[!sheetnames %in% 'INPUTRDC']
   }

   # loop through each sheet in each file, to read the data into RStudio, again accounting for different file types in one of the sheets.
    for(s in sheetnames) {

      if (index==11){
          dt <- data.table(read_excel(paste0(dir, PNLP_files$File.Names[index], ".xlsx"), sheet= s))
      } else{
          dt <- data.table(read_excel(paste0(dir, PNLP_files$File.Names[index], ".xls"), sheet= s))
      }
      
      # this line of code is what actually runs the function to prep the current excel sheet of data
        currentSheet <- prep_data(dt, s, index)
     
      # the following two lines can be commented out, but they show where the code is breaking if there is a problem
      # and also the number of rows after the data has been prepped so that this can be checked against the total number
      # of observations that we would expect. 
        print(s)
        print(nrow(currentSheet))
        
      # to figure out which rows are missing from the prepped data that shouldn't be
      # ----------------------------------------------
        # if (s=="NK"|s=="MN"){
        #   healthzone<-dt[["X__1"]]
        # } else {
        #   healthzone<-dt[["X__2"]]
        # }
        # 
        # healthzone = unique(healthzone)
        # healthzone = tolower(healthzone)
        # healthzoneprep <- currentSheet[["health_zone"]]
        # healthzoneprep <- unique(healthzoneprep)
        # missing_hz <- healthzone[!healthzone %in% healthzoneprep]
        # print(missing_hz)
        # 
        # for (h in healthzoneprep){
        #   if ( nrow(currentSheet[health_zone==h, ]) != 12){
        #     print( h )
        #     print( nrow(currentSheet[health_zone==h,]) )
        #   }
        # }
        #
        # to compare missing health zones between two years
        # hz_missing_2011 <- hzNK2014[!hzNK2014 %in% hzNK2011]
        # print(hz_missing_2011)
        # hz_missing_2014 <- hzNK2011[!hzNK2011 %in% hzNK2014]
        # print(hz_missing_2014)
        # ----------------------------------------------
        
      # need if statement to distinguish first sheet, and then
      # add subsequent sheets to the first sheet with rbind()
      if (i==1) fullData <- currentSheet
      if (i>1) fullData <- rbind(fullData, currentSheet, fill=TRUE)
      i <- i+1
    }
  }
# ----------------------------------------------
  
  
# ----------------------------------------------
# Test that the output has the right number of rows
  if (nrow(fullData)!=49320) stop('Output data has wrong number of rows!')
# ----------------------------------------------

  
# ---------------------------------------------- 
  # export fullData
  write.csv(fullData, paste0(dir_prepped, "PNLP_2010to2017_prepped.csv"))
# ----------------------------------------------     
