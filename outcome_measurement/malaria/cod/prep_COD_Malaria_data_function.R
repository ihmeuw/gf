# ----------------------------------------------
# Audrey Batzel
#
# 3/6/18
# Prepping DRC PNLP 2014-2016 data for analaysis
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
# --------------------


# ----------------------------------------------
# Overview - Files and Directories

# data directory
# J:/Project/Evaluation/GF/outcome_measurement/cod/National_Malaria_Program/Malaria_Data_2014_to_2016

# input file
  # file path where the files are stored
  dir <- "J:/Project/Evaluation/GF/outcome_measurement/cod/National_Malaria_Program/Malaria_Data_2014_to_2016"
  
  PNLP_files <- read.csv(paste0(dir,"/","PNLP_file_names.csv"), fileEncoding = "latin1")
  # input files are from 2014, 2015, and 2016 and each contain three sheets for three different provinces

# output files 
  #(note: output to prepped_data folder within cod folder)
  # COD_PNLP_Data_2016 - prepped data.table object, one per year (?)
  # cod_malaria_dataset_master - appended version
# ----------------------------------------------
  
  
# ----------------------------------------------
# Load data
  
  cod_mdata_OR16 <- data.table(read_excel(paste0(dir,"/", PNLP_files$File.Names.[1], '.xls'), sheet= "KIN"))
  cod_mdata_BDD16 <- data.table(read_excel(paste0(dir, "/", PNLP_files$File.Names.[1], '.xls'), sheet= "BDD"))
  cod_mdata_OR16 <- data.table(read_excel(paste0(dir,"/", PNLP_files$File.Names.[1], '.xls'), sheet= "OR"))
  
      #should I combine first and then clean? or clean data and then combine into one dataset?
# ----------------------------------------------
  
  
# ----------------------------------------------  
# Make this all into a function to replicate it for each sheet
  
  # START OF FUNCTION:  
  prep_data <- function(year, sheetname){
    
    # load data sheet
    dataSheet <- data.table(read_excel(paste0(dir,"/Données_", year, "_PNLP.xls"), sheet= sheetname))
# ----------------------------------------------   
# ----------------------------------------------
# Set names of columns
    # faster way to do this : setnames(dataSheet, c('', '', '', ''))
    
  colnames(dataSheet)[1] <- "Province"
  colnames(dataSheet)[2] <- "DPS"
  colnames(dataSheet)[3] <- "Health_Zone"
  colnames(dataSheet)[4] <- "Donor"
  colnames(dataSheet)[5] <- "Operational_Support_Partner"
  colnames(dataSheet)[6] <- "Population"
  colnames(dataSheet)[7] <- "Trimester"  #should this be quarter?
  colnames(dataSheet)[8] <- "Month"
  colnames(dataSheet)[9] <- "New_cases_malaria_Under5"
  colnames(dataSheet)[10] <- "New_cases_malaria_5andOlder"
  # not sure what this one is colnames(dataSheet)[11] <- " "
  colnames(dataSheet)[12] <- "Hospitalized_cases_Under5"
  colnames(dataSheet)[13] <- "Hospitalized_cases_5andOlder"
  # not sure what this one is colnames(dataSheet)[14] <- " "
  colnames(dataSheet)[15] <- "Simple_Malaria_TreatedAccordingToNationalPolicy_Under5"
  colnames(dataSheet)[16] <- "Simple_Malaria_TreatedAccordingToNationalPolicy_5andOlder"
  # not sure what this one is colnames(dataSheet)[17] <- " "
  colnames(dataSheet)[18] <- "Serious_Malaria_TreatedAccordingToNationalPolicy_Under5"
  colnames(dataSheet)[19] <- "Serious_Malaria_TreatedAccordingToNationalPolicy_5andOlder"
  # not sure what this one is colnames(dataSheet)[20] <- " "
  colnames(dataSheet)[21] <- "Malaria_Deaths_Under5"
  colnames(dataSheet)[22] <- "Malaria_Deaths_5andOlder"
  # not sure what this one is colnames(dataSheet)[23] <- " "
  colnames(dataSheet)[24] <- "ANC_1st"
  colnames(dataSheet)[25] <- "ANC_2nd"
  colnames(dataSheet)[26] <- "ANC_3rd"
  colnames(dataSheet)[27] <- "ANC_4th"
  colnames(dataSheet)[28] <- "SP_1st"  #why was SP within ANC (CPN)orginally? Should it relate to that in some way?
  colnames(dataSheet)[29] <- "SP_2nd"
  colnames(dataSheet)[30] <- "SP_3rd"
  colnames(dataSheet)[31] <- "ITN_received"
  colnames(dataSheet)[32] <- "ITN_distAtANC"
  colnames(dataSheet)[33] <- "ITN_distAtPreschool"
  # Not sure about this one colnames(dataSheet)[34] <- "VAR"
  colnames(dataSheet)[35] <- "ACT_2to11mos"
  colnames(dataSheet)[36] <- "ACT_1to5yrs"
  colnames(dataSheet)[37] <- "ACT_6to13yrs"
  colnames(dataSheet)[38] <- "ACT_14yrsAndOlder"
  colnames(dataSheet)[39] <- "ACT_total"
  colnames(dataSheet)[40] <- "ArtLum_receieved" #or is this requested?
  colnames(dataSheet)[41] <- "ArtLum_used"
  # translation for goutte epaisse? colnames(dataSheet)[42] <- "_completed_Under5""
  # colnames(dataSheet)[43] <- "_completed_5andOlder"
  # colnames(dataSheet)[44] <- "_positive_Under5"
  # colnames(dataSheet)[45] <- "_positive_5andOlder""
  colnames(dataSheet)[46] <- "RDT_completed_Under5"
  colnames(dataSheet)[47] <- "RDT_completed_5andOlder"
  colnames(dataSheet)[48] <- "RDT_positive_Under5"
  colnames(dataSheet)[49] <- "RDT_positive_5andOlder"
  colnames(dataSheet)[50] <- "Num_Reports_Received" # are these columns
  colnames(dataSheet)[51] <- "Num_Repots_Expected"  # needed - they are the same
  colnames(dataSheet)[52] <- "Num_Sanitary_Structures"
  colnames(dataSheet)[53] <- "Sanitary_Structures_NumTransmitted" # translation?
  colnames(dataSheet)[54] <- "Sanitary_Structures_NumTransmittedWithinDeadline" # translation?
        # different names for these? "transmitted"? is that correct?
# ----------------------------------------------
# ----------------------------------------------
# Get rid of rows you don't need- "subset"
  
  # delete first row if it's first value is "NA" or "PROVINCE" as a way to
    # only delete those unnecessary rows, and not any others.
  
  if ((is.na(dataSheet[1,"Province"])) && (dataSheet[2,"Province"] == "PROVINCE")){
    dataSheet <- dataSheet[-c(1, 2),]
  }
  # should I also remove the last row of totals?
# ----------------------------------------------
# ----------------------------------------------
# Export the data - to test
  
  # return current data sheet
  return(dataSheet)
  
  #COD_PNLP_Data_2016 <- dataSheet
  # write.csv(dataSheet, paste0("J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/COD_PNLP_", year, "_", sheetname, ".csv"))
      #to test, wrote to my own drive: write.csv(dataSheet, "C:/Users/abatzel/Documents/PCE/dataSheet.csv")
# ----------------------------------------------
}  #END OF FUNCTION
# ----------------------------------------------
  
  
# ----------------------------------------------
# Use a loop to run prep_data() on each of the three sheets for PNLP 2016 data
  years <- seq(2014, 2016)
  sheetnames <- c('KIN', 'BDD', 'OR')
  i <- 1
  for(y in years) { 
    for(s in sheetnames) { 
        currentSheet <- prep_data(y, s)
        
        if (i==1) fullData <- currentSheet
        if (i>1) fullData <- rbind(fullData, currentSheet)
        i <- i+1
      }  
  }
  
  prep_data(2016, 'KIN')
  prep_data(cod_mdata_BDD16)
# ----------------------------------------------  
  
  
# ----------------------------------------------
# Clean values any missing, or format numbers such as 1,000 as 1000 
    # (make it a number not a string)

# Reshape data

# Append provinces within years, and then all years together

# ----------------------------------------------
