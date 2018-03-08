# ----------------------------------------------
# Audrey Batzel
#
# 3/6/18
# Prepping DRC PNLP data for analaysis.
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
  
# output files 
  #(note: output to prepped_data folder within cod folder)
  # cod_malaria_dataset_(year) - prepped data.table object, one per year (?)
  # cod_malaria_dataset_master - appended version
# ----------------------------------------------


# ----------------------------------------------
# Load data
  
  cod_mdata_KIN16 <- data.table(read_excel(paste0(dir,"/", PNLP_files$File.Names.[1], '.xls'), sheet= "KIN"))
  cod_mdata_BDD16 <- data.table(read_excel(paste0(dir, "/", PNLP_files$File.Names.[1], '.xls'), sheet= "BDD"))
  cod_mdata_OR16 <- data.table(read_excel(paste0(dir,"/", PNLP_files$File.Names.[1], '.xls'), sheet= "OR"))
  
      #should I combine first and then clean? or clean data and then combine into one dataset?
# ----------------------------------------------


# ----------------------------------------------
# Set names of columns

  colnames(cod_mdata_OR16)[1] <- "Province"
  colnames(cod_mdata_OR16)[2] <- "DPS"
  colnames(cod_mdata_OR16)[3] <- "Health_Zone"
  colnames(cod_mdata_OR16)[4] <- "Donor"
  colnames(cod_mdata_OR16)[5] <- "Operational_Support_Partner"
  colnames(cod_mdata_OR16)[6] <- "Population"
  colnames(cod_mdata_OR16)[7] <- "Trimester"  #should this be quarter?
  colnames(cod_mdata_OR16)[8] <- "Month"
  colnames(cod_mdata_OR16)[9] <- "New_cases_malaria_Under5"
  colnames(cod_mdata_OR16)[10] <- "New_cases_malaria_5andOlder"
  # not sure what this one is colnames(cod_mdata_OR16)[11] <- " "
  colnames(cod_mdata_OR16)[12] <- "Hospitalized_cases_Under5"
  colnames(cod_mdata_OR16)[13] <- "Hospitalized_cases_5andOlder"
  # not sure what this one is colnames(cod_mdata_OR16)[14] <- " "
  colnames(cod_mdata_OR16)[15] <- "Simple_Malaria_TreatedAccordingToNationalPolicy_Under5"
  colnames(cod_mdata_OR16)[16] <- "Simple_Malaria_TreatedAccordingToNationalPolicy_5andOlder"
  # not sure what this one is colnames(cod_mdata_OR16)[17] <- " "
  colnames(cod_mdata_OR16)[18] <- "Serious_Malaria_TreatedAccordingToNationalPolicy_Under5"
  colnames(cod_mdata_OR16)[19] <- "Serious_Malaria_TreatedAccordingToNationalPolicy_5andOlder"
  # not sure what this one is colnames(cod_mdata_OR16)[20] <- " "
  colnames(cod_mdata_OR16)[21] <- "Malaria_Deaths_Under5"
  colnames(cod_mdata_OR16)[22] <- "Malaria_Deaths_5andOlder"
  # not sure what this one is colnames(cod_mdata_OR16)[23] <- " "
  colnames(cod_mdata_OR16)[24] <- "ANC_1st"
  colnames(cod_mdata_OR16)[25] <- "ANC_2nd"
  colnames(cod_mdata_OR16)[26] <- "ANC_3rd"
  colnames(cod_mdata_OR16)[27] <- "ANC_4th"
  colnames(cod_mdata_OR16)[28] <- "SP_1st"  #why was SP within ANC (CPN)orginally? Should it relate to that in some way?
  colnames(cod_mdata_OR16)[29] <- "SP_2nd"
  colnames(cod_mdata_OR16)[30] <- "SP_3rd"
  colnames(cod_mdata_OR16)[31] <- "ITN_received"
  colnames(cod_mdata_OR16)[32] <- "ITN_distAtANC"
  colnames(cod_mdata_OR16)[33] <- "ITN_distAtPreschool"
  # Not sure about this one colnames(cod_mdata_OR16)[34] <- "VAR"
  colnames(cod_mdata_OR16)[35] <- "ACT_2to11mos"
  colnames(cod_mdata_OR16)[36] <- "ACT_1to5yrs"
  colnames(cod_mdata_OR16)[37] <- "ACT_6to13yrs"
  colnames(cod_mdata_OR16)[38] <- "ACT_14yrsAndOlder"
  colnames(cod_mdata_OR16)[39] <- "ACT_total"
  colnames(cod_mdata_OR16)[40] <- "ArtLum_receieved" #or is this requested?
  colnames(cod_mdata_OR16)[41] <- "ArtLum_used"
  # translation for goutte epaisse? colnames(cod_mdata_OR16)[42] <- "_completed_Under5""
  # colnames(cod_mdata_OR16)[43] <- "_completed_5andOlder"
  # colnames(cod_mdata_OR16)[44] <- "_positive_Under5"
  # colnames(cod_mdata_OR16)[45] <- "_positive_5andOlder""
  colnames(cod_mdata_OR16)[46] <- "RDT_completed_Under5"
  colnames(cod_mdata_OR16)[47] <- "RDT_completed_5andOlder"
  colnames(cod_mdata_OR16)[48] <- "RDT_positive_Under5"
  colnames(cod_mdata_OR16)[49] <- "RDT_positive_5andOlder"
  colnames(cod_mdata_OR16)[50] <- "Num_Reports_Received" # are these columns
  colnames(cod_mdata_OR16)[51] <- "Num_Repots_Expected"  # needed - they are the same
  colnames(cod_mdata_OR16)[52] <- "Num_Sanitary_Structures"
  colnames(cod_mdata_OR16)[53] <- "Sanitary_Structures_NumTransmitted" # translation?
  colnames(cod_mdata_OR16)[54] <- "Sanitary_Structures_NumTransmittedWithinDeadline" # translation?
        # different names for these? "transmitted"? is that correct?
# ----------------------------------------------


# ----------------------------------------------
# Get rid of rows you don't need- "subset"
  
  # delete first row if it's first value is "NA" or "PROVINCE" as a way to
    # only delete those unnecessary rows, and not any others.
  
  if ((is.na(cod_mdata_OR16[1,"Province"])) && (cod_mdata_OR16[2,"Province"] == "PROVINCE")){
    cod_mdata_OR16 <- cod_mdata_OR16[-c(1, 2),]
  }
# ----------------------------------------------


# ----------------------------------------------
# Export the data - to test

  write.csv(cod_mdata_OR16, "J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/cod_mdata_OR16.csv")
      #to test, wrote to my own drive: write.csv(cod_mdata_OR16, "C:/Users/abatzel/Documents/PCE/cod_mdata_OR16.csv")
# ----------------------------------------------


# ----------------------------------------------
# Clean values any missing, or format numbers such as 1,000 as 1000 
    # (make it a number not a string)


# ----------------------------------------------


# ----------------------------------------------
# Reshape data

# Make this all into a function to replicate it for each sheet within
    # excel doc

# Append provinces within years, and then all years together

# ----------------------------------------------
