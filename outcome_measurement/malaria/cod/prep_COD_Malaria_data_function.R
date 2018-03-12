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
  library(tidyr)
  library(dplyr)
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
# Load data - to visualize before cleaning
  
  cod_mdata_KIN16 <- data.table(read_excel(paste0(dir,"/", PNLP_files$File.Names.[1], '.xls'), sheet= "KIN"))
  cod_mdata_BDD16 <- data.table(read_excel(paste0(dir, "/", PNLP_files$File.Names.[1], '.xls'), sheet= "BDD"))
  cod_mdata_OR16 <- data.table(read_excel(paste0(dir,"/", PNLP_files$File.Names.[1], '.xls'), sheet= "OR"))
  cod_mdata_KIN15 <- data.table(read_excel(paste0(dir,"/", PNLP_files$File.Names.[2], '.xls'), sheet= "KIN"))
  cod_mdata_BDD15 <- data.table(read_excel(paste0(dir, "/", PNLP_files$File.Names.[2], '.xls'), sheet= "BDD"))
  cod_mdata_OR15 <- data.table(read_excel(paste0(dir,"/", PNLP_files$File.Names.[2], '.xls'), sheet= "OR"))
  cod_mdata_KIN14 <- data.table(read_excel(paste0(dir,"/", PNLP_files$File.Names.[3], '.xls'), sheet= "KIN"))
  cod_mdata_BDD14 <- data.table(read_excel(paste0(dir, "/", PNLP_files$File.Names.[3], '.xls'), sheet= "BDD"))
  cod_mdata_OR14 <- data.table(read_excel(paste0(dir,"/", PNLP_files$File.Names.[3], '.xls'), sheet= "OR"))
# ----------------------------------------------
  
  
# ----------------------------------------------  
  # START OF FUNCTION:  
  prep_data <- function(year, sheetname){
    
  # Load data sheet
    dataSheet <- data.table(read_excel(paste0(dir,"/Données_", year, "_PNLP.xls"), sheet= sheetname))
# ----------------------------------------------   
# ----------------------------------------------
  # Set names of columns
    # faster way to do this : setnames(dataSheet, c('', '', '', ''))
    # since some of them have less columns, make a vector of names and then have "if ___, then drop these ___"
  colnames(dataSheet)[1] <- "province"
  colnames(dataSheet)[2] <- "dps"
  colnames(dataSheet)[3] <- "health_zone"
  colnames(dataSheet)[4] <- "donor"
  colnames(dataSheet)[5] <- "operational_support_partner"
  colnames(dataSheet)[6] <- "population"
  colnames(dataSheet)[7] <- "trimester"  #should this be quarter?
  colnames(dataSheet)[8] <- "month"
  colnames(dataSheet)[9] <- "new_cases_malaria_under5"
  colnames(dataSheet)[10] <- "new_cases_malaria_5andOlder"
  colnames(dataSheet)[11] <- "new_cases_malaria_pregnantWomen"
  colnames(dataSheet)[12] <- "hospitalized_cases_under5"
  colnames(dataSheet)[13] <- "hospitalized_cases_5andOlder"
  colnames(dataSheet)[14] <- "hospitalized_cases_pregnantWomen"
  colnames(dataSheet)[15] <- "simple_malaria_treatedAccordingToNationalPolicy_under5"
  colnames(dataSheet)[16] <- "simple_malaria_treatedAccordingToNationalPolicy_5andOlder"
  colnames(dataSheet)[17] <- "simple_malaria_treatedAccordingToNationalPolicy_pregnantWomen"
  colnames(dataSheet)[18] <- "serious_malaria_treatedAccordingToNationalPolicy_under5"
  colnames(dataSheet)[19] <- "serious_malaria_treatedAccordingToNationalPolicy_5andOlder"
  colnames(dataSheet)[20] <- "serious_malaria_treatedAccordingToNationalPolicy_pregnantWomen"
  colnames(dataSheet)[21] <- "malaria_deaths_under5"
  colnames(dataSheet)[22] <- "malaria_deaths_5andOlder"
  colnames(dataSheet)[23] <- "malaria_deaths_pregnantWomen"
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
  colnames(dataSheet)[34] <- "VAR"   #translation?
  colnames(dataSheet)[35] <- "ACT_2to11mos"
  colnames(dataSheet)[36] <- "ACT_1to5yrs"
  colnames(dataSheet)[37] <- "ACT_6to13yrs"
  colnames(dataSheet)[38] <- "ACT_14yrsAndOlder"
  colnames(dataSheet)[39] <- "ACT_total"
  colnames(dataSheet)[40] <- "ArtLum_receieved" #or is this requested?
  colnames(dataSheet)[41] <- "ArtLum_used"
  colnames(dataSheet)[42] <- "goutte_epaisse_completed_under5"  #translation?
  colnames(dataSheet)[43] <- "goutte_epaisse_completed_5andOlder"   #translation?
  colnames(dataSheet)[44] <- "goutte_epaisse_positive_under5"  #translation?
  colnames(dataSheet)[45] <- "goutte_epaisse_positive_5andOlder"  #translation?
  colnames(dataSheet)[46] <- "RDT_completed_under5"
  colnames(dataSheet)[47] <- "RDT_completed_5andOlder"
  colnames(dataSheet)[48] <- "RDT_positive_under5"
  colnames(dataSheet)[49] <- "RDT_positive_5andOlder"
  colnames(dataSheet)[50] <- "num_reports_received" 
  colnames(dataSheet)[51] <- "num_repots_expected" 
  colnames(dataSheet)[52] <- "num_sanitary_structures"
  colnames(dataSheet)[53] <- "sanitary_structures_numTransmitted" # translation?
  colnames(dataSheet)[54] <- "sanitary_structures_numTransmittedWithinDeadline" # translation?
  # add a column for the "year" to keep track of this variable as we add dataSheets to this one
  dataSheet$year <- year
# ----------------------------------------------
# ----------------------------------------------
  # Get rid of rows you don't need- "subset"
  
  # delete first row if it's first value is "NA" or "PROVINCE" as a way to
    # only delete those unnecessary rows, and not any others accidentally.
  
   if ((is.na(dataSheet[1,"province"])) && (dataSheet[2,"province"] == "PROVINCE")){
   dataSheet <-dataSheet[-c(1, 2),] }
  
  # using this to delete rows tacked on to the end of the DF with all NA values
    # by checking to see if 2nd column is NA
    dataSheet <- dataSheet[complete.cases(dataSheet[ , 2]),]
  
  # using this to delete "totals" rows which are present in all data sheets
    # except for 2016_BDD
    # if (year !=2016 && sheetname !="BDD"){
    #   # remove last row
    #   numRows <- ((dim(dataSheet))[1])
    #   dataSheet <- dataSheet[(numRows-1),]
    # }
# ----------------------------------------------
# ----------------------------------------------
  # Return current data sheet
    return(dataSheet)
# ----------------------------------------------
}  #END OF FUNCTION
# ----------------------------------------------
  
  
# ----------------------------------------------
# Use a loop to run prep_data() on each of the three data sheets for the three years.
  # variables needed:
  years <- c(2015, 2016)
  sheetnames <- c('KIN', 'BDD', 'OR')
  i <- 1

# currently there is a problem in the earlier years matching the number of columns 
# - will fix this with setting column names and then append them all together.
# add a column for "year"
  
 for(y in years) {
    for(s in sheetnames) {
      #to show where it is breaking if there is an error
      print(y)
      print(s)

      currentSheet <- prep_data(y, s)

      # need if statement to distinguish first sheet, and then
      # add to the first sheet with subsequent ones with rbind()
      if (i==1) fullData <- currentSheet
      if (i>1) fullData <- rbind(fullData, currentSheet, fill=TRUE)
      i <- i+1
    }
  }
# ----------------------------------------------  
  
  
# ----------------------------------------------
  # Export the cleaned data
  # COD_PNLP_Data_2016 <- dataSheet
  # write.csv(dataSheet, paste0("J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/COD_PNLP_", year, "_", sheetname, ".csv"))
# ----------------------------------------------  
  
  
# ----------------------------------------------
# TO DO:
  # Add to function:
    # Problem with less variables for 
    # Clean values any missing, or format numbers such as 1,000 as 1000
    # (make it a number not a string)

    # Reshape data

    # Append provinces within years, and then all years together - add column for year
# ----------------------------------------------
