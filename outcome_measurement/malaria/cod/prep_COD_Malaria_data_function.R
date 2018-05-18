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
  years <- c(2010:2017)

  i <- 1
  index <- 4
  
 for(index in 4:11) {
   
   if (index==11){
     sheetnames <- excel_sheets(paste0(dir, "/", PNLP_files$File.Names[index], ".xlsx"))
     sheetnames = sheetnames[!sheetnames %in% 'INPUTRDC']
   } else{
     sheetnames <- excel_sheets(paste0(dir, "/", PNLP_files$File.Names[index], ".xls"))
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
  # make sure all values are numeric that should be numeric
    # new_dat <- NULL 
    # varnames <- colnames(currentSheet)
    #  for (i in varnames){
    #   rows <- which(grepl("[^0-9]+", currentSheet[, i, with=F])==TRUE & !is.na(currentSheet[, i, with=F]))
    #   if (length(rows)>0){
    #     rows_paste <- paste(rows, collapse=", ")
    #     sub_dat <- cbind(i, rows_paste)
    #     new_dat <- rbind(new_dat, sub_dat)
    #   }
    #  }
    # 
    # rows <- which(grepl("[^0-9]+", currentSheet$newCasesMalariaMild_under5)==TRUE & !is.na(currentSheet$newCasesMalariaMild_under5))
  
  
# ----------------------------------------------
# Test that the output has the right number of rows
if (nrow(fullData)!=3201) stop('Output data has wrong number of rows!')
# ----------------------------------------------     
   
  
# ----------------------------------------------     
# Save a copy of the full data set, in wide form for use in multiple imputation
  # take a subset of fullData that will be used in MI
    ameliaDT <- fullData[, -c("donor", "operational_support_partner", "population", "quarter", "month", 
                                "stringdate", "healthFacilities_numReportedWithinDeadline","reports_expected", "reports_received", "ASAQ_total")]
  
  # new column to factor in the product of number of health facilities reporting and total number of health facilties
    ameliaDT[, healthFacilities_numReported := as.numeric(healthFacilities_numReported)]
    ameliaDT[, healthFacilities_total := as.numeric(healthFacilities_total)]
    ameliaDT[, RDT_completedUnder5 := as.numeric(RDT_completedUnder5)]
    ameliaDT[, RDT_completed5andOlder := as.numeric(RDT_completed5andOlder)]
    ameliaDT[, RDT_positiveUnder5 := as.numeric(RDT_positiveUnder5)]
    ameliaDT[, RDT_positive5andOlder := as.numeric(RDT_positive5andOlder)]
    ameliaDT[, smearTest_completedUnder5 := as.numeric(smearTest_completedUnder5)]
    ameliaDT[, smearTest_completed5andOlder := as.numeric(smearTest_completed5andOlder)]
    ameliaDT[, smearTest_positiveUnder5 := as.numeric(smearTest_positiveUnder5)]
    ameliaDT[, smearTest_positive5andOlder := as.numeric(smearTest_positive5andOlder)]
    
    ameliaDT$healthFacilitiesProduct <- ameliaDT[, .(healthFacilities_total * healthFacilities_numReported)]
    
    ameliaDT[, RDT_completed := ifelse( year == 2014, RDT_completed, (RDT_completedUnder5 + RDT_completed5andOlder))]
    ameliaDT[, RDT_positive := ifelse( year == 2014, RDT_positive, (RDT_positiveUnder5 + RDT_positive5andOlder))]
    ameliaDT[, smearTest_completed := ifelse( year == 2014, smearTest_completed,(smearTest_completedUnder5 + smearTest_completed5andOlder))]
    ameliaDT[, smearTest_positive := ifelse( year == 2014, smearTest_positive, (smearTest_positiveUnder5 + smearTest_positive5andOlder))]
    
    ameliaDT <- ameliaDT[, -c("year", "smearTest_completedUnder5", "smearTest_completed5andOlder", "smearTest_positiveUnder5", "smearTest_positive5andOlder",
                              "RDT_positive5andOlder", "RDT_positiveUnder5", "RDT_completedUnder5", "RDT_completed5andOlder")]
    
    # reorder columns
      ameliaDT <- ameliaDT[, c(36, 1:35, 43, 37:42)]
    
  # export data  
    write.csv(ameliaDT, paste0("J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/Full Data for MI.csv"))
  
  
# ----------------------------------------------      
  
  
# ----------------------------------------------     
# Split appended data into Indicators and Interventions Data
  COD_PNLP_Indicators <- fullData[, c(1:8, 44, 46, 9:23) ]
  #COD_PNLP_Indicators <- fullData[, c(geoTimeVars)]
  COD_PNLP_Interventions <- fullData[, c(1:8, 44, 46, 24:43, 47:61)]
# ----------------------------------------------    


# ----------------------------------------------   
# Further prep on appended datatables for indicators and interventions:
  
#---INDICATORS--------------------------------------- 
  # Reshape Indicators data
  COD_PNLP_Indicators_melt <- melt(COD_PNLP_Indicators, id=c("province", "dps", "health_zone", "donor", "operational_support_partner", "population",
      "quarter", "month", "year", "date"), measured=c("newCasesMalariaMild_under5", "newCasesMalariaMild_5andOlder", "newCasesMalariaMild_pregnantWomen", "newCasesMalariaSevere_under5", "newCasesMalariaSevere_5andOlder", "newCasesMalariaSevere_pregnantWomen", 
      "mildMalariaTreated_under5", "mildMalariaTreated_5andOlder", "mildMalariaTreated_pregnantWomen", 
      "severeMalariaTreated_under5", "severeMalariaTreated_5andOlder", "severeMalariaTreated_pregnantWomen", 
      "malariaDeaths_under5", "malariaDeaths_5andOlder", "malariaDeaths_pregnantWomen" ), variable.name = "indicator", value.name="value")
      
  # add column for "indicator codes" - to be added later
  COD_PNLP_Indicators_melt$indicator_code <- NA
  
      # Split Indicators data by subgroup
        COD_PNLP_Indicators_melt[, c("indicator", "subpopulation") := tstrsplit(indicator, "_", fixed=TRUE)]
          # reorder columns:
            COD_PNLP_Indicators_melt <- COD_PNLP_Indicators_melt[, c(1:10, 11, 13, 12)]
      
      # make the value for each indicator numeric
          COD_PNLP_Indicators_melt[, value := as.numeric(value)]  

    # dcast() so that indicators are their own columns
        COD_PNLP_Indicators_cast <- dcast(COD_PNLP_Indicators_melt, province + dps + health_zone + donor + operational_support_partner + population +
                                              quarter + month + year + date + subpopulation ~ indicator)
      
        # reorder columns:
          COD_PNLP_Indicators_cast <- COD_PNLP_Indicators_cast[, c(1:11, 14, 15, 13, 16, 12)]
          
      # add column for formula_used (to later populate with Y/N values indicating
        # whether or not a formula was used to develop/model the data)
        # Right now, fill with "No" which will be the default
          COD_PNLP_Indicators_melt$formula_used <- "No"

         # if modulus operator returns 0 then it should stay no, if it returns anything other than 0, change
            # formula_used to yes
            COD_PNLP_Indicators_melt[value%%1==0, formula_used:='No']
            COD_PNLP_Indicators_melt[value%%1!=0, formula_used:='Yes']
            
          
#---INTERVENTIONS---------------------------------------                        
  # Reshape Interventions data
    COD_PNLP_Interventions_melt <- melt(COD_PNLP_Interventions, id=c("province", "dps", "health_zone", "donor", "operational_support_partner", "population",
      "quarter", "month", "year", "date"), measured=c(), variable.name = "intervention", value.name="value")
      
      # Split Interventions data by subgroup
        COD_PNLP_Interventions_melt[, c("intervention", "intervention_spec") := tstrsplit(intervention, "_", fixed=TRUE)]
      
      # add column for "indicator codes" - to be added later
        COD_PNLP_Interventions_melt$indicator_code <- NA
      
      # reorder columns
        COD_PNLP_Interventions_melt <- COD_PNLP_Interventions_melt[, c(1:11, 13, 14, 12)]
# ----------------------------------------------
  
  
# ----------------------------------------------
  # Export the prepped data
  COD_PNLP_Data_Indicators_Long <- COD_PNLP_Indicators_melt
  COD_PNLP_Data_Indicators_Wide <- COD_PNLP_Indicators_cast
  COD_PNLP_Data_Indicators <- COD_PNLP_Indicators
  COD_PNLP_Data_Interventions_Long <- COD_PNLP_Interventions_melt 
  COD_PNLP_Data_Interventions_Wide <- COD_PNLP_Interventions

  # function to export data:
  export_data <- function(dfName){
    write.csv(get(dfName), paste0("J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/", dfName , ".csv"))
  }

#   export_data("COD_PNLP_Data_Indicators")
#   export_data("COD_PNLP_Data_Interventions_Long")
#   export_data("COD_PNLP_Data_Interventions_Wide")
  
  dfsToExport <- c("COD_PNLP_Data_Indicators_Long", "COD_PNLP_Data_Indicators_Wide", "COD_PNLP_Data_Interventions_Long", "COD_PNLP_Data_Interventions_Wide", "COD_PNLP_Data_Indicators")
  for (df in dfsToExport){
    export_data(df)
  }
# ----------------------------------------------  
