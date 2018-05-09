# ----------------------------------------------
# Audrey Batzel
#
# 3/6/18
# Prepping DRC PNLP 2014-2016 data for analaysis
# 5/1/18 
# Adding in new data from DRC PNLP that spans 2010-2017 and all provinces
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
  PNLP_files <- read.csv(paste0(dir,"/","PNLP_file_names.csv"), fileEncoding = "latin1")

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
    dataSheet <- data.table(read_excel(paste0(dir, "/", PNLP_files$File.Names[9], ".xls"), sheet= 'KIN'))
# ----------------------------------------------
  
  
# ----------------------------------------------  
  # START OF FUNCTION:  
  prep_data <- function(year, sheetname){
    
  # Load data sheet
    dataSheet <- data.table(read_excel(paste0(dir,"/Données_", year, "_PNLP.xls"), sheet= sheetname))
# ----------------------------------------------
  # Change column names, so that each column has 
    # since some of them have less columns, make a vector of names and then have "if ___, then drop these ___"
 
  columnNames <- c("province", "dps", "health_zone", "donor", "operational_support_partner", "population",
                           "quarter", "month", "newCasesMalariaMild_under5", "newCasesMalariaMild_5andOlder", "newCasesMalariaMild_pregnantWomen", "newCasesMalariaSevere_under5", "newCasesMalariaSevere_5andOlder", "newCasesMalariaSevere_pregnantWomen",
                           "mildMalariaTreated_under5", "mildMalariaTreated_5andOlder", "mildMalariaTreated_pregnantWomen",
                           "severeMalariaTreated_under5", "severeMalariaTreated_5andOlder", "severeMalariaTreated_pregnantWomen",
                           "malariaDeaths_under5", "malariaDeaths_5andOlder", "malariaDeaths_pregnantWomen", "ANC_1st", "ANC_2nd", "ANC_3rd", "ANC_4th", "SP_1st", "SP_2nd","SP_3rd", "ITN_received", "ITN_distAtANC",
                           "ITN_distAtPreschool", "VAR", "ASAQ_2to11mos", "ASAQ_1to5yrs", "ASAQ_6to13yrs", "ASAQ_14yrsAndOlder", "ASAQ_total", "ArtLum_received", "ArtLum_used", "smearTest_completedUnder5",
                           "smearTest_completed5andOlder", "smearTest_positiveUnder5", "smearTest_positive5andOlder", "RDT_completedUnder5", "RDT_completed5andOlder", "RDT_positiveUnder5", "RDT_positive5andOlder",
                           "reports_received", "reports_expected", "healthFacilities_total", "healthFacilities_numReported", "healthFacilities_numReportedWithinDeadline" )
    
  columnNamesComplete <- c("province", "dps", "health_zone", "donor", "operational_support_partner", "population", "quarter", "month", 
                           "totalCasesAllDiseases_under5", "totalCasesAllDiseases_5andOlder", "totalCasesAllDiseases_pregnantWomen", 
                           "suspectedMalaria_under5", "suspectedMalaria_5andOlder", "suspectedMalaria_pregnantWomen",
                           "presumedMalaria_under5", "presumedMalaria_5andOlder", "presumedMalaria_pregnantWomen",
                           "newCasesMalariaMild_under5", "newCasesMalariaMild_5andOlder", "newCasesMalariaMild_pregnantWomen", 
                           "totalHospAllDiseases_under5", "totalHospAllDiseases_5andOlder", "totalHospAllDiseases_pregnantWomen",
                           "newCasesMalariaSevere_under5", "newCasesMalariaSevere_5andOlder", "newCasesMalariaSevere_pregnantWomen",
                           "mildMalariaTreated_under5", "mildMalariaTreated_5andOlder", "mildMalariaTreated_pregnantWomen",
                           "severeMalariaTreated_under5", "severeMalariaTreated_5andOlder", "severeMalariaTreated_pregnantWomen",
                           "totalDeathsAllDiseases_under5", "totalDeathsAllDiseases_5andOlder", "totalDeathsAllDiseases_pregnantWomen",
                           "malariaDeaths_under5", "malariaDeaths_5andOlder", "malariaDeaths_pregnantWomen",  
                           "ANC_1st", "ANC_2nd", "ANC_3rd", "ANC_4th", "SP_1st", "SP_2nd","SP_3rd", 
                           "ITN_received", "ITN_distAtANC", "ITN_distAtPreschool", "VAR_0to11mos", 
                           "ASAQ_received_2to11mos", "ASAQ_received_1to5yrs", "ASAQ_received_6to13yrs", "ASAQ_received_14yrsAndOlder",
                           "ASAQ_used_2to11mos", "ASAQ_used_1to5yrs", "ASAQ_used_6to13yrs", "ASAQ_used_14yrsAndOlder", "ASAQ_used_total",
                           "ArtLum_received", "ArtLum_used",
                           "stockOut_SP", "stockOut_ASAQ_2to11mos", "stockOut_ASAQ_1to5yrs", "stockOut_ASAQ_6to13yrs", "stockOut_ASAQ_14yrsAndOlder", 
                           "stockOut_qui_pill", "stockOut_qui_inj", "stockOut_ASAQ_inj", "stockOut_RDT", "stockOut_artLum",
                           "smearTest_completed_under5", "smearTest_completed_5andOlder", "smearTest_positive_under5", "smearTest_positive_5andOlder", 
                           "RDT_received", "RDT_completed_under5", "RDT_completed_5andOlder", "RDT_positive_under5", "RDT_positive_5andOlder",
                           "peopleTested_under5", "peopleTested_5andOlder",
                           "PMA_ASAQ", "PMA_TPI", "PMA_ITN", "PMA_complete",
                           "reports_received", "reports_expected", "healthFacilities_total", "healthFacilities_numReported", "healthFacilities_numReportedWithinDeadline",
                           "hzTeam_supervisors_numPlanned", "hzTeam_supervisors_numActual", "hzTeam_employees_numPlanned", "hzTeam_employees_numActual",
                           "awarenessTrainings_numPlanned", "awarenessTrainings_numActual",
                           "SSC_fevers_under5", "SSC_fevers_5andOlder", "SSC_RDT_completed_under5", "SSC_RDT_completed_5andOlder", "SSC_RDT_positive_under5", "SSC_RDT_positive_5andOlder",
                           "SSC_ACT_under5", "SSC_ACT_5andOlder", "SSC_casesReferred_under5", "SSC_casesReferred_5andOlder",
                           "SSC_casesCrossReferred_under5", "SSC_casesCrossReferred_5andOlder")
  
  columnNames2014 <- c("province", "dps", "health_zone", "donor", "operational_support_partner", "population",
        "quarter", "month", "newCasesMalariaMild_under5", "newCasesMalariaMild_5andOlder", "newCasesMalariaMild_pregnantWomen", "newCasesMalariaSevere_under5", "newCasesMalariaSevere_5andOlder", "newCasesMalariaSevere_pregnantWomen",
        "mildMalariaTreated_under5", "mildMalariaTreated_5andOlder", "mildMalariaTreated_pregnantWomen",
        "severeMalariaTreated_under5", "severeMalariaTreated_5andOlder", "severeMalariaTreated_pregnantWomen",
        "malariaDeaths_under5", "malariaDeaths_5andOlder", "malariaDeaths_pregnantWomen", "ANC_1st", "SP_1st", "SP_2nd","SP_3rd", "ITN_received", "ITN_distAtANC",
        "ITN_distAtPreschool", "ASAQ_2to11mos", "ASAQ_1to5yrs", "ASAQ_6to13yrs", "ASAQ_14yrsAndOlder", "ASAQ_total", "smearTest_completed", "smearTest_positive", "RDT_completed", 
        "RDT_positive", "reports_received", "reports_expected", "healthFacilities_total", "healthFacilities_numReported")
  
  #for the new data this doesn't work based on the name of the file.  Assign a new variable for year?
    if ( PNLP_files$year[9] == 2014 ) {
      columnNames <- columnNames2014 
    } else {
      columnNames <- columnNamesComplete
    }

  names(dataSheet) <- columnNames
  
  # add a column for the "year" to keep track of this variable as we add dataSheets to this one
  dataSheet$year <- PNLP_files$year[9]
  
# ----------------------------------------------
  # Get rid of rows you don't need- "subset"
  
  # FIRST - delete first row if it's first value is "NA" or "PROVINCE" as a way to
  # only delete those unnecessary rows, and not any others accidentally - these
  # were the column headers in the original datasheet in excel.
  
  if (dataSheet[2,"province"] == "PROVINCE"){
    dataSheet <- dataSheet[-c(1, 2),] 
    }

  # clean "Province" column in BDD datasheet for 2016 and 2015 because
  # it has some missing/"0" values that should be "BDD" - doesn't work
  
  if (sheetname == "BDD"){
    dataSheet$province <- "BDD"
  }
    
  # using this to delete rows tacked on to the end of the DF with all NA values
    # by checking to see if 2nd column is NA
  dataSheet <- dataSheet[complete.cases(dataSheet[ , 2]),]
  
  # using this to delete "totals" rows
    # BDD 2016 sheet has total row in the middle of the data, the other sheets have it
    # in the last row of the sheet, sometimes in the first column, sometimes in the second;
    # sometimes as "Total" and sometimes "TOTAL"
     dataSheet <- dataSheet[!grepl(("TOTAL"), (dataSheet$province)),]
     dataSheet <- dataSheet[!grepl(("Total"), (dataSheet$province)),]
     dataSheet <- dataSheet[!grepl(("TOTAL"), (dataSheet$dps)),]
     dataSheet <- dataSheet[!grepl(("Total"), (dataSheet$dps)),]

  # translate french to numeric version of month Janvier=1
     # dataSheet[month=='Janvier', month:="01"]
     # grepl() to make sure that any that may have trailing white space are also changed
     dataSheet[grepl("Janvier", month), month:="01"]
     dataSheet[grepl("Février", month), month:="02"]
     dataSheet[grepl("Mars", month), month:="03"]
     dataSheet[grepl("Avril", month), month:="04"]
     dataSheet[grepl("Mai", month), month:="05"]
     dataSheet[grepl("Juin", month), month:="06"]
     dataSheet[grepl("Juillet", month), month:="07"]
     dataSheet[grepl("Août", month), month:="08"]
     dataSheet[grepl("Septembre", month), month:="09"]
     dataSheet[grepl("Octobre", month), month:="10"]
     dataSheet[grepl("Novembre", month), month:="11"]
     dataSheet[grepl("Décembre", month), month:="12"]     
     
    # make string version of the date
     dataSheet[, stringdate:=paste('01', month, year, sep='/')]
     
    # combine year and month into one variable
     dataSheet[, date:=as.Date(stringdate, "%d/%m/%Y")]
     
# ----------------------------------------------
# ----------------------------------------------
  # Return current data sheet
    return(dataSheet)
# ----------------------------------------------
}  #END OF FUNCTION
# ----------------------------------------------
  
  
# ----------------------------------------------
# Use a loop to run prep_data() on each of the three data sheets for the three years
  # for which we have the data.
  
  # variables needed:
  years <- c(2014:2016)
  sheetnames <- c('BDD', 'KIN', 'OR', 'KC', 'KOR', 'KOC', 'MN', 'NK', 'SK', 'EQ', 'KAT')
  i <- 1

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
# Test that the output has the right number of rows
if (nrow(fullData)!=3201) stop('Output data has wrong number of rows!')
# ----------------------------------------------     

 
      # ----------------------------------------------    
        # geoTimeVars <- c("year", "province", "dps", "health_zone", "donor", "operational_support_partner", "population",
        #                   "quarter", "month")
        # indicatorVars <- c("newCasesMalariaMild_under5", "newCasesMalariaMild_5andOlder", "newCasesMalariaMild_pregnantWomen", "newCasesMalariaSevere_under5", "newCasesMalariaSevere_5andOlder", "newCasesMalariaSevere_pregnantWomen", 
        #                         "mildMalariaTreated_under5", "mildMalariaTreated_5andOlder", "mildMalariaTreated_pregnantWomen", 
        #                         "severeMalariaTreated_under5", "severeMalariaTreated_5andOlder", "severeMalariaTreated_pregnantWomen", 
        #                         "malariaDeaths_under5", "malariaDeaths_5andOlder", "malariaDeaths_pregnantWomen" )  
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
