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
  # (output to prepped_data folder within cod folder)
  # file path: J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/", fileName , ".csv"))
  # file names: 
    # COD_PNLP_Data_Indicators - prepped data.table object, appended data from 2014-2016
    # COD_PNLP_Data_Interventions_long - prepped data.table object, appended data from 2014-2016
    # # COD_PNLP_Data_Interventions_wide
# ----------------------------------------------
  
  
# ----------------------------------------------
# Load data - to visualize before cleaning
  
  # cod_mdata_KIN16 <- data.table(read_excel(paste0(dir,"/", PNLP_files$File.Names.[1], '.xls'), sheet= "KIN"))
  # cod_mdata_BDD16 <- data.table(read_excel(paste0(dir, "/", PNLP_files$File.Names.[1], '.xls'), sheet= "BDD"))
  # cod_mdata_OR16 <- data.table(read_excel(paste0(dir,"/", PNLP_files$File.Names.[1], '.xls'), sheet= "OR"))
  # cod_mdata_KIN15 <- data.table(read_excel(paste0(dir,"/", PNLP_files$File.Names.[2], '.xls'), sheet= "KIN"))
  # cod_mdata_BDD15 <- data.table(read_excel(paste0(dir, "/", PNLP_files$File.Names.[2], '.xls'), sheet= "BDD"))
  # cod_mdata_OR15 <- data.table(read_excel(paste0(dir,"/", PNLP_files$File.Names.[2], '.xls'), sheet= "OR"))
  # cod_mdata_KIN14 <- data.table(read_excel(paste0(dir,"/", PNLP_files$File.Names.[3], '.xls'), sheet= "KIN"))
  # cod_mdata_BDD14 <- data.table(read_excel(paste0(dir, "/", PNLP_files$File.Names.[3], '.xls'), sheet= "BDD"))
  # cod_mdata_OR14 <- data.table(read_excel(paste0(dir,"/", PNLP_files$File.Names.[3], '.xls'), sheet= "OR"))
# ----------------------------------------------
  
  
# ----------------------------------------------  
  # START OF FUNCTION:  
  prep_data <- function(year, sheetname){
    
  # Load data sheet
    dataSheet <- data.table(read_excel(paste0(dir,"/Données_", year, "_PNLP.xls"), sheet= sheetname))
# ----------------------------------------------   
# ----------------------------------------------
  # Set names of columns
    # since some of them have less columns, make a vector of names and then have "if ___, then drop these ___"
 
  columnNamesComplete <- c("province", "dps", "health_zone", "donor", "operational_support_partner", "population",
        "quarter", "month", "newCasesMalaria_under5", "newCasesMalaria_5andOlder", "newCasesMalaria_pregnantWomen", "hospitalizedCases_under5", "hospitalizedCases_5andOlder", "hospitalizedCases_pregnantWomen",
        "simpleMalariaTreatedAccordingToNationalPolicy_under5", "simpleMalariaTreatedAccordingToNationalPolicy_5andOlder", "simpleMalariaTreatedAccordingToNationalPolicy_pregnantWomen",
        "seriousMalariaTreatedAccordingToNationalPolicy_under5", "seriousMalariaTreatedAccordingToNationalPolicy_5andOlder", "seriousMalariaTreatedAccordingToNationalPolicy_pregnantWomen",
        "malariaDeaths_under5", "malariaDeaths_5andOlder", "malariaDeaths_pregnantWomen", "ANC_1st", "ANC_2nd", "ANC_3rd", "ANC_4th", "SP_1st", "SP_2nd","SP_3rd", "ITN_received", "ITN_distAtANC",
        "ITN_distAtPreschool", "VAR", "ASAQ_2to11mos", "ASAQ_1to5yrs", "ASAQ_6to13yrs", "ASAQ_14yrsAndOlder", "ASAQ_total", "ArtLum_receieved", "ArtLum_used", "smear_test_completed_under5",
        "smear_test_completed_5andOlder", "smear_test_positive_under5", "smear_test_positive_5andOlder", "RDT_completed_under5", "RDT_completed_5andOlder", "RDT_positive_under5", "RDT_positive_5andOlder",
        "num_reports_received", "num_repots_expected", "num_health_facilities", "health_facilities_numReported", "health_facilities_numReportedWithinDeadline" )
  
  columnNames2014 <- c("province", "dps", "health_zone", "donor", "operational_support_partner", "population",
        "quarter", "month", "newCasesMalaria_under5", "newCasesMalaria_5andOlder", "newCasesMalaria_pregnantWomen", "hospitalizedCases_under5", "hospitalizedCases_5andOlder", "hospitalizedCases_pregnantWomen",
        "simpleMalariaTreatedAccordingToNationalPolicy_under5", "simpleMalariaTreatedAccordingToNationalPolicy_5andOlder", "simpleMalariaTreatedAccordingToNationalPolicy_pregnantWomen",
        "seriousMalariaTreatedAccordingToNationalPolicy_under5", "seriousMalariaTreatedAccordingToNationalPolicy_5andOlder", "seriousMalariaTreatedAccordingToNationalPolicy_pregnantWomen",
        "malariaDeaths_under5", "malariaDeaths_5andOlder", "malariaDeaths_pregnantWomen", "ANC_1st", "SP_1st", "SP_2nd","SP_3rd", "ITN_received", "ITN_distAtANC",
        "ITN_distAtPreschool", "ASAQ_2to11mos", "ASAQ_1to5yrs", "ASAQ_6to13yrs", "ASAQ_14yrsAndOlder", "ASAQ_total", "smear_test_completed", "smear_test_positive", "RDT_completed", 
        "RDT_positive", "num_reports_received", "num_repots_expected", "num_health_facilities", "health_facilities_numReported")
  
    if ( year == 2014 ) {
      columnNames <- columnNames2014 
    } else {
      columnNames <- columnNamesComplete
    }

  names(dataSheet) <- columnNames
  
  # colnames(dataSheet)[1] <- "province"
  # colnames(dataSheet)[2] <- "dps"
  # colnames(dataSheet)[3] <- "health_zone"
  # colnames(dataSheet)[4] <- "donor"
  # colnames(dataSheet)[5] <- "operational_support_partner"
  # colnames(dataSheet)[6] <- "population"
  # colnames(dataSheet)[7] <- "trimester"  #should this be quarter?
  # colnames(dataSheet)[8] <- "month"
  # colnames(dataSheet)[9] <- "newCasesMalaria_under5"
  # colnames(dataSheet)[10] <- "newCasesMalaria_5andOlder"
  # colnames(dataSheet)[11] <- "newCasesMalaria_pregnantWomen"
  # colnames(dataSheet)[12] <- "hospitalizedCases_under5"
  # colnames(dataSheet)[13] <- "hospitalizedCases_5andOlder"
  # colnames(dataSheet)[14] <- "hospitalizedCases_pregnantWomen"
  # colnames(dataSheet)[15] <- "simpleMalariaTreatedAccordingToNationalPolicy_under5"
  # colnames(dataSheet)[16] <- "simpleMalariaTreatedAccordingToNationalPolicy_5andOlder"
  # colnames(dataSheet)[17] <- "simpleMalariaTreatedAccordingToNationalPolicy_pregnantWomen"
  # colnames(dataSheet)[18] <- "seriousMalariaTreatedAccordingToNationalPolicy_under5"
  # colnames(dataSheet)[19] <- "seriousMalariaTreatedAccordingToNationalPolicy_5andOlder"
  # colnames(dataSheet)[20] <- "seriousMalariaTreatedAccordingToNationalPolicy_pregnantWomen"
  # colnames(dataSheet)[21] <- "malariaDeaths_under5"
  # colnames(dataSheet)[22] <- "malariaDeaths_5andOlder"
  # colnames(dataSheet)[23] <- "malariaDeaths_pregnantWomen"
  # colnames(dataSheet)[24] <- "ANC_1st"
  # colnames(dataSheet)[25] <- "ANC_2nd"
  # colnames(dataSheet)[26] <- "ANC_3rd"
  # colnames(dataSheet)[27] <- "ANC_4th"
  # colnames(dataSheet)[28] <- "SP_1st"  #why was SP within ANC (CPN)orginally? Should it relate to that in some way?
  # colnames(dataSheet)[29] <- "SP_2nd"
  # colnames(dataSheet)[30] <- "SP_3rd"
  # colnames(dataSheet)[31] <- "ITN_received"
  # colnames(dataSheet)[32] <- "ITN_distAtANC"
  # colnames(dataSheet)[33] <- "ITN_distAtPreschool"
  # colnames(dataSheet)[34] <- "VAR"   #translation?
  # colnames(dataSheet)[35] <- "ASAQ_2to11mos"
  # colnames(dataSheet)[36] <- "ASAQ_1to5yrs"
  # colnames(dataSheet)[37] <- "ASAQ_6to13yrs"
  # colnames(dataSheet)[38] <- "ASAQ_14yrsAndOlder"
  # colnames(dataSheet)[39] <- "ASAQ_total"
  # colnames(dataSheet)[40] <- "ArtLum_receieved"
  # colnames(dataSheet)[41] <- "ArtLum_used"
  # colnames(dataSheet)[42] <- "smear_test_completed_under5"
  # colnames(dataSheet)[43] <- "smear_test_completed_5andOlder"
  # colnames(dataSheet)[44] <- "smear_test_positive_under5"
  # colnames(dataSheet)[45] <- "smear_test_positive_5andOlder"
  # colnames(dataSheet)[46] <- "RDT_completed_under5"
  # colnames(dataSheet)[47] <- "RDT_completed_5andOlder"
  # colnames(dataSheet)[48] <- "RDT_positive_under5"
  # colnames(dataSheet)[49] <- "RDT_positive_5andOlder"
  # colnames(dataSheet)[50] <- "num_reports_received"
  # colnames(dataSheet)[51] <- "num_repots_expected"
  # colnames(dataSheet)[52] <- "num_health_facilities"
  # colnames(dataSheet)[53] <- "health_facilities_numReported"
  # colnames(dataSheet)[54] <- "health_facilities_numReportedWithinDeadline"

  # add a column for the "year" to keep track of this variable as we add dataSheets to this one
  dataSheet$year <- year
# ----------------------------------------------
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
  sheetnames <- c('BDD', 'KIN', 'OR')
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
  geoTimeVars <- c("year", "province", "dps", "health_zone", "donor", "operational_support_partner", "population",
                    "quarter", "month")
  indicatorVars <- c("newCasesMalaria_under5", "newCasesMalaria_5andOlder", "newCasesMalaria_pregnantWomen", "hospitalizedCases_under5", "hospitalizedCases_5andOlder", "hospitalizedCases_pregnantWomen", 
                          "simpleMalariaTreatedAccordingToNationalPolicy_under5", "simpleMalariaTreatedAccordingToNationalPolicy_5andOlder", "simpleMalariaTreatedAccordingToNationalPolicy_pregnantWomen", 
                          "seriousMalariaTreatedAccordingToNationalPolicy_under5", "seriousMalariaTreatedAccordingToNationalPolicy_5andOlder", "seriousMalariaTreatedAccordingToNationalPolicy_pregnantWomen", 
                          "malariaDeaths_under5", "malariaDeaths_5andOlder", "malariaDeaths_pregnantWomen" )  
# ----------------------------------------------    
  

# ----------------------------------------------     
# Split appended data into Indicators and Interventions Data
  COD_PNLP_Indicators <- fullData[, c(44, 1:23) ]
  #COD_PNLP_Indicators <- fullData[, c(geoTimeVars)]
  COD_PNLP_Interventions <- fullData[, c(44, 1:8, 24:43, 45:59)]
# ----------------------------------------------    


# ----------------------------------------------    
# Reshape appended and split data
  # Indicators data
  COD_PNLP_Indicators_melt <- melt(COD_PNLP_Indicators, id=c("year", "province", "dps", "health_zone", "donor", "operational_support_partner", "population",
      "quarter", "month"), measured=c("newCasesMalaria_under5", "newCasesMalaria_5andOlder", "newCasesMalaria_pregnantWomen", "hospitalizedCases_under5", "hospitalizedCases_5andOlder", "hospitalizedCases_pregnantWomen", 
      "simpleMalariaTreatedAccordingToNationalPolicy_under5", "simpleMalariaTreatedAccordingToNationalPolicy_5andOlder", "simpleMalariaTreatedAccordingToNationalPolicy_pregnantWomen", 
      "seriousMalariaTreatedAccordingToNationalPolicy_under5", "seriousMalariaTreatedAccordingToNationalPolicy_5andOlder", "seriousMalariaTreatedAccordingToNationalPolicy_pregnantWomen", 
      "malariaDeaths_under5", "malariaDeaths_5andOlder", "malariaDeaths_pregnantWomen" ), variable.name = "indicator", value.name="value")
  
  # Split Indicators data by subgroup
    COD_PNLP_Indicators_melt[, c("indicator", "subpopulation") := tstrsplit(indicator, "_", fixed=TRUE)]
      # reorder columns:
        COD_PNLP_Indicators_melt <- COD_PNLP_Indicators_melt[, c(1:9, 12, 10, 11)]

  # dcast() so that indicators are columns
    COD_PNLP_Indicators_cast <- dcast(COD_PNLP_Indicators_melt, year + province + dps + health_zone + donor + operational_support_partner + population +
         quarter + month + subpopulation ~ indicator)
      # reorder columns:
        COD_PNLP_Indicators_cast <- COD_PNLP_Indicators_cast[, c(1:10, 13, 11, 15, 14, 12)]
  
  # Interventions data
    COD_PNLP_Interventions_melt <- melt(COD_PNLP_Interventions, id=c("province", "dps", "health_zone", "donor", "operational_support_partner", "population",
      "quarter", "month", "year"), measured=c(), variable.name = "intervention", value.name="value")
  # add column for "indicator codes" - to be added later
    COD_PNLP_Interventions_melt$indicator_code <- NA
# ----------------------------------------------
  
  
# ----------------------------------------------
  # Export the prepped data
  COD_PNLP_Data_Indicators <- COD_PNLP_Indicators_cast
  COD_PNLP_Data_Interventions_Long <- COD_PNLP_Interventions_melt 
  COD_PNLP_Data_Interventions_Wide <- COD_PNLP_Interventions
  
  # hard-coded version:
  # write.csv(COD_PNLP_Data_Indicators, ("J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/COD_PNLP_Data_Indicators.csv")) 
  # write.csv(COD_PNLP_Data_Interventions, ("J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/COD_PNLP_Data_Interventions.csv"))
  
  # function to export data:
  export_data <- function(dfName){
    write.csv(get(dfName), paste0("J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/", dfName , ".csv"))
  }

#   export_data("COD_PNLP_Data_Indicators")
#   export_data("COD_PNLP_Data_Interventions_Long")
#   export_data("COD_PNLP_Data_Interventions_Wide")
  
  dfsToExport <- c("COD_PNLP_Data_Indicators", "COD_PNLP_Data_Interventions_Long", "COD_PNLP_Data_Interventions_Wide")

  for (df in dfsToExport){
    export_data(df)
  }
# ----------------------------------------------  
