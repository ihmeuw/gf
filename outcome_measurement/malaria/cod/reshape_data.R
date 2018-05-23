# ----------------------------------------------
# Audrey Batzel
#
# 5/22/18
# Further prep (reshaping) of DRC data
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
    dir_prepped <-"J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/"
  # output files
    output_dt_long <- "PNLP_2010to2017_long.csv"
    output_fullData <- "PNLP_2010to2017_fullPrepped.csv"
        # fullData with updated, further cleaned full data set
    output_dt <- "PNLP_2010to2017_indicatorsWide_subpopulationsLong.csv"
      # dt_wide has subpopulations long
    output_dt_forMI <- "PNLP_2010to2017_forMI.csv"
# ----------------------------------------------   

    
# ----------------------------------------------     
# read in data
  fullData <- fread( paste0(dir_prepped, "PNLP_2010to2017_prepped.csv")) 
# ----------------------------------------------     
    
    
# ----------------------------------------------   
# grab all of the variable names to work with
  # setnames(fullData, c("V1"), c("id"))
  fullData <- fullData[, "V1" := NULL ]
  
  all_vars <- c(colnames(fullData))
  
  id_vars <- c("id", "province", "dps", "health_zone", "donor", "operational_support_partner", "population", "quarter", "month",
             "year", "stringdate", "date") 
  
  
  measured_vars <- all_vars[!all_vars %in% id_vars]

# melt fullData to convert it to long format
  dt_long <- melt(fullData, id= id_vars, measured= measured_vars, variable.name = "indicator", value.name="value")
  dt_long <- dt_long[, old_value := value]

    # when you try to convert value to be numeric, it gives a warning that NAs are introduced by coercion
      # this helps us check where the NAs are introduced
      # re-run after running lines 70-77
      dt_numeric <- dt_long[, numValue := as.numeric(value)]
    
    # check for where NAs are introduced by conversion when changing to numeric 
      dtNAs <- dt_numeric[ !is.na(value) & is.na(numValue) ]

# fix cases where NAs are intoduced and we can interpret the value
  # there will be some NAs introduced even after this but you can run the above line of code again to
  # see what they are- most should just be NA because they are not interpretable as actual values.
    dt_long$value <- gsub("O", "0", dt_long$value)
    dt_long$value <- gsub("\\s", "", dt_long$value)
    dt_long$value <- gsub(",", "", dt_long$value)
    dt_long$value <- gsub("²", "", dt_long$value)
    dt_long$value <- gsub("36/8", "", dt_long$value)
    dt_long$value <- gsub("23/19", "", dt_long$value)
    dt_long$value <- gsub("[[:punct:]]", "", dt_long$value)
    dt_long$value <- gsub("\\D", "", dt_long$value)

# convert value in dt_melt to numeric now that we have fixed cases that were converted to NA and should not have been
  dt_long <- dt_long[, value := as.numeric(value)]
  # drop numValue and oldValue
    dt_long[, c("numValue","old_value"):=NULL]  
  
# cast wide to have a copy the cleaned, full data in a better format for some analyses/processing 
  fullData <- dcast(dt_long, id + province + dps + health_zone + donor + operational_support_partner + population + quarter + month + year + stringdate + date ~ indicator)
  fullData <- as.data.table(fullData)
  # export this dt before further alterations
    write.csv(fullData, paste0(dir_prepped, output_fullData))
  
  # clean up indicator names for tstrsplit to work
    dt_long$indicator <- gsub("SSC_", "SSC", dt_long$indicator)
    dt_long$indicator <- gsub("stockOut_", "stockOut", dt_long$indicator)
    dt_long$indicator <- gsub("hzTeam_", "", dt_long$indicator)
    dt_long$indicator <- gsub("ASAQ_u", "ASAQu", dt_long$indicator)
    dt_long$indicator <- gsub("ASAQ_r", "ASAQr", dt_long$indicator)
      
  # split indicator column into indicator and subpopulation
    dt_long <- dt_long[, c("indicator", "subpopulation") := tstrsplit(indicator, "_", fixed=TRUE)]
    # set a placeholder value for where there is no subpopulation, so that graphing doesn't mess up there
      dt_long <- dt_long[is.na(subpopulation), subpopulation:= "none"]
    # export this dt as a version with both indicators and subpopulations long
      write.csv(dt_long, paste0(dir_prepped, output_dt_long))
    
# cast indicators wide and keep subpopulations wide - format to save to basecamp for CEPs
  dt_wide <- dcast(dt_long, province + dps + health_zone + donor + operational_support_partner + population +
                                      quarter + month + year + stringdate + date + subpopulation ~ indicator) 
  # export this dt as a version with indicators wide and subpopulations long
    write.csv(dt_wide, paste0(dir_prepped, output_dt))
  
# ----------------------------------------------  


# ----------------------------------------------     
# Prepare data table for multiple imputation
  # take a subset of fullData that will be used in MI
  ameliaDT <- fullData[,-c("donor", "operational_support_partner", "population", "quarter", "month", 
                        "stringdate", "healthFacilities_numReportedWithinDeadline","reports_expected", "reports_received", "ASAQ_used_total",
                        "peopleTested_5andOlder", "peopleTested_under5", "PMA_ASAQ", "PMA_TPI","PMA_ITN","PMA_complete")]
  
  # new column to factor in the product of number of health facilities reporting and total number of health facilties
  ameliaDT$healthFacilitiesProduct <- ameliaDT[, .(healthFacilities_total * healthFacilities_numReported)]
  
  ameliaDT[, RDT_completed := ifelse( year <= 2014, RDT_completed, (RDT_completedUnder5 + RDT_completed5andOlder))]
  ameliaDT[, RDT_positive := ifelse( year <= 2014, RDT_positive, (RDT_positiveUnder5 + RDT_positive5andOlder))]
  ameliaDT[, smearTest_completed := ifelse( year <= 2014, smearTest_completed,(smearTest_completedUnder5 + smearTest_completed5andOlder))]
  ameliaDT[, smearTest_positive := ifelse( year <= 2014, smearTest_positive, (smearTest_positiveUnder5 + smearTest_positive5andOlder))]
  
  ameliaDT <- ameliaDT[, -c("year", "smearTest_completedUnder5", "smearTest_completed5andOlder", "smearTest_positiveUnder5", "smearTest_positive5andOlder",
                          "RDT_positive5andOlder", "RDT_positiveUnder5", "RDT_completedUnder5", "RDT_completed5andOlder")]
  
  
# this doesn't work because there are repeats of health zones?
  # rectangularize the data so there is an observation for every health zone and date
  hzs <- unique(ameliaDT$health_zone)
  months <- unique(ameliaDT$date)
  rect <- as.data.table(expand.grid(hzs, months))
  setnames(rect, c("health_zone", "date"))
  hzProvinceDPS <- ameliaDT[, .(health_zone, province, dps)]
  hzProvinceDPS <- unique(hzProvinceDPS)
  rect <- merge(rect, hzProvinceDPS, by=c("health_zone"), all=TRUE)
  
  dt <- merge(dt, rect, by=c("date", "health_zone", "dps", "province"), all=TRUE)
  
  # add an id column
  dt[, id:= .I]
  
  # reorder columns
  ameliaDT <- ameliaDT[, c(36, 1:35, 43, 37:42)]
  
  # export data  
  write.csv(ameliaDT, paste0("J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/Full Data for MI.csv"))
  

# ---------------------------------------------- 


# ---------------------------------------------- 
# different categories of variables for graphing/descriptive analysis
  indicators <- measured_vars[c(1:27, 95:97)]
  outputs <- measured_vars[c(28:43, 52:57, 70:75, 78:85)]
  SSC <- measured_vars[c(89:94, 100:111)]
  health_system <- measured_vars[c(62:69, 86:88)]
  stockouts <- measured_vars[c(44:51, 76:77)]

# ----------------------------------------------       
  
  
  
# ----------------------------------------------     
# OLD CODE : not sure if we still need any of this, but keeping it now for reference
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


dt_indicators <- dt_wide[, c(id_vars, indicators), with=F]
indicators_long <- melt(dt_indicators, id=id_vars, measured=indicators, variable.name = "indicator", value.name="value")


# Split Indicators data by subgroup
COD_PNLP_Indicators_melt[, c("indicator", "subpopulation") := tstrsplit(indicator, "_", fixed=TRUE)]
# reorder columns:
COD_PNLP_Indicators_melt <- COD_PNLP_Indicators_melt[, c(1:10, 11, 13, 12)]

indicators_long[, c("indicator", "subpopulation") := tstrsplit(indicator, "_", fixed=TRUE)]



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
