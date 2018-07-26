

# variables and variable names (to source into different R scripts)

# ----------------------------------------------       


# ----------------------------------------------  
# vectors of indicator names and intervention names to subset
  indicators <- c("newCasesMalariaMild_under5", "newCasesMalariaMild_5andOlder", "newCasesMalariaMild_pregnantWomen", 
                  "newCasesMalariaSevere_under5", "newCasesMalariaSevere_5andOlder", "newCasesMalariaSevere_pregnantWomen",
                  "mildMalariaTreated_under5", "mildMalariaTreated_5andOlder", "mildMalariaTreated_pregnantWomen",
                  "severeMalariaTreated_under5", "severeMalariaTreated_5andOlder", "severeMalariaTreated_pregnantWomen",
                  "malariaDeaths_under5", "malariaDeaths_5andOlder", "malariaDeaths_pregnantWomen",
                  "suspectedMalaria_under5", "suspectedMalaria_5andOlder", "suspectedMalaria_pregnantWomen",
                  "presumedMalaria_under5", "presumedMalaria_5andOlder", "presumedMalaria_pregnantWomen")
  
  other_indicators <- c("totalCasesAllDiseases_under5", "totalCasesAllDiseases_5andOlder", "totalCasesAllDiseases_pregnantWomen",
                        "totalHospAllDiseases_under5", "totalHospAllDiseases_5andOlder", "totalHospAllDiseases_pregnantWomen",
                        "totalDeathsAllDiseases_under5", "totalDeathsAllDiseases_5andOlder", "totalDeathsAllDiseases_pregnantWomen")
  
  outputs <- c("ANC_1st", "ANC_2nd", "ANC_3rd", "ANC_4th", "SP_1st", "SP_2nd","SP_3rd", "ITN_received", "ITN_distAtANC", "ITN_distAtPreschool", 
               "VAR_0to11mos", "ASAQreceived_2to11mos", "ASAQreceived_1to5yrs", "ASAQreceived_6to13yrs", "ASAQreceived_14yrsAndOlder", 
               "ASAQused_2to11mos", "ASAQused_1to5yrs", "ASAQused_6to13yrs", "ASAQused_14yrsAndOlder", "ArtLum_received", "ArtLum_used", 
               "smearTest_completed", "smearTest_positive", "thinSmearTest", "RDT_completed", "RDT_positive", "RDT_received")
  
  SSC <- c("SSCACT", "SSCACT_5andOlder", "SSCACT_under5", "SSCRDT_completed", "SSCRDT_completed5andOlder", "SSCRDT_completedUnder5",                      
            "SSCRDT_positive", "SSCRDT_positive5andOlder", "SSCRDT_positiveUnder5", "SSCcasesCrossReferred", "SSCcasesCrossReferred_5andOlder",           
            "SSCcasesCrossReferred_under5", "SSCcasesReferred", "SSCcasesReferred_5andOlder", "SSCcasesReferred_under5", "SSCfevers",                                 
            "SSCfevers_5andOlder", "SSCfevers_under5")
  
  stockouts <- c("stockOutSP", "stockOutASAQ_2to11mos", "stockOutASAQ_1to5yrs", "stockOutASAQ_6to13yrs", "stockOutASAQ_14yrsAndOlder", 
                 "stockOutqui_pill", "stockOutqui_inj", "stockOutASAQ_inj", "stockOutRDT", "stockOutartLum")
  
  health_system <- c( "healthFacilities_total", "healthFacilities_numReportedWithinDeadline", "healthFacilities_Product", 
                      "supervisors_numPlanned", "supervisors_numActual", "employees_numPlanned", "employees_numActual", "awarenessTrainings_numPlanned", 
                      "awarenessTrainings_numActual")
  
  all_vars <- c(indicators, other_indicators, outputs, SSC, stockouts, health_system)
  
# ----------------------------------------------  
  
  
# ----------------------------------------------      
  indicator_names <- c(
    `newCasesMalariaMild` = "Confirmed Cases of Uncomplicated Malaria",
    `newCasesMalariaSevere` = "Cases of Hospitalized (Severe) Malaria",
    `mildMalariaTreated` = "Cases of Uncomplicated Malaria Treated",
    `severeMalariaTreated` = "Cases of Severe Malaria Treated",
    `malariaDeaths` = "Number of Deaths from Malaria",
    `presumedMalaria` = "Cases of Presumed Malaria",
    `suspectedMalaria` = "Cases of Suspected Malaria"
  )
  
  output_names <- c(
    `ArtLum` = "Artéméther - Lumefatrine",
    `SP` = "SP administered during ANC",
    `ASAQ` = "Artesunate Amodiaquine (ASAQ)",
    `ITN` = "ITNs",
    `ANC` = "Antenatal Care Visits",
    `RDT` = "Rapid Diagnostic Tests",
    `smearTest` = "Smear Tests",
    `VAR` = "Measles Vaccine",
    `thinSmearTest` = "Thin Smear Test", 
    `ASAQreceived` = "Artesunate Amodiaquine (ASAQ) Received", 
    `ASAQused` = "Artesunate Amodiaquine (ASAQ) Used"
  )    
  
  stockout_names <- c(
    `stockOutSP`= "Stock Out of SP",
    `stockOutASAQ`= "Stock Out of Artesunate Amodiaquine (ASAQ)",
    `stockOutqui`= "Stock Out of Quinine",
    `stockOutRDT`= "Stock Out of RDTs",
    `stockOutartLum`= "Stock Out of Artéméther - Lumefatrine"
  )
  
  SSC_names <- c(
    
  )
  
  health_system_names <- c(
    
    
  )
  
  # `healthFacilities` = "Health Facilities Reporting",
  # `reports` = "Number of Reports"
  
  variable_names <- c(indicator_names, output_names, stockout_names, SSC_names, health_system_names)



  
    