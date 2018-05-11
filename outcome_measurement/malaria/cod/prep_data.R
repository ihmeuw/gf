

# function to prep the DRC PNLP data
  prep_data <- function(dataSheet, sheetname){
    
    # Load data sheet
    #dataSheet <- data.table(read_excel(paste0(dir, "/", PNLP_files$File.Names[j], ".xls"), sheet= s))
    # ----------------------------------------------
    # Change column names, so that each column has 
    # since some of them have less columns, make a vector of names and then have "if ___, then drop these ___"
    
    # columnNames <- c("province", "dps", "health_zone", "donor", "operational_support_partner", "population",
    #                          "quarter", "month", "newCasesMalariaMild_under5", "newCasesMalariaMild_5andOlder", "newCasesMalariaMild_pregnantWomen", "newCasesMalariaSevere_under5", "newCasesMalariaSevere_5andOlder", "newCasesMalariaSevere_pregnantWomen",
    #                          "mildMalariaTreated_under5", "mildMalariaTreated_5andOlder", "mildMalariaTreated_pregnantWomen",
    #                          "severeMalariaTreated_under5", "severeMalariaTreated_5andOlder", "severeMalariaTreated_pregnantWomen",
    #                          "malariaDeaths_under5", "malariaDeaths_5andOlder", "malariaDeaths_pregnantWomen", "ANC_1st", "ANC_2nd", "ANC_3rd", "ANC_4th", "SP_1st", "SP_2nd","SP_3rd", "ITN_received", "ITN_distAtANC",
    #                          "ITN_distAtPreschool", "VAR", "ASAQ_2to11mos", "ASAQ_1to5yrs", "ASAQ_6to13yrs", "ASAQ_14yrsAndOlder", "ASAQ_total", "ArtLum_received", "ArtLum_used", "smearTest_completedUnder5",
    #                          "smearTest_completed5andOlder", "smearTest_positiveUnder5", "smearTest_positive5andOlder", "RDT_completedUnder5", "RDT_completed5andOlder", "RDT_positiveUnder5", "RDT_positive5andOlder",
    #                          "reports_received", "reports_expected", "healthFacilities_total", "healthFacilities_numReported", "healthFacilities_numReportedWithinDeadline" )
    
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
    
    # columnNames2014 <- c("province", "dps", "health_zone", "donor", "operational_support_partner", "population",
    #       "quarter", "month", "newCasesMalariaMild_under5", "newCasesMalariaMild_5andOlder", "newCasesMalariaMild_pregnantWomen", "newCasesMalariaSevere_under5", "newCasesMalariaSevere_5andOlder", "newCasesMalariaSevere_pregnantWomen",
    #       "mildMalariaTreated_under5", "mildMalariaTreated_5andOlder", "mildMalariaTreated_pregnantWomen",
    #       "severeMalariaTreated_under5", "severeMalariaTreated_5andOlder", "severeMalariaTreated_pregnantWomen",
    #       "malariaDeaths_under5", "malariaDeaths_5andOlder", "malariaDeaths_pregnantWomen", "ANC_1st", "SP_1st", "SP_2nd","SP_3rd", "ITN_received", "ITN_distAtANC",
    #       "ITN_distAtPreschool", "ASAQ_2to11mos", "ASAQ_1to5yrs", "ASAQ_6to13yrs", "ASAQ_14yrsAndOlder", "ASAQ_total", "smearTest_completed", "smearTest_positive", "RDT_completed", 
    #       "RDT_positive", "reports_received", "reports_expected", "healthFacilities_total", "healthFacilities_numReported")
    
    # #for the new data this doesn't work based on the name of the file.  Assign a new variable for year?
    #   if ( PNLP_files$year[9] == 2014 ) {
    #     columnNames <- columnNames2014 
    #   } else {
    #     columnNames <- columnNamesComplete
    #   }
    
    names(dataSheet) <- columnNamesComplete
    
    # add a column for the "year" to keep track of this variable as we add dataSheets to this one
    dataSheet$year <- PNLP_files$year[9]
    
    # ----------------------------------------------
    # Get rid of rows you don't need- "subset"
    
    # delete rows where the month column is NA (totals rows or any trailing rows)
    dataSheet <- dataSheet[!is.na(month)]
    
    # clean "Province" column in BDD datasheet for 2016 and 2015 because
    # it has some missing/"0" values that should be "BDD" - doesn't work
    
    if (sheetname == "BDD"){
      dataSheet <- dataSheet[province==0, province := sheetname]
      dataSheet <- dataSheet[is.na(province), province := sheetname]
    }
    
    if (sheetname == "KOR"){
      dataSheet <- dataSheet[province==0, province := "K.Or"]
      dataSheet <- dataSheet[is.na(province), province := "K.Or"]
    }
    
    # delete first row if it's first value is "NA" or "PROVINCE" as a way to
    # only delete those unnecessary rows, and not any others accidentally - these
    # were the column headers in the original datasheet in excel.
    dataSheet <- dataSheet[!province %in% c('PROVINCE', 'Province')]
    
    # using this to delete rows tacked on to the end of the DF with all NA values
    # by checking to see if 2nd column is NA
    # maybe redundant after the above command; not sure if this way would be more fail safe to avoid
    # accidentally deleting any data
    dataSheet <- dataSheet[!is.na(dps),]
    
    # No longer needed??
    # using this to delete "totals" rows
    #   # BDD 2016 sheet has total row in the middle of the data, the other sheets have it
    #   # in the last row of the sheet, sometimes in the first column, sometimes in the second;
    #   # sometimes as "Total" and sometimes "TOTAL"
    #    dataSheet <- dataSheet[!grepl(("TOTAL"), (dataSheet$province)),]
    #    dataSheet <- dataSheet[!grepl(("Total"), (dataSheet$province)),]
    #    dataSheet <- dataSheet[!grepl(("TOTAL"), (dataSheet$dps)),]
    #    dataSheet <- dataSheet[!grepl(("Total"), (dataSheet$dps)),]
    #    
    # ----------------------------------------------
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
    # Return current data sheet
    return(dataSheet)
    # ----------------------------------------------
  }  #END OF FUNCTION
  # ----------------------------------------------
