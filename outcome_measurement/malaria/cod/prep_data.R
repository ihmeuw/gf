
# ----------------------------------------------
# function to prep the DRC PNLP data
  prep_data <- function(dataSheet, sheetname, index){
    
  # column names
  # ----------------------------------------------
      columnNames2016 <- c("province", "dps", "health_zone", "donor", "operational_support_partner", "population", "quarter", "month", 
                               "totalCasesAllDiseases_under5", "totalCasesAllDiseases_5andOlder", "totalCasesAllDiseases_pregnantWomen", 
                               "suspectedMalaria_under5", "suspectedMalaria_5andOlder", "suspectedMalaria_pregnantWomen",
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
                               "PMA_ASAQ", "PMA_TPI", "PMA_ITN", "PMA_complete",
                               "reports_received", "reports_expected", "healthFacilities_total", "healthFacilities_numReported", "healthFacilities_numReportedWithinDeadline",
                               "hzTeam_supervisors_numPlanned", "hzTeam_supervisors_numActual", "hzTeam_employees_numPlanned", "hzTeam_employees_numActual",
                               "awarenessTrainings_numPlanned", "awarenessTrainings_numActual",
                               "SSC_fevers", "SSC_RDT_completed", "SSC_RDT_positive", 
                               "SSC_ACT", "SSC_casesReferred", "SSC_casesCrossReferred")
      
      columnNames2015 <- c("province", "dps", "health_zone", "donor", "operational_support_partner", "population", "quarter", "month", 
                           "totalCasesAllDiseases_under5", "totalCasesAllDiseases_5andOlder", "totalCasesAllDiseases_pregnantWomen", 
                           "suspectedMalaria_under5", "suspectedMalaria_5andOlder", "suspectedMalaria_pregnantWomen",
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
                           "PMA_ASAQ", "PMA_TPI", "PMA_ITN", "PMA_complete",
                           "reports_received", "reports_expected", "healthFacilities_total", "healthFacilities_numReported", "healthFacilities_numReportedWithinDeadline",
                           "hzTeam_supervisors_numPlanned", "hzTeam_supervisors_numActual", 
                           "awarenessTrainings_numPlanned", "awarenessTrainings_numActual",
                           "SSC_fevers", "SSC_RDT_completed", "SSC_RDT_positive", 
                           "SSC_ACT", "SSC_casesReferred", "SSC_casesCrossReferred")
      
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
      
      columnNames2014 <- c("province", "dps", "health_zone", "donor", "operational_support_partner", "population", "quarter", "month", 
                           "totalCasesAllDiseases_under5", "totalCasesAllDiseases_5andOlder", "totalCasesAllDiseases_pregnantWomen", 
                           "suspectedMalaria_under5", "suspectedMalaria_5andOlder", "suspectedMalaria_pregnantWomen",
                           "newCasesMalariaMild_under5", "newCasesMalariaMild_5andOlder", "newCasesMalariaMild_pregnantWomen", 
                           "totalHospAllDiseases_under5", "totalHospAllDiseases_5andOlder", "totalHospAllDiseases_pregnantWomen",
                           "newCasesMalariaSevere_under5", "newCasesMalariaSevere_5andOlder", "newCasesMalariaSevere_pregnantWomen",
                           "mildMalariaTreated_under5", "mildMalariaTreated_5andOlder", "mildMalariaTreated_pregnantWomen",
                           "severeMalariaTreated_under5", "severeMalariaTreated_5andOlder", "severeMalariaTreated_pregnantWomen",
                           "totalDeathsAllDiseases_under5", "totalDeathsAllDiseases_5andOlder", "totalDeathsAllDiseases_pregnantWomen",
                           "malariaDeaths_under5", "malariaDeaths_5andOlder", "malariaDeaths_pregnantWomen",  
                           "ANC_1st", "SP_1st", "SP_2nd","SP_3rd", 
                           "ITN_received", "ITN_distAtANC", "ITN_distAtPreschool",  
                           "ASAQ_received_2to11mos", "ASAQ_received_1to5yrs", "ASAQ_received_6to13yrs", "ASAQ_received_14yrsAndOlder",
                           "ASAQ_used_2to11mos", "ASAQ_used_1to5yrs", "ASAQ_used_6to13yrs", "ASAQ_used_14yrsAndOlder", "ASAQ_used_total",
                           "stockOut_SP", "stockOut_ASAQ_2to11mos", "stockOut_ASAQ_1to5yrs", "stockOut_ASAQ_6to13yrs", "stockOut_ASAQ_14yrsAndOlder", 
                           "stockOut_qui_pill", "stockOut_qui_inj", "stockOut_ASAQ_inj",
                           "smearTest_completed", "smearTest_positive", "thinSmearTest", 
                           "RDT_received", "RDT_completed", "RDT_positive",
                           "PMA_ASAQ", "PMA_TPI", "PMA_ITN", "PMA_complete",
                           "reports_received", "reports_expected", "healthFacilities_total", "healthFacilities_numReported",
                           "hzTeam_supervisors_numPlanned", "hzTeam_supervisors_numActual",
                           "awarenessTrainings_numPlanned", "awarenessTrainings_numActual"
                          )
      
      columnNames2011to2013 <- c("province", "dps", "health_zone", "donor", "operational_support_partner", "population", "quarter", "month", 
                           "totalCasesAllDiseases_under5", "totalCasesAllDiseases_5andOlder", "totalCasesAllDiseases_pregnantWomen", 
                           "newCasesMalariaMild_under5", "newCasesMalariaMild_5andOlder", "newCasesMalariaMild_pregnantWomen", 
                           "totalHospAllDiseases_under5", "totalHospAllDiseases_5andOlder", "totalHospAllDiseases_pregnantWomen",
                           "newCasesMalariaSevere_under5", "newCasesMalariaSevere_5andOlder", "newCasesMalariaSevere_pregnantWomen",
                           "totalDeathsAllDiseases_under5", "totalDeathsAllDiseases_5andOlder", "totalDeathsAllDiseases_pregnantWomen",
                           "malariaDeaths_under5", "malariaDeaths_5andOlder", "malariaDeaths_pregnantWomen",  
                           "ANC_1st", "SP_1st", "SP_2nd",
                           "ITN_received", "ITN_distAtANC", "ITN_distAtPreschool",
                           "ASAQ_received_2to11mos", "ASAQ_received_1to5yrs", "ASAQ_received_6to13yrs", "ASAQ_received_14yrsAndOlder",
                           "ASAQ_used_2to11mos", "ASAQ_used_1to5yrs", "ASAQ_used_6to13yrs", "ASAQ_used_14yrsAndOlder", "ASAQ_used_total",
                           "stockOut_SP", "stockOut_ASAQ_2to11mos", "stockOut_ASAQ_1to5yrs", "stockOut_ASAQ_6to13yrs", "stockOut_ASAQ_14yrsAndOlder", 
                           "stockOut_qui_pill", "stockOut_qui_inj", 
                           "smearTest_completed", "smearTest_positive", 
                           "RDT_received", "RDT_completed", "RDT_positive",
                           "PMA_ASAQ", "PMA_TPI", "PMA_ITN", "PMA_complete",
                           "reports_received", "reports_expected", "healthFacilities_total", "healthFacilities_numReported"
                          )
      
      columnNames2010 <- c("province", "dps", "health_zone", "donor", "operational_support_partner", "population", "quarter", "month", 
                                   "totalCasesAllDiseases_under5", "totalCasesAllDiseases_5andOlder", "totalCasesAllDiseases_pregnantWomen", 
                                   "newCasesMalariaMild_under5", "newCasesMalariaMild_5andOlder", "newCasesMalariaMild_pregnantWomen", 
                                   "totalHospAllDiseases_under5", "totalHospAllDiseases_5andOlder", "totalHospAllDiseases_pregnantWomen",
                                   "newCasesMalariaSevere_under5", "newCasesMalariaSevere_5andOlder", "newCasesMalariaSevere_pregnantWomen",
                                   "totalDeathsAllDiseases_under5", "totalDeathsAllDiseases_5andOlder", "totalDeathsAllDiseases_pregnantWomen",
                                   "malariaDeaths_under5", "malariaDeaths_5andOlder", "malariaDeaths_pregnantWomen",  
                                   "ANC_1st", "SP_1st", "SP_2nd",
                                   "ITN_received", "ITN_distAtANC", "ITN_distAtPreschool",
                                   "ASAQ_received_2to11mos", "ASAQ_received_1to5yrs", "ASAQ_received_6to13yrs", "ASAQ_received_14yrsAndOlder",
                                   "ASAQ_used_2to11mos", "ASAQ_used_1to5yrs", "ASAQ_used_6to13yrs", "ASAQ_used_14yrsAndOlder", "ASAQ_used_total",
                                   "stockOut_SP", "stockOut_ASAQ_2to11mos", "stockOut_ASAQ_1to5yrs", "stockOut_ASAQ_6to13yrs", "stockOut_ASAQ_14yrsAndOlder", 
                                   "stockOut_qui_pill", "stockOut_qui_inj", 
                                   "smearTest_completed", "smearTest_positive", 
                                   "RDT_received", "RDT_completed", "RDT_positive",
                                   "PMA_ASAQ", "PMA_TPI", "PMA_ITN", "PMA_complete",
                                   "reports_received", "reports_expected")
      
      
  # ----------------------------------------------
    # fix various issues in spelling/typos or extra columns in the data sheet:
      if ( PNLP_files$year[index] == 2011 | PNLP_files$year[index] == 2010 ) {
        dataSheet <- dataSheet[ , -c("X__14") ]
      }
      
      if ((PNLP_files$year[index] == 2014 | PNLP_files$year[index] == 2013| PNLP_files$year[index] == 2012 )& sheetname == "BC") {
        #dataSheet <- dataSheet[,names(dataSheet)[-length(names(dataSheet))], with=F]
        dataSheet <- dataSheet[, -ncol(dataSheet), with=F ]
      }
      
      if ( PNLP_files$year[index] == 2016 & sheetname == "KIN") {
        dataSheet <- dataSheet[ , -c("X__72", "X__73", "X__74") ]
      }
      
  
    # set column names, depending on differences in years and/or sheets
      if ( PNLP_files$year[index] == 2014 ) {
        columnNames <- columnNames2014
      } else if (PNLP_files$year[index] < 2014 & PNLP_files$year[index] != 2010) {
        columnNames <- columnNames2011to2013
      } else if (PNLP_files$year[index] == 2010) {
        columnNames <- columnNames2010
      } else if (PNLP_files$year[index] == 2016) {
        columnNames <- columnNames2016
      } else if (PNLP_files$year[index] == 2015) {
        columnNames <- columnNames2015
      } else {
        columnNames <- columnNamesComplete
      }
     
      names(dataSheet) <- columnNames
        
    if (PNLP_files$year[index] == 2012 & sheetname == "EQ"){
      dataSheet <- dataSheet[(totalCasesAllDiseases_under5=="971" & totalCasesAllDiseases_5andOlder=="586" & totalCasesAllDiseases_pregnantWomen=="99"), 
                    province:="Equateur"]
    }
      if (PNLP_files$year[index] == 2012 & sheetname == "EQ"){
        dataSheet <- dataSheet[(totalCasesAllDiseases_under5=="971" & totalCasesAllDiseases_5andOlder=="586" & totalCasesAllDiseases_pregnantWomen=="99"), 
                               dps:="Sud Uban"]
      }
      if (PNLP_files$year[index] == 2012 & sheetname == "EQ"){
        dataSheet <- dataSheet[(totalCasesAllDiseases_under5=="971" & totalCasesAllDiseases_5andOlder=="586" & totalCasesAllDiseases_pregnantWomen=="99"), 
                               health_zone:="Libenge"]
      }
      if (PNLP_files$year[index] == 2012 & sheetname == "EQ"){
        dataSheet <- dataSheet[(totalCasesAllDiseases_under5=="971" & totalCasesAllDiseases_5andOlder=="586" & totalCasesAllDiseases_pregnantWomen=="99"), 
                               month:= "Janvier"]
      }

    # add a column for the "year" to keep track of this variable as we add dataSheets to this one
      dataSheet$year <- PNLP_files$year[index]
      
      # ----------------------------------------------
      # Get rid of rows you don't need- "subset"
      
      # delete rows where the month column is NA (totals rows or any trailing rows)
      dataSheet <- dataSheet[!is.na(month)]
      dataSheet <- dataSheet[!month==0]
      
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
      
      if (sheetname == "SK"){
        dataSheet <- dataSheet[province==0, province := "SK"]
        dataSheet <- dataSheet[is.na(province), province := "SK"]
      }
      
      # delete first row if it's first value is "NA" or "PROVINCE" as a way to
      # only delete those unnecessary rows, and not any others accidentally - these
      # were the column headers in the original datasheet in excel.
      dataSheet <- dataSheet[!province %in% c('PROVINCE', 'Province')]
      
        # BDD 2016 sheet has total row in the middle of the data, the other sheets have it
        # in the last row of the sheet, sometimes in the first column, sometimes in the second;
        # sometimes as "Total" and sometimes "TOTAL"
         dataSheet <- dataSheet[!grepl(("TOTAL"), (dataSheet$province)),]
         dataSheet <- dataSheet[!grepl(("Total"), (dataSheet$province)),]
         dataSheet <- dataSheet[!grepl(("total"), (dataSheet$province)),]
         dataSheet <- dataSheet[!grepl(("TOTAL"), (dataSheet$dps)),]
         dataSheet <- dataSheet[!grepl(("Total"), (dataSheet$dps)),]
         dataSheet <- dataSheet[!grepl(("total"), (dataSheet$province)),]

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
      dataSheet[grepl("janvier", month), month:="01"]
      dataSheet[grepl("février", month), month:="02"]
      dataSheet[grepl("mars", month), month:="03"]
      dataSheet[grepl("avril", month), month:="04"]
      dataSheet[grepl("mai", month), month:="05"]
      dataSheet[grepl("juin", month), month:="06"]
      dataSheet[grepl("juillet", month), month:="07"]
      dataSheet[grepl("août", month), month:="08"]
      dataSheet[grepl("septembre", month), month:="09"]
      dataSheet[grepl("octobre", month), month:="10"]
      dataSheet[grepl("novembre", month), month:="11"]
      dataSheet[grepl("décembre", month), month:="12"]    
      
      # accounting for spelling mistakes/typos/other variations
      dataSheet[grepl("fevrier", month), month:="02"]
      dataSheet[grepl("Fevrier", month), month:="02"]
      dataSheet[grepl("JUIN", month), month:="06"]
      dataSheet[grepl("Aout", month), month:="08"]
      dataSheet[grepl("Septembr", month), month:="09"]
      dataSheet[grepl("Decembre", month), month:="12"]
      
      
      # make string version of the date
      dataSheet[, stringdate:=paste('01', month, year, sep='/')]
      
      # combine year and month into one variable
      dataSheet[, date:=as.Date(stringdate, "%d/%m/%Y")]
      
      # make names of health zones consistent (change abbreviatons to full name in select cases)
      if (PNLP_files$year[index] == 2014 & sheetname == "EQ"){
        dataSheet <- dataSheet[(health_zone=="Libenge" & month=="01" & totalCasesAllDiseases_under5=="754"), health_zone:= "Mawuya"]
      }
      if (PNLP_files$year[index] == 2013 & sheetname == "EQ"){
        dataSheet <- dataSheet[(health_zone=="Libenge" & month=="01" & totalCasesAllDiseases_under5=="1628"), health_zone:= "Mawuya"]
      }
      
      if (PNLP_files$year[index] == 2014 & sheetname == "KAT"){
        dataSheet <- dataSheet[health_zone=="Mutshat", health_zone:= "Mutshatsha"]
        dataSheet <- dataSheet[health_zone=="Malem Nk", health_zone:= "Malemba Nkulu"]
      }
      if (PNLP_files$year[index] == 2013 & sheetname == "BDD"){
        dataSheet <- dataSheet[health_zone=="Koshiba", health_zone:= "Koshibanda"]
      }
      
      if ((PNLP_files$year[index] == 2013 | PNLP_files$year[index] == 2012 )& sheetname == "KOR"){
        dataSheet <- dataSheet[health_zone=="Mbuji May", health_zone:= "Bimpemba"]
      }
      
      # there are still some added rows that happen to have something in the month column but are missing data everywhere else
      dataSheet <- dataSheet[!is.na(province)]
      
      # ----------------------------------------------
      # Return current data sheet
      return(dataSheet)
      # ----------------------------------------------
    }  
# ----------------------------------------------

 # currentSheet[health_zone=="Mbuji May"| health_zone== "Bimpemba", c(1:9)]
  