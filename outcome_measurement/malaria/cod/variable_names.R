

# variable names (to source into different R scripts)

# vectors of indicator names and intervention names to subset
  indicators <- c("newCasesMalariaMild_under5", "newCasesMalariaMild_5andOlder", "newCasesMalariaMild_pregnantWomen", "newCasesMalariaSevere_under5", "newCasesMalariaSevere_5andOlder", "newCasesMalariaSevere_pregnantWomen",
                  "mildMalariaTreated_under5", "mildMalariaTreated_5andOlder", "mildMalariaTreated_pregnantWomen",
                  "severeMalariaTreated_under5", "severeMalariaTreated_5andOlder", "severeMalariaTreated_pregnantWomen",
                  "malariaDeaths_under5", "malariaDeaths_5andOlder", "malariaDeaths_pregnantWomen")
  
  indicator_names <- c(
    `newCasesMalariaMild` = "Confirmed Cases of Uncomplicated Malaria",
    `newCasesMalariaSevere` = "Confirmed Cases of Severe Malaria",
    `mildMalariaTreated` = "Cases of Uncomplicated Malaria Treated",
    `severeMalariaTreated` = "Cases of Severe Malaria Treated",
    `malariaDeaths` = "Number of Deaths from Malaria"
  )
  
  intervention_names <- c(
    `ArtLum` = "Artéméther - Lumefatrine",
    `SP` = "SP administered during ANC",
    `ASAQ` = "Artesunate Amodiaquine (ASAQ)",
    `ITN` = "ITNs",
    `ANC` = "Antenatal Care Visits",
    `RDT` = "Rapid Diagnostic Tests",
    `smearTest` = "Smear Tests",
    `VAR` = "Measles Vaccine",
    `healthFacilities` = "Health Facilities Reporting",
    `reports` = "Number of Reports"
  )    
  
  variable_names <- c(indicator_names, intervention_names)
  
  interventions <- c("ANC_1st", "ANC_2nd", "ANC_3rd", "ANC_4th", "SP_1st", "SP_2nd","SP_3rd", "ITN_received", "ITN_distAtANC",
                     "ITN_distAtPreschool", "VAR", "ASAQ_2to11mos", "ASAQ_1to5yrs", "ASAQ_6to13yrs", "ASAQ_14yrsAndOlder", "ArtLum_received", "ArtLum_used",
                     "smearTest_completed", "smearTest_positive", "RDT_completed", "RDT_positive", "healthFacilities_total", "healthFacilitiesProduct")

  # totalCasesAllDiseases	under5
  # totalCasesAllDiseases	5andOlder
  # totalCasesAllDiseases	pregnantWomen
  # suspectedMalaria	under5
  # suspectedMalaria	5andOlder
  # suspectedMalaria	pregnantWomen
  # presumedMalaria	under5
  # presumedMalaria	5andOlder
  # presumedMalaria	pregnantWomen
  # newCasesMalariaMild	under5
  # newCasesMalariaMild	5andOlder
  # newCasesMalariaMild	pregnantWomen
  # totalHospAllDiseases	under5
  # totalHospAllDiseases	5andOlder
  # totalHospAllDiseases	pregnantWomen
  # newCasesMalariaSevere	under5
  # newCasesMalariaSevere	5andOlder
  # newCasesMalariaSevere	pregnantWomen
  # mildMalariaTreated	under5
  # mildMalariaTreated	5andOlder
  # mildMalariaTreated	pregnantWomen
  # severeMalariaTreated	under5
  # severeMalariaTreated	5andOlder
  # severeMalariaTreated	pregnantWomen
  # totalDeathsAllDiseases	under5
  # totalDeathsAllDiseases	5andOlder
  # totalDeathsAllDiseases	pregnantWomen
  # malariaDeaths	under5
  # malariaDeaths	5andOlder
  # malariaDeaths	pregnantWomen
  # ANC	1st
  # ANC	2nd
  # ANC	3rd
  # ANC	4th
  # SP	1st
  # SP	2nd
  # SP	3rd
  # ITN	received
  # ITN	distAtANC
  # ITN	distAtPreschool
  # VAR	0to11mos
  # VAR	none
  # ASAQreceived	2to11mos
  # ASAQreceived	1to5yrs
  # ASAQreceived	6to13yrs
  # ASAQreceived	14yrsAndOlder
  # ASAQused	2to11mos
  # ASAQused	1to5yrs
  # ASAQused	6to13yrs
  # ASAQused	14yrsAndOlder
  # ASAQused	total
  # ArtLum	received
  # ArtLum	used
  # stockOutSP	none
  # stockOutASAQ	2to11mos
  # stockOutASAQ	1to5yrs
  # stockOutASAQ	6to13yrs
  # stockOutASAQ	14yrsAndOlder
  # stockOutqui	pill
  # stockOutqui	inj
  # stockOutASAQ	inj
  # stockOutRDT	none
  # stockOutartLum	none
  # smearTest	completedUnder5
  # smearTest	completed5andOlder
  # smearTest	positiveUnder5
  # smearTest	positive5andOlder
  # smearTest	completed
  # smearTest	positive
  # RDT	received
  # RDT	completedUnder5
  # RDT	completed5andOlder
  # RDT	positiveUnder5
  # RDT	positive5andOlder
  # RDT	completed
  # RDT	positive
  # peopleTested	under5
  # peopleTested	5andOlder
  # PMA	ASAQ
  # PMA	TPI
  # PMA	ITN
  # PMA	complete
  # reports	received
  # reports	expected
  # healthFacilities	total
  # healthFacilities	numReported
  # healthFacilities	numReportedWithinDeadline
  # supervisors	numPlanned
  # supervisors	numActual
  # employees	numPlanned
  # employees	numActual
  # awarenessTrainings	numPlanned
  # awarenessTrainings	numActual
  # 