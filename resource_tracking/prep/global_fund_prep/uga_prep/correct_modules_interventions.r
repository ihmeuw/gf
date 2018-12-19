# --------------------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Correct unmapped modules for UGA resource tracking database. 
#         Add initial, date, and filepath.
# DATE: Last updated December 2018
# --------------------------------------------------------------------

correct_modules_interventions <- function(resource_database){
 
  #Irena Chen, before November 2018 
  resource_database$module = ifelse(resource_database$module == "tbhivc", "tbhiv", resource_database$module)
  resource_database$intervention = ifelse(resource_database$intervention == "supportiveenvironmentotherspecifycssmonitoringevaluationevidencebuilding",
                                "supportiveenvironmentmonitoringdrugresistance", resource_database$intervention)
  resource_database$intervention = ifelse(resource_database$intervention == "informationsystemoperationalresearch" & resource_database$module == 'malhealthsystemsstrengthening',
                                "informationsystem", resource_database$intervention)
  
  # EKL 11/21/18, LFA reviewed UGA-T-MOFPED PUDR PE 31Dec2015_final.xlsx
  resource_database$intervention = ifelse(resource_database$module == "tbhivcommunitytbcaredelivery" & resource_database$intervention == "all", "communitytbhivcaredelivery", resource_database$intervention)
  resource_database$module = ifelse(resource_database$module == "tbhivcommunitytbcaredelivery" & resource_database$intervention == "communitytbhivcaredelivery", "tbhiv", resource_database$module)
  
  resource_database$intervention = ifelse(resource_database$module == "tbcareandpreventiontreatment" & resource_database$intervention == "all", "treatment", resource_database$intervention)
  resource_database$module = ifelse(resource_database$module == "tbcareandpreventiontreatment" & resource_database$intervention == "treatment", "tbcareandprevention", resource_database$module)
  
  resource_database$intervention = ifelse(resource_database$module == "tbhivtbhivcollaborativeinterventions" & resource_database$intervention == "all", "tbhivcollaborativeinterventions", resource_database$intervention)
  resource_database$module = ifelse(resource_database$module == "tbhivtbhivcollaborativeinterventions" & resource_database$intervention == "tbhivcollaborativeinterventions", "tbhiv", resource_database$module)
  
  resource_database$intervention = ifelse(resource_database$module == "tbhivcollaborativeactivitieswithotherprogramsandsectors" & resource_database$intervention == "all", "collaborativeactivitieswithotherprogramsandsectors", resource_database$intervention)
  resource_database$module = ifelse(resource_database$module == "tbhivcollaborativeactivitieswithotherprogramsandsectors" & resource_database$intervention == "collaborativeactivitieswithotherprogramsandsectors", "tbhiv", resource_database$module)
  
  resource_database$intervention = ifelse(resource_database$module == "mdrtbcasedetectionanddiagnosismdrtb" & resource_database$intervention == "all", "casedetectionanddiagnosismdrtb", resource_database$intervention)
  resource_database$intervention = ifelse(resource_database$module == "mdrtbpreventionformdrtb" & resource_database$intervention == "all", "preventionformdrtb", resource_database$intervention)
  resource_database$module = ifelse(resource_database$module == "mdrtbtreatmentmdrtb" & resource_database$intervention == "all", "treatmentmdrtb", resource_database$module)
  resource_database$intervention = ifelse(resource_database$module == "mdrtbcommunitytbcaredelivery" & resource_database$intervention == "all", "communitymdrtbcaredelivery", resource_database$intervention)
  resource_database$intervention = ifelse(resource_database$module == "mdrtbkeyaffectedpopulations" & resource_database$intervention == "all", "keyaffectedpopulations", resource_database$intervention)
  
  resource_database$module = ifelse(resource_database$module %in% c("mdrtbcasedetectionanddiagnosismdrtb", "mdrtbpreventionformdrtb", "mdrtbcommunitytbcaredelivery", "mdrtbkeyaffectedpopulations"), "mdrtb", resource_database$module)
  
  resource_database$intervention = ifelse(resource_database$module == "healthinformationsystemsandmeroutinereporting" & resource_database$intervention == "all", "routinereporting", resource_database$intervention)
  resource_database$intervention = ifelse(resource_database$module == "healthinformationsystemsandmeanalysisreviewandtransparency" & resource_database$intervention == "all", "analysisreviewandtransparency", resource_database$intervention)
  resource_database$intervention = ifelse(resource_database$module == "healthinformationsystemsandmesurveys" & resource_database$intervention == "all", "surveys", resource_database$intervention)
  
  resource_database$module = ifelse(resource_database$module %in% c("healthinformationsystemsandmeroutinereporting", "healthinformationsystemsandmeanalysisreviewandtransparency", "healthinformationsystemsandmesurveys"), "healthinformationsystemsandme", resource_database$module)
  
  
  resource_database$intervention = ifelse(resource_database$module == "communitysystemsstrengtheningcommunitybasedmonitoringforaccountability" & resource_database$intervention == "all", "communitybasedmonitoringforaccountability", resource_database$intervention)
  resource_database$intervention = ifelse(resource_database$module == "communitysystemsstrengtheningsocialmobilizationbuildingcommunitylinkagescollaborationandcoordination" & resource_database$intervention == "all", "socialmobilizationbuildingcommunitylinkagescollaborationandcoordination", resource_database$intervention)
  resource_database$module = ifelse(resource_database$module %in% c("communitysystemsstrengtheningcommunitybasedmonitoringforaccountability", "communitysystemsstrengtheningsocialmobilizationbuildingcommunitylinkagescollaborationandcoordination"), "communitysystemsstrengthening", resource_database$module)
  
  #EKL 12/18/18 
  resource_database[module== "rsshhumanresourcesforhealthhrhincludingcommunityhealthworkers", module := 'rsshhumanresourcesforhealthincludingcommunityhealthworkers']
  resource_database[module == "mdrtbtreatmentmdrtb", intervention:= "treatmentmdrtb"]
  resource_database[module == "mdrtbtreatmentmdrtb", module:= "mdrtb"]
  resource_database[module == "hivhealthsystemsstrengthening" & intervention == "tostrengthenhealthandcommunitysystemstoenablethedeliveryoftheproposedpmtctandtreatmentinterventions", 
                    intervention := 'servicedelivery']
  resource_database[module == "supportiveenvironment" & sda_activity == "Supportive environment: Program management and administration", intervention:='supportiveenvironmentprogrammanagementandadministration']
  resource_database[module == 'hivhealthsystemsstrengthening' & sda_activity == 'HSS: Service delivery', intervention:= 'servicedelivery']
  resource_database[module == 'hivhealthsystemsstrengthening' & sda_activity == "HSS: Information system & Operational research", intervention:= 'informationsystemoperationalresearch']
  resource_database[module == 'healthsystemstrengthening', module:= 'healthsystemsstrengthening']
  resource_database[module == 'healthsystemsstrengthening' & sda_activity == 'HSS: Community Systems Strengthening', intervention := 'communitysystemsstrengthening']
  resource_database[module == 'healthsystemsstrengthening' & sda_activity == 'HSS: Information system & Operational research', intervention := 'informationsystemoperationalresearch']
  resource_database[module == 'healthsystemsstrengthening' & sda_activity == 'Supportive environment: Program management and administration', intervention := 'supportiveenvironmentprogrammanagementandadministration']
  
  #EKL 12/18/18 "UGD-708-G08-M_PUDR 30Jun14.xls"
  resource_database[module == "pleaseselect", module:= 'all']
  
  
  return(resource_database) 
}
