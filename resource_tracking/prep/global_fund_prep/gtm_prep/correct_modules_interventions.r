# --------------------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Correct unmapped modules for GTM resource tracking database. 
#         Add initial, date, and filepath.
# DATE: Last updated December 2018
# --------------------------------------------------------------------

correct_modules_interventions = function(resource_database){
  
  #EKL 10/25/18, official_budgets/03. Presupuesto detallado.xlsx 
  resource_database$intervention = ifelse(resource_database$intervention == "detecciondecasosydiagnosticotbmdr", "detecciondecasosydiagnosticotbmr", 
                                resource_database$intervention)
  resource_database$intervention = ifelse(resource_database$intervention == "detecciondecasosydiagnostico" & resource_database$module == "paqueteparatbmr", "detecciondecasosydiagnosticotbmr", 
                                resource_database$intervention)
  resource_database$module = ifelse(resource_database$intervention == "detecciondecasosydiagnosticotbmr", "paqueteparatbmr", resource_database$module)
  resource_database$intervention = ifelse(resource_database$module == "paqueteparatbmr" & resource_database$intervention == "tratamiento", "tratamientotbmr", resource_database$intervention)        
  resource_database$intervention = ifelse(resource_database$module == "paqueteparatbmr" & resource_database$intervention == "prestaciondeatencioncomunitariaparatbmdr", 
                                "prestaciondeserviciosdeatenciondelatuberculosisenlacomunidad", resource_database$intervention)
  
  #EKL 10/30/18, official_budgets/03. Presupuesto detallado.xlsx
  resource_database$module = ifelse(resource_database$module == "atencionyprevenciondetuberculosis" & resource_database$intervention == "srssrespuestaysistemacomunitrio", 
                          "ssrsrespuestasysistemascomunitarios", 
                          resource_database$module)
  resource_database$intervention = ifelse(resource_database$module == "ssrsrespuestasysistemascomunitarios" & resource_database$intervention == "srssrespuestaysistemacomunitrio", 
                                "otrasintervencionespararespuestasysistemascomunitarios", resource_database$intervention)
  resource_database$intervention = ifelse(resource_database$module =="atencionyprevenciondetuberculosis" & resource_database$intervention == "implicaratodoslosproveedoresdeatencionatencionyprevenciondetb", 
                                "implicaratodoslosproveedoresdeasistencia", resource_database$intervention)
  
  #EKL 12/17/18 "03. Presupuesto detallado.xlsx" "GTM_T_Full_Budget_9Sept2018.xlsx"
  resource_database[module == "atencionyprevenciondetuberculosis" & intervention == "implicaratodoslosproveedoresdeatencionatencionyprevenciondetb",
                    intervention:="implicaratodoslosproveedoresdeasistencia"]
  resource_database[module == "detecciondecasosydiagnosticotbmdr", module:= "detecciondecasosydiagnosticotbmr"]
  
  return(resource_database)
}