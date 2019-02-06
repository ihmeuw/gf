# --------------------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Correct 'disease' column of resource tracking database so 
#         it applies at the intervention level, not the file level. 
# DATE: Last updated December 2018
# --------------------------------------------------------------------

remap_diseases <- function(resource_database){
 
  #English corrections
  resource_database[module=='hivhealthsystemsstrengthening', disease:='hiv']
  resource_database[module=='malhealthsystemsstrengthening', disease:='malaria']
  resource_database[module=='tbhealthsystemsstrengthening', disease:='tb']
  
  #Need to fix diseases for these. 
  # map = map[!(module == 'programmanagement'  & disease != 'hss' & prefix=='R')]
  # map = map[!(module == 'gestiondelasubvention' & disease != 'hss' & prefix=='R')]
  # map = map[!(module == 'gestiondesubvenciones' & disease != 'hss' & prefix=='R')]
  # map = map[!(module == 'gestiondeprogramas' & disease != 'hss' & prefix=='R')]
  # map = map[!(module == 'gestiondeprogramme' & disease != 'hss' & prefix=='R')]
  # map = map[!(module == 'gestiondeprogrammegestiondesubvention' & intervention == 'gestiondeprogrammegestiondesubvention' & prefix == 'R')]
  # map = map[!(module == 'programmanagementandadministration' & intervention == 'all' & code == 'R8')]
  
  #Try running a test here grepping on 'hiv', 'malaria', and 'tb' and see if that will work as a blanket change. 
<<<<<<< Updated upstream:resource_tracking/prep/3a_remap_diseases.R
=======
  
  #Remap all RSSH codes to the RSSH disease, and make sure there aren't any HSS diseases still hanging around. 
  resource_database[, short_code:=substring(resource_database$code, 1, 1)]
  resource_database[short_code=='R', disease:='rssh']
  resource_database[disease == 'hss', disease:='rssh']
  
  
  resource_database[, short_code:=NULL]
>>>>>>> Stashed changes:resource_tracking/prep/global_fund_prep/4_remap_diseases.R
 
  return(resource_database) 
}
