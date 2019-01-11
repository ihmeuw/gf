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
 
  return(resource_database) 
}
