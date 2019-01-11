# --------------------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Correct 'disease' column of resource tracking database so 
#         it applies at the intervention level, not the file level. 
# DATE: Last updated December 2018
# --------------------------------------------------------------------

remap_diseases <- function(resource_database){
 
  resource_database[module=='hivhealthsystemsstrengthening', disease:='hiv']
  resource_database[module=='malhealthsystemsstrengthening', disease:='malaria']
  resource_database[module=='tbhealthsystemsstrengthening', disease:='tb']
  
  #Try running a test here grepping on 'hiv', 'malaria', and 'tb' and see if that will work as a blanket change. 
 
  return(resource_database) 
}
