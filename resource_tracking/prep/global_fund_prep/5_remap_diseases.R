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
  
  #French corrections 
  resource_database[module == 'priseenchargeetpreventiondelatuberculose' & disease == 'hiv', disease:='tb']
  
  return(resource_database) 
}
