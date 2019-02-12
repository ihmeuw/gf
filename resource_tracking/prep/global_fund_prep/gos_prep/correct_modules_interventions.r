# --------------------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Correct modules/interventions in GOS data 
# DATE: Last updated February 2018
# --------------------------------------------------------------------

correct_modules_interventions = function(gos){
  
  gos = split_mods_interventions(gos, "preventionbehavioralchangecommunicationcommunityoutreach", "prevention")
  gos = split_mods_interventions(gos, "preventionbloodsafetyanduniversalprecautions", "prevention")
  
  return(gos)
}