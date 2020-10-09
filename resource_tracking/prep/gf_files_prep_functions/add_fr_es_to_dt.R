##### NOTE
# I added this code to the file "_common/shared_functions" because it wasn't being sourced by the RT rep code
# I am not sure if it's ok to leave there but perhaps this file can be archived and future edits can happen to the "shared_functions" file.
# --Francisco 8/14/2020

add_fr_es_to_dt = function(dt, module_merge_col= 'gf_module', intervention_merge_col= 'gf_intervention'){
  map_fr_es = readRDS("\\\\ihme.washington.edu/ihme/snfs/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/2018_2020_MF.rds")
  map_fr_es = map_fr_es[, .(disease, gf_module, gf_intervention, gf_module_fr, gf_intervention_fr, gf_module_esp, gf_intervention_esp)]
  
  dt = merge(dt, map_fr_es, all.x = TRUE, by.x = c(module_merge_col, intervention_merge_col, 'disease'), by.y = c('gf_module', 'gf_intervention', 'disease'))
  
  return(dt)
}