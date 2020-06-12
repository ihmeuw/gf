add_fr_es_to_dt = function(dt, module_merge_col= 'gf_module', intervention_merge_col= 'gf_intervention'){
  map_fr_es = readRDS("J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/2018_2020_MF.rds")
  map_fr_es = map_fr_es[, .(disease, gf_module, gf_intervention, gf_module_fr, gf_intervention_fr, gf_module_esp, gf_intervention_esp)]
  
  dt = merge(dt, map_fr_es, all.x = TRUE, by.x = c(module_merge_col, intervention_merge_col, 'disease'), by.y = c('gf_module', 'gf_intervention', 'disease'))
  
  return(dt)
}