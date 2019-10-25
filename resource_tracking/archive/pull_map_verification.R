#Review the current version of the map, and pull out a version to be hand-verified by a researcher. 
# Only want to keep the rows of the map that are not in the previously approved version, and are currently 
# being applied to the final files in our database. 

rm(list=ls())
user = "elineb" #Change to your username 
code_dir = ifelse(Sys.info()[1]=='Windows', paste0("C:/Users/", user, "/Documents/gf/"), paste0('/homes/', user, '/gf/'))
source(paste0(code_dir, "resource_tracking/prep/_common/set_up_r.R"), encoding="UTF-8")

#Read in files 
full_map = readRDS(paste0(mapping_dir, "gf_mapping.rds"))
map_additions = fread(paste0(mapping_dir, "gf_mapping_additions.csv"))
all_interventions = fread(paste0(mapping_dir, "all_interventions.csv"))

files = readRDS(paste0(combined_output_dir, "budget_pudr_iterations.rds"))
gos = readRDS(paste0(gos_prepped, "prepped_gos_data.rds"))

files_gos = rbind(files, gos, fill = TRUE)
files_gos = files_gos[, .(disease, module, intervention)]
files_gos = unique(files_gos)

#Prep your file. Only keep the observations from map_additions that are currently being used in final file, and that 
# are not previously approved. 
saveFile = merge(map_additions, files_gos, by=c('module', 'intervention', 'disease'))
print(paste0(nrow(map_additions), " rows in map_additions file"))
print(paste0(nrow(saveFile), " rows in final file that need to be verified"))

saveFile = merge(saveFile, all_interventions, by=c('code', 'disease'), all.x = TRUE)
saveFile = saveFile[, .(code, disease, module, intervention, coefficient, date_added, mf_format, notes, associated_file, module_eng, intervention_eng)]
setnames(saveFile, old=c('module', 'intervention', 'module_eng', 'intervention_eng'), new=c('orig_module', 'orig_intervention', 'new_module', 'new_intervention'))
col_order = c("orig_module", "orig_intervention", "new_module", "new_intervention", "disease", "coefficient", "mf_format", "associated_file")
saveFile = saveFile[, ..col_order]

saveFile[, verified_by:='']
saveFile[, verified_date:='']
#Save final output file 
write.csv(saveFile, paste0(mapping_dir, "to_verify_", Sys.Date(), ".csv"), row.names=F)
