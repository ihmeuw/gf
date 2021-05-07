# verify mapping document for HIV target populations

# Merge the 2020-2022 module map onto the module/intervention pairs pulled from raw data. 
# This code never needs to be rerun (unless the framework changes!), just leaving for documentation. Emily Linebarger 2/12/2020 
local_dir <- "C:/Users/frc2/Desktop/"
map_20_22 = data.table(read_xlsx(paste0(local_dir, "2020-2022 Modular Framework.xlsx"), sheet="HIV Interventions"))
map_20_22[, disease:="hiv"]

setnames(map_20_22, old = c("population", "population_esp", "population_fr"), 
                            new = c("gf_population", "gf_population_esp", "gf_population_fr"))

saveRDS(map_20_22, paste0(local_dir, "2020_2022_MF_hiv_population.rds"))

map_20_22 = readRDS(paste0(local_dir, "2020_2022_MF_hiv_population.rds"))


# Take the full list of interventions, strip the diacritics and spaces, and add these onto the map as valid raw options. 
all_eng = map_20_22[, .(code, gf_module, gf_intervention, disease, gf_population)]
setnames(all_eng, old=c('gf_module', 'gf_intervention', 'gf_population'), new=c('module', 'intervention', 'population'))
all_fr = map_20_22[, .(code, gf_module_fr, gf_intervention_fr, disease, gf_population_fr)]
setnames(all_fr, old=c('gf_module_fr', 'gf_intervention_fr', 'gf_population_fr'), new=c('module', 'intervention', 'population'))
all_esp = map_20_22[, .(code, gf_module_esp, gf_intervention_esp, disease, gf_population_esp)]
setnames(all_esp, old=c('gf_module_esp', 'gf_intervention_esp', 'gf_population_esp'), new=c('module', 'intervention', 'population'))

all_langs = rbindlist(list(all_eng, all_fr, all_esp))
all_langs[, coefficient:=1]
all_langs = strip_chars(all_langs)
all_langs = strip_chars_pop(all_langs)
all_langs = all_langs[, -c('orig_module', 'orig_intervention', 'orig_population')]
module_map <- copy(all_langs)
module_map = unique(module_map) # This is your new 'Master' list for 2020-2022 raw extracted module/intervention pairs. 

# -------------------------------
#       FORMAT DATA 
#--------------------------------
original_map <- copy(module_map) #Save an original copy for comparison later 
new_rows <- fread(paste0(local_dir, "gf_population_mapping_additions_nfm3.csv")) #Add in new rows to previously approved map
new_rows = new_rows[, .(module, intervention, population, code, disease)]
module_map = rbind(module_map, new_rows, fill = TRUE)

stopifnot(nrow(module_map[is.na(module)])==0) 

# -------------------------------
#   CLEAN DATA BEFORE VALIDATING 
#--------------------------------
#These are the variables that are merged onto the raw data, so it's important to check duplicates with these. 
keyVars = c('module', 'intervention', 'population')

#Remove whitespace from 'code' column before doing checks below
module_map[, code:=trimws(code)]
map_20_22[, code:=trimws(code)]

stopifnot(nrow(module_map[code==""])==0) 

duplicates_check <- module_map[duplicated(module_map, by = keyVars), ]
duplicates_coeff_one <- duplicates_check[coefficient == 1]
# duplicates_coeff_one <- merge(duplicates_coeff_one, module_map, by = c(keyVars, 'coefficient', 'abbreviated_module')) #Merge back onto module_map because some duplicates don't have coefficients of 1. 
duplicates_coeff_one <- duplicates_coeff_one[order(module, intervention, disease)]

# Check
include_stops <- TRUE
if (nrow(duplicates_coeff_one[!module%in%c('paymentforresults', 'financiacionbasadaenlosresultados', 'financementbasesurlesresultats'  )]) != 0 & include_stops == TRUE){
  print(duplicates_coeff_one) 
  stop("Module/Intervention/Disease duplicates with coefficients of 1!")
}

stopifnot(nrow(module_map[code==""])==0) 

#--------------------------------------------------------------------------------
# Merge mapped codes to final mappings 
#--------------------------------------------------------------------------------
module_map = module_map[, .(code, module, intervention, disease, population)]
map_20_22 = unique(map_20_22)
map_20_22$disease <- NULL # Don't need this, because it actually represents the disease of the grant, not the disease of the intervention. 
module_map = merge(module_map, map_20_22, by=c('code'), all.x = TRUE)

stopifnot(nrow(module_map[code==""])==0)  

write.csv(module_map, paste0(local_dir, "gf_hivpop_mapping_nfm3.csv"), row.names = FALSE)
saveRDS(module_map, paste0(local_dir, "gf_hivpop_mapping_nfm3.rds"))

print("NFM3 HIV Key Population mapping completed.")
