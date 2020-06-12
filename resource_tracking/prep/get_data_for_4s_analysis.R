# Subset Tableau data to the relevant data for the 4S framework analysis
# Only RSSH data for the approved in grant-making budgets
# Down to activity/cost input level

library(data.table)

#Box filepaths - these should be used to source raw files, and to save final prepped files. 
user=as.character(Sys.info()[7])
box = paste0("C:/Users/",user,"/Box Sync/Global Fund Files/")
inFile = paste0(box, 'tableau_data/all_budget_revisions_activityLevel.csv')
outFile = paste0(box, 'tableau_data/rssh_4s_analysis_data.csv')

dt = as.data.table(read.csv(inFile))

dt = dt[budget_version == 'approved' & disease == 'rssh']

dt[loc_name %in% c('DRC', 'Senegal'), gf_module_orig := gf_module_fr]
dt[loc_name %in% c('DRC', 'Senegal'), gf_intervention_orig := gf_intervention_fr]
dt[loc_name %in% c('Guatemala'), gf_module_orig := gf_module_esp]
dt[loc_name %in% c('Guatemala'), gf_intervention_orig := gf_intervention_esp]
dt[loc_name %in% c('Uganda'), gf_module_orig := gf_module]
dt[loc_name %in% c('Uganda'), gf_intervention_orig := gf_intervention]
setnames(dt, c('gf_module', 'gf_intervention'), c('gf_module_en', 'gf_intervention_en'))

dt = dt[, .(loc_name, budget_version, grant, grant_period, gf_module_orig, gf_module_en, gf_intervention_orig, gf_intervention_en, 
            activity_description, cost_category, budget)]
setorderv(dt, cols = c('loc_name'))

check = dt[, sum(budget), by = .(loc_name, grant)]

write.csv(dt, outFile, row.names = FALSE)

