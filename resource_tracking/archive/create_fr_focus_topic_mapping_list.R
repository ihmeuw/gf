# create a codebook using unique values of the funding request interventions that are not already in the old identifyTopicAreas_PCE2020_forsubsetting
library(data.table)
old <- fread("J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/archive/identifyTopicAreas_PCE2020_forSubsetting.csv")
new <- fread("C:/Users/frc2/Box Sync/Global Fund Files/tableau_data/fr_budgets_all.csv")

# keep certain columns from new

new <- new[,.(loc_name, disease, gf_module, gf_intervention)]
new <- unique(new)


# add suffix to column names
setnames(new, 
         old=c("loc_name", "disease", "gf_module", "gf_intervention"), 
         new=c("loc_name_new", "disease_new", "gf_module_new", "gf_intervention_new"))

# keep only the modules or interventions that are not already included in the previous sheet ("Old")
new <- new[!new$gf_module_new %in% old$gf_module|!new$gf_intervention_new %in% old$gf_intervention,]

# output file
write.csv(new, "J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/identifyTopicAreas_PCE2020_forSubsettingFRs_blank.csv")
