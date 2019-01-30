# ------------------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Map prepped GOS and GF files to final mappings, split HIV/TB
#          combined grants, and save to final save location. 
# DATE: Last updated February 2019. 
# ------------------------------------------------------------------

#----------------------------------------------------------------------------
# Read in the version of the data you want to map (logic variables set in master file)
#----------------------------------------------------------------------------

#----------------------------------------------------------------------------
# Merge with module map on module, intervention, and disease to pull in code
#----------------------------------------------------------------------------
mapped_data <- merge(resource_database, module_map, by=c("module", "intervention", "disease"), all.x=TRUE)

#Merge with intervention using code merged from previous line to pull in gf_module and gf_intervention. 
map_dir <- "J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/"
final_mapping <- load_mapping_list(paste0(map_dir, "intervention_and_indicator_list.xlsx")
                                   , include_rssh_by_disease = FALSE) ##set the boolean to false for just mapping
setnames(final_mapping, c("module", "intervention"), c("gf_module", "gf_intervention"))
final_mapping = final_mapping[, .(code, gf_module, gf_intervention, abbrev_module, abbrev_intervention)]

# Merge the dataset with gf_module/intervention to the raw file data by code. 
mapped_data<- merge(mapped_data, final_mapping, by="code", all.x=TRUE) 
dropped_mods <- mapped_data[is.na(mapped_data$gf_module), ]

if(nrow(dropped_mods) >0){
  # Check if anything is dropped in the merge -> if you get an error. Check the mapping spreadsheet
  print(unique(dropped_mods[, c("module", "intervention", "disease"), with= FALSE]))
  stop("Modules/interventions were dropped! - Check Mapping Spreadsheet codes vs intervention tabs")
}

#-------------------------------------------------------
# Split HIV/TB combined grants  
# ------------------------------------------------------

mapped_data = split_hiv_tb(mapped_data)

#-----------------------------------------
# Apply redistribution coefficients
# ----------------------------------------
mapped_data$budget <- mapped_data$budget*mapped_data$coefficient
mapped_data$expenditure <- mapped_data$expenditure*mapped_data$coefficient
mapped_data$disbursement <- mapped_data$disbursement*mapped_data$coefficient

#-----------------------------------------
# Add in location variables
# ----------------------------------------
if(country == "cod"){
  mapped_data$adm1 <- 171
  mapped_data$adm2 <- 171
  mapped_data$country <- "Congo (Democratic Republic)"
} else if (country == "gtm"){
  mapped_data$adm1 <- 128 
  mapped_data$adm2 <- 128 
  mapped_data$country <- "Guatemala" 
} else if (country == "uga"){
  mapped_data$adm1 <- 190
  mapped_data$adm2 <- 190
  mapped_data$country <- "Uganda"
}

mapped_data$loc_name = country

#Emily still not surewhy we're doing this. - This needs to happen in the summary budgets. 
#mapped_data$sda_activity <- ifelse(tolower(mapped_data$sda_activity) == "all" | mapped_data$sda_activity == "0", "Unspecified (Summary budget)", mapped_data$sda_activity)

# --------------------------------------------------------
#Validate the columns in final data and the storage types  
# --------------------------------------------------------

#Note that I'm dropping 'module' and 'intervention' - which were corrected from the original text, but are just used for mapping. EKL 1/29/19
mapped_data = mapped_data[, .(abbrev_intervention, abbrev_module, adm1, adm2, budget, code, cost_category, data_source, disbursement, disease, 
                                              expenditure, file_iteration, fileName, gf_intervention, gf_module, grant_number, grant_period, lang, loc_name,
                                              orig_intervention, orig_module, period, primary_recipient, sda_activity, secondary_recipient, start_date, year)]

desired_cols <- c("abbrev_intervention", "abbrev_module", "adm1", "adm2", "budget", "code", "cost_category", "data_source", "disbursement", "disease", 
                  "expenditure", "file_iteration", "fileName", "gf_intervention", "gf_module", "grant_number", "grant_period", "intervention", "lang", "loc_name", "module", 
                  "orig_intervention", "orig_module", "period", "primary_recipient", "sda_activity", "secondary_recipient", "start_date", "year")
stopifnot(sort(colnames(mapped_data)) == desired_cols)  #Emily we do want to have correct column names here. 

#EMILY WANT TO HAVE GRANT STATUS HERE!! Active or not active. 

#------------------------------------------------------------
# Remove any special characters so .csv will store correctly 
#------------------------------------------------------------
mapped_data$sda_activity <- str_replace_all(mapped_data$sda_activity, "[^[:alnum:]]", " ")
mapped_data$orig_module <- str_replace_all(mapped_data$orig_module, "[^[:alnum:]]", " ")
mapped_data$orig_intervention <- str_replace_all(mapped_data$orig_intervention, "[^[:alnum:]]", " ")

# ----------------------------------------------
# Write the prepped data as .csvs
# ---------------------------------------------

if (prep_files == TRUE){
  final_budgets <- mapped_data[file_iteration == "final" & data_source == "fpm"] #Emily should we remove the expenditure column here? 
  final_expenditures <- mapped_data[file_iteration == "final" & data_source == "pudr"]
  
  # Save RDS file
  saveRDS(final_budgets, paste0(export_dir, "final_budgets.rds"))
  saveRDS(final_expenditures, paste0(export_dir, "final_expenditures.rds"))
  saveRDS(mapped_data, paste0(export_dir, "budget_pudr_iterations.rds"))
}

if (prep_gos == TRUE){
  saveRDS(mapped_data, paste0(export_dir, "prepped_gos_data.csv"))
}