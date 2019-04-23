# ------------------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Map prepped GOS and GF files to final mappings, split HIV/TB
#          combined grants, and save to final save location. 
# DATE: Last updated February 2019. 
# ------------------------------------------------------------------

#----------------------------------------------------------------------------
# Read in the version of the data you want to map (logic variables set in master file)
#----------------------------------------------------------------------------
if (prep_gos == TRUE){
  raw_data = totalGos_qtr
} else if (prep_files == TRUE){
  raw_data = resource_database
}

#-------------------------------------------------------
# Prep mapping data for merge 
#-------------------------------------------------------
#Remove whitespaces, punctuation, and unwanted characters from module and intervention. 
raw_data = strip_chars(raw_data)

#Correct common acronyms in the resource database and the module map. 
raw_data[, module:=replace_acronyms(module)]
raw_data[, intervention:=replace_acronyms(intervention)]

module_map[, module:=replace_acronyms(module)]
module_map[, intervention:=replace_acronyms(intervention)]

#Make some raw corrections here - These weren't accurate enough to put in the map, but we still need to account for them. 
if (prep_files == TRUE){
  raw_data = correct_modules_interventions(raw_data)
}

#------------------------------------------------------------
# Map budgets and PUDRs to module mapping framework 
#------------------------------------------------------------

# Check for unmapped modules/interventions before mapping
gf_concat <- paste0(module_map$module, module_map$intervention)
rt_concat <- paste0(raw_data$module, raw_data$intervention)
unmapped_mods <- raw_data[!rt_concat%in%gf_concat]

if(nrow(unmapped_mods)>0){
  print(unique(unmapped_mods[, c("module", "intervention"), with= FALSE]))
  print(unique(unmapped_mods$fileName)) #For documentation in the comments above. 
  stop("You have unmapped original modules/interventions!")
}

#------------------------------------------------------------
# Remap diseases so they apply at the intervention level, 
#   not the grant-level (assigned in the file list) 
#------------------------------------------------------------

#Correct all tb/hiv to hiv/tb
raw_data[disease == 'tb/hiv', disease:='hiv/tb']

#English corrections
raw_data[module=='hivhealthsystemsstrengthening', disease:='hiv']
raw_data[module=='malhealthsystemsstrengthening', disease:='malaria']
raw_data[module=='tbhealthsystemsstrengthening', disease:='tb']

#French corrections 
raw_data[module == 'priseenchargeetpreventiondelatuberculose' & disease == 'hiv', disease:='tb']

#----------------------------------------------------------------------------
# Merge with module map on module, intervention, and disease to pull in code
#----------------------------------------------------------------------------
if (prep_files == TRUE){
  pre_coeff_check = raw_data[, lapply(.SD, sum_na_rm), .SDcols=c('budget', 'expenditure', 'disbursement')]
} else {
  pre_coeff_check = raw_data[, lapply(.SD, sum_na_rm), .SDcols=c('budget', 'expenditure')]
  pre_coeff_check[[1]] = round(pre_coeff_check[[1]])
  pre_coeff_check[[2]] = round(pre_coeff_check[[2]])
}
mergeVars = c('disease', 'module', 'intervention')
#module_map = unique(module_map)
module_map = module_map[!is.na(code)]

mapped_data <- merge(raw_data, module_map, by=mergeVars, all.x = TRUE, allow.cartesian = TRUE)
dropped_mods <- mapped_data[is.na(mapped_data$gf_module), ]

if(nrow(dropped_mods) >0){
  # Check if anything is dropped in the merge -> if you get an error. Check the mapping spreadsheet
  print(unique(dropped_mods[, c("module", "intervention", "disease"), with= FALSE]))
  stop("Modules/interventions were dropped! - Check Mapping Spreadsheet codes vs intervention tabs")
}

#-------------------------------------------------------
# #Remap all RSSH codes to the RSSH disease, and make sure 
#there aren't any HSS diseases still hanging around. Remap all codes to their correct disease.  
# ------------------------------------------------------
mapped_data[substring(code, 1, 1)=='R', disease:='rssh']
mapped_data[disease == 'hss', disease:='rssh']

mapped_data[substring(code, 1, 1)=='H', disease:='hiv']
mapped_data[substring(code, 1, 1)=='T', disease:='tb']
mapped_data[substring(code, 1, 1)=='M', disease:='malaria']

#-------------------------------------------------------
# Split HIV/TB combined grants  
# ------------------------------------------------------
mapped_data = split_hiv_tb(mapped_data)

#-------------------------------------------------------
# Redistribute using mapped coefficient 
# ------------------------------------------------------
remapped_rows = nrow(mapped_data[coefficient != 1])
print(paste0("A total of ", remapped_rows, " rows will be redistributed."))
mapped_data[, budget:=budget*coefficient]
mapped_data[, expenditure:=expenditure*coefficient]
if (prep_files == TRUE){
  mapped_data[, disbursement:=disbursement*coefficient]
}

if (prep_files == TRUE){
  post_coeff_check = mapped_data[, lapply(.SD, sum_na_rm), .SDcols=c('budget', 'expenditure', 'disbursement')]
  stopifnot(pre_coeff_check[[1]] == post_coeff_check[[1]] & pre_coeff_check[[2]] == post_coeff_check[[2]] & pre_coeff_check[[3]] == post_coeff_check[[3]])
} else {
  post_coeff_check = mapped_data[, lapply(.SD, sum_na_rm), .SDcols=c('budget', 'expenditure')]
  post_coeff_check[[1]] = round(post_coeff_check[[1]])
  post_coeff_check[[2]] = round(post_coeff_check[[2]])
  stopifnot(pre_coeff_check[[1]] == post_coeff_check[[1]] & pre_coeff_check[[2]] == post_coeff_check[[2]])
}

#-----------------------------------------------------------
# Add in a variable for 'includes RSSH'
#-----------------------------------------------------------
#By file and grant (to catch both budgets and GOS), should be "TRUE"
#if there is at least one 'R' code. 
mapped_data[, code_start:=substring(code, 1, 1)]
codes = unique(mapped_data[, .(code_start, grant, file_name)])
codes = dcast(codes, grant+file_name~code_start, value.var='file_name')
codes[is.na(R), includes_rssh:=FALSE]
codes[!is.na(R), includes_rssh:=TRUE]
codes = codes[, .(grant, file_name, includes_rssh)]

mapped_data = merge(mapped_data, codes, all.x=T, by=c('grant', 'file_name'))

#-----------------------------------------------------------
# Add in variable for current grant, and location variable
# ----------------------------------------------------------
mapped_data$current_grant = FALSE 
for (i in 1:length(current_cod_grants)){
  mapped_data[grant==current_cod_grants[i] & grant_period==current_cod_grant_period[i], 
              current_grant:=TRUE]
}
for (i in 1:length(current_gtm_grants)){
  mapped_data[grant==current_gtm_grants[i] & grant_period==current_gtm_grant_period[i], 
              current_grant:=TRUE]
}
for (i in 1:length(current_uga_grants)){
  mapped_data[grant==current_uga_grants[i] & grant_period==current_uga_grant_period[i], 
              current_grant:=TRUE]
}

if (prep_files==TRUE){
  mapped_data$loc_name = country
}
for (i in 1:nrow(code_lookup_tables)){
  mapped_data[loc_name==code_lookup_tables$iso_code[i], country:=code_lookup_tables$country[i]]
}

# --------------------------------------------------------
#Verify that everything is at the quarter-level here 
# --------------------------------------------------------
unique(mapped_data[, start_date-end_date])

# --------------------------------------------------------
#Validate the columns in final data and the storage types  
# --------------------------------------------------------

#Note that I'm dropping 'module' and 'intervention' - which were corrected from the original text, but are just used for mapping. EKL 1/29/19
# Only keep the variable names that are in the codebook for consistency. This should constantly be reviewed. 
mapped_data = mapped_data[, names(mapped_data)%in%codebook$Variable, with=FALSE]

#After variables are removed, collapse dataset to simplify
byVars <- colnames(mapped_data)
if (prep_files == TRUE){
  byVars = byVars[byVars != 'budget' & byVars != 'expenditure' & byVars !='disbursement']
  mapped_data = mapped_data[, lapply(.SD, function(x) sum(x, na.rm=TRUE)), .SDcols=c('budget', 'expenditure', 'disbursement'), by=byVars]
} else if (prep_gos == TRUE){
  byVars = byVars[byVars != 'budget' & byVars != 'expenditure']
  mapped_data = mapped_data[, lapply(.SD, function(x) sum(x, na.rm=TRUE)), .SDcols=c('budget', 'expenditure'), by=byVars]
}

#Reorder data 
if (prep_files == TRUE){
  mapped_data = mapped_data[order(grant, start_date, year, gf_module, gf_intervention, activity_description, country, loc_name, 
                                budget, expenditure, disbursement, orig_module, orig_intervention, current_grant, file_name)]
} else if (prep_gos == TRUE){
  mapped_data = mapped_data[order(grant, start_date, year, gf_module, gf_intervention, country, loc_name, 
                                  budget, expenditure, orig_module, orig_intervention, current_grant, file_name)]
}
#------------------------------------------------------------
# Remove any special characters so .csv will store correctly 
#------------------------------------------------------------
mapped_data$activity_description <- str_replace_all(mapped_data$activity_description, "[^[:alnum:]]", " ")
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
  
  write.csv(final_budgets, paste0(export_dir, "final_budgets.csv"))
  write.csv(final_expenditures, paste0(export_dir, "final_expenditures.csv"))
  write.csv(mapped_data, paste0(export_dir, "budget_pudr_iterations.csv"))
}

if (prep_gos == TRUE){
  saveRDS(mapped_data, paste0(gos_prepped, "prepped_gos_data.rds"))
  write.csv(mapped_data, paste0(gos_prepped, "prepped_gos_data.csv"), row.names = FALSE)
}