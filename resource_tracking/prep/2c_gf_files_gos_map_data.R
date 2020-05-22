# ------------------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Map prepped GOS and GF files to final mappings, split HIV/TB
#          combined grants, and save to final save location. 
# DATE: Last updated May 2019. 
# ------------------------------------------------------------------

#----------------------------------------------------------------------------
# Read in the version of the data you want to map (logic variables set in master file)
#----------------------------------------------------------------------------
if (prep_gos == TRUE){
  raw_data = copy(totalGos_qtr)
  raw_data[, lfa_exp_adjustment:=0] #There is no LFA expenditure adjustment in GOS data; but add it to make code run. 
} else if (prep_files == TRUE){
  raw_data = copy(resource_database)
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
if (!'activity_description'%in%names(raw_data)){ #If this column doesn't exist, add it as 'NA' so the code below can run
  raw_data[, activity_description:=NA]
}
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
  print(unique(unmapped_mods[, c("module", "intervention", "disease"), with= FALSE]))
  print(unique(unmapped_mods$fileName)) #For documentation in the comments above. 
  stop("You have unmapped original modules/interventions!")
}

#------------------------------------------------------------
# Remap diseases so they apply at the intervention level, 
#   not the grant-level (assigned in the file list)  #THESE SHOULD BE REMOVED AND RESOLVED USING THE MODULE MAP EL 9/23/2019
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
if ('disbursement'%in%names(raw_data)){
  pre_coeff_check = raw_data[, lapply(.SD, sum_na_rm), .SDcols=c('budget', 'expenditure', 'lfa_exp_adjustment', 'disbursement')]
} else {
  pre_coeff_check = raw_data[, lapply(.SD, sum_na_rm), .SDcols=c('budget', 'expenditure', 'lfa_exp_adjustment')]
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
  stop("Modules/interventions were dropped!")
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
mapped_data[, lfa_exp_adjustment:=lfa_exp_adjustment*coefficient]
if ('disbursement'%in%names(mapped_data)){
  mapped_data[, disbursement:=disbursement*coefficient]
}

if ('disbursement'%in%names(mapped_data)){
  post_coeff_check = mapped_data[, lapply(.SD, sum_na_rm), .SDcols=c('budget', 'expenditure', 'lfa_exp_adjustment', 'disbursement')]
  post_coeff_check[[1]] = round(post_coeff_check[[1]])
  post_coeff_check[[2]] = round(post_coeff_check[[2]])
  post_coeff_check[[3]] = round(post_coeff_check[[3]])
  post_coeff_check[[4]] = round(post_coeff_check[[4]])
  
  stopifnot(abs(pre_coeff_check[[1]]-post_coeff_check[[1]]) < 1) #Decision by David Phillips 5/10/19 - it's okay if there is less than one cent difference between the pre- and post-redistribution. 
  stopifnot(abs(pre_coeff_check[[2]]-post_coeff_check[[2]]) < 1)
  stopifnot(abs(pre_coeff_check[[3]]-post_coeff_check[[3]]) < 1)
  stopifnot(abs(pre_coeff_check[[4]]-post_coeff_check[[4]]) < 1)
} else {
  post_coeff_check = mapped_data[, lapply(.SD, sum_na_rm), .SDcols=c('budget', 'expenditure', 'lfa_exp_adjustment')]
  post_coeff_check[[1]] = round(post_coeff_check[[1]])
  post_coeff_check[[2]] = round(post_coeff_check[[2]])
  post_coeff_check[[3]] = round(post_coeff_check[[3]])
  
  stopifnot(abs(pre_coeff_check[[1]]-post_coeff_check[[1]]) < 1) #Decision by David Phillips 5/10/19 - it's okay if there is less than one cent difference between the pre- and post-redistribution. 
  stopifnot(abs(pre_coeff_check[[2]]-post_coeff_check[[2]]) < 1)
  stopifnot(abs(pre_coeff_check[[3]]-post_coeff_check[[3]]) < 1)
}

#Debug the check above, if needed. 
#What line items got changed between the two files? 
# test = unique(raw_data[, .(module, intervention, disease, budget)])
# test = test[budget!=0]
# test[, set:='raw']
# test2 = mapped_data[, .(module, intervention, disease, budget, coefficient)]
# mapped_data = mapped_data[budget!=0]
# test2[, set:='mapped']
# check = merge(test, test2, by=c('module', 'intervention', 'disease'), all=TRUE, allow.cartesian=TRUE)
# #You want to find the cases here where raw budget/coefficient doesn't equal mapped budget
# 
# check = check[is.na(set.x)|is.na(set.y)][order(module, intervention, disease)]
# check = check[budget!=0]
# View(check[1:300])
# 
# check[is.na(set.x) & module=="atencionyprevenciondetuberculosis" & intervention=="poblacionesclaveafectadas"]
# check[is.na(set.y) & module=="atencionyprevenciondetuberculosis" & intervention=="poblacionesclaveafectadas"]
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
for (i in 1:length(current_sen_grants)){
  mapped_data[grant==current_sen_grants[i] & grant_period==current_sen_grant_period[i], 
              current_grant:=TRUE]
}

if (prep_files==TRUE){
  mapped_data$loc_name = country
  for (i in 1:nrow(code_lookup_tables)){
    mapped_data[loc_name==code_lookup_tables$iso_code[i], country:=code_lookup_tables$country[i]]
  }
}

#--------------------------------------------------------------------------------
#Add in a variable for the disease of the grant #Yuck - Emily try to rewrite this code. 
#--------------------------------------------------------------------------------
mapped_data[, disease_split:=strsplit(grant, "-")]
potential_diseases = c('C', 'H', 'T', 'M', 'S', 'R', 'Z')

for (i in 1:nrow(mapped_data)){
  if (mapped_data$disease_split[[i]][2]%in%potential_diseases){
    mapped_data[i, grant_disease:=sapply(disease_split, "[", 2 )]
  } else if (mapped_data$disease_split[[i]][3]%in%potential_diseases){
    mapped_data[i, grant_disease:=sapply(disease_split, "[", 3 )]
  } else if (mapped_data$disease_split[[i]][4]%in%potential_diseases){
    mapped_data[i, grant_disease:=sapply(disease_split, "[", 4 )]
  }
}

mapped_data[, disease_split:=NULL]

unique(mapped_data[!grant_disease%in%potential_diseases, .(grant, grant_disease)]) #Visual check that these all make sense. 

mapped_data[grant_disease=='C', grant_disease:='hiv/tb']
mapped_data[grant_disease=='H', grant_disease:='hiv']
mapped_data[grant_disease=='T', grant_disease:='tb']
mapped_data[grant_disease=='S' | grant_disease=='R', grant_disease:='rssh']
mapped_data[grant_disease=='M', grant_disease:='malaria']
mapped_data[grant_disease=='Z' & grant=='SEN-Z-MOH', grant_disease:='tb'] #oNLY ONE CASE OF THIS. 

stopifnot(unique(mapped_data$grant_disease)%in%c('hiv', 'tb', 'hiv/tb', 'rssh', 'malaria'))

# --------------------------------------------------------
# Convert currencies to USD 
# --------------------------------------------------------
stopifnot(mapped_data$file_currency%in%c("LOC","EUR","USD")) #After visual review, even local currencies (LOC) are actually Euros or USD. EL 11/19/2019. 

needs_conversion = mapped_data[file_currency!='USD']
if (nrow(needs_conversion)!=0){
  in_USD = mapped_data[file_currency=="USD"]
  converted_to_USD = convert_currency(needs_conversion, 'year', convertFrom="EUR", convertTo="USD", 
                                      finVars=c('budget', 'expenditure', 'disbursement', 'lfa_exp_adjustment'))
  mapped_data = rbind(in_USD, converted_to_USD, use.names=TRUE)
}

#-----------------------------------------------
# Add in a variable for PR type 
#-----------------------------------------------
mapped_data[grant%in%governmental_prs, pr_type:="Governmental"] # The variables 'governmental_prs' and 'civil_society_prs' are set in the global variables script. 
mapped_data[grant%in%civil_society_prs, pr_type:="Civil Society"]
mapped_data[is.na(pr_type), pr_type:="Unknown"]
stopifnot(nrow(mapped_data[pr_type=="Unknown"])==0)

# --------------------------------------------------------
#Validate the columns in final data and the storage types  
# --------------------------------------------------------

#Note that I'm dropping 'module' and 'intervention' - which were corrected from the original text, but are just used for mapping. EKL 1/29/19
# Only keep the variable names that are in the codebook for consistency. This should constantly be reviewed. 
dropped_vars = names(mapped_data)[!names(mapped_data)%in%codebook$Variable]
if (length(dropped_vars)!=0){
  print("Some variables are being dropped because they aren't in the codebook - Review to make sure these shouldn't be in the final data.")
  print(dropped_vars)
}
mapped_data = mapped_data[, names(mapped_data)%in%codebook$Variable, with=FALSE]

#After variables are removed, collapse dataset to simplify
byVars <- colnames(mapped_data)
if ('disbursement'%in%names(mapped_data)){
  byVars = byVars[byVars != 'budget' & byVars != 'expenditure' & byVars !='disbursement' & byVars !='lfa_exp_adjustment']
  mapped_data = mapped_data[, lapply(.SD, function(x) sum(x, na.rm=TRUE)), .SDcols=c('budget', 'expenditure', 'lfa_exp_adjustment', 'disbursement'), by=byVars]
} else {
  byVars = byVars[byVars != 'budget' & byVars != 'expenditure' & byVars != 'lfa_exp_adjustment']
  mapped_data = mapped_data[, lapply(.SD, function(x) sum(x, na.rm=TRUE)), .SDcols=c('budget', 'expenditure', 'lfa_exp_adjustment'), by=byVars]
}

#Reorder data 
if ('disbursement'%in%names(mapped_data)){
  mapped_data = mapped_data[order(grant, start_date, year, gf_module, gf_intervention, activity_description, country, loc_name, 
                                budget, expenditure, lfa_exp_adjustment, disbursement, orig_module, orig_intervention, current_grant, file_name)]
} else{
  mapped_data = mapped_data[order(grant, start_date, year, gf_module, gf_intervention, country, loc_name, 
                                  budget, expenditure, lfa_exp_adjustment, orig_module, orig_intervention, current_grant, file_name)]
}
#------------------------------------------------------------
# Remove any special characters so .csv will store correctly 
#------------------------------------------------------------
mapped_data$activity_description <- str_replace_all(mapped_data$activity_description, "[^[:alnum:]]", " ")
mapped_data$orig_module <- str_replace_all(mapped_data$orig_module, "[^[:alnum:]]", " ")
mapped_data$orig_intervention <- str_replace_all(mapped_data$orig_intervention, "[^[:alnum:]]", " ")

# ----------------------------------------------------------------------------
# Create unique datasets - final budgets, final expenditures, and absorption
# ---------------------------------------------------------------------------

if (prep_files){
  source(paste0(code_dir, '2d_gf_split_datasets.r'))
}

# ----------------------------------------------
# Write the prepped data as .csvs
# ---------------------------------------------

# Save these files in a country-specific directory
if (prep_files){
  
  # First, delete the currently saved files (these are all archived so we don't need these copies)
  saved_files = list.files(export_dir, full.names=TRUE, pattern=".csv") # Only find the .csv files - these are the prepped data files. As of 3/18/20, there is one RDS 'raw bound files' from step 2B. EL
  stopifnot(length(saved_files)==6) # There should be exactly 6 files you're going to delete. FRC 5/20/2020 added one in
  sapply(saved_files, unlink) # Delete the five files in this folder.
  
  # (GEP and CEP files are exactly the same, except GEP files have additional variables saved in the data for more advanced visualization)
  # Save user-facing files (for CEPs and PATH) on Box as .csvs
  write.csv(approved_budgets_cep, paste0(export_dir, "approved_budgets_", country, "_", Sys.Date(), ".csv"), row.names=F)
  write.csv(most_recent_revisions_cep, paste0(export_dir, "most_recent_budgets_", country, "_", Sys.Date(), ".csv"), row.names=F)
  write.csv(revisions_cep, paste0(export_dir, "all_budget_revisions_", country, "_", Sys.Date(), ".csv"), row.names=F)
  write.csv(absorption_cep, paste0(export_dir, "most_recent_absorption_", country, "_", Sys.Date(), ".csv"), row.names=F)
  write.csv(cumulative_absorption_cep, paste0(export_dir, "cumulative_absorption_", country, "_", Sys.Date(), ".csv"), row.names=F)
  write.csv(all_absorption, paste0(export_dir, "all_absorption_", country, "_", Sys.Date(), ".csv"), row.names=F)
  
  
  # Save admin-level files on J. 
  saveRDS(approved_budgets_gep, paste0(dir, "_gf_files_gos/tableau_data/", country, "/approved_budgets_", country, "_", Sys.Date(), ".rds"))
  saveRDS(most_recent_revisions_gep, paste0(dir, "_gf_files_gos/tableau_data/", country, "/most_recent_budgets_", country, "_", Sys.Date(), ".rds"))
  saveRDS(revisions_gep, paste0(dir, "_gf_files_gos/tableau_data/", country, "/all_budget_revisions_", country, "_", Sys.Date(), ".rds"))
  saveRDS(absorption_gep, paste0(dir, "_gf_files_gos/tableau_data/", country, "/most_recent_absorption_", country, "_", Sys.Date(), ".rds"))
  saveRDS(cumulative_absorption_gep, paste0(dir, "_gf_files_gos/tableau_data/", country, "/cumulative_absorption_", country, "_", Sys.Date(), ".rds"))
  saveRDS(all_absorption_gep, paste0(dir, "_gf_files_gos/tableau_data/", country, "/all_absorption_", country, "_", Sys.Date(), ".rds"))
  
  # Save copy for archive.
  saveRDS(approved_budgets_gep, paste0(dir, "_gf_files_gos/tableau_data/archive/approved_budgets_", country, "_", Sys.Date(), ".rds"))
  saveRDS(most_recent_revisions_gep, paste0(dir, "_gf_files_gos/tableau_data/archive/most_recent_budgets_", country, "_", Sys.Date(), ".rds"))
  saveRDS(revisions_gep, paste0(dir, "_gf_files_gos/tableau_data/archive/all_budget_revisions_", country, "_", Sys.Date(), ".rds"))
  saveRDS(absorption_gep, paste0(dir, "_gf_files_gos/tableau_data/archive/most_recent_absorption_", country, "_", Sys.Date(), ".rds"))
  saveRDS(cumulative_absorption_gep, paste0(dir, "_gf_files_gos/tableau_data/archive/cumulative_absorption_", country, "_", Sys.Date(), ".rds"))
  saveRDS(all_absorption_gep, paste0(dir, "_gf_files_gos/tableau_data/archive/all_absorption_", country, "_", Sys.Date(), ".rds"))
  
}

if (prep_gos == TRUE){
  saveRDS(mapped_data, paste0(gos_prepped, "prepped_gos_data.rds"))
  write.csv(mapped_data, paste0(gos_prepped, "prepped_gos_data.csv"), row.names = FALSE)

  #Save archive version.
  saveRDS(mapped_data, paste0(gos_prepped, "archive/prepped_gos_data", Sys.Date(), ".rds"))
}

print("Step C: Map data completed. Outputs saved in prepped_data folder.")