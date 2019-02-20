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
  raw_data <- copy(totalGos)
} else if (prep_files == TRUE){
  raw_data <- copy(resource_database)
}

#-------------------------------------------------------
# Prep data for merge 
#-------------------------------------------------------
#Remove whitespaces, punctuation, and unwanted characters from module and intervention. 
raw_data <- strip_chars(raw_data)

#Correct common acronyms in the resource database and the module map. 
raw_data[, module:=replace_acronyms(module)]
raw_data[, intervention:=replace_acronyms(intervention)]

module_map[, module:=replace_acronyms(module)]
module_map[, intervention:=replace_acronyms(intervention)]

#Temporary fix for UGA - should be removed!
module_map[, module:=gsub("hss", "", module)]
module_map[, intervention:=gsub("hss", "", intervention)]

raw_data[, module:=gsub("hss", "", module)]
raw_data[, intervention:=gsub("hss", "", intervention)]

#--------------------------------------------------------
# Adjust module and intervention manually in the raw data 
#-------------------------------------------------------
if (prep_files == TRUE){
  source(paste0(gf_prep_code, "budget_pudr_prep/", country, "_prep/correct_modules_interventions.R"))
} else if (prep_gos == TRUE){
  source(paste0(gf_prep_code, "gos_prep/correct_modules_interventions.R"))
}

raw_data = correct_modules_interventions(raw_data)
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

source(paste0(gf_prep_code, "5_remap_diseases.R"))
raw_data = remap_diseases(raw_data)

#----------------------------------------------------------------------------
# Merge with module map on module, intervention, and disease to pull in code
#----------------------------------------------------------------------------
mapped_data <- merge(raw_data, module_map, by=c("module", "intervention", "disease"), all.x=TRUE)

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
# #Remap all RSSH codes to the RSSH disease, and make sure 
#there aren't any HSS diseases still hanging around. 
# ------------------------------------------------------
mapped_data[substring(code, 1, 1)=='R', disease:='rssh']
mapped_data[disease == 'hss', disease:='rssh']

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
} else if (country == "gtm"){
  mapped_data$adm1 <- 128 
} else if (country == "uga"){
  mapped_data$adm1 <- 190
}

mapped_data$loc_name <- country

#-----------------------------------------
# Add in variable for current grant 
# ----------------------------------------
mapped_data$current_grant = FALSE 
if(country == "cod"){
  for (i in 1:length(current_cod_grants)){
    mapped_data[grant_number==current_cod_grants[i] & grant_period==current_cod_grant_period[i], 
              current_grant:=TRUE]
  }
} else if (country == "gtm"){
  for (i in 1:length(current_gtm_grants)){
    mapped_data[grant_number==current_gtm_grants[i] & grant_period==current_gtm_grant_period[i], 
              current_grant:=TRUE]
  }
} else if (country == "uga"){
  for (i in 1:length(current_uga_grants)){
    mapped_data[grant_number==current_uga_grants[i] & grant_period==current_uga_grant_period[i], 
              current_grant:=TRUE]
  }
}

#--------------------------------------------------------
# Split data into quarters - Emily just verify that this split is definitely happening in the prep functions. 
# -------------------------------------------------------
# stopifnot(nrow(test_split[is.na(period) | period == 0])) #Make sure you have a 'period' variable to splice up file!
# 
# #Delete this bit! 
# test_split <- copy(final_budgets)
# test_split = test_split[period != 90][order(sda_activity)]
# test_split = test_split[1:10] 
# 
# #Find how many quarters each line needs to be split into.
# #If not an even number, round up, and put the last bit in one extra quarter beyond.
# test_split[, qsplit:=period/90] #90 days in each period
# test_split[, num_quarters:=ceiling(qsplit)]
# test_split[, qremainder:=qsplit%%1]
# 
# #Expand data by num_quarters
# test_split <- expandRows(test_split, "num_quarters")
# 
# #Reformat date variable, and generate 'quarter' variable
# byVars = colnames(test_split)
# test_split[, seq:=seq(from=0, to=100), by=byVars] #100 is an arbitrary number here, we just need something that's greater than the max # of quarters in any file
# test_split[, quarter:=quarter(start_date)]
# test_split[, year:=year(start_date)] 
# 
# #While seq is not 0, go through the loop below. 
# #If seq is greater than or equal to 4, add 1 to year and divide everything by 4. Continue this loop while max(seq) > 4. 
# #Can you use lapply to apply this to every row? 
# while (test_split$seq >0){
#   if (test_split$quarter == 4){
#     test_split$quarter == 1
#     test_split$year = test_split$year + 1
#   } else {
#     test_split$quarter = test_split$quarter +1
#   }
#   seq = seq - 1
# }
# 
# 
# #Increment year and quarter using the 'seq' variable to flag subsequent quarters
# #Quarter = 1 := q=2, year same 
# # q = 2 := q:=3, year same 
# # q = 3 := q= 4, year same 
# # q = 4 := q = 1, increment year by 1
# 
# test_split = test_split[, .(module, intervention, sda_activity, period, start_date, budget, expenditure, 
#                             disbursement, qsplit, qremainder, seq, quarter, year)] #DELETE ME!! 
# 
# #Split financial variables - start by sectioning off any remainder. #EMILY START HERE
# for (i in 1:20){
#   while (qsplit>1){
# 
#   }
# }
# 
# 
# test_split[-c('qsplit', 'num_quarters', 'qremainder', 'seq')]
# #If there is a 'remainder' quarter, split that bit off and save

# --------------------------------------------------------
#Validate the columns in final data and the storage types  
# --------------------------------------------------------

#Note that I'm dropping 'module' and 'intervention' - which were corrected from the original text, but are just used for mapping. EKL 1/29/19
mapped_data = mapped_data[, .(abbrev_intervention, abbrev_module, adm1, budget, code, current_grant, data_source, disbursement, disease, 
                                              expenditure, file_iteration, fileName, gf_intervention, gf_module, grant_number, grant_period, lang, loc_name,
                                              orig_intervention, orig_module, period, primary_recipient, sda_activity, secondary_recipient, start_date, year)]

desired_cols <- c("abbrev_intervention", "abbrev_module", "adm1", "budget", "code", "current_grant", "data_source", "disbursement", "disease", 
                  "expenditure", "file_iteration", "fileName", "gf_intervention", "gf_module", "grant_number", "grant_period", "lang", "loc_name", 
                  "orig_intervention", "orig_module", "period", "primary_recipient", "sda_activity", "secondary_recipient", "start_date", "year")
stopifnot(sort(colnames(mapped_data)) == desired_cols)  #Emily we do want to have correct column names here. 

#Rename columns 
colnames(mapped_data)[colnames(mapped_data)=='gf_module'] <- 'module'
colnames(mapped_data)[colnames(mapped_data)=='gf_intervention'] <- 'intervention'
colnames(mapped_data)[colnames(mapped_data)=='sda_activity'] <- 'activity_description'


#After variables are removed, collapse dataset to simplify
byVars <- colnames(mapped_data)
byVars = byVars[byVars != 'budget' & byVars != 'expenditure' & byVars !='disbursement']
mapped_data = mapped_data[, lapply(.SD, function(x) sum(x, na.rm=TRUE)), .SDcols=c('budget', 'expenditure', 'disbursement'), by=byVars]

#Reorder data 
mapped_data = mapped_data[order(grant_number, start_date, year, module, intervention, activity_description, loc_name, adm1, 
                                budget, expenditure, disbursement, abbrev_module, abbrev_intervention, orig_module, orig_intervention, current_grant, fileName)]
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
  saveRDS(mapped_data, paste0(export_dir, "prepped_gos_data.rds"))
}