# ----------------------------------------------
# AUTHOR: Audrey Batzel and Francisco Rios Casas based on code by Emily Linebarger and Irena Chen
# PURPOSE: Prep commonly-formatted FR budgets using the prep_fr_budgets() function - runs all countries at once
# DATE: Last updated June 2020.

# TO DO: 
# ----------------------------------------------
rm(list=ls())

# ----------------------------------------------
# Initial set up
# ----------------------------------------------
user=as.character(Sys.info()[7])
if (Sys.info()[1]=='Windows'){
  setwd(paste0("C:/Users/",user,"/Documents/gf/")) #Change to the root of your repository
} else {
  setwd(paste0("/ihme/homes/",user,"/gf/"))
}
source("./resource_tracking/prep/_common/set_up_r.R", encoding="UTF-8")
source("./resource_tracking/prep/_common/load_master_list.r", encoding="UTF-8")
source("./resource_tracking/prep/gf_files_prep_functions/prep_fr_budgets.R", encoding="UTF-8")

#Source document prep functions 
doc_prep_functions = list.files(paste0(code_dir, "gf_files_prep_functions"), full.names=TRUE)
for (file in doc_prep_functions){
  source(file, encoding="UTF-8")
}
# ----------------------------------------------

# ----------------------------------------------
# switches from main file that are necessary here:
verbose = FALSE 
# load files to loop through for FR prep:
file_list = load_master_list(purpose = "financial") #This function is sourced from the _common folder in master script. 
file_list = file_list[data_source == 'funding_request' & file_iteration == 'approved_gm',] # here, 'approved_gm' is a misnomer, but we used it to fit in with the naming schema that had already been used for the file list
# ----------------------------------------------

# ----------------------------------------------
# 1. Loop through FR budget files to extract data
# ----------------------------------------------
for(i in 1:nrow(file_list)){
  # Set up file path 
  folder = "funding_requests"
  
  if (file_list[i, year(version_date) <= "2019"]){
    folder_cont = "fr_budgets_2017"
  } else if (file_list[i, (year(version_date) >= "2019" & year(version_date) <= '2021')]){
    folder_cont= "fr_budgets_2020"
  } else {
    stop('Something went wrong - version date is missing or incorrect')
  }
  
  grant_period = file_list$grant_period[i]
  
  file_dir = paste0(box, toupper(file_list$loc_name[i]), '/raw_data/', file_list$grant_status[i], "/", 
                    folder, '/', folder_cont, '/')
  
  args = list(file_dir, file_list$file_name[i], file_list$sheet_financial[i], file_list$start_date_financial[i], 
              file_list$period_financial[i], file_list$qtr_number_financial[i], file_list$language_financial[i])
  
  ### RUN THE PREP FUNCTION HERE ###
  tmpData = do.call(prep_fr_budgets, args)
  ###
  
  #Add indexing data
  append_cols = file_list[i, .(loc_name, disease, file_name, data_source, grant_period, grant_status, 
                               file_currency, file_iteration, budget_version, version_date, 
                               function_financial, start_date_financial, language_financial, 
                               period_financial, qtr_number_financial,
                               mod_framework_format, update_date)]
  
  stopifnot(nrow(append_cols)==1)
  
  tmpData = cbind(tmpData, append_cols)
  
  tmpData[, year := year(start_date)]
  tmpData[, file_start_date:=min(start_date), by='file_name']
  
  #Bind data together 
  if(i==1){
    prepped_frs = tmpData
  } else {
    prepped_frs = rbind(prepped_frs, tmpData, use.names=TRUE, fill = TRUE)
  }
  
  print(paste0(i, " ", file_list$data_source[i], " ", file_list$function_financial[i], " ", file_list$file_name[i])) ## if the code breaks, you know which file it broke on
}
# ----------------------------------------------

# ----------------------------------------------
# 2. Run some checks to make sure this data was prepped correctly. 
# ----------------------------------------------
# check the totals
# prepped_frs[, sum(budget, na.rm=TRUE), by = c('file_name', 'loc_name')]

#Make sure all budget data pulled is actually numeric- this is an easy check to see if prep functions are working correctly. 
verify_numeric_budget = prepped_frs[, .(budget=gsub("[[:digit:]]", "", budget))]
verify_numeric_budget = verify_numeric_budget[, .(budget=gsub("[[:punct:]]", "", budget))]
verify_numeric_budget = verify_numeric_budget[!is.na(budget) & budget != ""]
stopifnot(nrow(verify_numeric_budget)==0)

#Make sure you have all the files here that you started with in your filelist. 
rt_files <- unique(prepped_frs$file_name)
warning1 = (length(unique(file_list$file_name)) == length(rt_files))
if (!warning1){
  warning("The length of the original file list is not the same as the number of processed files.")
}
warning2 = sort(rt_files) == sort(unique(file_list$file_name))
if (all(warning2)!=TRUE){
  warning("The files in the processed data are not the same as the files in the file list.")
}
# ----------------------------------------------

# ----------------------------------------------
# 3. Map prepped files to final mappings
# ----------------------------------------------
# this bit of code will help map data, but I just realized that this includes code mapped to both the old (2018-2020) and 
# new modular framework

# split resource list into two and then combine together again at the end?
prepped_frs_nfm2 <- prepped_frs[grant_period%in%c("2018-2020", "2019-2021")]
prepped_frs_nfm3 <- prepped_frs[grant_period%in%c("2021-2023")]

# source mapping file
include_stops = TRUE
source(paste0(code_dir, "2a_gf_files_verify_mapping.R"))

# ----------------------------------------------
# Prep raw data for mapping
# ----------------------------------------------
raw_data = copy(prepped_frs_nfm2)
# Remove whitespaces, punctuation, and unwanted characters from module and intervention. 
raw_data = strip_chars(raw_data)
# Correct common acronyms in the resource database and the module map. 
raw_data[, module:=replace_acronyms(module)]
raw_data[, intervention:=replace_acronyms(intervention)]

module_map[, module:=replace_acronyms(module)]
module_map[, intervention:=replace_acronyms(intervention)]

# Make some raw corrections here - These weren't accurate enough to put in the map, but we still need to account for them. 
if (!'activity_description'%in%names(raw_data)){ #If this column doesn't exist, add it as 'NA' so the code below can run
  raw_data[, activity_description:=NA]
}

raw_data = correct_modules_interventions(raw_data)

# ----------------------------------------------
# Map budgets and PUDRs to module mapping framework 
# ----------------------------------------------
# Check for unmapped modules/interventions before mapping
gf_concat <- paste0(module_map$module, module_map$intervention)
rt_concat <- paste0(raw_data$module, raw_data$intervention)
unmapped_mods <- raw_data[!rt_concat%in%gf_concat]

if(nrow(unmapped_mods)>0){
  print(unique(unmapped_mods[, c("module", "intervention", "disease"), with= FALSE]))
  print(unique(unmapped_mods$file_name)) #For documentation in the comments above. 
  stop("You have unmapped original modules/interventions!")
}

# ----------------------------------------------
##### HERE MAP BUDGETS from new MODULAR FRAMEwork and then rbind the data together again to finish mapping everything

# ----------------------------------------------

# ----------------------------------------------
# Remap diseases so they apply at the intervention level, 
#   not the grant-level (assigned in the file list)  #THESE SHOULD BE REMOVED AND RESOLVED USING THE MODULE MAP EL 9/23/2019
# ----------------------------------------------
# Correct all tb/hiv to hiv/tb
raw_data[disease == 'tb/hiv', disease:='hiv/tb']

# English corrections
raw_data[module=='hivhealthsystemsstrengthening', disease:='hiv']
raw_data[module=='malhealthsystemsstrengthening', disease:='malaria']
raw_data[module=='tbhealthsystemsstrengthening', disease:='tb']

#French corrections 
raw_data[module == 'priseenchargeetpreventiondelatuberculose' & disease == 'hiv', disease:='tb']

# ----------------------------------------------
# Merge with module map on module, intervention, and disease to pull in code
# ----------------------------------------------
# if ('disbursement'%in%names(raw_data)){
#   pre_coeff_check = raw_data[, lapply(.SD, sum_na_rm), .SDcols=c('budget', 'expenditure', 'lfa_exp_adjustment', 'disbursement')]
# } else {
#   pre_coeff_check = raw_data[, lapply(.SD, sum_na_rm), .SDcols=c('budget', 'expenditure', 'lfa_exp_adjustment')]
#   pre_coeff_check[[1]] = round(pre_coeff_check[[1]])
#   pre_coeff_check[[2]] = round(pre_coeff_check[[2]])
# }

mergeVars = c('disease', 'module', 'intervention')
#module_map = unique(module_map)
module_map = module_map[!is.na(code)]

mapped_data <- merge(raw_data, module_map, by=mergeVars, all.x = TRUE)
dropped_mods <- mapped_data[is.na(mapped_data$gf_module), ]

if(nrow(dropped_mods) >0){
  # Check if anything is dropped in the merge -> if you get an error. Check the mapping spreadsheet
  print(unique(dropped_mods[, c("module", "intervention", "disease"), with= FALSE]))
  stop("Modules/interventions were dropped!")
}

# ----------------------------------------------
# Remap all RSSH codes to the RSSH disease, and make sure 
#   there aren't any HSS diseases still hanging around. Remap all codes to their correct disease.  
# ----------------------------------------------
mapped_data[substring(code, 1, 1)=='R', disease:='rssh']
mapped_data[disease == 'hss', disease:='rssh']

mapped_data[substring(code, 1, 1)=='H', disease:='hiv']
mapped_data[substring(code, 1, 1)=='T', disease:='tb']
mapped_data[substring(code, 1, 1)=='M', disease:='malaria']

# ----------------------------------------------
# Split HIV/TB combined grants  
# ----------------------------------------------
mapped_data = split_hiv_tb(mapped_data)

# ----------------------------------------------
# Add in a variable for 'includes RSSH'
# ----------------------------------------------
# By file and grant (to catch both budgets and GOS), should be "TRUE"
# if there is at least one 'R' code. 
mapped_data[, code_start:=substring(code, 1, 1)]
codes = unique(mapped_data[, .(code_start, loc_name, file_name)])
codes = dcast(codes, loc_name+file_name~code_start, value.var='file_name')
codes[is.na(R), includes_rssh:=FALSE]
codes[!is.na(R), includes_rssh:=TRUE]
codes = codes[, .(loc_name, file_name, includes_rssh)]

mapped_data = merge(mapped_data, codes, all.x=T, by=c('loc_name', 'file_name'))

# ----------------------------------------------
# Convert currencies to USD 
# ----------------------------------------------
stopifnot(mapped_data$file_currency%in%c("LOC","EUR","USD")) #After visual review, even local currencies (LOC) are actually Euros or USD. EL 11/19/2019. 

needs_conversion = mapped_data[file_currency!='USD']
if (nrow(needs_conversion)!=0){
  in_USD = mapped_data[file_currency=="USD"]
  converted_to_USD = convert_currency(needs_conversion, 'year', convertFrom="EUR", convertTo="USD", 
                                      finVars=c('budget'))
  mapped_data = rbind(in_USD, converted_to_USD, use.names=TRUE)
}

# ----------------------------------------------
# Validate the columns in final data and the storage types  
# ----------------------------------------------
# Note that I'm dropping 'module' and 'intervention' - which were corrected from the original text, but are just used 
# for mapping. EKL 1/29/19
# Only keep the variable names that are in the codebook for consistency. This should constantly be reviewed. 
dropped_vars = names(mapped_data)[!names(mapped_data)%in%codebook$Variable]
if (length(dropped_vars)!=0){
  print("Some variables are being dropped because they aren't in the codebook - Review to make sure these shouldn't be in the final data.")
  print(dropped_vars)
}
mapped_data = mapped_data[, names(mapped_data)%in%codebook$Variable, with=FALSE]

# After variables are removed, collapse dataset to simplify
byVars = colnames(mapped_data)
byVars = byVars[byVars != 'budget']
mapped_data = mapped_data[, lapply(.SD, function(x) sum(x, na.rm=TRUE)), .SDcols=c('budget'), by=byVars]

# Reorder data 
mapped_data = mapped_data[order(start_date, year, gf_module, gf_intervention, loc_name, 
                                budget, orig_module, orig_intervention, file_name)]

# setcolorder(mapped_data, 
#             c("loc_name", "disease", "file_name", 
#               "gf_module", "gf_intervention", "activity_description", "cost_category", "implementer", "budget",
#               "start_date", "year", "quarter", "data_source","grant_period","grant_status",
#               "file_iteration","budget_version","version_date", "update_date",
#               "language_financial","period_financial","orig_module","orig_intervention","code",
#               "rssh","kp","equity",
#               "gf_module_fr","gf_intervention_fr",  
#               "gf_module_esp","gf_intervention_esp",
#               "abbrev_mod","abbrev_int","includes_rssh"))

# ----------------------------------------------
# Remove any special characters so .csv will store correctly 
# ----------------------------------------------
mapped_data$activity_description <- str_replace_all(mapped_data$activity_description, "[^[:alnum:]]", " ")
mapped_data$orig_module <- str_replace_all(mapped_data$orig_module, "[^[:alnum:]]", " ")
mapped_data$orig_intervention <- str_replace_all(mapped_data$orig_intervention, "[^[:alnum:]]", " ")
# ----------------------------------------------

# ----------------------------------------------
# Save data:
# ----------------------------------------------
# Do we want to split the dataset into different components? maybe 2017 or 2020 files? or maybe not necessary? 

# add a column for PR:
mapped_data[ implementer == 'Ministry of Finance, Planning and Economic Development of the Government of the Republic of Uganda', pr := 'MoFPED']
mapped_data[ implementer == 'The AIDS Support Organisation (Uganda) Limited', pr := 'TASO']
mapped_data[ implementer == 'Ministry of Health and Population of the Government of the Democratic Republic of Congo', pr := 'MOH']
mapped_data[ implementer == 'PR Societe Civile', pr := 'SANRU/CORDAID']
mapped_data[ implementer == 'INCAP', pr := 'INCAP']
mapped_data[ implementer == 'SR', pr := 'SR']
mapped_data[ implementer == 'Organizacion Panamericana de la Salud (OPS/OMS)', pr := 'Organizacion Panamericana de la Salud (OPS/OMS)']
mapped_data[ implementer == 'Ministry of Health and Social Assistance of the Republic of Guatemala', pr := 'MSPAS']
mapped_data[ implementer == 'Ministry of Health and Social Action of the Government of the Republic of Senegal', pr := 'MOH']
mapped_data[ implementer == 'Plan International, Inc.', pr := 'Plan International, Inc.']

# specify the columns to keep in the output:
keep_cols = c('file_name', 'loc_name', 'gf_module', 'gf_intervention', 'activity_description', 'cost_category', 
              'disease', 'implementer', 'pr', 'budget',  
              'start_date', 'quarter', 'year', 
              'data_source', 'grant_period', 'grant_status', 
              'budget_version', 'version_date',
              'kp', 'rssh', 'equity')

fr_budgets = mapped_data[, .(budget=sum(budget, na.rm=T)), by=keep_cols]

write.csv(fr_budgets, paste0(box, "tableau_data/fr_budgets_nfm2.csv"), row.names=FALSE)
# ----------------------------------------------
