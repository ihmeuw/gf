# ----------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Prep country-level budgets and PUDRs
# DATE: Last updated March 2019 
# ----------------------------------------------

rm(list=ls())
# ----------------------------------------------
# STEP 1: SET UP R
# ----------------------------------------------
if (Sys.info()[1]=='Windows'){
  setwd("C:/Users/elineb/Documents/gf/") #Change to the root of your repository
} else {
  setwd("/ihme/homes/elineb/gf/")
}
include_stops=FALSE
verbose=FALSE
source("./resource_tracking/prep/_common/set_up_r.R", encoding="UTF-8")
source("./resource_tracking/prep/2a_gf_files_verify_mapping.R", encoding="UTF-8")
source("C:/Users/elineb/Documents/gf/resource_tracking/prep/gf_files_prep_functions/prep_drc_provincial_budget.R", encoding="UTF-8")
#----------------------------------------------------
# Read in file list 
#----------------------------------------------------
file_list = data.table(read_xlsx("C:/Users/elineb/Box Sync/Global Fund Files/master_file_list.xlsx"))
file_list = file_list[geography_detail=="SUBNATIONAL" & loc_name=="cod"]
file_list[, start_date_financial:=as.numeric(start_date_financial)]
file_list[, start_date_financial:=as.Date(start_date_financial, origin="1899-12-30")]

# Correct some sheet names 
file_list[sheet_financial =="PTB_BDOM KIN", sheet_financial:="PTB_BDOM KIN "]

#----------------------------------------------------
# 1. Rerun prep functions, or read in prepped files
#----------------------------------------------------
file_loc = "C:/Users/elineb/Box Sync/Global Fund Files/COD/raw_data/active/multiple/budgets/provincial_budgets/"
for(i in 1:nrow(file_list)){
  # Set up file path 
  args = list(file_loc, file_list$file_name[i], file_list$sheet_financial[i], 
              file_list$start_date_financial[i], file_list$period_financial[i], file_list$qtr_number_financial[i], 
              file_list$language_financial[i])
  
  tmpData = do.call(prep_general_detailed_budget, args)
    
  #Add indexing data
  append_cols = file_list[i, .(data_source, grant_period, primary_recipient, secondary_recipient, file_name, grant_status, disease, grant, 
                               mod_framework_format, file_iteration, language_financial, file_currency, pudr_semester_financial, 
                               period_financial, update_date)]
  for (col in names(append_cols)){
    tmpData[, (col):=append_cols[, get(col)]]
  }  
  tmpData$year <- year(tmpData$start_date)
  tmpData[, file_start_date:=min(start_date), by='file_name']
  
  #Bind data together 
  if(i==1){
    resource_database = tmpData
  } else {
    resource_database = rbind(resource_database, tmpData, use.names=TRUE, fill = TRUE)
  }
  print(paste0(i, " ", file_list$data_source[i], " ", file_list$function_financial[i], " ", file_list$grant[i])) ## if the code breaks, you know which file it broke on
}

#Secondary recipient is actually the province in this case 
setnames(resource_database, 'secondary_recipient', 'province')

#------------------------------------------------------------------
# 2. Run some checks to make sure this data was prepped correctly. 
#-----------------------------------------------------------------
original_db <- copy(resource_database)
#Make sure all budget data pulled is actually numeric- this is an easy check to see if prep functions are working correctly. 
verify_numeric_budget = resource_database[, .(budget=gsub("[[:digit:]]", "", budget))]
verify_numeric_budget = verify_numeric_budget[, .(budget=gsub("[[:punct:]]", "", budget))]
verify_numeric_budget = verify_numeric_budget[!is.na(budget) & budget != ""]
stopifnot(nrow(verify_numeric_budget)==0)

# Make sure there are no overlapping quarters for the same grant (duplicate files. )
budget_overlap <- duplicated(resource_database[data_source == "budget" & file_iteration == "final", .(grant, start_date)])
pudr_overlap <- duplicated(resource_database[data_source == "pudr" & file_iteration == "final", .(grant, start_date)])
stopifnot(nrow(budget_overlap)==0 & nrow(pudr_overlap)==0)

rm(budget_overlap, pudr_overlap)

#Make sure all budget and expenditure variables are numeric. 
resource_database$budget <- as.numeric(resource_database$budget)
resource_database$expenditure <- as.numeric(resource_database$expenditure)
resource_database$disbursement <- as.numeric(resource_database$disbursement)

#Hacky fix - this should be fixed earlier in the prep functions, but remove anything at this point that has NAs for module, intervention, and budget OR expenditure. 
resource_database[module=='all', module:='unspecified']
resource_database[tolower(intervention)=='all', intervention:='unspecified']
resource_database[is.na(module), module:='unspecified'] 
resource_database[is.na(intervention), intervention:='unspecified']
resource_database = resource_database[!(module=='unspecified' & intervention=='unspecified' & budget == 0 & expenditure == 0)]

#check for duplicates, and sum their values if they exist:
dups<-resource_database[duplicated(resource_database) | duplicated(resource_database, fromLast=TRUE)]
print(paste0(nrow(dups), " duplicates found in database; values will be summed"))
byVars = names(resource_database)[!names(resource_database)%in%c('budget', 'expenditure', 'disbursement')]
resource_database= resource_database[, list(budget=sum(na.omit(budget)) ,expenditure=sum(na.omit(expenditure)), disbursement=sum(na.omit(disbursement))), by=byVars]

#Make sure you have all the files here that you started with in your filelist. 
rt_files <- unique(resource_database$file_name)
warning1 = (length(unique(file_list$file_name)) == length(rt_files))
if (!warning1){
  warning("The length of the original file list is not the same as the number of processed files.")
}
warning2 = sort(rt_files) == sort(unique(file_list$file_name))
if (!warning2){
  warning("The files in the processed data are not the same as the files in the file list.")
}

#---------------------------------
# MAP DATA 
#---------------------------------

#----------------------------------------------------------------------------
# Read in the version of the data you want to map (logic variables set in master file)
#----------------------------------------------------------------------------
raw_data = copy(resource_database)

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
  pre_coeff_check = raw_data[, lapply(.SD, sum_na_rm), .SDcols=c('budget')]
} else {
  pre_coeff_check = raw_data[, lapply(.SD, sum_na_rm), .SDcols=c('budget')]
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
if ('disbursement'%in%names(mapped_data)){
  mapped_data[, disbursement:=disbursement*coefficient]
}
#------------------------------------------------------------
# Remove any special characters so .csv will store correctly 
#------------------------------------------------------------
mapped_data$activity_description <- str_replace_all(mapped_data$activity_description, "[^[:alnum:]]", " ")
mapped_data$orig_module <- str_replace_all(mapped_data$orig_module, "[^[:alnum:]]", " ")
mapped_data$orig_intervention <- str_replace_all(mapped_data$orig_intervention, "[^[:alnum:]]", " ")

# Save this data 
saveRDS(mapped_data, paste0("C:/Users/elineb/Box Sync/Global Fund Files/COD/prepped_data/drc_provincial_budgets.RDS"))
saveRDS(mapped_data, paste0("C:/Users/elineb/Box Sync/Global Fund Files/COD/prepped_data/archive/drc_provincial_budgets_", Sys.Date(), ".RDS"))

