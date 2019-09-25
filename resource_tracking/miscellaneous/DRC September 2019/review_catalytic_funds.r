# ----------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Review catalytic funding for TB in DRC
# DATE: September 2019
# ----------------------------------------------

#Set up 

rm(list=ls())
# ----------------------------------------------------------------------
# To do list for this code: 
# - add in an option to only rework one file (make database append-only)
# ---------------------------------------------------------------------

prep_files = TRUE
prep_gos = FALSE
prep_odah = FALSE
prep_ghe = FALSE

#Processing options 
include_stops = TRUE #Set to true if you would like scripts to stop when errors are found (specifically, module mapping) Recommended to always leave as TRUE. 
verbose = FALSE #Set to true if you would like warning messages printed (helpful for debugging functions). Urgent messages will always be flagged regardless of this switch. 
rerun_filelist = TRUE #Set to TRUE if you want to prep all files in the file list again. 
limit_filelist = TRUE #Set to TRUE if you want to only run files that will be saved in final budgets and expenditures. 
test_current_files = TRUE #Set to true if you would like to run unit tests on current database. Set to false if you would like to run tests on archived database. 

# ----------------------------------------------
# STEP 1: SET UP R
# ----------------------------------------------
if (Sys.info()[1]=='Windows'){
  setwd("C:/Users/elineb/Documents/gf/") #Change to the root of your repository
} else {
  setwd("/ihme/homes/elineb/gf/")
}
source("./resource_tracking/prep/_common/set_up_r.R", encoding="UTF-8")
source("./resource_tracking/prep/_common/load_master_list.r", encoding="UTF-8")


country = "cod" #Change to the country you want to update. Options are "cod", "gtm", "sen", or "uga".  
master_file_dir = paste0(dir, "_gf_files_gos/", country, "/raw_data/")
export_dir = paste0(dir, "_gf_files_gos/", country, "/prepped_data/")


#Source document prep functions 
doc_prep_functions = list.files(paste0(code_dir, "gf_files_prep_functions"), full.names=TRUE)
for (file in doc_prep_functions){
  source(file, encoding="UTF-8")
}

# Load and verify mapping, prep data, and map data. 
source(paste0(code_dir, "2a_gf_files_verify_mapping.R"))
#Just copying over step 2b - prep GF files. 

#----------------------------------------------------
# Read in file list 
#----------------------------------------------------

file_list = load_master_list(purpose = "financial") #This function is sourced from the _common folder in master script. 
file_list = file_list[loc_name==country] #Just do one country at a time. 

#Make sure you don't have the same start date for the same grant (quick check; it would be better )
file_list[file_iteration=='final', date_dup:=sequence(.N), by=c('grant', 'sheet_financial', 'start_date_financial', 'data_source', 'pudr_semester_financial')] #EMILY NEED TO RETHINK THIS. 
file_list[, date_dup:=date_dup-1]#This indexes at one, so you need to decrement it

if ( nrow(file_list[date_dup>0])!=0){
  print(file_list[date_dup > 0, .(file_name, file_iteration, grant, grant_period, start_date_financial)][order(grant, grant_period, start_date_financial)])
  print("There are duplicates in final files - review file list.")
}

file_list[data_source=="pudr" & file_iteration=="final", pudr_dup:=sequence(.N), by=c('grant', 'grant_period', 'pudr_semester_financial')]
file_list[, pudr_dup:=pudr_dup-1] #This variable indexes at 1.
if (nrow(file_list[pudr_dup>0 & !is.na(pudr_dup)])>0){
  print(file_list[pudr_dup>0 & !is.na(pudr_dup)])
  stop("There are duplicates in PUDRs between semesters - review file list.")
}

# Just subset to the two files you need from COD-T-MOH. 
file_list = file_list[(grant=="COD-T-MOH" | grant=="COD-C-CORDAID") & grant_period=="2018-2020" & data_source=="budget"]

#Only keep the initial files for COD-T-MOH. 
file_list = file_list[!(file_iteration=="initial" & grant=="COD-C-CORDAID")]

#----------------------------------------------------
# 1. Rerun prep functions, or read in prepped files
#----------------------------------------------------

pudr_mod_approach_sheet_financials <- c('LFA Expenditure_7B', 'LFA AFR_7B', 'PR Expenditure_7A', 'RFA ALF_7B', 'ALF RFR_7')
general_detailed_budget_sheet_financials <- c('Detailed Budget', 'Detailed budget', 'DetailedBudget', 'Recomm_Detailed Budget', '1.Detailed Budget', "Detailed Budget Revise",
                                              'DETAIL', 'Detailed _ budget AGYW', 'Detailed Budget _ Human rights')

budget_cols = c("activity_description", "budget", "cost_category", "implementer", "intervention", "module", "quarter", "start_date", "year") #These are the only columns that should be returned from a budget function. 
pudr_cols = c("budget", "expenditure", "intervention", "module", "quarter", "start_date", "year") #These are the only columns that should be returned from a pudr function. 

for(i in 1:nrow(file_list)){
  # Set up file path 
  folder = "budgets"
  folder = ifelse (file_list$data_source[i] == "pudr", "pudrs", folder)
  if (file_list$file_iteration[i]=="initial"){
    version = "iterations"
  } else if (file_list$file_iteration[i]=="revision"){
    version= "revisions"
  } else {
    version = ""
  }
  grant_period = file_list$grant_period[i]
  
  file_dir = paste0(master_file_dir, file_list$grant_status[i], "/", file_list$grant[i], "/", grant_period, "/", folder, "/")
  if (version != ""){
    file_dir = paste0(file_dir, version, "/")
  }
  
  args = list(file_dir, file_list$file_name[i], file_list$sheet_financial[i], file_list$start_date_financial[i], file_list$period_financial[i])
  
  if(file_list$function_financial[i] == 'detailed' & file_list$sheet_financial[i]%in%general_detailed_budget_sheet_financials){ #Prep standardized detailed budgets. 
    args[length(args)+1] = file_list$qtr_number_financial[i]
    args[length(args)+1] = file_list$language_financial[i]
    tmpData = do.call(prep_general_detailed_budget, args)
    
    stopifnot(sort(names(tmpData)) == budget_cols)
    
  }
  
  #Add indexing data
  append_cols = file_list[i, .(data_source, grant_period, primary_recipient, file_name, grant_status, disease, grant, 
                               mod_framework_format, file_iteration, language_financial, file_currency, pudr_semester_financial, period_financial, update_date)]
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

saveRDS(resource_database, paste0(export_dir, "raw_bound_gf_files.RDS"))

#If you don't have lfa_exp_adjustment in any of the files for this country, add it as NA so checks later will work. 
if (!'lfa_exp_adjustment'%in%names(resource_database)){
  resource_database[, lfa_exp_adjustment:=NA]
}


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

#Add files here that had a sum total for 0 in raw file. 
verified_0_budget <- c('UGD-708-G08-M_PUDR 30Nov2011.xls', 'UGD-708-G08-M_PUDR_30June2012.xls', "Core_SANRU_PU_P3141116.xlsm",
                       "PSI PU NFM S1 2016 09102016.xlsm", "Core_PUDR_P30_HivosGT_231116_ LFA Signed.xlsx", 
                       "Core_PUDR_MALARIA_P12_03-03-17_Revisado ALF.xlsx")
#Add PUDRs here that did not report any expenditure.
verified_0_expenditure <- c("UGA-C-TASO_PU_PEJune2017_LFA_30Nov17.xlsx", "UGA-M-TASO_PU_PEJune2017_LFA_30Nov17.xlsx", 
                            "UGA-S-TASO_PU_PEJune2017_LFA_30Nov17.xlsx", "GTM-T-MSPAS_Progress Report_31Dec2017 LFA REVIEW.xlsx", 
                            "GTM-T-MSPAS_Progress Report jul _31Dec2018_v2  rev LFA.xlsx", "GTM-H-HIVOS_Progress Report_31Dec2018_v1.xlsx", 
                            "GTM-T-MSPAS_Progress Report_LFA18Mar19.xlsx", "Core_SANRU_PU_P3141116.xlsm", "PSI PU NFM S1 2016 09102016.xlsm", 
                            "Core_PUDR_P30_HivosGT_231116_ LFA Signed.xlsx", "Core_PUDR_MALARIA_P12_03-03-17_Revisado ALF.xlsx",  
                            "GTM-T-MSPAS_Progress Report_31Dec2017 LFA REVIEW.XLSX") #These files have 0 for all expenditure.

#Make sure that no files have a total sum of 0; this would indicate an error in the prep code. 
check_0_budgets <- resource_database[, .(budget = sum(budget, na.rm = TRUE)), by=.(file_name)]
check_0_budgets = check_0_budgets[budget == 0 & !file_name%in%verified_0_budget]
check_0_expenditure <- resource_database[data_source == 'pudr', .(expenditure = sum(expenditure, na.rm = TRUE)), by=.(file_name)]
check_0_expenditure <- check_0_expenditure[expenditure == 0 & !file_name%in%verified_0_expenditure]
stopifnot(nrow(check_0_budgets)==0 & nrow(check_0_expenditure)==0)

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

#Run mapping code 
if (prep_gos == TRUE){
  raw_data = totalGos_qtr
  raw_data[, lfa_exp_adjustment:=0] #There is no LFA expenditure adjustment in GOS data; but add it to make code run. 
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
orig_rows = nrow(mapped_data)
stopifnot(mapped_data$file_currency%in%c("LOC","EUR","USD"))

needs_conversion = mapped_data[file_currency!='USD']
if (nrow(needs_conversion)!=0){
  #Do a check before and after converting to make sure you've got the same totals. 
  pre_conversion_check = mapped_data[, .(pre_budget=sum(budget, na.rm=T), pre_expenditure=sum(expenditure, na.rm=T), 
                                         pre_lfa_exp=sum(lfa_exp_adjustment, na.rm=T)), by='file_name']
  
  #Pull apart the files that are in Euros vs. USD, and convert Euros. 
  valueVars = c('budget', 'expenditure', 'disbursement', 'lfa_exp_adjustment')
  in_USD = copy(mapped_data)
  in_USD = in_USD[file_currency=='USD']
  in_USD[, budget_new:=budget]
  in_USD[, expenditure_new:=expenditure]
  in_USD[, lfa_exp_adjustment_new:=lfa_exp_adjustment]
  in_USD[, disbursement_new:=disbursement]
  
  stopifnot(needs_conversion$file_currency%in%c("LOC", "EUR")) #These are the only currencies the function supports. 
  converted_to_USD = convert_eur_usd(needs_conversion, 'year')
  mapped_data = rbind(in_USD, converted_to_USD, fill=TRUE, use.names=TRUE) #You're not losing any rows here. 
  stopifnot(nrow(mapped_data)==orig_rows)
  
  # #Post-check. 
  # mapped_data$eur_usd <- NULL
  # post_conversion_check = convert_usd_eur(mapped_data, 'year')
  # post_conversion_check = post_conversion_check[, .(post_budget=sum(budget, na.rm=T), post_expenditure=sum(expenditure, na.rm=T)), by='file_name']
  # 
  # conversion_check = merge(pre_conversion_check, post_conversion_check, by='file_name', all=T)
  # conversion_check = conversion_check[, lapply(.SD, round), .SDcols = 2:5, by='file_name'][, lapply(.SD, as.integer), .SDcols = 2:5, by='file_name']
  # if (nrow(conversion_check[pre_budget!=post_budget | pre_expenditure!= post_expenditure])==0){
  #   View(conversion_check[pre_budget!=post_budget | pre_expenditure!= post_expenditure])
  #   stop("Errors in currency conversion - review 'conversion_check'." )
  # }
  
  #If the check above works, then you're okay to rename budget and expenditure 
  mapped_data = mapped_data[, -c('budget', 'expenditure', 'disbursement', 'lfa_exp_adjustment')]
  setnames(mapped_data, c('budget_new', 'expenditure_new', 'disbursement_new', 'lfa_exp_adjustment_new'), 
           c('budget', 'expenditure', 'disbursement', 'lfa_exp_adjustment'))
}

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
# Reshape dataset wide to review revisions - copy budget revision code. 
# ---------------------------------------------------------------------------
#Order the data using the file iteration variable - 'initial' should come first. 
revisions = copy(mapped_data)

revisions_collapse = revisions[, .(budget=sum(budget, na.rm=T)), by=c('grant', 'grant_period', 'file_iteration', 'year', 'quarter', 'gf_module', 'gf_intervention', 
                                                                      'orig_module', 'orig_intervention', 'activity_description')]
revisions_collapse[, quarter:=paste0('q', quarter)]

#Cast wide 
revisions_collapse = dcast(revisions_collapse, grant+grant_period+gf_module+gf_intervention+orig_module+orig_intervention+activity_description+year+quarter~file_iteration, value.var='budget')

saveRDS(revisions_collapse, paste0(j, "Project/Evaluation/GF/resource_tracking/_gf_files_gos/cod/prepped_data/tb_catalytic_funding_budgets.rds"))

#--------------------------------------------------------
# ANALYSIS 
# -------------------------------------------------------
#What could be seen as a grant revision? Either where 
#1. The activity description did not exist in the initial file, or 
#2. More money was allocated to the activity in the final grant than in the initial. 
# catalytic_funding = revisions_collapse[is.na(initial) | final>initial] 
# 
# #However, right now we don't know whether these are specifically catalytic funding, or just additions to the budget between initial - revision, so flag 
# # some likely activities that are probably catalytic funding for active case detection. 
# keywords = c('investigation des contacts', 'recherche', 'occasions manquees', 'cas manquants', 'xpert', 'diagnostic de la tb', 'cplt', 'activites communautaires')
# for (word in keywords){
#   catalytic_funding[grepl(word, tolower(activity_description)), likely_catalytic:=TRUE]
# }
# catalytic_funding[is.na(likely_catalytic), likely_catalytic:=FALSE]
# 
# #Review the activity descriptions by hand. 
# catalytic_funding[likely_catalytic==TRUE, unique(activity_description)]
# catalytic_funding[likely_catalytic==FALSE, unique(activity_description)]
# 
# saveRDS(catalytic_funding, "J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/cod/prepped_data/tb_catalytic_funding_budgets.rds")
# 
# # Pull out just the likely catalytic funds and see what's going on. 
# # catalytic1 = catalytic_funding[likely_catalytic==TRUE]
# 
# #What's the total funding allocated by catalytic funds? (compare with grant documents. )
# catalytic_funding[, sum(final, na.rm=T)] - catalytic_funding[, sum(initial, na.rm=T)] # $1,852,116
# 
# #Break this down by activity description. 
# catalytic_funding[is.na(initial), initial:=0]
# catalytic_funding[, addl_funds:=final-initial]
# 
# 
# catalytic_funding[, .(catalytic_funds=sum(addl_funds)), by='activity_description'][order(-catalytic_funds)]
