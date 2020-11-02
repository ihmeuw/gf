# Set up

# The current working directory should be the root of this repository
# ----------------------------------------------


####### THIS WAS COPIED FROM THE RESOURCE TRACKING MASTER FILE SO THAT SAME SHEET CAN BE USED INSTEAD ####################
rm(list=ls())

# ----------------------------------------------
# STEP 1: SET UP R
# ----------------------------------------------
user=as.character(Sys.info()[7])
if (Sys.info()[1]=='Windows'){
  setwd(paste0("C:/Users/",user,"/Documents/gf/")) #Change to the root of your repository
} else {
  setwd(paste0("/ihme/homes/",user,"/gf/"))
}
source("./resource_tracking/prep/_common/set_up_r.R", encoding="UTF-8")
source("./resource_tracking/prep/_common/load_master_list.r", encoding="UTF-8")

# ---------------------------------------
# Boolean logic switches 
# ---------------------------------------
#What datasets do you want to run? 
prep_files = TRUE
prep_gos = FALSE
prep_odah = FALSE
prep_ghe = FALSE
prep_cost_categories = FALSE
prep_commitments = FALSE

#Processing options 
include_stops = TRUE #Set to TRUE if you would like scripts to stop when errors are found (specifically, module mapping) Recommended to always leave as TRUE. 
verbose = FALSE #Set to true if you would like warning messages printed (helpful for debugging functions). Urgent messages will always be flagged regardless of this switch. 
rerun_filelist = TRUE  #Set to TRUE if you want to prep all files in the file list again. 
limit_filelist = TRUE #Set to TRUE if you want to only run files that will be saved in final budgets and expenditures. 
only_new_files = FALSE # Set to TRUE if, when you re-run file list, you only want to process files that are additional. TRUE is the default. 
include_zero_pudrs = FALSE # Set to TRUE if when you re-run file list, the final data set will include PUDRs with zero expenditure, if FALSE the PUDRs will not be included. FALSE is default

country = "sen" #Change to the country you want to update. Options are "cod", "gtm", "sen", or "uga". 


master_file_dir = ifelse(Sys.info()[1]=='Windows', paste0(box, toupper(country), "/raw_data/"), 
                         paste0(dir, "_gf_files_gos/", country, "/raw_data/"))
export_dir = ifelse(Sys.info()[1]=="Windows", paste0(box, country, "/prepped_data/"),
                    paste0(dir, "_gf_files_gos/", country, "/prepped_data/"))

#Source document prep functions 
doc_prep_functions = list.files(paste0(code_dir, "gf_files_prep_functions"), full.names=TRUE)
for (file in doc_prep_functions){
  source(file, encoding="UTF-8")
}

######################################################################################

# load original_file_list
if (prep_files == TRUE){# THIS COULD PROBABLY BE SIMPLIFIED
  original_file_list = load_master_list(purpose = "financial") #This function is sourced from the _common folder in master script. 
  original_file_list = original_file_list[loc_name==country] #Just do one country at a time. 
  
  #Prioritize GOS data where we have it
  original_file_list = prioritize_gos(original_file_list)
  
  #Drop cases where grant is unknown (some subnational budgets in DRC are like this)
  original_file_list = original_file_list[grant!="unknown"]
  
  #Make sure you don't have the same start date for the same grant (quick check; it would be better )
  original_file_list[file_iteration=='approved_gm' & data_source!='funding_request', date_dup:=sequence(.N), by=c('grant', 'sheet_financial', 'start_date_financial', 'data_source', 'pudr_semester_financial')] #EMILY NEED TO RETHINK THIS. 
  original_file_list[, date_dup:=date_dup-1]#This indexes at one, so you need to decrement it
  
  if ( nrow(original_file_list[date_dup>0])!=0){
    print(original_file_list[date_dup > 0, .(file_name, file_iteration, grant, grant_period, start_date_financial)][order(grant, grant_period, start_date_financial)])
    print("There are duplicates in final files - review file list.")
  }
  
  original_file_list[data_source=="pudr" & file_iteration=='approved_gm', pudr_dup:=sequence(.N), by=c('grant', 'grant_period', 'pudr_semester_financial')]
  original_file_list[, pudr_dup:=pudr_dup-1] #This variable indexes at 1.
  if (nrow(original_file_list[pudr_dup>0 & !is.na(pudr_dup)])>0){
    print(original_file_list[pudr_dup>0 & !is.na(pudr_dup)])
    stop("There are duplicates in PUDRs between semesters - review file list.")
  }
  
  # limit original_file_list to only prepping PUDRs
  original_file_list = original_file_list[((data_source =='pudr' & file_iteration %in% c('approved_gm', 'revision') & grant_status == 'active'))]
  
  # limit original_file_list to only prepping PUDRs from current grant cycle
  if (country=="uga") {
    original_file_list = original_file_list[grant %in% current_uga_grants & grant_period %in% current_uga_grant_period]
  } else if (country=="cod") {
    original_file_list = original_file_list[grant %in% current_cod_grants & grant_period %in% current_cod_grant_period]
  } else if (country=="sen") {
    original_file_list = original_file_list[grant %in% current_sen_grants & grant_period %in% current_sen_grant_period]
  } else if (country=="gtm") {
    original_file_list = original_file_list[grant %in% current_gtm_grants & grant_period %in% current_gtm_grant_period]
  }
}

# load this package required for data cleaning of percent signs
library(scales)


#######################################################
# source the prep function
# unfortunately this will currently only set up to read in one sheet from the pudr at a time

pudr_mod_approach_sheet_financials <- c('LFA Expenditure_7B', 'LFA AFR_7B', 'PR Expenditure_7A', 'RFA ALF_7B', 'ALF RFR_7')

#########################
### First prep the LFA column of LFA verified PUDRs
##########################

# some Senegal and guatemala files have been reviewed by the LFA but they did not insert comments on the expenditure
if (country=="sen") {
  file_list <- original_file_list[lfa_verified==TRUE & sheet_financial=="LFA Expenditure_7B"]
} else if (country=="gtm") {
  file_list <- original_file_list[lfa_verified==TRUE & sheet_financial=="LFA Expenditure_7B"]
} else {
  file_list <- original_file_list[lfa_verified==TRUE]
}
  

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
  
  # args = list(file_dir, file_list$file_name[i], file_list$sheet_financial[i], file_list$start_date_financial[i], file_list$period_financial[i])
  args = list(file_dir, file_list$file_name[i], file_list$sheet_financial[i], file_list$start_date_financial[i])
  
  if (file_list$function_financial[i] == 'pudr' & file_list$sheet_financial[i]%in%pudr_mod_approach_sheet_financials){ #Prep standardized 'modular approach' PUDRs.
    # args[length(args)+1] = file_list$qtr_number_financial[i]
    tmpData = do.call(prep_modular_approach_pudr_qualitative, args)
  
  } else {
    print(paste0("File not being processed: ", file_list$file_name[i]))
    print(paste0("Check logic conditions. This file has the function_financial: ", file_list$function_financial[i],
                 " and the sheet_financial name: ", file_list$sheet_financial[i]))
  }
  
  #Add indexing data
  append_cols = file_list[i, .(data_source, grant_period, primary_recipient, file_name, grant_status, disease, grant, 
                               mod_framework_format, file_iteration, budget_version, language_financial, 
                               pudr_semester_financial, period_financial, lfa_verified)]
  
  stopifnot(nrow(append_cols)==1)
  
  tmpData = cbind(tmpData, append_cols)
  
  # tmpData$year <- year(tmpData$start_date)
  # tmpData[, file_start_date:=min(start_date), by='file_name']
  
  # need to split out by implementer for UGA file: FR188-UGA-C_DB_14Mar17  Revised 06 April 2017-1.xlsx
  if (unique(tmpData$file_name) == 'FR188-UGA-C_DB_14Mar17  Revised 06 April 2017-1.xlsx'){
    tmpData_TASO = tmpData[implementer == 'The AIDS Support Organisation (Uganda) Limited', ]
    tmpData_MoFPED = tmpData[implementer == 'Ministry of Finance, Planning and Economic Development of the Government of the Republic of Uganda']
    tmpData_MoFPED[, grant := 'UGA-H-MoFPED']
    tmpData_MoFPED[, primary_recipient := 'MoFPED']
    tmpData_MoFPED[, disease := 'hiv']
    tmpData = rbind(tmpData_MoFPED, tmpData_TASO)
  }
  
  #Bind data together 
  if(i==1){
    resource_database_lfa = tmpData
  } else {
    resource_database_lfa = rbind(resource_database_lfa, tmpData, use.names=TRUE, fill = TRUE)
  }
  print(paste0(i, " ", file_list$data_source[i], " ", file_list$function_financial[i], " ", file_list$grant[i], " ", file_list$sheet_financial[i])) ## if the code breaks, you know which file it broke on
}

### source the prep function to prep PR comments

### HERE PREP ONLY THE COMMENTS THAT WERE FROM LFA-VERIFIED PUDRS with PR COMMENTS AS WELL
if (country=="gtm"){
  file_list <- original_file_list[lfa_verified==TRUE & sheet_financial!="INTEGRACION"]
} else {
  file_list <- original_file_list[lfa_verified==TRUE]
}

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
  
  # args = list(file_dir, file_list$file_name[i], file_list$sheet_financial[i], file_list$start_date_financial[i], file_list$period_financial[i])
  sheet_name <- "PR Expenditure_7A"
  args = list(file_dir, file_list$file_name[i], sheet_name, file_list$start_date_financial[i])
  
  if (file_list$function_financial[i] == 'pudr' & file_list$sheet_financial[i]%in%pudr_mod_approach_sheet_financials){ #Prep standardized 'modular approach' PUDRs.
    # args[length(args)+1] = file_list$qtr_number_financial[i]
    tmpData = do.call(prep_modular_approach_pudr_qualitative, args)
    
    
  } else {
    print(paste0("File not being processed: ", file_list$file_name[i]))
    print(paste0("Check logic conditions. This file has the function_financial: ", file_list$function_financial[i],
                 " and the sheet_financial name: ", file_list$sheet_financial[i]))
  }
  
  #Add indexing data
  append_cols = file_list[i, .(data_source, grant_period, primary_recipient, file_name, grant_status, disease, grant, 
                               mod_framework_format, file_iteration, budget_version, language_financial, 
                               pudr_semester_financial, period_financial, lfa_verified)]
  
  stopifnot(nrow(append_cols)==1)
  
  tmpData = cbind(tmpData, append_cols)
  
  # tmpData$year <- year(tmpData$start_date)
  # tmpData[, file_start_date:=min(start_date), by='file_name']
  
  # need to split out by implementer for UGA file: FR188-UGA-C_DB_14Mar17  Revised 06 April 2017-1.xlsx
  if (unique(tmpData$file_name) == 'FR188-UGA-C_DB_14Mar17  Revised 06 April 2017-1.xlsx'){
    tmpData_TASO = tmpData[implementer == 'The AIDS Support Organisation (Uganda) Limited', ]
    tmpData_MoFPED = tmpData[implementer == 'Ministry of Finance, Planning and Economic Development of the Government of the Republic of Uganda']
    tmpData_MoFPED[, grant := 'UGA-H-MoFPED']
    tmpData_MoFPED[, primary_recipient := 'MoFPED']
    tmpData_MoFPED[, disease := 'hiv']
    tmpData = rbind(tmpData_MoFPED, tmpData_TASO)
  }
  
  #Bind data together 
  if(i==1){
    resource_database_pr = tmpData
  } else {
    resource_database_pr = rbind(resource_database_pr, tmpData, use.names=TRUE, fill = TRUE)
  }
  print(paste0(i, " ", file_list$data_source[i], " ", file_list$function_financial[i], " ", file_list$grant[i]," ", sheet_name)) ## if the code breaks, you know which file it broke on
}

resource_database <- rbind(resource_database_pr, resource_database_lfa)


############################################################################
# Data Prep
##############################################################################
library(xlsx)
# resource_database <- readRDS(paste0(dir, "qualitative_data/quali_data_pudr.RDS"))

# subset columns to keep
resource_database <- resource_database[,.(module, intervention, absorption_rate, comments, sheet_name, breakdown_type, grant, grant_period, pudr_semester_financial)]

# # add information on pudr semester
setnames(resource_database, 'pudr_semester_financial', 'pudr_code')
pudr_labels_abbrev = read.xlsx(paste0(dir, "documentation/PUDR Semester Labeling Qual.xlsx"), sheetIndex = 1)
resource_database = merge(resource_database, pudr_labels_abbrev, by=c('pudr_code'), all.x=T)
if (nrow(resource_database[is.na(sem_abbrev)])>0){
  print(unique(resource_database[is.na(sem_abbrev), .(pudr_code)]))
  stop("Values of pudr_code did not merge correctly.")
}

# split
###########################
# modular framework comments
###########################
if (country=="gtm") {
comments_ma <- resource_database[breakdown_type=="Modular_approach"]

comments_ma_pr <- comments_ma[sheet_name=="PR Expenditure_7A",.(module, intervention, comments, grant, grant_period, sem_abbrev)]
comments_ma_lfa <- comments_ma[sheet_name=="LFA Expenditure_7B",.(module, intervention, absorption_rate, comments, grant, grant_period, sem_abbrev)]

# remove duplicated rows probably an extraction issue and can come back and fix later on if necessary
if (country=="cod"){
  # remove duplicated rows
  comments_ma_pr <- comments_ma_pr[-244]
  comments_ma_lfa <- comments_ma_lfa[-244]
}

setnames(comments_ma_pr, old = "comments", new = "pr_comments")
setnames(comments_ma_lfa, old= "comments", new = "lfa_comments")

comments_merge1 <- merge(comments_ma_pr, comments_ma_lfa, by=c("module", "intervention", "grant", "grant_period", "sem_abbrev"), all = TRUE)

# cast data wide
merge1 <- dcast(comments_merge1, grant + grant_period + module + intervention ~ sem_abbrev, value.var = c("absorption_rate", "pr_comments", "lfa_comments"))

##########################
# Cost category comments
#########################
comments_cc <- resource_database[breakdown_type=="Cost_category"]

comments_cc_pr <- comments_cc[sheet_name=="PR Expenditure_7A",.(module, comments, grant, grant_period, sem_abbrev)]
comments_cc_lfa <- comments_cc[sheet_name=="LFA Expenditure_7B",.(module, absorption_rate, comments, grant, grant_period, sem_abbrev)]

setnames(comments_cc_pr, old = c("module", "comments"), new = c("cost_category", "pr_comments"))
setnames(comments_cc_lfa, old= c("module", "comments"), new = c("cost_category", "lfa_comments"))

comments_merge2 <- merge(comments_cc_pr, comments_cc_lfa, by=c("cost_category", "grant", "grant_period", "sem_abbrev"), all = TRUE)

# cast data wide
merge2 <- dcast(comments_merge2, grant + grant_period + cost_category ~ sem_abbrev, value.var = c("absorption_rate", "pr_comments", "lfa_comments"))

#########################
# Implementing entity
#########################
comments_ie <- resource_database[breakdown_type=="Implementing_entity"]

comments_ie_pr <- comments_ie[sheet_name=="PR Expenditure_7A",.(module, comments, grant, grant_period, sem_abbrev)]
comments_ie_lfa <- comments_ie[sheet_name=="LFA Expenditure_7B",.(module, absorption_rate, comments, grant, grant_period, sem_abbrev)]

setnames(comments_ie_pr, old = c("module", "comments"), new = c("implementing_entity", "pr_comments"))
setnames(comments_ie_lfa, old= c("module", "comments"), new = c("implementing_entity", "lfa_comments"))

comments_merge3 <- merge(comments_ie_pr, comments_ie_lfa, by=c("implementing_entity", "grant", "grant_period", "sem_abbrev"), all = TRUE)

# cast data wide
merge3 <- dcast(comments_merge3, grant + grant_period + implementing_entity ~ sem_abbrev, value.var = c("absorption_rate", "pr_comments", "lfa_comments"))
} else {
  comments_ma <- resource_database[breakdown_type=="Modular_approach"]
  
  comments_ma_pr <- comments_ma[sheet_name=="PR Expenditure_7A",.(module, intervention, comments, grant, sem_abbrev)]
  comments_ma_lfa <- comments_ma[sheet_name=="LFA Expenditure_7B",.(module, intervention, absorption_rate, comments, grant, sem_abbrev)]
  
  # remove duplicated rows probably an extraction issue and can come back and fix later on if necessary
  if (country=="cod"){
    # remove duplicated rows
    comments_ma_pr <- comments_ma_pr[-244]
    comments_ma_lfa <- comments_ma_lfa[-244]
  }
  
  setnames(comments_ma_pr, old = "comments", new = "pr_comments")
  setnames(comments_ma_lfa, old= "comments", new = "lfa_comments")
  
  comments_merge1 <- merge(comments_ma_pr, comments_ma_lfa, by=c("module", "intervention", "grant", "sem_abbrev"), all = TRUE)
  
  # cast data wide
  merge1 <- dcast(comments_merge1, grant + module + intervention ~ sem_abbrev, value.var = c("absorption_rate", "pr_comments", "lfa_comments"))
  
  ##########################
  # Cost category comments
  #########################
  comments_cc <- resource_database[breakdown_type=="Cost_category"]
  
  comments_cc_pr <- comments_cc[sheet_name=="PR Expenditure_7A",.(module, comments, grant, sem_abbrev)]
  comments_cc_lfa <- comments_cc[sheet_name=="LFA Expenditure_7B",.(module, absorption_rate, comments, grant, sem_abbrev)]
  
  setnames(comments_cc_pr, old = c("module", "comments"), new = c("cost_category", "pr_comments"))
  setnames(comments_cc_lfa, old= c("module", "comments"), new = c("cost_category", "lfa_comments"))
  
  comments_merge2 <- merge(comments_cc_pr, comments_cc_lfa, by=c("cost_category", "grant", "sem_abbrev"), all = TRUE)
  
  # cast data wide
  merge2 <- dcast(comments_merge2, grant + cost_category ~ sem_abbrev, value.var = c("absorption_rate", "pr_comments", "lfa_comments"))
  
  #########################
  # Implementing entity
  #########################
  comments_ie <- resource_database[breakdown_type=="Implementing_entity"]
  
  comments_ie_pr <- comments_ie[sheet_name=="PR Expenditure_7A",.(module, comments, grant, sem_abbrev)]
  comments_ie_lfa <- comments_ie[sheet_name=="LFA Expenditure_7B",.(module, absorption_rate, comments, grant, sem_abbrev)]
  
  setnames(comments_ie_pr, old = c("module", "comments"), new = c("implementing_entity", "pr_comments"))
  setnames(comments_ie_lfa, old= c("module", "comments"), new = c("implementing_entity", "lfa_comments"))
  
  comments_merge3 <- merge(comments_ie_pr, comments_ie_lfa, by=c("implementing_entity", "grant", "sem_abbrev"), all = TRUE)
  
  # cast data wide
  merge3 <- dcast(comments_merge3, grant + implementing_entity ~ sem_abbrev, value.var = c("absorption_rate", "pr_comments", "lfa_comments"))
}

# set column order for files
if (country=="uga") {
  setcolorder(merge1,
            c("grant", "module", "intervention", 
              "absorption_rate_sem_1", "pr_comments_sem_1", "lfa_comments_sem_1",
              "absorption_rate_sem_1-2", "pr_comments_sem_1-2", "lfa_comments_sem_1-2",
              "absorption_rate_sem_2", "pr_comments_sem_2", "lfa_comments_sem_2",
              "absorption_rate_sem_3", "pr_comments_sem_3", "lfa_comments_sem_3",
              "absorption_rate_sem_3-4", "pr_comments_sem_3-4", "lfa_comments_sem_3-4"))

setcolorder(merge2,
            c("grant", "cost_category", 
              "absorption_rate_sem_1", "pr_comments_sem_1", "lfa_comments_sem_1",
              "absorption_rate_sem_1-2", "pr_comments_sem_1-2", "lfa_comments_sem_1-2",
              "absorption_rate_sem_2", "pr_comments_sem_2", "lfa_comments_sem_2",
              "absorption_rate_sem_3", "pr_comments_sem_3", "lfa_comments_sem_3",
              "absorption_rate_sem_3-4", "pr_comments_sem_3-4", "lfa_comments_sem_3-4"))

setcolorder(merge3,
            c("grant", "implementing_entity", 
              "absorption_rate_sem_1", "pr_comments_sem_1", "lfa_comments_sem_1",
              "absorption_rate_sem_1-2", "pr_comments_sem_1-2", "lfa_comments_sem_1-2",
              "absorption_rate_sem_2", "pr_comments_sem_2", "lfa_comments_sem_2",
              "absorption_rate_sem_3", "pr_comments_sem_3", "lfa_comments_sem_3",
              "absorption_rate_sem_3-4", "pr_comments_sem_3-4", "lfa_comments_sem_3-4"))
} else if (country=="gtm"){
  setcolorder(merge1,
            c("grant", "grant_period", "module", "intervention", 
              "absorption_rate_sem_1", "pr_comments_sem_1", "lfa_comments_sem_1",
              "absorption_rate_sem_1-2", "pr_comments_sem_1-2", "lfa_comments_sem_1-2",
              "absorption_rate_sem_1-3", "pr_comments_sem_1-3", "lfa_comments_sem_1-3",
              "absorption_rate_sem_5", "pr_comments_sem_5", "lfa_comments_sem_5",
              "absorption_rate_sem_5-6", "pr_comments_sem_5-6", "lfa_comments_sem_5-6"))
  
  setcolorder(merge2,
              c("grant", "grant_period", "cost_category", 
                "absorption_rate_sem_1", "pr_comments_sem_1", "lfa_comments_sem_1",
                "absorption_rate_sem_1-2", "pr_comments_sem_1-2", "lfa_comments_sem_1-2",
                "absorption_rate_sem_1-3", "pr_comments_sem_1-3", "lfa_comments_sem_1-3",
                "absorption_rate_sem_5", "pr_comments_sem_5", "lfa_comments_sem_5",
                "absorption_rate_sem_5-6", "pr_comments_sem_5-6", "lfa_comments_sem_5-6"))
  
  setcolorder(merge3, 
              c("grant", "grant_period", "implementing_entity", 
                "absorption_rate_sem_1", "pr_comments_sem_1", "lfa_comments_sem_1",
                "absorption_rate_sem_1-2", "pr_comments_sem_1-2", "lfa_comments_sem_1-2",
                "absorption_rate_sem_1-3", "pr_comments_sem_1-3", "lfa_comments_sem_1-3",
                "absorption_rate_sem_5", "pr_comments_sem_5", "lfa_comments_sem_5",
                "absorption_rate_sem_5-6", "pr_comments_sem_5-6", "lfa_comments_sem_5-6"))
  }  else{
  setcolorder(merge1,
              c("grant", "module", "intervention", 
                "absorption_rate_sem_1", "pr_comments_sem_1", "lfa_comments_sem_1",
                "absorption_rate_sem_1-2", "pr_comments_sem_1-2", "lfa_comments_sem_1-2",
                "absorption_rate_sem_3", "pr_comments_sem_3", "lfa_comments_sem_3",
                "absorption_rate_sem_3-4", "pr_comments_sem_3-4", "lfa_comments_sem_3-4",
                "absorption_rate_sem_5", "pr_comments_sem_5", "lfa_comments_sem_5"))
  
  setcolorder(merge2,
              c("grant", "cost_category", 
                "absorption_rate_sem_1", "pr_comments_sem_1", "lfa_comments_sem_1",
                "absorption_rate_sem_1-2", "pr_comments_sem_1-2", "lfa_comments_sem_1-2",
                "absorption_rate_sem_3", "pr_comments_sem_3", "lfa_comments_sem_3",
                "absorption_rate_sem_3-4", "pr_comments_sem_3-4", "lfa_comments_sem_3-4",
                "absorption_rate_sem_5", "pr_comments_sem_5", "lfa_comments_sem_5"))
  
  setcolorder(merge3,
              c("grant", "implementing_entity", 
                "absorption_rate_sem_1", "pr_comments_sem_1", "lfa_comments_sem_1",
                "absorption_rate_sem_1-2", "pr_comments_sem_1-2", "lfa_comments_sem_1-2",
                "absorption_rate_sem_3", "pr_comments_sem_3", "lfa_comments_sem_3",
                "absorption_rate_sem_3-4", "pr_comments_sem_3-4", "lfa_comments_sem_3-4",
                "absorption_rate_sem_5", "pr_comments_sem_5", "lfa_comments_sem_5"))
}

# aggregate files into one per grant and save on shared drive

grants_processed <- unique(resource_database$grant)
for (g in grants_processed){
  write.xlsx(merge1[grant==g], file = paste0(dir,"qualitative_data/",country,"/", g,"_pudr_comments",".xlsx"), sheetName="modules", row.names=FALSE)
  write.xlsx(merge2[grant==g], file = paste0(dir,"qualitative_data/",country,"/", g,"_pudr_comments",".xlsx"), sheetName="cost_category", append=TRUE, row.names=FALSE)
  write.xlsx(merge3[grant==g], file = paste0(dir,"qualitative_data/",country,"/", g,"_pudr_comments",".xlsx"), sheetName="implementer", append=TRUE, row.names=FALSE)
  print(paste0(g, " pudr comments saved in output directory")) ## if the code breaks, you know which file it broke on
}

# save on box as well?
# currently copying over manually to box folder -> preped_data -> qualitative pudr data ->