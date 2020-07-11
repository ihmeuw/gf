# -----------------------------------------------
# AUTHOR: Francisco Rios Casas
# PURPOSE: Identify budget items that relate to country focus topics
# DATE: June 12, 2020

# The current working directory should be the root of this repository

# pending issues
# add option to specify budget_version?
# -----------------------------------------------

# -----------------------------------------------
# STEP 1: SET UP R
# -----------------------------------------------
user=as.character(Sys.info()[7])
if (Sys.info()[1]=='Windows'){
  setwd(paste0("C:/Users/",user,"/Documents/gf/")) #Change to the root of your repository
} else {
  setwd(paste0("/ihme/homes/",user,"/gf/"))
}

# source files with other functions and common resource tracking filepaths
source("./resource_tracking/prep/_common/set_up_r.R", encoding="UTF-8")
# -----------------------------------------------

# -----------------------------------------------
# Download new version of keyword search log from google drive
# -----------------------------------------------
# library(googledrive)
# keywordsearchlogfile <- drive_get(as_id("1TOE7EYnHozN5oNNrkILuiGWvFc-yQyGvLZg1JJFjb5o")) # ID of file on drive
# local_file = paste0(mapping_dir, "keyword_search/focus_topic_keyword_search_log.csv") # where to save copy of file locally
# drive_download(file=keywordsearchlogfile,
#                path=local_file,
#                overwrite = TRUE)
# -----------------------------------------------

# -----------------------------------------------
# FUNCTION
# -----------------------------------------------
# Function to read in detailed budgets and search through activities for keywords, could be saved in common_functions folder later on
id_focus_topics <- function(country, include_module_intervention = FALSE) {
  
  # # example - can uncomment the lines below to troubleshoot/test
  # country <- 'Senegal'
  # include_module_intervention = FALSE
  
  # step 0: make sure inputs are currect
  # if () stop("Error: country must be either 'sen', 'uga', 'gtm', or 'cod'") or (Senegal, Uganda, Guatemala, DRC)
  # if (class(inFile)!='character') stop('Error: inFile argument must be a string!')
  # if (class(year)=='character') stop('Error: year argument must be a number!')
  # -----------------------------------------------------------------------------
  outFile = paste0(dir, "/modular_framework_mapping/keyword_search/test_", tolower(country), "_focus_topic_search_", 
                   month(Sys.Date()), "_", day(Sys.Date()), "_", year(Sys.Date()), ".csv")
  
  # step 1: read in data at activity level
  data <- as.data.table(read.csv(paste0(dir, 'modular_framework_mapping/PCE2020_FocusTopicAreas.csv'))) #make sure this can access the correct data source
  data <- data[loc_name==country]
  # just resetting these because I had manually entered some but I want it to be set here instead:
  data[, topicAreaDesc := '']
  data[, keyword_topic_area := FALSE]
  
  # step 2: read in keywords for each focus topic
  key_words_file <- fread(paste0(mapping_dir, "keyword_search/focus_topic_keyword_search_log.csv"))
  topic_areas = key_words_file[loc_name==country, unique(focus_topic)]
  
  # use activity description or the combination of module/intervention/activity to search for key words
  if (include_module_intervention == TRUE){
    data[, search_column := paste(gf_module, gf_intervention, activity_description)]
  } else {
    data[, search_column := activity_description]
  }
  
  data[,search_column := tolower(search_column)]
  data[,search_column := trimws(search_column)]
  
  for(t in topic_areas){
    # these are the key words we settle on for a certain topic area in a given country.
    key_words <- key_words_file[loc_name==country & focus_topic==t, keyword]
    key_words = tolower(key_words)
    key_words = trimws(key_words)
    
    # step 3: loop through specific columns to use to search for hits
    # this produces a vector of TRUE/FALSE for every row in data where a key word was identified in character column of activity descriptions, 
    # the `|` function makes sure that each key word is looked for indepently (as opposed to `&`, where it looks for the unity)
    hits <- Reduce(`|`, lapply(key_words, function(x) grepl(x, data$search_column))) 
    data[hits, keyword_topic_area := TRUE]
    data[hits, topicAreaDesc := paste(topicAreaDesc, toupper(t))] # to identify if both topic areas get id'ed by the keywords
  }
  
  # remove search column and clean up results
  data[, search_column := NULL]
  data[, X := NULL]
  data[, topicAreaDesc := trimws(topicAreaDesc)]
  
  # save data in new folder - save a separate file for now on J
  write.csv(data, outFile, row.names = FALSE)
  print(paste0("Data saved on J drive: ", outFile))
  
  # Step 4: check out the results compare to what the CEPs have ID'ed previously
  # which module/intervention pairs were identified that are also not currently being hand coded?
  new_ids = data[cep_topic_area == FALSE & keyword_topic_area == TRUE,]
  
  # insert print statement
  print(paste0("There were ", length(unique(new_ids$activity_description)),  " additional activities identified as focus topics through keyword search." ))

  # Are any of the previously ID'ed activities now NOT ID'ed? 
  missing_ids = data[cep_topic_area == TRUE & keyword_topic_area == FALSE,]
  
  # insert print statements
  print(paste0("There were ", length(unique(missing_ids$activity_description)),  " activities missing from what were originally ID'ed by CEPs." ))
  print(paste0("There were ", length(unique(new_ids$gf_intervention)), " additional interventions identified as focus topics through keyword search."))
  print(paste0("There were ", length(unique(missing_ids$gf_intervention)), " interventions missing from what were originally ID'ed by CEPs."))
}
# -----------------------------------------------

# -----------------------------------------------
# TEST on country data:
# -----------------------------------------------
id_focus_topics("Senegal", include_module_intervention = TRUE)
# for guatemala only run certain keywords on the HIV grant

id_focus_topics('Guatemala', include_module_intervention = TRUE)

id_focus_topics('Uganda', include_module_intervention = TRUE)
id_focus_topics('DRC', include_module_intervention = TRUE)
# -----------------------------------------------

# -----------------------------------------------
# merge budget data to all country data:
# -----------------------------------------------
drc = as.data.table(read.csv(paste0(dir, 'modular_framework_mapping/keyword_search/DRC/test_drc_focus_topic_search_7_8_2020.csv')))
uga = as.data.table(read.csv(paste0(dir, 'modular_framework_mapping/keyword_search/UGA/test_uganda_focus_topic_search.csv')))
gtm = as.data.table(read.csv(paste0(dir, 'modular_framework_mapping/keyword_search/test_guatemala_focus_topic_search.csv')))
sen = as.data.table(read.csv(paste0(dir, 'modular_framework_mapping/keyword_search/test_senegal_focus_topic_search.csv')))

dt = rbindlist(list(drc,uga,gtm,sen), use.names = TRUE, fill = TRUE)

budget_rev = as.data.table(read.csv(paste0(box, 'tableau_data/all_budget_revisions_activityLevel.csv')))
budget_rev = budget_rev[, -c('isTopicArea', 'topicAreaDesc')]
budget_rev = budget_rev[ file_iteration %in% c('revision', 'approved_gm')]

budget_dt = merge(dt, budget_rev, all.y = TRUE, by = c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description"))

# # check that there's one file per budget version/grant
# budget_dt[, length(unique(file_name)), by = c('grant', 'budget_version')]

dt = budget_dt[, c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description", 'cep_topic_area', 'keyword_topic_area', 'topicAreaDesc', 'grant', 'budget_version', 'budget')]
dt = dt[!is.na(budget_version), ]

# sum over cost categories 
by_cols = names(dt)[!names(dt) %in% 'budget']
dt = dt[, .(activity_budget = sum(budget)), by = by_cols]
dt[, intervention_budget := sum(activity_budget), by = .(loc_name, gf_module, gf_intervention, grant, budget_version)]
dt[, activity_percent_of_intervention := round((activity_budget/intervention_budget)*100, 2)]

dt_act_number = dt[, .(loc_name, disease, gf_module, gf_intervention, activity_description, cep_topic_area, keyword_topic_area,
                       topicAreaDesc, grant, budget_version, activity_budget)]
dt_act_percent = dt[, .(loc_name, disease, gf_module, gf_intervention, activity_description, cep_topic_area, keyword_topic_area,
                        topicAreaDesc, grant, budget_version, activity_percent_of_intervention)]

dt_wide_number = dcast.data.table(dt_act_number, loc_name + disease + gf_module + gf_intervention + activity_description + cep_topic_area + keyword_topic_area + topicAreaDesc + grant ~ budget_version )
dt_wide_percent = dcast.data.table(dt_act_percent, loc_name + disease + gf_module + gf_intervention + activity_description + cep_topic_area + keyword_topic_area + topicAreaDesc + grant ~ budget_version )

write.csv(dt_wide_number, paste0(dir, 'modular_framework_mapping/keyword_search/combined_keyword_search_results_activity_budgets.csv'), row.names = FALSE)
write.csv(dt_wide_percent, paste0(dir, 'modular_framework_mapping/keyword_search/combined_keyword_search_results_activity_percent_of_intervention.csv'), row.names = FALSE)
# -----------------------------------------------

# -----------------------------------------------
# merge budget data to DRC data:
# -----------------------------------------------
# read in the budget revisions data set
dt = as.data.table(read.csv(paste0(dir, 'modular_framework_mapping/keyword_search/DRC/test_drc_focus_topic_search_7_8_2020.csv')))
dt = dt[, -c('isTopicArea')]
# read in the budger revisions dataset and subset to just DRC
budget_rev = as.data.table(read.csv(paste0(box, 'tableau_data/all_budget_revisions_activityLevel.csv')))
budget_rev = budget_rev[loc_name == 'DRC', ]
budget_rev = budget_rev[, -c('isTopicArea', 'topicAreaDesc')]

# merge the data sets
ta_budget_dt = merge(dt, budget_rev, all = TRUE, by = c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description"))
dt = ta_budget_dt[, c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description", 'cep_topic_area', 'keyword_topic_area', 'topicAreaDesc', 'grant', 'budget_version', 'budget')]
dt = dt[!is.na(budget_version), ]

# sum over cost categories 
by_cols = names(dt)[!names(dt) %in% 'budget']
dt = dt[, .(activity_budget = sum(budget)), by = by_cols]
dt[, intervention_budget := sum(activity_budget), by = .(loc_name, gf_module, gf_intervention, grant, budget_version)]
dt[, activity_percent_of_intervention := round((activity_budget/intervention_budget)*100, 2)]

dt_act_number = dt[, .(loc_name, disease, gf_module, gf_intervention, activity_description, cep_topic_area, keyword_topic_area,
                       topicAreaDesc, grant, budget_version, activity_budget)]
dt_act_percent = dt[, .(loc_name, disease, gf_module, gf_intervention, activity_description, cep_topic_area, keyword_topic_area,
                        topicAreaDesc, grant, budget_version, activity_percent_of_intervention)]

dt_wide_number = dcast.data.table(dt_act_number, loc_name + disease + gf_module + gf_intervention + activity_description + cep_topic_area + keyword_topic_area + topicAreaDesc + grant ~ budget_version )
dt_wide_percent = dcast.data.table(dt_act_percent, loc_name + disease + gf_module + gf_intervention + activity_description + cep_topic_area + keyword_topic_area + topicAreaDesc + grant ~ budget_version )

subset_for_writeup_number = dt_wide_number[keyword_topic_area==TRUE,]
write.csv(subset_for_writeup_number, paste0(dir, 'modular_framework_mapping/keyword_search/DRC/allocations_for_activities_identified_by_keywordsearch.csv'), row.names = FALSE)

subset_for_writeup_percent = dt_wide_percent[keyword_topic_area==TRUE,]
write.csv(subset_for_writeup_percent, paste0(dir, 'modular_framework_mapping/keyword_search/DRC/activity_percent_of_intervention.csv'), row.names = FALSE)
# -----------------------------------------------

# -----------------------------------------------
# Compare key word search versions: 
## MAKE THIS A FUNCTION TO COMPARE ANY TWO? 
# -----------------------------------------------
# read in both key word searches
dt1 = as.data.table(read.csv(paste0(dir, 'modular_framework_mapping/keyword_search/DRC/test_drc_focus_topic_search_6_17_2020.csv')))
dt2 = as.data.table(read.csv(paste0(dir, 'modular_framework_mapping/keyword_search/DRC/test_drc_focus_topic_search_7_1_2020.csv')))
dt1 = dt1[, -c('isTopicArea', 'topicAreaDesc')]
dt2 = dt2[, -c('isTopicArea', 'topicAreaDesc')]

# compare the differences: 
setnames(dt1, 'keyword_topic_area', 'keyword_ta_dt1')
setnames(dt2, 'keyword_topic_area', 'keyword_ta_dt2')

dt= merge(dt1, dt2, by = names(dt1)[!names(dt1)%in%c('keyword_ta_dt1')])
now_false = dt[ keyword_ta_dt1 == TRUE & keyword_ta_dt2 == FALSE, ]
now_true = dt[ keyword_ta_dt1 == FALSE & keyword_ta_dt2 == TRUE, ]
# -----------------------------------------------

# -----------------------------------------------
# # merging budget data and troubleshooting:
# -----------------------------------------------
# dt = read_xlsx(paste0(dir, 'modular_framework_mapping/keyword_search/uganda_keyword_search_focus_topic_areas.xlsx'))
# dt = as.data.table(dt)
# nrow(unique(dt[,c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description")]))
# dt = unique(dt[,c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description")])
# setorderv(dt, c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description"))
# dt[, id := .I]
# 
# budget_rev = read.csv(paste0(box, 'tableau_data/all_budget_revisions_activityLevel.csv'))
# budget_rev = as.data.table(budget_rev)
# budget_rev = budget_rev[loc_name == 'Uganda', ]
# budget_rev = budget_rev[, -c('isTopicArea', 'topicAreaDesc')]
# budget_rev = unique(budget_rev[,c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description")])
# budget_rev = budget_rev[, lapply(.SD, as.character), .SDcols = names(budget_rev)]
# budget_rev = budget_rev[, lapply(.SD, trimws), .SDcols = names(budget_rev)]
# budget_rev = budget_rev[, activity_description:=str_replace_all(x, "[\r\n]" , "")]
# 
# setorderv(budget_rev, c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description"))
# budget_rev[, id := .I]
# 
# check = merge(dt, budget_rev, all = TRUE, by = c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description"))
# 
# 
# budget_rev = budget_rev[grant_period == '2018-2020' & file_iteration %in% c('approved_gm', 'revision'),]
# budget_rev = budget_rev[, .(budget = sum(budget)), by = c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description", "grant", "budget_version")]
# 
# budget_rev = dcast.data.table(budget_rev, loc_name + disease + gf_module + gf_intervention + activity_description + grant ~ budget_version)
# -----------------------------------------------
