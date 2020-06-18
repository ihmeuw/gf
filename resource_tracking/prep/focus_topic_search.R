# AUTHOR: Francisco Rios Casas
# PURPOSE: Identify budget items that relate to country focus topics
# DATE: June 12, 2020

# The current working directory should be the root of this repository


# pending issues
# add option to specify budget_version?

# ----------------------------------------------
# STEP 1: SET UP R
# ----------------------------------------------
user=as.character(Sys.info()[7])
if (Sys.info()[1]=='Windows'){
  setwd(paste0("C:/Users/",user,"/Documents/gf/")) #Change to the root of your repository
} else {
  setwd(paste0("/ihme/homes/",user,"/gf/"))
}

# source files with other functions and common resource tracking filepaths
source("./resource_tracking/prep/_common/set_up_r.R", encoding="UTF-8")
# source("./resource_tracking/prep/_common/load_master_list.r", encoding="UTF-8") # not sure if we will need this one

# -----------------------------------------------
# FUNCTION
# -----------------------------------------------

# Function to read in detailed budgets and search through activities for keywords, could be saved in common_functions folder later on
id_focus_topics <- function(country, include_module_intervention = FALSE) {
  
  # # example - can uncomment the lines below to troubleshoot/test
  country <- 'Guatemala'
  include_module_intervention = FALSE
  
  # step 0: make sure inputs are currect
  # if () stop("Error: country must be either 'sen', 'uga', 'gtm', or 'cod'") or (Senegal, Uganda, Guatemala, DRC)
  # if (class(inFile)!='character') stop('Error: inFile argument must be a string!')
  # if (class(year)=='character') stop('Error: year argument must be a number!')
  # -----------------------------------------------------------------------------
  
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
  
  for(t in topic_areas){
    # these are the key words we settle on for a certain topic area in a given country.
    key_words <- key_words_file[loc_name==country & focus_topic==t, keyword]
    
    # step 3: loop through specific columns to use to search for hits
    # this produces a vector of TRUE/FALSE for every row in data where a key word was identified in character column of activity descriptions, 
    # the `|` function makes sure that each key word is looked for indepently (as opposed to `&`, where it looks for the unity)
    hits <- Reduce(`|`, lapply(key_words, function(x) grepl(x, tolower(data$search_column)))) 
    data[hits, keyword_topic_area := TRUE]
    data[hits, topicAreaDesc := paste(topicAreaDesc, toupper(t))] # to identify if both topic areas get id'ed by the keywords
  }
  
  # remove search column and clean up results
  data[, search_column := NULL]
  data[, X := NULL]
  data[, topicAreaDesc := trimws(topicAreaDesc)]
  
  # save data in new folder - save a separate file for now on J
  write.csv(data, file = paste0(dir, "/modular_framework_mapping/keyword_search/test_", tolower(country), "_focus_topic_search.csv"), row.names = FALSE)
  print("Data saved on J drive")
  
  # Step 4: check out the results compare to what the CEPs have ID'ed previously
  # which module/intervention pairs were identified that are also not currently being hand coded?
  new_ids = data[cep_topic_area == FALSE & keyword_topic_area == TRUE,]
    
  # create table to visualize
  # print(unique(new_ids[,.(gf_module, gf_intervention, activity_description)]))
  
  # insert print statement
  print(paste0("There were ", length(unique(new_ids$activity_description)),  " additional activities identified as focus topics through keyword search." ))

  # Are any of the previously ID'ed activities now NOT ID'ed? 
  missing_ids = data[cep_topic_area == TRUE & keyword_topic_area == FALSE,]
  
  # curious how many different interventions those activities are found in...
  # create table to visualize
  # print(unique(missing_ids[,.(gf_module, gf_intervention, activity_description)]))
  
  # insert print statement
  print(paste0("There were ", length(unique(missing_ids$activity_description)),  " activities missing from what were originally ID'ed by CEPs." ))
}

# -----
# TEST USING SENEGAL DATA
# ----

id_focus_topics("Senegal")
id_focus_topics('Guatemala', include_module_intervention = FALSE)

id_focus_topics('Uganda', include_module_intervention = TRUE)


dt = read_xlsx(paste0(dir, 'modular_framework_mapping/keyword_search/uganda_keyword_search_focus_topic_areas.xlsx'))
dt = as.data.table(dt)
nrow(unique(dt[,c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description")]))
dt = unique(dt[,c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description")])
setorderv(dt, c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description"))
dt[, id := .I]

budget_rev = read.csv(paste0(box, 'tableau_data/all_budget_revisions_activityLevel.csv'))
budget_rev = as.data.table(budget_rev)
budget_rev = budget_rev[loc_name == 'Uganda', ]
budget_rev = budget_rev[, -c('isTopicArea', 'topicAreaDesc')]
budget_rev = unique(budget_rev[,c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description")])
budget_rev = budget_rev[, lapply(.SD, as.character), .SDcols = names(budget_rev)]
budget_rev = budget_rev[, lapply(.SD, trimws), .SDcols = names(budget_rev)]
budget_rev = budget_rev[, activity_description:=str_replace_all(x, "[\r\n]" , "")

setorderv(budget_rev, c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description"))
budget_rev[, id := .I]

check = merge(dt, budget_rev, all = TRUE, by = c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description"))


budget_rev = budget_rev[grant_period == '2018-2020' & file_iteration %in% c('approved_gm', 'revision'),]
budget_rev = budget_rev[, .(budget = sum(budget)), by = c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description", "grant", "budget_version")]

budget_rev = dcast.data.table(budget_rev, loc_name + disease + gf_module + gf_intervention + activity_description + grant ~ budget_version)

