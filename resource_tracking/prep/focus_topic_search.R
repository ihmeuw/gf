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


# Function to read in detailed budgets and search through activities for keywords, could be saved in common_functions folder later on
id_focus_topics <- function(country, topic_area) {
  
  # #example - can uncomment two lines below to troubleshoot/test
  # country <- 'Senegal'
  # topic_area <- 'diagnostic'
  
  # step 0: make sure inputs are currect
  # if () stop("Error: country must be either 'sen', 'uga', 'gtm', or 'cod'") or (Senegal, Uganda, Guatemala, DRC)
  # if (class(inFile)!='character') stop('Error: inFile argument must be a string!')
  # if (class(year)=='character') stop('Error: year argument must be a number!')
  # -----------------------------------------------------------------------------
  
  # step 1: read in data
  data <- fread(paste0(box, "tableau_data/all_budget_revisions_activityLevel.csv")) #make sure this can access the correct data source
  data <- data[loc_name==country]
  
  # step 2: read in keywords for each focus topic
  key_words_file <- fread(paste0(mapping_dir, "focus_topic_keyword_search_log.csv"))
  key_words <- key_words_file[loc_name==country & focus_topic==topic_area, keyword] ##these are the key words we settle on for a certain topic area in a given country.
  
  # step 3: loop through specific columns to use to search for hits
  hits <- Reduce(`|`, lapply(key_words, function(x) grepl(x, tolower(data$activity_description)))) ## this produces a vector of TRUE/FALSE for every row in ‘data’ where a key word was identified in character column of activity descriptions, the `|` function makes sure that each key word is looked for indepently (as opposed to `&`, where it looks for the unity)
  data[hits,paste0("ta_", topic_area):=1]
  data[!hits,paste0("ta_", topic_area):=0]
  
  # which module/intervention pairs were identified that are also not currently being hand coded?
  # insert code here
  ta_indic <- as.name(paste0("ta_", topic_area))
  tabletest <- data[isTopicArea==FALSE & budget_version=="approved" & eval(ta_indic)==1] # need to change name of variable in subsetting dynamically
  
  # create table to visualize
  # print((unique(tabletest[,.(gf_module, gf_intervention, activity_description, budget_version)])))
  
  # insert print statement
  print(paste0("There were ",length(unique(tabletest$gf_intervention)),  " additional interventions in final approved budgets marked as focus topics through keyword search." ))
  
  # save data in new folder to visualize
  # save data on box or prepped data folder in J?
}

 id_focus_topics("Senegal", "diagnostic")
