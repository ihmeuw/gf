# ----------------------------------------------
# AUTHOR: Audrey Batzel and Francisco Rios Casas based on code by Emily Linebarger and Irena Chen
# PURPOSE: Prep commonly-formatted FR budgets using the prep_fr_budgets() function - runs all countries at once
# DATE: Last updated June 2020.

# Update April 2021: This file helps extract relevant data on Key Populations in HIV Funding Requests

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
file_list = file_list[data_source %in% c('funding_request', 'budget')] 
file_list = file_list[grant_period=="2021-2023"]
file_list = file_list[loc_name %in% c("sen", "gtm")]
file_list = file_list[file_iteration=="approved_gm"] # here, 'approved_gm' is a misnomer, but we used it to fit in with the naming schema that had already been used for the file list
file_list = file_list[disease=="hiv"]
# ----------------------------------------------

# ----------------------------------------------
# 1. Loop through budget files to extract data
# ----------------------------------------------
for(i in 1:nrow(file_list)){
  # Set up file path 
  if (file_list$data_source[i]=="funding_request") {
    folder = "funding_requests"
  } else if (file_list$data_source[i] == "budget" ) {
    folder = file_list$grant[i]
  }
  
  if (file_list[i, year(version_date) <= "2019"]){
    folder_cont = "fr_budgets_2017"
  } else if (file_list[i, (year(version_date) >= "2019" & year(version_date) <= '2021')]){
    folder_cont= "fr_budgets_2020"
  } else {
    stop('Something went wrong - version date is missing or incorrect')
  }
  
  grant_period = file_list$grant_period[i]
  
  if (file_list$data_source[i]=="funding_request") {
    file_dir = paste0(box, toupper(file_list$loc_name[i]), '/raw_data/', file_list$grant_status[i], "/", 
                      folder, '/', folder_cont, '/')
  } else if (file_list$data_source[i]=="budget" ){
    file_dir = paste0(box, toupper(file_list$loc_name[i]), '/raw_data/', file_list$grant_status[i], "/", 
                      folder, '/', grant_period, '/budgets/')
  }
  
  args = list(file_dir, file_list$file_name[i], "Population", file_list$start_date_financial[i], 
              file_list$qtr_number_financial[i], file_list$language_financial[i])
  
  ### RUN THE PREP FUNCTION HERE ###
  tmpData = do.call(prep_target_populations, args)
  
  ###
  
  #Add indexing data
  append_cols = file_list[i, .(loc_name, disease, file_name, data_source, grant_period, grant_status, 
                               file_currency, file_iteration, budget_version, version_date, 
                               function_financial, start_date_financial, language_financial, 
                               period_financial, qtr_number_financial,
                               mod_framework_format, update_date)]
  
  stopifnot(nrow(append_cols)==1)
  
  tmpData = cbind(tmpData, append_cols)
  
  tmpData[, year := year(start_date_financial)]
  tmpData[, file_start_date:=min(start_date_financial), by='file_name']
  
  #Bind data together 
  if(i==1){
    prepped_kvp = tmpData
  } else {
    prepped_kvp = rbind(prepped_kvp, tmpData, use.names=TRUE, fill = TRUE)    
  }
  
  print(paste0(i, " ", file_list$data_source[i], " ", file_list$function_financial[i], " ", file_list$file_name[i])) ## if the code breaks, you know which file it broke on
}


# ----------------------------------------------
# save raw output
# ----------------------------------------------
write.csv(prepped_kvp, paste0(box, 'tableau_data/raw_extracted_hiv_population_data.csv'), row.names = FALSE)
# ----------------------------------------------

### for now only
# prepped_kvp <- as.data.table(read.csv(paste0(box, 'tableau_data/raw_extracted_hiv_population_data.csv')))

# ----------------------------------------------
# 2. Run some checks to make sure this data was prepped correctly. 
# ----------------------------------------------
#Make sure all budget data pulled is actually numeric- this is an easy check to see if prep functions are working correctly. 
verify_numeric_budget = prepped_kvp[, .(budget=gsub("[[:digit:]]", "", budget))]
verify_numeric_budget = verify_numeric_budget[, .(budget=gsub("[[:punct:]]", "", budget))]
verify_numeric_budget = verify_numeric_budget[!is.na(budget) & budget != ""]
stopifnot(nrow(verify_numeric_budget)==0)

#Make sure you have all the files here that you started with in your filelist. 
rt_files <- unique(prepped_kvp$file_name)
warning1 = (length(unique(file_list$file_name)) == length(rt_files))
if (!warning1){
  warning("The length of the original file list is not the same as the number of processed files.")
}
warning2 = sort(rt_files) == sort(unique(file_list$file_name))
if (all(warning2)!=TRUE){
  warning("The files in the processed data are not the same as the files in the file list.")
}

#
# Remove rows were budget is equal to zero
#
prepped_kvp <- prepped_kvp[!budget==0]

# ----------------------------------------------
# 3. Map prepped files to final mappings
# ----------------------------------------------
initial_rows = nrow(prepped_kvp) # save to run a check later

raw_data <- prepped_kvp

# remap modules any time there are new modules/interventions added to dataset
# include_stops = TRUE
source(paste0(code_dir, "hiv_population_module_map.R"))

module_map <- readRDS(paste0(local_dir,"gf_hivpop_mapping_nfm3.rds"))

# module_map <- module_map[disease=="hiv"]
# keep only the relevant columns
# module_map <- module_map[,.(module, intervention, 
#                             gf_module, gf_intervention, 
#                             gf_module_fr, gf_intervention_fr,
#                             gf_module_esp, gf_intervention_esp)]

#Remove whitespaces, punctuation, and unwanted characters from module and intervention. 
raw_data = strip_chars(raw_data)
raw_data = strip_chars_pop(raw_data)

# # # remove underscore from population column
# raw_data$population = gsub("_", "", raw_data$orig_population)

#Correct common acronyms in the resource database and the module map. 
raw_data[, module:=replace_acronyms(module)]
raw_data[, intervention:=replace_acronyms(intervention)]
raw_data[, population:=replace_acronyms(population)]

module_map[, module:=replace_acronyms(module)]
module_map[, intervention:=replace_acronyms(intervention)]
module_map[, population:=replace_acronyms(population)]

#Make some raw corrections here - These weren't accurate enough to put in the map, but we still need to account for them. 
if (!'activity_description'%in%names(raw_data)){ #If this column doesn't exist, add it as 'NA' so the code below can run
  raw_data[, activity_description:=NA]
}

# Check for unmapped modules/interventions before mapping
gf_concat <- paste0(module_map$module, module_map$intervention, module_map$population)
rt_concat <- paste0(raw_data$module, raw_data$intervention, raw_data$population)
unmapped_mods <- raw_data[!rt_concat%in%gf_concat]

if(nrow(unmapped_mods)>0){
  print(unique(unmapped_mods[, c("module", "intervention", "population"), with= FALSE]))
  print(unique(unmapped_mods$file_name)) #For documentation in the comments above. 
  stop("You have unmapped original modules/interventions/populations!")
}

mergeVars = c('module', 'intervention', 'population')
mapped_data <- merge(raw_data, module_map, by=mergeVars, all.x = TRUE, allow.cartesian = FALSE)
dropped_mods <- mapped_data[is.na(mapped_data$gf_module), ]

if(nrow(dropped_mods) >0){
  # Check if anything is dropped in the merge -> if you get an error. Check the mapping spreadsheet
  print(unique(dropped_mods[, c("module", "intervention", "population"), with= FALSE]))
  stop("Modules/interventions were dropped!")
}

keep <- c('loc_name', 'budget_year', 'gf_module', 'gf_intervention', 'gf_population', 
          'budget', 'file_name', 'data_source', 'grant_period', 
          'file_currency', 'file_iteration', 'budget_version',
          'gf_module_fr', 'gf_intervention_fr', 'gf_population_fr',
          'gf_module_esp', 'gf_intervention_esp', 'gf_population_esp', 
          'orig_module', 'orig_intervention', 'orig_population')

final_prepped_dt <- mapped_data[,..keep]


# ----------------------------------------------
# 4. Save data:
# ----------------------------------------------
# save data
write.csv(final_prepped_dt, paste0(box, "tableau_data/prepped_hiv_kvp_budgets_all.csv"), row.names=FALSE)
