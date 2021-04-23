# -----------------------------------------------
# Audrey Batzel and Francisco Rios Casas
# 4/8/2021

# Prep the coded 2S data into usable format
# Make figures showing 
  # Changes in support vs. strengthening allocations from approved budgets in NFM2 to approved budgets in NFM3 
    # ^^^ Kath said this is the focus
  # Changes in support vs. strengthening allocations from NFM3 FR to NFM3 approved
  # ^Both by country overall, and by country/module

# OTHER TO DO:
# - verify budget values in the 2S data are up to date? 
# -----------------------------------------------

# -----------------------------------------------
# set up
# -----------------------------------------------
rm(list = ls())
library(data.table)
library(ggplot2)
library(readxl)
library(stringr)

#Box filepaths - these should be used to source raw files, and to save final prepped files. 
user=as.character(Sys.info()[7])
box = paste0("C:/Users/",user,"/Box Sync/Global Fund Files/")

inFile = paste0(box, '2s_data/2S Analysis Template.xlsx')

user=as.character(Sys.info()[7])
if (Sys.info()[1]=='Windows'){
  if(user == 'abatzel'){ 
    repo_root = "C:/local/gf/"
  } else {
    repo_root = paste0("C:/Users/", user, "/Documents/gf/")} #Change to the root of your repository
  setwd(repo_root)
} else {
  setwd(paste0("/ihme/homes/",user,"/gf/"))
}
source("./resource_tracking/prep/_common/set_up_r.R", encoding="UTF-8")
source("./resource_tracking/prep/_common/load_master_list.r", encoding="UTF-8")
# -----------------------------------------------

# -----------------------------------------------
# read in data and prep one sheet at a time
# -----------------------------------------------
countries_to_run = c('DRC', 'UGA', 'GTM', 'SEN')

# sheets = c('2020(GA)', ' 2017', ' 2020') # this might not work without more specific if else statement...
#### FRC: I turned this part into an if statement and moved it into the loop below since it looks like sometimes countries share 
#### sheet names but not always

prepped_dt = data.table()

# loop through countries and sheets  to prep

for(country in countries_to_run) {
  
  if (country=='UGA') {
    sheets = c('2017(GA)', '2020(FR)', '2020(GA)')
  } else if (country=='GTM') {
    sheets = c(' 2017', ' 2020', '2020(GA)')
  } else if (country=='DRC') {
    sheets = c(' 2017', ' 2020 (update)', ' 2020(GA)') 
  } else if (country=='SEN') {
    sheets = c(' 2017', ' 2020(GA)', ' 2020(FR)')
  }
    
  prepped_dt_country = data.table()
  
  for(sheet in sheets){
    inSheet = paste0(country, sheet)
    
    # read in data
    dt = as.data.table(read_xlsx(inFile, sheet = inSheet))
    
    # prep columns for r usable format
    colnames(dt) = unlist(dt[1,])
    colnames(dt) = tolower(colnames(dt))
    colnames(dt) = gsub(pattern = ' ', replacement = '_', x = colnames(dt))
    dt = dt[-c(1),]
    
    if(inSheet %in% c('UGA2017(GA)', 'UGA2020(FR)', 'GTM 2017', 'GTM 2020', 'DRC 2017', 'SEN 2017')){
      final_coding_col = 'sensitivity_2'
    } else if(inSheet %in% c('UGA2020(GA)', 'GTM2020(GA)', 'DRC 2020 (update)', 'DRC 2020', 'SEN 2020(GA)')) {
      final_coding_col = 'finaldesignation'
    } else if (inSheet == 'SEN 2020(FR)'){
      final_coding_col = 'sensitivity_analysis_2'
    }
    
    # remove extra row for guatemala (contains summed total of data)--might not apply elsewhere
    if (inSheet%in%c("GTM2020(GA)", "GTM 2020")){
      dt = dt[!(is.na(activity_description))]
    }
    
    # keep just relevant columns and final coding designation
    if (!"country"%in%colnames(dt)) {
      dt[, loc_name := country]
    } else if ("country"%in%colnames(dt)) {
      dt[, country:=NULL]
      dt[, loc_name := country]
      }
    
    dt[, cycle := ifelse(grepl('2017', inSheet), 'NFM2', 'NFM3') ]
    
    if (country == "UGA"){
      dt[, version := ifelse(grepl('FR', inSheet), 'funding_request', 'approved_budget') ]
    } else {
      if (inSheet %in% c('GTM 2020', 'DRC 2020 (update)', 'SEN 2020(FR)')){
        dt[, version := 'funding_request']
      }else {
        dt[, version := 'approved_budget']
      }
    }

    # not sure if this will apply to other countries but need to make the NFM3 award match other data
    if (sheet %in% c('2020(GA)', ' 2020(GA)')){
      # finaldesignation applies to just the newly coded activities, so we will use the previous
      # coding (final_designation), for all activities that were already coded but are in the award budget
      if (country=='UGA') {
        dt = dt[is.na(finaldesignation), finaldesignation := final_designation]
      }else if (country=='GTM'){
        dt = dt[is.na(finaldesignation), finaldesignation := final_designation_fr]
      } #in DRC and in Senegal, the finaldesignation column is fully filled out. 
      setnames(dt, 'gf_module', 'module')
      setnames(dt, 'gf_intervention', 'intervention')
      setnames(dt, 'cost_category', 'cost_input')
      # use file name to create grant variable - we want this to be able to compare NFM2 and NFM3
      dt[, grant := lapply(file_name, function(x){paste(unlist(str_split(x, '_'))[c(1,2,3)], collapse = '_')})]
      dt[, grant := unlist(grant)]
    }
    
    if (inSheet == 'DRC 2020 (update)'){
      setnames(dt, 'activity', 'activity_description')
      dt[, grant := NA]
    }
    
    dt = dt[, c('loc_name', 'grant', 'cycle', 'grant_period', 'version', 'module', 'intervention', 'activity_description', 'cost_input', 'budget', final_coding_col), with = FALSE]
    setnames(dt, final_coding_col, 'coding_2s')
    dt[, grant := gsub(pattern = '-', '_', grant)]
    
    # save each prepped data sheet to prepped_dt 
    if(nrow(prepped_dt_country) == 0){
      prepped_dt_country = dt
    } else {
      prepped_dt_country = rbindlist(list(prepped_dt_country, dt), use.names = TRUE, fill = TRUE)
    }
  }
  # save country data individually
  saveRDS(prepped_dt_country[loc_name==country], paste0(box, '2s_data/prepped_2s_data_', country, '.rds'))
  write.csv(prepped_dt_country[loc_name==country], paste0(box, '2s_data/prepped_2s_data_', country, '.csv'), row.names = FALSE)
  if(nrow(prepped_dt) == 0){
    prepped_dt = prepped_dt_country
  } else {
    prepped_dt = rbindlist(list(prepped_dt, prepped_dt_country), use.names = TRUE, fill = TRUE)
  }
}

prepped_dt[, budget := as.numeric(budget)]
prepped_dt = prepped_dt[budget != 0, ]

# # check/compare budget values to tableau to ensure accuracy
# check = prepped_dt[loc_name == 'UGA' & cycle == 'NFM3', .(budget = sum(budget)), by=.(module, intervention, cycle, version)]
# check = dcast.data.table(check, module + intervention + cycle ~ version, value.var = 'budget')
# -----------------------------------------------


###############################################
# clean raw module names for making figures
###############################################
# read in the data that was just prepped---won't be saved after this...
# raw_data <- readRDS(pase0(prepped_dt, paste0(box, '2s_data/prepped_2s_data_all_countries.rds')))

initial_rows = nrow(prepped_dt) # save to run a check later

raw_data <- prepped_dt

# read NFM2 and NFM3 module maps but only for RSSH
nfm2_mf_map <- readRDS("J:\\Project\\Evaluation\\GF\\resource_tracking\\modular_framework_mapping\\gf_mapping.rds")
nfm2_mf_map$cycle <- "NFM2"
nfm3_mf_map <- readRDS("J:\\Project\\Evaluation\\GF\\resource_tracking\\modular_framework_mapping\\gf_mapping_nfm3.rds")
nfm3_mf_map$cycle <- "NFM3"

module_map <- rbind(nfm2_mf_map, nfm3_mf_map, fill=TRUE)

module_map <- module_map[disease=="rssh"]
module_map <- module_map[,.(module, intervention, 
                            gf_module, gf_intervention, 
                            gf_module_fr, gf_intervention_fr,
                            gf_module_esp, gf_intervention_esp)]

module_map <- module_map[!duplicated(module_map[,c('module','intervention')]),]

# source shared functions
source('./resource_tracking/prep/_common/shared_functions.r', encoding="UTF-8")

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
# raw_data = correct_modules_interventions(raw_data)

# Check for unmapped modules/interventions before mapping
gf_concat <- paste0(module_map$module, module_map$intervention)
rt_concat <- paste0(raw_data$module, raw_data$intervention)
unmapped_mods <- raw_data[!rt_concat%in%gf_concat]

if(nrow(unmapped_mods)>0){
  print(unique(unmapped_mods[, c("module", "intervention"), with= FALSE]))
  print(unique(unmapped_mods$fileName)) #For documentation in the comments above. 
  stop("You have unmapped original modules/interventions!")
}

mergeVars = c('module', 'intervention')
#module_map = unique(module_map)
# module_map = module_map[!is.na(code)]

mapped_data <- merge(raw_data, module_map, by=mergeVars, all.x = TRUE, allow.cartesian = FALSE)
dropped_mods <- mapped_data[is.na(mapped_data$gf_module), ]

if(nrow(dropped_mods) >0){
  # Check if anything is dropped in the merge -> if you get an error. Check the mapping spreadsheet
  print(unique(dropped_mods[, c("module", "intervention"), with= FALSE]))
  stop("Modules/interventions were dropped!")
}

# subset mapped_data to what we will use
keep <- c('loc_name', 'grant', 'grant_period', 'cycle', 'version', 
          'gf_module', 'gf_intervention', 'activity_description', 'cost_input', 
          'budget', 'coding_2s',
          'gf_module_fr', 'gf_intervention_fr', 
          'gf_module_esp', 'gf_intervention_esp')

prepped_dt <- mapped_data[,..keep]


if(!nrow(mapped_data)==initial_rows){
  # Check if anything is duplicated in the merge -> if you get an error. Check the module map
    stop("Modules/interventions were duplicated or dropped!")
}

# rename the modules to match the plot data
setnames(prepped_dt, 
         c("gf_module", "gf_intervention"),
         c("module", "intervention"))

# save full data
saveRDS(prepped_dt, paste0(box, '2s_data/prepped_2s_data_all_countries.rds'))
write.csv(prepped_dt, paste0(box, '2s_data/prepped_2s_data_all_countries.csv'), row.names = FALSE)
# -----------------------------------------------
#################################################
# -----------------------------------------------

# # -----------------------------------------------
# # Save/check UGA data for David to use
# # -----------------------------------------------
# prepped_dt = as.data.table(read.csv(paste0(box, '2s_data/prepped_2s_data_all_countries.csv')))
# dt_uga = prepped_dt[loc_name == 'UGA']
# nrow(dt_uga) == nrow(unique(dt_uga[,.(cycle, version, module, intervention, activity_description, cost_input)]))
# dt_uga = dt_uga[,.(grant, cycle, version, module, intervention, activity_description, cost_input, budget, coding_2s)]
# setorderv(dt_uga, c('module', 'intervention', 'activity_description' ))
# write.csv(dt_uga, paste0(box, '2s_data/UGA_2sata_activityLevel.csv'), row.names = FALSE)
# # -----------------------------------------------