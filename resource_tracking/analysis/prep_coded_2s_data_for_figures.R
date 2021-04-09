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
# -----------------------------------------------

# -----------------------------------------------
# read in data and prep one sheet at a time
# -----------------------------------------------
countries_to_run = c('UGA', 'GTM')

# sheets = c('2020(GA)', ' 2017', ' 2020') # this might not work without more specific if else statement...
#### FRC: I turned this part into an if statement and moved it into the loop below since it looks like sometimes countries share 
#### sheet names but not always

prepped_dt = data.table()
prepped_dt_country = data.table()

# loop through countries and sheets  to prep

for(country in countries_to_run) {
  
  if (country=='UGA') {
    sheets = c('2017(GA)', '2020(FR)', '2020(GA)')
  } else if (country=='GTM') {
    sheets = c(' 2017', ' 2020', '2020(GA)')
  }
  
  for(sheet in sheets){
    inSheet = paste0(country, sheet)
    
    # read in data
    dt = as.data.table(read_xlsx(inFile, sheet = inSheet))
    
    # prep columns for r usable format
    colnames(dt) = unlist(dt[1,])
    colnames(dt) = tolower(colnames(dt))
    colnames(dt) = gsub(pattern = ' ', replacement = '_', x = colnames(dt))
    dt = dt[-c(1),]
    
    if(inSheet %in% c('UGA2017(GA)', 'UGA2020(FR)', 'GTM 2017', 'GTM 2020')){
      final_coding_col = 'sensitivity_2'
    } else if(inSheet %in% c('UGA2020(GA)', 'GTM2020(GA)')) {
      final_coding_col = 'finaldesignation'
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
        dt[, version := ifelse(grepl('GA', inSheet), 'approved_budget', 'funding_request') ]}
    
    # not sure if this will apply to other countries but need to make the NFM3 award match other data
    if (sheet == '2020(GA)'){
      # finaldesignation applies to just the newly coded activities, so we will use the previous
      # coding (final_designation), for all activities that were already coded but are in the award budget
      if (country=='UGA') {
        dt = dt[is.na(finaldesignation), finaldesignation := final_designation]
      }else if (country=='GTM'){
        dt = dt[is.na(finaldesignation), finaldesignation := final_designation_fr]
      }
      setnames(dt, 'gf_module', 'module')
      setnames(dt, 'gf_intervention', 'intervention')
      setnames(dt, 'cost_category', 'cost_input')
      # use file name to create grant variable - we want this to be able to compare NFM2 and NFM3
      dt[, grant := lapply(file_name, function(x){paste(unlist(str_split(x, '_'))[c(1,2,3)], collapse = '_')})]
      dt[, grant := unlist(grant)]
    }
    
    dt = dt[, c('loc_name', 'grant', 'cycle', 'grant_period', 'version', 'module', 'intervention', 'activity_description', 'cost_input', 'budget', final_coding_col), with = FALSE]
    
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

# save full data
saveRDS(prepped_dt, paste0(box, '2s_data/prepped_2s_data_all_countries.rds'))
write.csv(prepped_dt, paste0(box, '2s_data/prepped_2s_data_all_countries.csv'), row.names = FALSE)
# -----------------------------------------------

# -----------------------------------------------
# make figures/save by country
# -----------------------------------------------
# Compare NFM2 Award to NFM3 Award overall, by grant, by grant/module, by module


# Compare NFM3 FR to NFM3 Award overall, by module, (by intervention?)

# -----------------------------------------------





