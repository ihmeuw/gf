#----------------------------------------------------------
# AUTHOR: Emily Linebarger 
# DATE: May 3, 2019
# PURPOSE: Prep Uganda budgets containing catalytic funding for 
#   2018-2020 grant cycle. 
#----------------------------------------------------------

#----------------------------------------------
# SET UP DIRECTORIES AND READ IN FILES 
#----------------------------------------------
rm(list=ls())
user = Sys.info()['user']
setwd(paste0("C:/Users/", user, "/Documents/gf")) #This sets to the root of my repository. 

source("./resource_tracking/prep/_common/set_up_r.R", encoding="UTF-8")
source("./resource_tracking/miscellaneous/prep_uga_catalytic_budget.R")

country = "uga" #Change to the country you want to update. Options are "cod", "gtm", or "uga".  
master_file_dir = paste0(dir, "_gf_files_gos/", country, "/raw_data/")
export_dir = paste0(dir, "_gf_files_gos/", country, "/prepped_data/")
file_list = fread(paste0(master_file_dir, country, "_catalytic_funds.csv"), stringsAsFactors = FALSE, encoding="Latin-1")
file_list$start_date <- as.Date(file_list$start_date, format = "%m/%d/%Y")

#----------------------------------------------
# PREP RAW DATA 
#----------------------------------------------

budget_cols = c("activity_description", "budget", "cost_category", "implementer", "intervention", "module", "quarter", "start_date", "year")

for(i in 1:nrow(file_list)){
  folder = "budgets"
  version = ifelse(file_list$file_iteration[i] == "initial", "iterations", "")
  file_dir = paste0(master_file_dir, file_list$grant_status[i], "/", file_list$grant[i], "/", folder, "/")
  if (version != ""){
    file_dir = paste0(file_dir, version, "/")
  }
  
  
  inFile = paste0(file_dir, file_list$file_name[i])
  args = list(file_dir, file_list$file_name[i], file_list$sheet[i], file_list$start_date[i], file_list$period[i], 
              file_list$qtr_number[i], file_list$language[i])
 
  tmpData = do.call(prep_general_detailed_budget, args)
  stopifnot(sort(names(tmpData))==budget_cols)
  
  #Add indexing data
  append_cols = file_list[i, .(data_source, grant_period, primary_recipient, secondary_recipient, file_name, grant_status, disease, grant, 
                               mod_framework_format, file_iteration, language, catalytic_funding)]
  for (col in names(append_cols)){
    tmpData[, (col):=append_cols[, get(col)]]
  }  
  tmpData$year <- year(tmpData$start_date)
  
  #Bind data together 
  if(i==1){
    dt = tmpData
  } 
  if(i>1){
    dt = rbind(dt, tmpData, use.names=TRUE, fill = TRUE)
  }
  print(paste0(i, " ", file_list$data_source[i], " ", file_list$function_type[i], " ", file_list$grant[i])) ## if the code breaks, you know which file it broke on
}

saveRDS(dt, paste0(export_dir, "uga_catalytic_funding_18_20.rds"))
write.csv(dt, paste0(export_dir, "uga_catalytic_funding_18_20.rds"), row.names=F)

#----------------------------------------------
# RUN SOME ANALYSIS  
#----------------------------------------------
head(dt)
unique(dt$implementer)
unique(dt[, .(grant, implementer, catalytic_funding)])

#Try this same thing, but are budget numbers different? 
agyw_budget = unique(dt[catalytic_funding=='agyw', .(start_date, module, intervention, budget)])
agyw_budget[, funding:='agyw']
hr_budget = unique(dt[catalytic_funding=='hr', .(start_date, module, intervention, budget)])
hr_budget[, funding:='hr']
general_budget = unique(dt[, .(start_date, module, intervention, budget)])
general_budget[, funding:='general']

budget_check = merge(agyw_budget, hr_budget, by=c('start_date', 'module', 'intervention', 'budget'), all=T)
budget_check = merge(budget_check, general_budget, by=c('start_date', 'module', 'intervention', 'budget'), all=T)
nrow(budget_check[is.na(funding)]) #These would represent cases where the budget line item DOESN'T exist in the general budget tab

#Are there any module/intervention pairs on the sub-tabs that weren't in the general budget? Are we sure these aren't just a subset? 
dt[, mod_combo:=paste0(module, intervention, activity_description)]
agyw_mods = unique(dt[catalytic_funding=='agyw', .(mod_combo, module, intervention)])
hr_mods = unique(dt[catalytic_funding=='hr', .(mod_combo, module, intervention)])
general_mods = unique(dt[catalytic_funding=='general', .(mod_combo, module, intervention)])

agyw_mods[!mod_combo%in%general_mods$mod_combo, .(module, intervention)] #These are modules/interventions that are only represented under the AGYW tab
hr_mods[!mod_combo%in%general_mods$mod_combo, .(module, intervention)] #These are modules/interventions that are only represented under the HR tab

#Calculate budget numbers for each subset. 
dt[catalytic_funding=='agyw', .(agyw=sum(budget, na.rm=T)), by=c('module', 'intervention')][order(-agyw)]
dt[catalytic_funding=='hr', .(hr=sum(budget, na.rm=T)), by=c('module', 'intervention')][order(-hr)]



# 
# 
# #Work in progress. 
# unique(dt$catalytic_funding)
# unique(dt[, .(grant, catalytic_funding, implementer)][order(grant, implementer, catalytic_funding)])
# unique(dt[catalytic_funding=='general', .(grant, catalytic_funding, implementer)][order(grant, implementer, catalytic_funding)])
# 
# unique(dt[, .(module, intervention), by=c('catalytic_funding')])
# unique(dt[, .(module, intervention), by=c('implementer')])

