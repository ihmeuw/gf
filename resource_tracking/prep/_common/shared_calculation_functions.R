# ----------------------------------------------------------------------------------
# AUTHOR: Emily Linebarger, based on code by Irena Chen
# PURPOSE: Shared functions for the resource tracking database. All of these functions 
# perform calculations, for string-prep and file-loading functions 
# review 'shared_prep_functions'. 
# DATE: Last updated March 2019. 
#-----------------------------------------------------------------------------------

#----------------------------------------------
#Functions to verify budgets, used in step 5. 
#----------------------------------------------
#Sums budget by key variables
check_budgets_pudrs = function(dt){
  keyVars = c("start_date", "file_name", "grant", "data_source")
  #Deciding not to split by disease here because we just want the total for the whole quarter. 
  dt[, budget:=as.numeric(budget)]
  dt[, expenditure:=as.numeric(expenditure)]
  
  dt[is.na(budget), budget:=0]
  dt[is.na(expenditure), expenditure:=0]
  #Replacing budget and expenditure as NA 
  budgets = dt[ , 
               lapply(.SD, sum), 
                by = keyVars, 
               .SDcols = c("budget", "expenditure")]
  budgets <- unique(budgets)
  return(budgets)
}

check_SICOIN_dates = function(dt){
  # ouptut can be used to compare to "Data Seeking Spreadsheets"
  dt = dt[data_source == "sicoin"]
  dt$budget <- as.numeric(dt$budget)
  dt[,bug_sum := sum(budget, na.rm = TRUE), by = c("start_date", "grant_number", "end_date", "period", "fileName", "grant_period", "disease")]
  dt$sdaDetail = ifelse(dt$financing_source == 'gf', "Summary", "None")
  dt$geog = ifelse(dt$adm1 == 100, "National", "Municipality")
  sicoin_dt = unique(dt[, c("data_source", "financing_source","grant_period", "start_date", "end_date", 'sdaDetail', 'geog', "period", "grant_number", "disease", "fileName", "bug_sum")])
  sicoin_dt$financing_source = ifelse(sicoin_dt$financing_source == "other_dah", "donacions", sicoin_dt$financing_source)
  sicoin_dt$data_source = paste0(sicoin_dt$data_source, "-", sicoin_dt$financing_source)
  sicoin_dt$financing_source = NULL
  sicoin_dt$grant_period = "none"
  
  # files with "Municiaplity" also have "National estimates, removing those for geographic detail purposes
  dt_muni = sicoin_dt[geog == 'Municipality']
  dt_national = sicoin_dt[geog == 'National']
  dt_national <- subset(dt_national, !fileName %in% dt_muni$fileName)
  sicoin_dt = rbind(dt_muni, dt_national)
  sicoin_dt$start_date = as.Date(sicoin_dt$start_date)
  sicoin_dt$end_date = as.Date(sicoin_dt$end_date)
  return(sicoin_dt)
}

total_budget_by_grantPeriod = function(dt_g){
  dt_g[,bug_sum := sum(budget, na.rm = TRUE), by = c('grant_number', "grant_period", "fileName")]
  return(unique(dt_g[data_source == "fpm", c("grant_number", "grant_period", "bug_sum", "fileName")]))
}

#-------------------------- 
# Sum, removing NA's (simple function to add into data tables) 
#---------------------------
sum_na_rm <- function(col){
  col = sum(col, na.rm = TRUE)
  return(col)
}


#--------------------------------------------------------------------------------
# Orders a dataset to be at the quarter-level given a start and an end date. 
#--------------------------------------------------------------------------------



