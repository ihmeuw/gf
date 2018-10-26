
# ----------------------------------------------

# Naomi Provost
# Code the check the resource tracking database
# This code can be used to check the "Data Seeking Spreadsheets" or answer questions about budgets

# ----------------------------------------------
###### Set up R / install packages  ###### 
rm(list=ls())
library(lubridate)
library(data.table)
library(readxl)
library(stats)
library(stringr)
library(rlang)
library(zoo)
library(dplyr)

check_budget_dates = function(dt_g){
  # ouptut can be used to compare to "Data Seeking Spreadsheets"
  dt_g[,bug_sum := sum(budget, na.rm = TRUE), by = c("start_date", "grant_number", "end_date", "period", "fileName", "grant_period", "disease")]
  return(unique(dt_g[data_source == "fpm", c("data_source","grant_period", "start_date", "end_date", "period", "grant_number", "disease", "fileName", "bug_sum")]))
}

check_pudr_dates = function(dt_g){
  # ouptut can be used to compare to "Data Seeking Spreadsheets"
  dt_g[,exp_sum := sum(expenditure, na.rm = TRUE), by = c("start_date", "grant_number", "end_date", "period", "fileName", "grant_period", "disease")]
  return(unique(dt_g[data_source == "pudr", c("data_source","grant_period", "start_date", "end_date", "period", "grant_number", "disease", "fileName","exp_sum")]))
}

check_SICOIN_dates = function(dt_g){
  # ouptut can be used to compare to "Data Seeking Spreadsheets"
  dt_g = dt_g[data_source == "sicoin"]
  dt_g[,bug_sum := sum(budget, na.rm = TRUE), by = c("start_date", "grant_number", "end_date", "period", "fileName", "grant_period", "disease")]
  dt_g$sdaDetail = ifelse(dt_g$financing_source == 'gf', "Summary", "None")
  dt_g$geog = ifelse(dt_g$adm1 == 100, "National", "Municipality")
  sicoin_dt = unique(dt_g[, c("data_source", "financing_source","grant_period", "start_date", "end_date", 'sdaDetail', 'geog', "period", "grant_number", "disease", "fileName", "bug_sum")])
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

# input files
file_dir <- "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/"
fil = "total_resource_tracking_data.csv"
dt = fread(paste0(file_dir, fil))


######################################## GTM Analysis ###########################################
dt_gtm = dt[loc_name == "gtm"]

sicoin_dir <- "J:/Project/Evaluation/GF/resource_tracking/gtm/"
fil_sic = "sicoin_missing_files.csv"
missing_sic = unique(fread(paste0(sicoin_dir, fil_sic)))
missing_sic$start_date = as.Date(missing_sic$start_date, "%m/%d/%Y")
missing_sic$end_date = as.Date(missing_sic$end_date, "%m/%d/%Y")

# to check Data Seeking Spreadsheet
gtm_budgets = check_budget_dates(dt_gtm)
gtm_pudrs = check_pudr_dates(dt_gtm)
gtm_sicoin = check_SICOIN_dates(dt_gtm)

dt3 <- full_join(x = missing_sic, y = gtm_sicoin, by = c("data_source", "grant_period", "start_date", "end_date", "sdaDetail", "geog", "period", "disease"))

test = merge(missing_sic, gtm_sicoin, by = c("data_source", "grant_period", "start_date", "end_date", "sdaDetail", "geog", "period", "disease"))
test = rbind(missing_sic, gtm_sicoin,fill = TRUE)

# to check total budgeted per grant (should match Grant Anaylsis Worksheet)
gtm_grant_check = total_budget_by_grantPeriod(dt_gtm)

######################################## UGA Analysis ###########################################
dt_uga = dt[loc_name == "uga"]

# to check Data Seeking Spreadsheet
uga_budgets = check_budget_dates(dt_uga)
uga_pudrs = check_pudr_dates(dt_uga)

# to check total budgeted per grant (should match Grant Anaylsis Worksheet)
uga_grant_check = total_budget_by_grantPeriod(dt_uga)


######################################## DRC Analysis ###########################################
dt_drc = dt[loc_name == "cod"]

# to check Data Seeking Spreadsheet
drc_budgets = check_budget_dates(dt_drc)
drc_pudrs = check_pudr_dates(dt_drc)

# to check total budgeted per grant (should match Grant Anaylsis Worksheet)
drc_grant_check = total_budget_by_grantPeriod(dt_drc)

