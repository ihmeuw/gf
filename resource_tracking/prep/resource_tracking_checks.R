rm(list=ls())
library(lubridate)
library(data.table)
library(readxl)
library(stats)
library(stringr)
library(rlang)
library(zoo)

check_budget_dates = function(dt_g){
  # lists budgets by date and filename
  dt_g[,bug_sum := sum(budget, na.rm = TRUE), by = c("start_date", "grant_number", "end_date", "period", "fileName")]
  return(unique(dt_g[data_source == "fpm", c("start_date", "grant_number", "end_date", "period", "fileName", "bug_sum")]))
}

check_pudr_dates = function(dt_g){
  # lists pudr by date and filename
  dt_g[,exp_sum := sum(expenditure, na.rm = TRUE), by = c("start_date", "grant_number", "end_date", "period", "fileName")]
  return(unique(dt_g[data_source == "pudr", c("start_date", "grant_number", "end_date", "period", "fileName", "exp_sum")]))
}

# input files
file_dir <- "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/"
fil = "total_resource_tracking_data.csv"
dt = fread(paste0(file_dir, fil))

#GTM Analysis
dt_gtm = dt[loc_name == "gtm"]
gtm_budgets = check_budget_dates(dt, "gtm")
gtm_pudrs = check_pudr_dates(dt, "gtm")


#UGA Analysis
dt_uga = dt[loc_name == "uga"]

uga_budgets = check_budget_dates(dt_uga)
uga_pudrs = check_pudr_dates(dt_uga)

dt_gtm[,bug_sum := sum(budget, na.rm = TRUE), by = c('grant_number', "grant_period", 'fileName')]
unique(dt_gtm[data_source == "fpm" & grant_period == "2018", c("grant_number", "grant_period", "bug_sum", "fileName")])


#GTM Analysis
dt_drc = dt[loc_name == "drc"]
drc_budgets = check_budget_dates(dt, "cod")
drc_pudrs = check_pudr_dates(dt, "cod")


