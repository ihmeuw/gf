
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
missing_sic$end_date = NULL

# to check Data Seeking Spreadsheet
gtm_budgets = check_budget_dates(dt_gtm)
gtm_pudrs = check_pudr_dates(dt_gtm)
gtm_sicoin = check_SICOIN_dates(dt_gtm)
gtm_sicoin$grant_number = "unknown"

# THIS IS TO JUST FILL IN THE SICOIN DATA
col_na = c("data_source", "grant_period", "start_date", "sdaDetail", "geog", "period", "disease", "grant_number")
sicoin_withFileNames = merge(missing_sic, gtm_sicoin, by = col_na, all.x = TRUE, all.y = TRUE)
sicoin_withFileNames$end_date = ifelse(is.na(sicoin_withFileNames$end_date), as.Date(sicoin_withFileNames$start_date + sicoin_withFileNames$period, origin = "1899-12-30"), as.Date(sicoin_withFileNames$end_date, origin = "1899-12-30"))
sicoin_withFileNames$end_date = as.Date(sicoin_withFileNames$end_date, origin = "1969-12-30")
sicoin_withFileNames$notes = " "
sicoin_withFileNames$Country = "gtm"

dt_sicoin = sicoin_withFileNames[,c("data_source", "grant_period", "start_date", "end_date", "sdaDetail", "geog",  "period", "grant_number", "disease", "Country", "notes", "fileName")]
write.csv(dt_sicoin, paste0(sicoin_dir, fil_sic), row.names = FALSE)

# to check total budgeted per grant (should match Grant Anaylsis Worksheet)
gtm_grant_check = total_budget_by_grantPeriod(dt_gtm)

#################
#               #
#   UNIT TESTS  # 
#               #
#################

gtm_tests<-read.csv("J:/Project/Evaluation/GF/resource_tracking/multi_country/gf/testing_budget_numbers/gtm_tests.csv")
gtm_tests$start_date = substring(gtm_tests$start_date, 2, 11)
gtm_merge <- merge(gtm_tests, gtm_budgets, by=c("fileName", "start_date"), all.x = TRUE) #Should have 16 columns.
gtm_merge <- merge(gtm_merge, gtm_pudrs, by=c("fileName", "start_date"), all.x = TRUE)   #Should have 16 columns.
sort(gtm_merge$bug_sum)
#temporary drop; actually need all of these to work. #EMILY NEED TO START HERE. 
#gtm_merge <- gtm_merge[, !is.na(gtm_merge$bug_sum)]
gtm_merge$bug_sum = round(gtm_merge$bug_sum)

for(i in 1:length(gtm_merge$fileName)){
    stopifnot(gtm_merge$correct_bug_sum[i] != gtm_merge$bug_sum[i])
    print(paste0(i, " ", gtm_merge$fileName, " ", gtm_merge$start_date))
}

print("ALL GUATEMALA TESTS PASSED")



######################################## UGA Analysis ###########################################
dt_uga = dt[loc_name == "uga"]

# to check Data Seeking Spreadsheet
uga_budgets = check_budget_dates(dt_uga)
uga_pudrs = check_pudr_dates(dt_uga)

# to check total budgeted per grant (should match Grant Anaylsis Worksheet)
uga_grant_check = total_budget_by_grantPeriod(dt_uga)

#################
#               #
#   UNIT TESTS  # 
#               #
#################

#Check if any budget has a total of 0. #CHECK THIS FUNCTION WITH DAVID. Do we need the join of all 3 of these? Is just one missing a problem?  
empty_budgets = dt_uga[bug_sum == 0 & exp_sum == 0 & disbursement ==0]
if(nrow(empty_budgets)>0){
  print(unique(empty_budgets[, c("fileName", "start_date", "end_date", "bug_sum", "exp_sum", "disbursement"), with= FALSE]))
  stop(paste0("Quarters have a total budget of 0. Review."))
}

#Check first and last quarter totals for each type of function (fpm, init_fpm_dec, init_fpm_july, pudr)
  #fpm 
  stopifnot(round(uga_budgets$bug_sum[which(uga_budgets$start_date == "2018-01-01" & uga_budgets$end_date == "2018-03-31" & uga_budgets$grant_number == "UGA-H-MoFPED")]) == 698102)
  stopifnot(round(uga_budgets$bug_sum[which(uga_budgets$start_date == "2020-10-01" & uga_budgets$end_date == "2020-12-29" & uga_budgets$grant_number == "UGA-H-MoFPED")]) == 512815)
  
  stopifnot(round(uga_budgets$bug_sum[which(uga_budgets$start_date == "2018-01-01" & uga_budgets$end_date == "2018-03-31" & uga_budgets$grant_number == "UGA-H-MoFPED")]) == 698102)
  

#Check grants with strange quarter numbers
  #10 quarters
  stopifnot(round(uga_budgets$bug_sum[which(uga_budgets$start_date == "2015-07-01" & uga_budgets$end_date == "2015-09-28" & uga_budgets$grant_number == "UGA-C-TASO")]) ==  298607)
  stopifnot(round(uga_budgets$bug_sum[which(uga_budgets$start_date == "2015-07-01" & uga_budgets$end_date == "2015-09-28" & uga_budgets$grant_number == "UGA-C-TASO")]) ==  298607)
            
  #3 quarters
  #6 quarters 




######################################## DRC Analysis ###########################################
dt_drc = dt[loc_name == "cod"]

# to check Data Seeking Spreadsheet
drc_budgets = check_budget_dates(dt_drc)
drc_pudrs = check_pudr_dates(dt_drc)

# to check total budgeted per grant (should match Grant Anaylsis Worksheet)
drc_grant_check = total_budget_by_grantPeriod(dt_drc)

#################
#               #
#   UNIT TESTS  # 
#               #
#################

print("ALL TESTS PASSED.")

