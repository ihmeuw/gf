# ----------------------------------------------------------------------
# AUTHOR: Emily Linebarger, based on code by Naomi Provost. 
# PURPOSE: Verify budget/expenditure totals for Global Fund budgets and pudrs, 
#           and SICOIN data in Guatemala. 
#         
# DATE: Last updated December 2018. 
# -----------------------------------------------------------------------

# input files
file_dir <- "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/"
fil = "total_resource_tracking_data.csv"
dt = fread(paste0(file_dir, fil))

# -----------------------
# Guatemala file prep 
# -----------------------
{
dt_gtm = dt[loc_name == "gtm"]
# to check Data Seeking Spreadsheet
gtm_budgets = check_budgets_pudrs(dt_gtm)
gtm_sicoin = check_SICOIN_dates(dt_gtm)
gtm_sicoin$grant_number = "unknown"

# THIS IS TO JUST FILL IN THE SICOIN DATA
# col_na = c("data_source", "grant_period", "start_date", "sdaDetail", "geog", "period", "disease", "grant_number")
# sicoin_withFileNames = merge(missing_sic, gtm_sicoin, by = col_na, all.x = TRUE, all.y = TRUE)
# sicoin_withFileNames$end_date = ifelse(is.na(sicoin_withFileNames$end_date), as.Date(sicoin_withFileNames$start_date + sicoin_withFileNames$period, origin = "1899-12-30"), as.Date(sicoin_withFileNames$end_date, origin = "1899-12-30"))
# sicoin_withFileNames$end_date = as.Date(sicoin_withFileNames$end_date, origin = "1969-12-30")
# sicoin_withFileNames$notes = " "
# sicoin_withFileNames$Country = "gtm"
# 
# dt_sicoin = sicoin_withFileNames[,c("data_source", "grant_period", "start_date", "end_date", "sdaDetail", "geog",  "period", "grant_number", "disease", "Country", "notes", "fileName")]
# write.csv(dt_sicoin, paste0(sicoin_dir, fil_sic), row.names = FALSE)
# 
# # to check total budgeted per grant (should match Grant Anaylsis Worksheet)
# gtm_grant_check = total_budget_by_grantPeriod(dt_gtm)

# -----------------------
# Guatemala unit tests 
# -----------------------

gtm_tests<-read.csv("J:/Project/Evaluation/GF/resource_tracking/multi_country/gf/testing_budget_numbers/gtm_tests.csv")
gtm_tests$start_date = substring(gtm_tests$start_date, 2, 11)
gtm_merge <- merge(gtm_tests, gtm_budgets, by = c('start_date', 'fileName'), all.X = TRUE) 
stopifnot(nrow(gtm_merge)==nrow(gtm_tests)) 
sort(gtm_merge$budget)

gtm_merge$budget = round(gtm_merge$budget)

for(i in 1:nrow(gtm_merge)){
    print(paste0(i, " ", gtm_merge$fileName[i], " ", gtm_merge$start_date[i]))
    stopifnot(gtm_merge$correct_bug_sum[i] != gtm_merge$budget[i])
}

print("ALL GUATEMALA BUDGET TESTS PASSED")

}
# ------------------
# Uganda file prep 
# ------------------
{
dt_uga = dt[loc_name == "uga"]

# to check Data Seeking Spreadsheet
uga_budgets = check_budget_dates(dt_uga)
uga_pudrs = check_pudr_dates(dt_uga)

# to check total budgeted per grant (should match Grant Anaylsis Worksheet)
uga_grant_check = total_budget_by_grantPeriod(dt_uga)


# ------------------
# Uganda unit tests
# ------------------

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
}

# ------------------
# DRC File prep 
# ------------------
{
dt_drc = dt[loc_name == "cod"]

# to check Data Seeking Spreadsheet
drc_budgets = check_budget_dates(dt_drc)
drc_pudrs = check_pudr_dates(dt_drc)

# to check total budgeted per grant (should match Grant Anaylsis Worksheet)
drc_grant_check = total_budget_by_grantPeriod(dt_drc)

# ------------------
# DRC Unit tests 
# ------------------

print("ALL DRC BUDGET TESTS PASSED.")
}
