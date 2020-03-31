# ----------------------------------------------------------------------
# AUTHOR: Emily Linebarger, based on code by Naomi Provost. 
# PURPOSE: Verify budget/expenditure totals for Global Fund budgets and pudrs
#         
# DATE: Last updated March 2020 
# -----------------------------------------------------------------------

#------------------------------------
# Set up file paths and globals
#------------------------------------
base_dir = paste0(box, "tableau_data/")
unit_test_dir = paste0(paste0(j, "/Project/Evaluation/GF/resource_tracking/_gf_files_gos/unit_testing/"))

#--------------------------------
# Read in data 
#--------------------------------
# We are currently testing most recent budgets, approved budgets from grant-making, most recent semester of absorption, and cumulative absorption. 
# We may want to revisit this and make sure all files released to partners (all budget revisions, performance indicators) are being tested. EL 3/30/20

# Tableau data 
# budgets
most_recent_budgets = fread(paste0(base_dir, "most_recent_budgets.csv"))
approved_budgets = fread(paste0(base_dir, "approved_budgets.csv"))
budgets = rbind(most_recent_budgets, approved_budgets)
budgets = budgets[, .(budget=round(sum(budget, na.rm=T))), by=c('file_name', 'start_date')]
budgets[, start_date:=as.Date(start_date, fmt="%Y-%m-%d")]

# Most recent absorption 
most_recent_absorption = fread(paste0(base_dir, "most_recent_absorption.csv"))
most_recent_absorption = most_recent_absorption[, .(budget=round(sum(budget, na.rm=T)), expenditure=round(sum(expenditure, na.rm=T))), by=c('grant', 'grant_period', 'start_date', 'end_date')]
most_recent_absorption[, absorption:=round((expenditure/budget)*100)]
most_recent_absorption[, start_date:=as.Date(start_date, fmt="%Y-%m-%d")]
most_recent_absorption[, end_date:=as.Date(end_date, fmt="%Y-%m-%d")]

# Cumulative absorption 
cumulative_absorption = fread(paste0(base_dir, "cumulative_absorption.csv"))
cumulative_absorption = cumulative_absorption[, .(cumulative_budget=round(sum(cumulative_budget, na.rm=T)), 
                                                  cumulative_expenditure=round(sum(cumulative_expenditure, na.rm=T))), 
                                              by=c('grant', 'grant_period', 'start_date', 'end_date')]
cumulative_absorption[, cumulative_absorption:=round((cumulative_expenditure/cumulative_budget)*100)]
cumulative_absorption[, start_date:=as.Date(start_date, fmt="%Y-%m-%d")]
cumulative_absorption[, end_date:=as.Date(end_date, fmt="%Y-%m-%d")]

# Unit tests
# Budget
cod_budget_tests = data.table(read_xlsx(paste0(unit_test_dir, "cod_tests.xlsx"), sheet="budget"))
gtm_budget_tests = data.table(read_xlsx(paste0(unit_test_dir, "gtm_tests.xlsx"), sheet="budget"))
uga_budget_tests = data.table(read_xlsx(paste0(unit_test_dir, "uga_tests.xlsx"), sheet="budget"))
sen_budget_tests = data.table(read_xlsx(paste0(unit_test_dir, "sen_tests.xlsx"), sheet="budget"))

budget_tests = rbindlist(list(cod_budget_tests, gtm_budget_tests, uga_budget_tests, sen_budget_tests), fill=TRUE)
budget_tests[, start_date:=as.Date(start_date)]
budget_tests[, correct_bug_sum:=round(correct_bug_sum)]

# Most recent absorption 
cod_absorption_tests = data.table(read_xlsx(paste0(unit_test_dir, "cod_tests.xlsx"), sheet="absorption"))
gtm_absorption_tests = data.table(read_xlsx(paste0(unit_test_dir, "gtm_tests.xlsx"), sheet="absorption"))
uga_absorption_tests = data.table(read_xlsx(paste0(unit_test_dir, "uga_tests.xlsx"), sheet="absorption"))
sen_absorption_tests = data.table(read_xlsx(paste0(unit_test_dir, "sen_tests.xlsx"), sheet="absorption"))

absorption_tests = rbindlist(list(cod_absorption_tests, gtm_absorption_tests, uga_absorption_tests, sen_absorption_tests), fill=T)
absorption_tests[, start_date:=as.Date(start_date)]
absorption_tests[, end_date:=as.Date(end_date)]
absorption_tests[, correct_budget:=round(correct_budget)]
absorption_tests[, correct_expenditure:=round(correct_expenditure)]
absorption_tests[, correct_absorption:=round(correct_absorption)]

# Cumulative absorption 
cod_cumulative_absorption_tests = data.table(read_xlsx(paste0(unit_test_dir, "cod_tests.xlsx"), sheet="cumulative_absorption"))
gtm_cumulative_absorption_tests = data.table(read_xlsx(paste0(unit_test_dir, "gtm_tests.xlsx"), sheet="cumulative_absorption"))
uga_cumulative_absorption_tests = data.table(read_xlsx(paste0(unit_test_dir, "uga_tests.xlsx"), sheet="cumulative_absorption"))
sen_cumulative_absorption_tests = data.table(read_xlsx(paste0(unit_test_dir, "sen_tests.xlsx"), sheet="cumulative_absorption"))

cumulative_absorption_tests = rbindlist(list(cod_cumulative_absorption_tests, gtm_cumulative_absorption_tests, uga_cumulative_absorption_tests, sen_cumulative_absorption_tests), fill=T)

cumulative_absorption_tests[, start_date:=as.Date(start_date)]
cumulative_absorption_tests[, end_date:=as.Date(end_date)]
cumulative_absorption_tests[, correct_cumulative_budget:=round(correct_cumulative_budget)]
cumulative_absorption_tests[, correct_cumulative_expenditure:=round(correct_cumulative_expenditure)]
cumulative_absorption_tests[, correct_cumulative_absorption:=round(correct_cumulative_absorption)]

#----------------------------
# TEST BUDGETS 
#---------------------------- 
print("Testing budgets...")
merged_budgets = merge(budgets, budget_tests, by=c('file_name', 'start_date'), all.x=T)

#Check to make sure all files are being tested. 
all_untested_budgets = unique(merged_budgets[is.na(correct_bug_sum), file_name])
all_tested_budgets = unique(merged_budgets[!is.na(correct_bug_sum), file_name])
all_untested_budgets = all_untested_budgets[!all_untested_budgets%in%all_tested_budgets]
if (length(all_untested_budgets)!=0){
  print("ERROR: There are untested budgets in final data. Review 'all_untested_budgets'.")
}

all_failed_budgets = merged_budgets[correct_bug_sum!=budget]
if (length(all_failed_budgets)!=0){
  print("ERROR: Budget unit tests failed. Review 'all_failed_budgets'.")
}

#----------------------------
# TEST MOST RECENT ABSORPTION 
#----------------------------
print("Testing most recent absorption...")
merged_absorption = merge(most_recent_absorption, absorption_tests, by=c('grant', 'grant_period', 'start_date', 'end_date'), all.x=T)

#Check to make sure all files are being tested. 
all_untested_absorption = unique(merged_absorption[is.na(correct_budget), .(grant, grant_period, start_date, end_date)])
if (nrow(all_untested_absorption)!=0){
  print("ERROR: There are untested absorption numbers. Review 'all_untested_absorption'.")
}

all_failed_absorption = merged_absorption[budget!=correct_budget | expenditure!=correct_expenditure | absorption!=correct_absorption]
if (nrow(all_failed_absorption)!=0){
  print("ERROR: Absorption unit tests failed. Review 'all_failed_absorption'.")
}

#----------------------------
# TEST CUMULATIVE ABSORPTION 
#----------------------------
print("Testing cumulative absorption...")
merged_cumul_absorption = merge(cumulative_absorption, cumulative_absorption_tests, by=c('grant', 'grant_period', 'start_date', 'end_date'), all.x=T)

#Check to make sure all files are being tested. 
all_untested_cumul_absorption = unique(merged_cumul_absorption[is.na(correct_cumulative_budget), .(grant, grant_period, start_date, end_date)])
if (nrow(all_untested_cumul_absorption)!=0){
  print("ERROR: There are untested absorption numbers. Review 'all_untested_cumul_absorption'.")
}

all_failed_cumul_absorption = merged_cumul_absorption[cumulative_budget!=correct_cumulative_budget |
                                                        cumulative_expenditure!=correct_cumulative_expenditure | 
                                                        cumulative_absorption!=correct_cumulative_absorption]
if (nrow(all_failed_cumul_absorption)!=0){
  print("ERROR: Absorption unit tests failed. Review 'all_failed_cumul_absorption'.")
}
# ------------------------------------
# Print results and summary statistics
# ------------------------------------

#Summary statistics 
print("...")
print("...")
print("UNIT TEST RESULTS") 
print(paste0("Total number of failed budget tests: ", nrow(all_failed_budgets)))
print(paste0("Total number of failed absorption tests: ", nrow(all_failed_absorption)+nrow(all_failed_cumul_absorption)))
print("...")

any_tests_missing = ((length(all_untested_budgets) + nrow(all_untested_absorption) + nrow(all_untested_cumul_absorption))>0)
any_failures = ((nrow(all_failed_budgets) + nrow(all_failed_absorption) + nrow(all_failed_cumul_absorption))>0)

# Flag if any files did not merge 
if (any_tests_missing){
  print("SOME FINAL DATA IS NOT BEING TESTED; REVIEW UNTESTED FILES.")
}

#Flag if any unit tests have failed. 
if (any_failures){
  print("UNIT TESTS FAILED; REVIEW PREP CODE")
}

if (!any_failures & !any_tests_missing) {
  print("ALL UNIT TESTS BUILT AND PASSING.")
}

print("Step 2F: Verify outputs completed.")


