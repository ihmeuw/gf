# ----------------------------------------------------------------------
# AUTHOR: Emily Linebarger, based on code by Naomi Provost. 
# PURPOSE: Verify budget/expenditure totals for Global Fund budgets and pudrs, 
#           and SICOIN data in Guatemala. 
#         
# DATE: Last updated December 2018. 
# -----------------------------------------------------------------------

#-------------------------
#To do: 
# - incorporate SICOIN checks again
#--------------------------

base_dir = "J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/"

if (!test_current_files){
  print("WARNING: TESTING ARCHIVED DATABASE. REVIEW SWITCH 'test_current_files'")
  # Old resource tracking database, for comparison. Was archived on Dec 3, 2018
  file_iterations = fread("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/archive/total_resource_tracking_data 12032018.csv")
  file_iterations$start_date <- as.Date(file_iterations$start_date)
  file_iterations$budget <- as.numeric(file_iterations$budget)
  file_iterations$expenditure <- as.numeric(file_iterations$expenditure)
  gos_data = file_iterations[type == 'gos']
  setnames(file_iterations, old=c('fileName', 'grant_number'), new=c('file_name', 'grant'))
  
  #EMILY DO WE WANT TO KEEP THIS CHECK AROUND? CODE BELOW NO LONGER WORKS. 
} 
# -----------------------
# Country-level tests 
# -----------------------
loc_names = c('cod', 'gtm', 'uga', 'sen')
countries = c('DRC', 'Guatemala', 'Uganda', 'Senegal')

all_failed_budgets = data.table() 
all_failed_expenditures = data.table() 
all_failed_absorption = data.table()

all_untested_budgets = data.table() 
all_untested_expenditures = data.table() 
all_untested_absorption = data.table() 
for (i in 1:length(loc_names)){
  loc_name = loc_names[i]
  country = countries[i]
  print(paste0("TESTING ", country ))
  print("...")
  
  budgets = readRDS(paste0(base_dir, loc_name, "/prepped_data/final_budgets.rds"))
  expenditures = readRDS(paste0(base_dir, loc_name, "/prepped_data/final_expenditures.rds"))
  absorption = readRDS(paste0(base_dir, loc_name, "/prepped_data/absorption_", loc_name, ".rds"))
  
  budget_tests = read.xlsx(paste0(base_dir, "unit_testing/", loc_name, "_tests.xlsx"), sheet="budget", detectDates=T)
  expenditure_tests = read.xlsx(paste0(base_dir, "unit_testing/", loc_name, "_tests.xlsx"), sheet="expenditure", detectDates=T)
  absorption_tests = read.xlsx(paste0(base_dir, "unit_testing/", loc_name, "_tests.xlsx"), sheet="absorption", detectDates=T)
  
  #----------------------------
  # BUDGET
  #---------------------------- 
  budgets1 = budgets[, .(budget=sum(budget, na.rm=T)), by=c('file_name', 'start_date')] #Collapse budget file. 
  budgets1 = merge(budgets1, budget_tests, by=c('file_name', 'start_date'), all=T)
  budgets1[, correct_bug_sum:=round(correct_bug_sum)]
  budgets1[, budget:=round(budget)]
  
  #Check to make sure all files are being tested. 
  untested_files = unique(budgets1[is.na(correct_bug_sum), file_name])
  tested_files = unique(budgets1[!is.na(correct_bug_sum), file_name])
  untested_files = untested_files[!untested_files%in%tested_files]
  if (length(untested_files)!=0){
    print(paste0("Some budget files are not being tested for ", country, "."))
    all_untested_budgets = rbind(all_untested_budgets, untested_files, fill=T)
  }
  
  failed_budgets = budgets1[correct_bug_sum!=budget]
  failed_budgets[, loc:=loc_name]
  all_failed_budgets = rbind(all_failed_budgets, failed_budgets, fill=T)
 
  #----------------------------
  # EXPENDITURE
  #----------------------------
  expenditures1 = expenditures[, .(expenditure=sum(expenditure, na.rm=T)), by=c('grant', 'grant_period', 'start_date')] #Collapse expenditure file. 
  expenditures1 = merge(expenditures1, expenditure_tests, by=c('grant', 'grant_period', 'start_date'), all=T)
  expenditures1[, correct_exp:=round(correct_exp)]
  expenditures1[, expenditure:=round(expenditure)]
  
  #Check to make sure all files are being tested. 
  untested_grants = unique(expenditures1[is.na(correct_exp), .(grant, grant_period, start_date)])
  untested_grants[, concat:=paste0(grant, "_", grant_period, "_", start_date)]
  expenditures1[!is.na(correct_exp), concat:=paste0(grant, "_", grant_period, "_", start_date)]
  untested_grants = untested_grants[!concat%in%expenditures1$concat]
  if (nrow(untested_grants)!=0){
    print(paste0("Some expenditure numbers are not being tested for ", country, "."))
    all_untested_expenditures = rbind(all_untested_expenditures, untested_grants, fill=T)
  }
  
  failed_expenditures = expenditures1[correct_exp!=expenditure]
  failed_expenditures[, loc:=loc_name]
  all_failed_expenditures = rbind(all_failed_expenditures, failed_expenditures, fill=T) 
  
  #----------------------------
  # ABSORPTION
  #----------------------------
  absorption1 = absorption[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('grant', 'grant_period', 'semester')] #Collapse absorption file. 
  absorption1[, absorption:=(expenditure/budget)*100]
  absorption1 = merge(absorption1, absorption_tests, by=c('grant', 'grant_period', 'semester'), all=T)
  for (var in c('budget', 'expenditure', 'absorption', 'correct_budget', 'correct_expenditure', 'correct_absorption')){
    absorption1[, (var):=round(get(var), 2)]
  }
  
  #Check to make sure all files are being tested. 
  untested_grants = unique(absorption1[is.na(correct_budget), .(grant, grant_period, semester)])
  untested_grants[, concat:=paste0(grant, "_", grant_period, "_", semester)]
  absorption1[!is.na(correct_budget), concat:=paste0(grant, "_", grant_period, "_", semester)]
  untested_grants = untested_grants[!concat%in%absorption1$concat]
  if (nrow(untested_grants)!=0){
    print(paste0("Some absorption numbers are not being tested for ", country, "."))
    all_untested_absorption = rbind(all_untested_absorption, untested_grants, fill=T)
  }
  
  failed_absorption = absorption1[absorption!=correct_absorption | round(expenditure)!=round(correct_expenditure)]
  failed_absorption[, loc:=loc_name]
  all_failed_absorption = rbind(all_failed_absorption, failed_absorption, fill=T)
  
}

# ------------------
# RSSH tests
# ------------------
# print("...")
# rssh_tests <- fread(paste0(dir, "_gf_files_gos/rssh_tests.csv"))
# rssh_by_rt_code <- all_budgets[substring(code, 1, 1) == 'R']
# rssh_by_rt_code = rssh_by_rt_code[, .(rt_code_rssh = round(sum(budget, na.rm = TRUE))), by = c('file_name')]
# rssh_by_rt_code[is.na(rt_code_rssh), rt_code_rssh:=0]
# 
# check_rssh <- merge(rssh_tests, rssh_by_rt_code, by = c('file_name'), all.y = TRUE)
# unwritten_rssh_tests = check_rssh[is.na(correct_rssh)]
# 
# #unmerged_tests = anti_join(rssh_tests, rssh_by_rt_code, by='file_name')
# if(nrow(unwritten_rssh_tests)!=0){
#   print("ERROR: Some files are don't have a unit test written for RSSH. Review 'unwritten rssh tests'. ")
# }
# check_rssh = check_rssh[!is.na(correct_rssh)]
# 
# failed_rssh_tests = check_rssh[correct_rssh!=rt_code_rssh]

# ------------------
# GOS Tests 
# ------------------

# gos_tests = fread(paste0(dir, "_gf_files_gos/gos/gos_tests.csv"))
# 
# gos_rssh = gos_data[substring(code, 1, 1)=='R']
# gos_rssh = gos_rssh[, .(gos_rssh = round(sum(budget, na.rm = TRUE))), by=c('grant')]
# 
# gos_data[is.na(budget), budget:=0]
# gos_data[is.na(expenditure), expenditure:=0]
# gos_data = gos_data[ , 
#               lapply(.SD, sum) , 
#               by = 'grant', 
#               .SDcols = c("budget", "expenditure")]
# gos_data <- unique(gos_data)
# gos_data[, budget:=round(budget)]
# gos_data[, expenditure:=round(expenditure)]
# 
# gos_data = merge(gos_data, gos_rssh, by='grant', all = TRUE)
# gos_merge = merge(gos_data, gos_tests, by='grant', all.x = TRUE)
# 
# #Find failed tests and untested grants 
# untested_gos = gos_merge[is.na(correct_bug_sum)]
# if (nrow(untested_gos)!=0){
#   print("ERROR: Some GOS grants don't have tests written for them. Review 'untested gos'.")
# }
# gos_merge = gos_merge[!is.na(correct_bug_sum)] #Only review failures for tests you've written and merged. 
# failed_gos_rssh = gos_merge[gos_rssh!=correct_rssh]
# failed_tests_gos = gos_merge[correct_bug_sum!=budget | correct_exp_sum != expenditure]

# ------------------------------------
# Print results and summary statistics
# ------------------------------------
{

  #Country statistics 
  print("...")
  print("...")
  print("UNIT TEST RESULTS") 
  print(paste0("Total number of failed budget tests: ", nrow(all_failed_budgets)))
  print(paste0("Total number of failed expenditure tests: ", nrow(all_failed_expenditures)))
  print(paste0("Total number of failed absorption tests: ", nrow(all_failed_absorption)))
  print("...")
  
  # Flag if any files did not merge 
  if (nrow(all_untested_budgets) + nrow(all_untested_expenditures) + nrow(all_untested_absorption)>0){
    print("SOME FINAL DATA IS NOT BEING TESTED; REVIEW UNTESTED FILES.")
  }
  
  #Flag if any unit tests have failed. 
  if (nrow(all_failed_budgets) + nrow(all_failed_expenditures) + nrow(all_failed_absorption)>0){
    print("UNIT TESTS FAILED; REVIEW PREP CODE")
  }



}
