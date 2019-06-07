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

if (test_current_files != TRUE){
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
loc_names = c('sen')
countries = c('Senegal')

all_failed_budgets = data.table() 
all_failed_expenditures = data.table() 
all_failed_absorption = data.table()
for (i in 1:length(loc_names)){
  loc_name = loc_names[i]
  country = countries[i]
  
  budgets = readRDS(paste0(base_dir, loc_name, "/prepped_data/final_budgets.rds"))
  expenditures = readRDS(paste0(base_dir, loc_name, "/prepped_data/final_expenditures.rds"))
  absorption = readRDS(paste0(base_dir, loc_name, "/prepped_data/absorption.rds"))
  
  budget_tests = read.xlsx(paste0(base_dir, loc_name, "/", loc_name, "_tests.xlsx"), sheet="budget", detectDates=T)
  expenditure_tests = read.xlsx(paste0(base_dir, loc_name, "/", loc_name, "_tests.xlsx"), sheet="expenditure", detectDates=T)
  absorption_tests = read.xlsx(paste0(base_dir, loc_name, "/", loc_name, "_tests.xlsx"), sheet="absorption", detectDates=T)
  
  #----------------------------
  # BUDGET
  #---------------------------- 
  budgets1 = budgets[, .(budget=sum(budget, na.rm=T)), by=c('file_name', 'start_date')] #Collapse budget file. 
  budgets1 = merge(budgets1, budget_tests, by=c('file_name', 'start_date'))
  budgets1[, correct_bug_sum:=round(correct_bug_sum)]
  budgets1[, budget:=round(budget)]
  
  #Check to make sure everything merged 
  if (nrow(budget_tests)!=nrow(budgets1)){
    print(paste0("Some tests did not merge for ", country, ". Review merge."))
  }
  
  #Check to make sure all files are being tested. 
  untested_files = unique(budgets$file_name)
  untested_files = untested_files[!untested_files%in%budgets1$file_name]
  if (length(untested_files)!=0){
    print(paste0("Some files are not being tested for ", country, "."))
    print(untested_files)
  }
  
  failed_budgets = budgets1[correct_bug_sum!=budget]
  failed_budgets[, loc:=loc_name]
  all_failed_budgets = rbind(all_failed_budgets, failed_budgets)
 
  #----------------------------
  # EXPENDITURE
  #----------------------------
  expenditures1 = expenditures[, .(expenditure=sum(expenditure, na.rm=T)), by=c('grant', 'grant_period', 'pudr_grant_year', 'semester')] #Collapse expenditure file. 
  expenditures1 = merge(expenditures1, expenditure_tests, by=c('grant', 'grant_period', 'pudr_grant_year', 'semester'))
  expenditures1[, correct_exp:=round(correct_exp)]
  expenditures1[, expenditure:=round(expenditure)]
  
  #Check to make sure everything merged 
  if (nrow(expenditure_tests)!=nrow(expenditures1)){
    print(paste0("Some tests did not merge for ", country, ". Review merge."))
  }
  
  #Check to make sure all files are being tested. 
  untested_grants = unique(expenditures[, .(grant, grant_period, pudr_grant_year, semester)])
  untested_grants[, concat:=paste0(grant, "_", grant_period, "_", pudr_grant_year, "_", semester)]
  expenditures1[, concat:=paste0(grant, "_", grant_period, "_", pudr_grant_year, "_", semester)]
  untested_grants = untested_grants[!concat%in%expenditures1$concat]
  if (length(untested_files)!=0){
    print(paste0("Some files are not being tested for ", country, "."))
    print(untested_files)
  }
  
  failed_expenditures = expenditures1[correct_exp!=expenditure]
  failed_expenditures[, loc:=loc_name]
  all_failed_expenditures = rbind(all_failed_expenditures, failed_expenditures) 
  
  #----------------------------
  # ABSORPTION
  #----------------------------
  absorption1 = absorption[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('grant', 'grant_period', 'semester')] #Collapse absorption file. 
  absorption1[, absorption:=(expenditure/budget)*100]
  absorption1 = merge(absorption1, absorption_tests, by=c('grant', 'grant_period', 'semester'))
  for (var in c('budget', 'expenditure', 'absorption', 'correct_budget', 'correct_expenditure', 'correct_absorption')){
    absorption1[, (var):=round(get(var), 2)]
  }
 
  #Check to make sure everything merged 
  if (nrow(absorption_tests)!=nrow(absorption1)){
    print(paste0("Some tests did not merge for ", country, ". Review merge."))
  }
  
  #Check to make sure all files are being tested. 
  untested_grants = unique(absorption[, .(grant, grant_period, semester)])
  untested_grants[, concat:=paste0(grant, "_", grant_period, "_", semester)]
  absorption1[, concat:=paste0(grant, "_", grant_period, "_", semester)]
  untested_grants = untested_grants[!concat%in%absorption1$concat]
  if (length(untested_files)!=0){
    print(paste0("Some files are not being tested for ", country, "."))
    print(untested_files)
  }
  
  failed_absorption = absorption1[absorption!=correct_absorption]
  failed_absorption[, loc:=loc_name]
  all_failed_absorption = rbind(all_failed_absorption, failed_absorption)
  
}

# ------------------
# RSSH tests
# ------------------
print("...")
rssh_tests <- fread(paste0(dir, "_gf_files_gos/rssh_tests.csv"))
rssh_by_rt_code <- file_iterations[substring(code, 1, 1) == 'R']
rssh_by_rt_code = rssh_by_rt_code[, .(rt_code_rssh = round(sum(budget, na.rm = TRUE))), by = c('file_name')]
rssh_by_rt_code[is.na(rt_code_rssh), rt_code_rssh:=0]

check_rssh <- merge(rssh_tests, rssh_by_rt_code, by = c('file_name'), all.y = TRUE)
unwritten_rssh_tests = check_rssh[is.na(correct_rssh)]

#unmerged_tests = anti_join(rssh_tests, rssh_by_rt_code, by='file_name')
if(nrow(unwritten_rssh_tests)!=0){
  print("ERROR: Some files are don't have a unit test written for RSSH. Review 'unwritten rssh tests'. ")
}
check_rssh = check_rssh[!is.na(correct_rssh)]

failed_rssh_tests = check_rssh[correct_rssh!=rt_code_rssh]

# ------------------
# GOS Tests 
# ------------------

gos_tests = fread(paste0(dir, "_gf_files_gos/gos/gos_tests.csv"))

gos_rssh = gos_data[substring(code, 1, 1)=='R']
gos_rssh = gos_rssh[, .(gos_rssh = round(sum(budget, na.rm = TRUE))), by=c('grant')]

gos_data[is.na(budget), budget:=0]
gos_data[is.na(expenditure), expenditure:=0]
gos_data = gos_data[ , 
              lapply(.SD, sum) , 
              by = 'grant', 
              .SDcols = c("budget", "expenditure")]
gos_data <- unique(gos_data)
gos_data[, budget:=round(budget)]
gos_data[, expenditure:=round(expenditure)]

gos_data = merge(gos_data, gos_rssh, by='grant', all = TRUE)
gos_merge = merge(gos_data, gos_tests, by='grant', all.x = TRUE)

#Find failed tests and untested grants 
untested_gos = gos_merge[is.na(correct_bug_sum)]
if (nrow(untested_gos)!=0){
  print("ERROR: Some GOS grants don't have tests written for them. Review 'untested gos'.")
}
gos_merge = gos_merge[!is.na(correct_bug_sum)] #Only review failures for tests you've written and merged. 
failed_gos_rssh = gos_merge[gos_rssh!=correct_rssh]
failed_tests_gos = gos_merge[correct_bug_sum!=budget | correct_exp_sum != expenditure]

# ------------------------------------
# Print results and summary statistics
# ------------------------------------
{

#Country statistics 
for (i in 1:length(loc_names)){
  assign(paste0(loc_names[i], "_filelist"), fread(paste0(dir, "_gf_files_gos/", loc_names[i], "/raw_data/", loc_names[i], "_budget_filelist.csv"))) #The raw country file list. 
  assign(paste0(loc_names[i], "_unique_tests"), get(paste0(loc_names[i], "_tests"))[!duplicated(file_name)]) #A full DT of the unique files tested. 
  assign(paste0(loc_names[i], "_tested_grants"), unique(get(paste0(loc_names[i], "_tests"))[, .(file_name)])) #A unique list of the file names tested.
}
  
  print("...")
failed_tests <- rbind(gtm_failed_tests, cod_failed_tests, uga_failed_tests, sen_failed_tests, fill = TRUE)
failed_rssh_tests = rbind(failed_rssh_tests, failed_gos_rssh, fill = TRUE)
  
if (nrow(failed_tests) != 0 | nrow(failed_rssh_tests)!=0){
  print("Unit tests failed; review budget calculations.")

  for (i in 1:length(loc_names)){
    n_failed = nrow(failed_tests[loc_name==loc_names[i]])
    unique_tests = nrow(get(paste0(loc_names[i], "_tested_grants")))
    fail_pct = round(((n_failed/unique_tests)*100), 2)
    print(paste0(n_failed, " ", countries[i], " tests failed (", fail_pct, "%) of unique files."))
    print("...")
  }
 
} else {
  print("All unit tests passed.")
  print("...")
}

# #Country statistics 
# for (i in 1:length(loc_names)){
#   print(paste0("Testing ", round(all_tests[type == "pudr", .N]/filelist[type == "pudr", .N]*100, 2), "% of PUDRs and ", 
#                round(gtm_tests_nodup[type != "pudr", .N]/gtm_filelist[type != "pudr", .N]*100, 2), "% of budgets in Guatemala"))
#   
# }


# print(paste0("Testing ", round(gtm_tests_nodup[type == "pudr", .N]/gtm_filelist[type == "pudr", .N]*100, 2), "% of PUDRs and ", 
#              round(gtm_tests_nodup[type != "pudr", .N]/gtm_filelist[type != "pudr", .N]*100, 2), "% of budgets in Guatemala"))
# print(paste0("Testing ", round(nrow(unique(gtm_filelist[grant_status=='active' & file_name%in%gtm_tested_grants$file_name, .(file_name)]))/nrow(unique(gtm_filelist[grant_status=='active', .(file_name)]))*100, 2), 
#              "% of active files and ", round(nrow(unique(gtm_filelist[grant_status=='not_active' & file_name%in%gtm_tested_grants$file_name, .(file_name)]))/nrow(unique(gtm_filelist[grant_status=='not_active', .(file_name)]))*100, 2)
#              , "% of not active files Guatemala"))
# print("...")


# total_tests <- nrow(cod_tests) + nrow(gtm_tests) + nrow(uga_tests) + nrow(sen_tests)
# total_merges <- nrow(cod_merge) + nrow(gtm_merge) + nrow(uga_merge) + nrow(sen_merge)
# total_unmerged <- total_tests - total_merges
# 
# print(paste0("Total merged tests that failed either a budget or expenditure calculation: ", round(nrow(failed_tests)/total_merges*100, 2), "%")) #Total failed tests over total of rows of merge datasets. 
# print(paste0("Total tests that failed, including non-merges: ", round((total_unmerged + nrow(failed_tests))/total_tests*100, 2), "%")) #/total tests 
# print(paste0("Total tests merged: ", total_merges, " out of ", total_tests, ", or ", round((total_merges/total_tests)*100, 2), "%"))
# print(paste0("Total RSSH tests failed: ", nrow(failed_rssh_tests)))
# print(paste0("Percentage of RSSH tests failed: ", round(nrow(failed_rssh_tests)/nrow(check_rssh)*100, 2), "%"))
# 

#What are the files you need to review? Only keep these, so it's easier to go through them. 
rm(list= ls()[!(ls() %in% c('failed_tests','cod_failed_tests', 'gtm_failed_tests', 'uga_failed_tests', 'sen_failed_tests', 'failed_tests_gos',
                            'cod_unmerged_tests', 'gtm_unmerged_tests', 'sen_unmerged_tests', 'uga_unmerged_tests', 
                            'cod_not_tested', 'gtm_not_tested', 'sen_not_tested', 'uga_not_tested',
                             'failed_rssh_tests', 'unwritten_rssh_tests',
                            'untested_gos', 'combined_output_dir'))])


}
