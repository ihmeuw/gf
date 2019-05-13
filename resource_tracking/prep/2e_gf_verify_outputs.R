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

base_dir = "J:/Project/Evaluation/GF/resource_tracking/"

if (test_current_files == TRUE){
  file_iterations <- readRDS(paste0(combined_output_dir, "/budget_pudr_iterations.rds"))
  gos_data = readRDS(paste0(gos_prepped, "/prepped_gos_data.rds"))
} else {
  print("WARNING: TESTING ARCHIVED DATABASE. REVIEW SWITCH 'test_current_files'")
  # Old resource tracking database, for comparison. Was archived on Dec 3, 2018
  file_iterations = fread("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/archive/total_resource_tracking_data 12032018.csv")
  file_iterations$start_date <- as.Date(file_iterations$start_date)
  file_iterations$budget <- as.numeric(file_iterations$budget)
  file_iterations$expenditure <- as.numeric(file_iterations$expenditure)
  gos_data = file_iterations[type == 'gos']
  setnames(file_iterations, old=c('fileName', 'grant_number'), new=c('file_name', 'grant'))
  
}

# -----------------------
# Country-level tests 
# -----------------------
loc_names = c('cod', 'gtm', 'sen', 'uga')
countries = c('DRC', 'Guatemala', 'Senegal', 'Uganda')

for (i in 1:length(loc_names)){
  #Sum budget by quarter for each country/file. 
  budget_pudrs = file_iterations[loc_name == loc_names[i]]
  budget_pudrs = budget_pudrs[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('file_name', 'start_date')]
  
  #Read in country-level tests. 
  tests = fread(paste0(base_dir, "_gf_files_gos/", loc_names[i], "/", loc_names[i], "_tests.csv"))
  tests[, start_date:=as.Date(start_date, format = "%m/%d/%Y")] 
  assign(paste0(loc_names[i], "_tests"), tests) #Make a country-specific test file for summary statistics later. 
  
  #Merge tests and data, and check this merge. 
  merge <- merge(tests, budget_pudrs, by = c('start_date', 'file_name'))
  if(nrow(merge) != nrow(tests)){
    print(paste0("Warning: Not all ", countries[i], " tests merged."))
    assign(paste0(loc_names[i], "_unmerged_tests") , tests[!file_name%in%merge$file_name, .(file_name)])
  }
  assign(paste0(loc_names[i], "_not_tested"), unique(budget_pudrs[!file_name%in%merge$file_name, .(file_name)]))
  if (nrow(get(paste0(loc_names[i], "_not_tested")))!=0){
    print(paste0("ERROR: Some files in ", countries[i], " are not being tested."))
  }
 
  #Simplify numbers and format of merge data table
  merge[, budget:=round(budget)]
  merge[, expenditure:=round(expenditure)]
  merge[, loc_name:=loc_names[i]] #For sorting out failed tests later. 
  merge <- merge[, .(file_name, correct_bug_sum, correct_exp_sum, budget, expenditure, start_date, type, loc_name)]
  
  #Pull out which tests failed, i.e. where budget or expenditure did not match their expected values. 
  assign(paste0(loc_names[i], "_failed_tests"), merge[correct_bug_sum!=budget | correct_exp_sum != expenditure], envir=.GlobalEnv)
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
