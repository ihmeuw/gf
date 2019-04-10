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
  gos_data = file_iterations[data_source == 'gos']
  setnames(file_iterations, old=c('fileName', 'grant_number'), new=c('file_name', 'grant'))
  
}

# -----------------------
# Guatemala file prep 
# -----------------------
{
gtm_budgets = file_iterations[loc_name == "gtm"]
gtm_budgets = check_budgets_pudrs(gtm_budgets)

# -----------------------
# Guatemala unit tests
# -----------------------

gtm_tests<-fread(paste0(dir, "_gf_files_gos/gtm/gtm_tests.csv"))
gtm_tests$correct_bug_sum <- gsub("[[:punct:]]", "", gtm_tests$correct_bug_sum)
gtm_tests$correct_exp_sum <- gsub("[[:punct:]]", "", gtm_tests$correct_exp_sum)
gtm_tests$correct_bug_sum <- as.numeric(gtm_tests$correct_bug_sum)
gtm_tests$correct_exp_sum <- as.numeric(gtm_tests$correct_exp_sum)

gtm_tests$start_date <- as.Date(gtm_tests$start_date, format = "%m/%d/%Y")
gtm_merge <- merge(gtm_tests, gtm_budgets, by = c('start_date', 'file_name'))

gtm_merge <- merge(gtm_tests, gtm_budgets, by = c('start_date', 'file_name')) 
if(nrow(gtm_merge) != nrow(gtm_tests)){
  print("ERROR: Not all Guatemala tests merged.")
  unmerged_gtm_tests = gtm_tests[!file_name%in%gtm_merge$file_name, .(file_name)]
}
not_tested_gtm = unique(gtm_budgets[!file_name%in%gtm_merge$file_name, .(file_name)])
if(nrow(not_tested_gtm)!=0){
  print("ERROR: Some files in Guatemala are not being tested.")
  not_tested_gtm = unique(gtm_budgets[!file_name%in%gtm_merge$file_name, .(file_name)])
}

gtm_merge$budget = round(gtm_merge$budget)
gtm_merge$expenditure = round(gtm_merge$expenditure)

gtm_merge <- gtm_merge[, .(file_name, correct_bug_sum, correct_exp_sum, budget, expenditure, start_date, data_source)]
gtm_merge$country <- "gtm" #For sorting out failed tests later if any.

failed_budgets_gtm <- gtm_merge[correct_bug_sum != budget, ]
failed_expenditures_gtm <- gtm_merge[correct_exp_sum != expenditure, ]
failed_tests_gtm = unique(rbind(failed_budgets_gtm, failed_expenditures_gtm))

}
# ------------------
# Uganda file prep 
# ------------------
{
  dt_uga = file_iterations[loc_name == "uga"]
  uga_budgets = check_budgets_pudrs(dt_uga)
  uga_budgets[, quarter:=quarter(start_date)]
  uga_budgets[, year:=year(start_date)]
  setnames(uga_budgets, old='file_name', new='file_name')
  # ------------------
  # DRC unit tests
  # ------------------
  
  uga_tests<-fread(paste0(dir, "_gf_files_gos/uga/uga_tests.csv"), encoding = "Latin-1")
  uga_tests$start_date <- as.Date(uga_tests$start_date, format="%m/%d/%Y")
  
  uga_tests[, quarter:=quarter(start_date)]
  uga_tests[, year:=year(start_date)]
  
  uga_tests$correct_bug_sum <- gsub("[[:punct:]]", "", uga_tests$correct_bug_sum)
  uga_tests$correct_exp_sum <- gsub("[[:punct:]]", "", uga_tests$correct_exp_sum)
  uga_tests$correct_bug_sum <- as.numeric(uga_tests$correct_bug_sum)
  uga_tests$correct_exp_sum <- as.numeric(uga_tests$correct_exp_sum)
  
  uga_merge <- merge(uga_tests, uga_budgets, by = c('start_date', 'file_name')) 
  if(nrow(uga_merge) != nrow(uga_tests)){
    print("ERROR: Not all Uganda tests merged.")
    unmerged_uga_tests = uga_tests[!file_name%in%uga_merge$file_name, .(file_name)]
  }
  not_tested_uga = unique(uga_budgets[!file_name%in%uga_merge$file_name, .(file_name)])
  if(nrow(not_tested_uga)!=0){
    print("ERROR: Some files in Uganda are not being tested.")
    not_tested_uga = unique(uga_budgets[!file_name%in%uga_merge$file_name, .(file_name)])
  }
  
  
  uga_merge$budget = round(uga_merge$budget)
  uga_merge$expenditure = round(uga_merge$expenditure)
  
  uga_merge <- uga_merge[, .(file_name, correct_bug_sum, correct_exp_sum, budget, expenditure, start_date, data_source.x)]
  uga_merge$country <- "uga" #For sorting out failed tests later if any. 
  
  failed_budgets_uga <- uga_merge[correct_bug_sum != budget, ]
  failed_expenditures_uga <- uga_merge[correct_exp_sum != expenditure, ]
  failed_tests_uga = unique(rbind(failed_budgets_uga, failed_expenditures_uga)) 

}

# ------------------
# DRC file prep 
# ------------------
{
  dt_cod = file_iterations[loc_name == "cod"]
  cod_budgets = check_budgets_pudrs(dt_cod)
  cod_budgets[, quarter:=quarter(start_date)]
  cod_budgets[, year:=year(start_date)]
  setnames(cod_budgets, old='file_name', new='file_name')
  # ------------------
  # DRC unit tests
  # ------------------
  
  cod_tests<-fread(paste0(dir, "_gf_files_gos/cod/cod_tests.csv"), encoding = "Latin-1")
  cod_tests$start_date <- as.Date(cod_tests$start_date, format="%m/%d/%Y")
  
  cod_tests[, quarter:=quarter(start_date)]
  cod_tests[, year:=year(start_date)]
  
  cod_tests$correct_bug_sum <- gsub("[[:punct:]]", "", cod_tests$correct_bug_sum)
  cod_tests$correct_exp_sum <- gsub("[[:punct:]]", "", cod_tests$correct_exp_sum)
  cod_tests$correct_bug_sum <- as.numeric(cod_tests$correct_bug_sum)
  cod_tests$correct_exp_sum <- as.numeric(cod_tests$correct_exp_sum)
  
  cod_merge <- merge(cod_tests, cod_budgets, by = c('start_date', 'file_name')) 
  if(nrow(cod_merge) != nrow(cod_tests)){
    print("ERROR: Not all DRC tests merged.")
    unmerged_cod_tests = cod_tests[!file_name%in%cod_merge$file_name, .(file_name)]
  }
  not_tested_cod = unique(cod_budgets[!file_name%in%cod_merge$file_name, .(file_name)])
  if(nrow(not_tested_cod)!=0){
    print("ERROR: Some files in DRC are not being tested.")
    not_tested_cod = unique(cod_budgets[!file_name%in%cod_merge$file_name, .(file_name)])
  }
  
  
  cod_merge$budget = round(cod_merge$budget)
  cod_merge$expenditure = round(cod_merge$expenditure)
  
  cod_merge <- cod_merge[, .(file_name, correct_bug_sum, correct_exp_sum, budget, expenditure, start_date, data_source.x)]
  cod_merge$country <- "cod" #For sorting out failed tests later if any. 
  
  failed_budgets_cod <- cod_merge[correct_bug_sum != budget, ]
  failed_expenditures_cod <- cod_merge[correct_exp_sum != expenditure, ]
  failed_tests_cod = unique(rbind(failed_budgets_cod, failed_expenditures_cod)) 
  
}

# ------------------
# RSSH tests
# ------------------

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

  print("...")
failed_tests <- rbind(failed_tests_gtm, failed_tests_cod, failed_tests_uga, fill = TRUE)
failed_rssh_tests = rbind(failed_rssh_tests, failed_gos_rssh, fill = TRUE)
  
if (nrow(failed_tests) != 0 | nrow(failed_rssh_tests)!=0){
  print("Unit tests failed; review budget calculations.")
  print(paste0(nrow(failed_tests_gtm), " Guatemala tests failed (", round(nrow(failed_tests_gtm)/nrow(gtm_merge)*100, 2), "%); ", 
               nrow(gtm_merge), " out of ", nrow(gtm_tests), " tests are included in this percentage."))
  print(paste0(nrow(failed_tests_uga), " Uganda tests failed (", round(nrow(failed_tests_uga)/nrow(uga_merge)*100, 2), "%); ",
               nrow(uga_merge), " out of ", nrow(uga_tests), " tests are included in this percentage."))
  print(paste0(nrow(failed_tests_cod), " DRC tests failed (", round(nrow(failed_tests_cod)/nrow(cod_merge)*100, 2), "%); ", 
               nrow(cod_merge), " out of ", nrow(cod_tests), " tests are included in this percentage."))
  print(paste0(nrow(failed_tests_gos), " GOS tests for all three countries failed (", round(nrow(failed_tests_gos)/nrow(gos_merge)*100, 2), "%); ", 
               nrow(gos_merge), " out of ", nrow(gos_tests), " tests are included in this percentage."))
  print("...")
  
} else {
  print("All unit tests passed.")
  print("...")
}

uga_filelist <- fread(paste0(dir, "_gf_files_gos/uga/raw_data/uga_budget_filelist.csv"))
cod_filelist <- fread(paste0(dir, "_gf_files_gos/cod/raw_data/cod_budget_filelist.csv"))
gtm_filelist <- fread(paste0(dir, "_gf_files_gos/gtm/raw_data/gtm_budget_filelist.csv"))

gtm_tests_nodup <- gtm_tests[!duplicated(file_name)]
cod_tests_nodup <- cod_tests[!duplicated(file_name)]
uga_tests_nodup <- uga_tests[!duplicated(file_name)]

gtm_tested_grants <- unique(gtm_tests[, .(file_name)])
cod_tested_grants <- unique(cod_tests[, .(file_name)])
uga_tested_grants <- unique(uga_tests[, .(file_name)])

print(paste0("Testing ", round(gtm_tests_nodup[format == "pudr", .N]/gtm_filelist[data_source == "pudr", .N]*100, 2), "% of PUDRs and ", 
             round(gtm_tests_nodup[format != "pudr", .N]/gtm_filelist[data_source != "pudr", .N]*100, 2), "% of budgets in Guatemala"))
print(paste0("Testing ", round(nrow(unique(gtm_filelist[grant_status=='active' & file_name%in%gtm_tested_grants$file_name, .(file_name)]))/nrow(unique(gtm_filelist[grant_status=='active', .(file_name)]))*100, 2), 
             "% of active files and ", round(nrow(unique(gtm_filelist[grant_status=='not_active' & file_name%in%gtm_tested_grants$file_name, .(file_name)]))/nrow(unique(gtm_filelist[grant_status=='not_active', .(file_name)]))*100, 2)
             , "% of not active files Guatemala"))
print("...")

print(paste0("Testing ", round(uga_tests_nodup[type == "pudr", .N]/uga_filelist[data_source == "pudr", .N]*100, 2), "% of PUDRs and ", 
             round(uga_tests_nodup[type != "pudr", .N]/uga_filelist[data_source != "pudr", .N]*100, 2), "% of budgets in Uganda"))
print(paste0("Testing ", round(nrow(unique(uga_filelist[grant_status=='active' & file_name%in%uga_tested_grants$file_name, .(file_name)]))/nrow(unique(uga_filelist[grant_status=='active', .(file_name)]))*100, 2), 
            "% of active files and ", round(nrow(unique(uga_filelist[grant_status=='not_active' & file_name%in%uga_tested_grants$file_name, .(file_name)]))/nrow(unique(uga_filelist[grant_status=='not_active', .(file_name)]))*100, 2)
             , "% of not active files Uganda"))
print("...")

print(paste0("Testing ", round(cod_tests_nodup[type == "pudr", .N]/cod_filelist[data_source == "pudr", .N]*100, 2), "% of PUDRs and ", 
             round(cod_tests_nodup[type != "pudr", .N]/cod_filelist[data_source != "pudr", .N]*100, 2), "% of budgets in DRC"))
print(paste0("Testing ", round(nrow(unique(cod_filelist[grant_status=='active' & file_name%in%cod_tested_grants$file_name, .(file_name)]))/nrow(unique(cod_filelist[grant_status=='active', .(file_name)]))*100, 2), 
"% of active files and ", round(nrow(unique(cod_filelist[grant_status=='not_active' & file_name%in%cod_tested_grants$file_name, .(file_name)]))/nrow(unique(cod_filelist[grant_status=='not_active', .(file_name)]))*100, 2)
             , "% of not active files DRC"))
print("...")

if (nrow(not_tested_cod)!=0){
  print("Warning: There are files in your database for DRC that aren't currently in the list of tests.")
  print(not_tested_cod)
}

total_tests <- nrow(cod_tests) + nrow(gtm_tests) + nrow(uga_tests)
total_merges <- nrow(cod_merge) + nrow(gtm_merge) + nrow(uga_merge)
total_unmerged <- total_tests - total_merges

print(paste0("Total merged tests that failed either a budget or expenditure calculation: ", round(nrow(failed_tests)/total_merges*100, 2), "%")) #Total failed tests over total of rows of merge datasets. 
print(paste0("Total tests that failed, including non-merges: ", round((total_unmerged + nrow(failed_tests))/total_tests*100, 2), "%")) #/total tests 
print(paste0("Total tests merged: ", total_merges, " out of ", total_tests, ", or ", round((total_merges/total_tests)*100, 2), "%"))
print(paste0("Total RSSH tests failed: ", nrow(failed_rssh_tests)))
print(paste0("Percentage of RSSH tests failed: ", round(nrow(failed_rssh_tests)/nrow(check_rssh)*100, 2), "%"))


rm(list= ls()[!(ls() %in% c('failed_tests','failed_tests_cod', 'failed_tests_gtm', 'failed_tests_uga', 'failed_tests_gos'
                            , 'gtm_tests', 'gtm_merge', 'cod_tests', 'cod_merge', 'uga_tests', 'uga_merge', 
                            'unmerged_cod_tests', 'unmerged_gtm_tests', 'unmerged_uga_tests', 
                             'failed_rssh_tests', 'unwritten_rssh_tests', 'not_tested_cod', 'not_tested_gtm', 'not_tested_uga',
                            'untested_gos', 'combined_output_dir'))])


}
