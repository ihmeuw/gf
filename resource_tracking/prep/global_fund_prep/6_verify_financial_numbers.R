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
# - add expenditure checks to UGA
# - add RSSH checks for all countries. 
# - Are we testing each prep function for each country? 
# - 

#--------------------------
# final_budgets <- readRDS(paste0(file_dir, "final_budgets.rds")) #Change to final budgets for right now, but will want to test all files eventually! 
# final_expenditures <- readRDS(paste0(file_dir, "final_expenditures.rds"))
# file_iterations <- rbind(final_budgets, final_expenditures, fill = TRUE)

#file_iterations <- readRDS(paste0(combined_output_dir, "/budget_pudr_iterations.rds"))
file_iterations <- readRDS(paste0(j, "/Project/Evaluation/GF/resource_tracking/cod/prepped/budget_pudr_iterations.rds"))

# -----------------------
# Guatemala file prep 
# -----------------------
{
gtm_budgets = file_iterations[loc_name == "gtm"]
gtm_budgets = check_budgets_pudrs(gtm_budgets)

# -----------------------
# Guatemala unit tests 
# -----------------------

gtm_tests<-fread(paste0(j, "/Project/Evaluation/GF/resource_tracking/multi_country/gf/testing_budget_numbers/gtm_tests.csv"))
gtm_tests$correct_bug_sum <- gsub("[[:punct:]]", "", gtm_tests$correct_bug_sum)
gtm_tests$correct_exp_sum <- gsub("[[:punct:]]", "", gtm_tests$correct_exp_sum)
gtm_tests$correct_bug_sum <- as.numeric(gtm_tests$correct_bug_sum)
gtm_tests$correct_exp_sum <- as.numeric(gtm_tests$correct_exp_sum)

gtm_tests$start_date <- as.Date(gtm_tests$start_date, format = "%m/%d/%Y")
gtm_merge <- merge(gtm_tests, gtm_budgets, by = c('start_date', 'fileName')) 

if(nrow(gtm_merge) != nrow(gtm_tests)){
  print("ERROR: Not all Guatemala tests merged.")
  unmerged_gtm_tests = gtm_tests[!(fileName%in%gtm_merge$fileName)][order(fileName, start_date)]
}

gtm_merge$budget = round(gtm_merge$budget)
gtm_merge$expenditure = round(gtm_merge$expenditure)

gtm_merge <- gtm_merge[, .(fileName, correct_bug_sum, correct_exp_sum, budget, expenditure, start_date, data_source.x)]
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

# ------------------
# Uganda unit tests
# ------------------

uga_tests<-fread(paste0(j, "/Project/Evaluation/GF/resource_tracking/multi_country/gf/testing_budget_numbers/uga_tests.csv"))
uga_tests$start_date <- as.Date(uga_tests$start_date, format="%m/%d/%Y")

uga_tests$correct_bug_sum <- gsub("[[:punct:]]", "", uga_tests$correct_bug_sum)
uga_tests$correct_exp_sum <- gsub("[[:punct:]]", "", uga_tests$correct_exp_sum)
uga_tests$correct_bug_sum <- as.numeric(uga_tests$correct_bug_sum)
uga_tests$correct_exp_sum <- as.numeric(uga_tests$correct_exp_sum)

uga_merge <- merge(uga_tests, uga_budgets, by = c('start_date', 'fileName')) 
if(nrow(uga_merge) != nrow(uga_tests)){
  print("ERROR: Not all Uganda tests merged.")
  unmerged_uga_tests = uga_tests[!(fileName%in%uga_merge$fileName)][order(fileName, start_date)]
}

uga_merge$budget = round(uga_merge$budget)
uga_merge$expenditure = round(uga_merge$expenditure)

uga_merge <- uga_merge[, .(fileName, correct_bug_sum, correct_exp_sum, budget, expenditure, start_date, data_source.x)]
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
  
  # ------------------
  # DRC unit tests
  # ------------------
  
  cod_tests<-fread(paste0(j, "/Project/Evaluation/GF/resource_tracking/multi_country/gf/testing_budget_numbers/cod_tests.csv"), encoding = "Latin-1")
  cod_tests$start_date <- as.Date(cod_tests$start_date, format="%m/%d/%Y")
  
  cod_tests[, quarter:=quarter(start_date)]
  cod_tests[, year:=year(start_date)]
  
  cod_tests$correct_bug_sum <- gsub("[[:punct:]]", "", cod_tests$correct_bug_sum)
  cod_tests$correct_exp_sum <- gsub("[[:punct:]]", "", cod_tests$correct_exp_sum)
  cod_tests$correct_bug_sum <- as.numeric(cod_tests$correct_bug_sum)
  cod_tests$correct_exp_sum <- as.numeric(cod_tests$correct_exp_sum)
  
  cod_merge <- merge(cod_tests, cod_budgets, by = c('quarter', 'year', 'file_name')) 
  if(nrow(cod_merge) != nrow(cod_tests)){
    print("ERROR: Not all DRC tests merged.")
    unmerged_cod_tests = cod_tests[!(file_name%in%cod_merge$file_name)][order(file_name, start_date)]
  }
  
  cod_merge$budget = round(cod_merge$budget)
  cod_merge$expenditure = round(cod_merge$expenditure)
  
  cod_merge <- cod_merge[, .(file_name, correct_bug_sum, correct_exp_sum, budget, expenditure, quarter, year, data_source.x)]
  cod_merge$country <- "cod" #For sorting out failed tests later if any. 
  
  failed_budgets_cod <- cod_merge[correct_bug_sum != budget, ]
  failed_expenditures_cod <- cod_merge[correct_exp_sum != expenditure, ]
  failed_tests_cod = unique(rbind(failed_budgets_cod, failed_expenditures_cod)) 
  
}

# ------------------
# RSSH tests
# ------------------

rssh_tests <- fread(paste0(j, "/Project/Evaluation/GF/resource_tracking/multi_country/gf/testing_budget_numbers/rssh_tests.csv"))
setnames(rssh_tests, old='fileName', new='file_name')
rssh_by_rt_code <- file_iterations[substring(code, 1, 1) == 'R']
rssh_by_rt_code = rssh_by_rt_code[, .(rt_code_rssh = round(sum(budget, na.rm = TRUE))), by = c('file_name')]
rssh_by_rt_code[is.na(rt_code_rssh), rt_code_rssh:=0]

check_rssh <- merge(rssh_tests, rssh_by_rt_code, by = c('file_name'), all.y = TRUE)
unwritten_rssh_tests = check_rssh[is.na(correct_rssh)]

#unmerged_tests = anti_join(rssh_tests, rssh_by_rt_code, by='fileName')
if(nrow(unwritten_rssh_tests)!=0){
  print("ERROR: Some files are don't have a unit test written for RSSH. Review 'unwritten rssh tests'. ")
}
check_rssh = check_rssh[!is.na(correct_rssh)]

failed_rssh_tests = check_rssh[correct_rssh!=rt_code_rssh]
# ------------------------------------
# Print results and summary statistics
# ------------------------------------
{

  print("...")
failed_tests <- rbind(failed_tests_gtm, failed_tests_cod, failed_tests_uga, fill = TRUE)
  
if (nrow(failed_tests) != 0){
  print("Unit tests failed; review budget calculations.")
  print(paste0(nrow(failed_tests_gtm), " Guatemala tests failed (", round(nrow(failed_tests_gtm)/nrow(gtm_merge)*100, 2), "%); ", 
               nrow(gtm_merge), " out of ", nrow(gtm_tests), " tests are included in this percentage."))
  print(paste0(nrow(failed_tests_uga), " Uganda tests failed (", round(nrow(failed_tests_uga)/nrow(uga_merge)*100, 2), "%); ",
               nrow(uga_merge), " out of ", nrow(uga_tests), " tests are included in this percentage."))
  print(paste0(nrow(failed_tests_cod), " DRC tests failed (", round(nrow(failed_tests_cod)/nrow(cod_merge)*100, 2), "%); ", 
               nrow(cod_merge), " out of ", nrow(cod_tests), " tests are included in this percentage."))
  print("...")
  
} else {
  print("All unit tests passed.")
  print("...")
}

uga_filelist <- fread(paste0(j, "/Project/Evaluation/GF/resource_tracking/uga/grants/uga_budget_filelist.csv"))
cod_filelist <- fread(paste0(j, "/Project/Evaluation/GF/resource_tracking/cod/grants/cod_budget_filelist.csv"))
gtm_filelist <- fread(paste0(j, "/Project/Evaluation/GF/resource_tracking/gtm/grants/gtm_budget_filelist.csv"))

gtm_tests_nodup <- gtm_tests[!duplicated(fileName)]
cod_tests_nodup <- cod_tests[!duplicated(fileName)]
uga_tests_nodup <- uga_tests[!duplicated(fileName)]

gtm_tested_grants <- unique(gtm_tests[, .(fileName)])
cod_tested_grants <- unique(cod_tests[, .(fileName)])
uga_tested_grants <- unique(uga_tests[, .(fileName)])

print(paste0("Testing ", round(gtm_tests_nodup[format == "pudr", .N]/gtm_filelist[data_source == "pudr", .N]*100, 2), "% of PUDRs and ", 
             round(gtm_tests_nodup[format != "pudr", .N]/gtm_filelist[data_source != "pudr", .N]*100, 2), "% of budgets in Guatemala"))
print(paste0("Testing ", round(nrow(unique(gtm_filelist[grant_status=='active' & file_name%in%gtm_tested_grants$fileName, .(file_name)]))/nrow(unique(gtm_filelist[grant_status=='active', .(file_name)]))*100, 2), 
             "% of active files and ", round(nrow(unique(gtm_filelist[grant_status=='not_active' & file_name%in%gtm_tested_grants$fileName, .(file_name)]))/nrow(unique(gtm_filelist[grant_status=='not_active', .(file_name)]))*100, 2)
             , "% of not active files Guatemala"))
print("...")

print(paste0("Testing ", round(uga_tests_nodup[type == "pudr", .N]/uga_filelist[data_source == "pudr", .N]*100, 2), "% of PUDRs and ", 
             round(uga_tests_nodup[type != "pudr", .N]/uga_filelist[data_source != "pudr", .N]*100, 2), "% of budgets in Uganda"))
print(paste0("Testing ", round(nrow(unique(uga_filelist[grant_status=='active' & file_name%in%uga_tested_grants$fileName, .(file_name)]))/nrow(unique(uga_filelist[grant_status=='active', .(file_name)]))*100, 2), 
            "% of active files and ", round(nrow(unique(uga_filelist[grant_status=='not_active' & file_name%in%uga_tested_grants$fileName, .(file_name)]))/nrow(unique(uga_filelist[grant_status=='not_active', .(file_name)]))*100, 2)
             , "% of not active files Uganda"))
print("...")

print(paste0("Testing ", round(cod_tests_nodup[type == "pudr", .N]/cod_filelist[data_source == "pudr", .N]*100, 2), "% of PUDRs and ", 
             round(cod_tests_nodup[type != "pudr", .N]/cod_filelist[data_source != "pudr", .N]*100, 2), "% of budgets in DRC"))
print(paste0("Testing ", round(nrow(unique(cod_filelist[grant_status=='active' & file_name%in%cod_tested_grants$fileName, .(file_name)]))/nrow(unique(cod_filelist[grant_status=='active', .(file_name)]))*100, 2), 
"% of active files and ", round(nrow(unique(cod_filelist[grant_status=='not_active' & file_name%in%cod_tested_grants$fileName, .(file_name)]))/nrow(unique(cod_filelist[grant_status=='not_active', .(file_name)]))*100, 2)
             , "% of not active files DRC"))
print("...")


total_tests <- nrow(cod_tests) + nrow(gtm_tests) + nrow(uga_tests)
total_merges <- nrow(cod_merge) + nrow(gtm_merge) + nrow(uga_merge)
total_unmerged <- total_tests - total_merges

print(paste0("Total merged tests that failed either a budget or expenditure calculation: ", round(nrow(failed_tests)/total_merges*100, 2), "%")) #Total failed tests over total of rows of merge datasets. 
print(paste0("Total tests that failed, including non-merges: ", round((total_unmerged + nrow(failed_tests))/total_tests*100, 2), "%")) #/total tests 
print(paste0("Total tests merged: ", total_merges, " out of ", total_tests, ", or ", round((total_merges/total_tests)*100, 2), "%"))
print(paste0("Total RSSH tests failed: ", nrow(failed_rssh_tests)))
print(paste0("Percentage of RSSH tests failed: ", round(nrow(failed_rssh_tests)/nrow(check_rssh)*100, 2), "%"))


rm(list= ls()[!(ls() %in% c('failed_tests','failed_tests_cod', 'failed_tests_gtm', 'failed_tests_uga'
                            , 'gtm_tests', 'gtm_merge', 'cod_tests', 'cod_merge', 'uga_tests', 'uga_merge', 
                            'unmerged_cod_tests', 'unmerged_gtm_tests', 'unmerged_uga_tests', 
                             'failed_rssh_tests', 'unwritten_rssh_tests'))])


}
