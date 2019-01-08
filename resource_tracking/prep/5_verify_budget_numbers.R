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
# - is this checking expenditure for PUDRs as well? 
# - add correct_exp_sum to all unit tests for PUDRs
# - how can you verify that all tests are merging correctly? 
# - It looks like you're just missing a lot of files in Uganda??? 
# - Are we testing each prep function for each country? 
# 

#--------------------------


# input files
source("C:/Users/elineb/Documents/gf/resource_tracking/prep/shared_mapping_functions.R")
file_dir <- "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/"
file_iterations <- read.csv(paste0(file_dir, "budget_pudr_iterations.csv"))
setDT(file_iterations)
file_iterations[, start_date:=as.Date(start_date, format = "%Y-%m-%d")]

# -----------------------
# Guatemala file prep 
# -----------------------
{
gtm_budgets = file_iterations[loc_name == "gtm"]
gtm_budgets = check_budgets_pudrs(gtm_budgets)

# -----------------------
# Guatemala unit tests 
# -----------------------

gtm_tests<-fread("J:/Project/Evaluation/GF/resource_tracking/multi_country/gf/testing_budget_numbers/gtm_tests.csv")
gtm_tests$start_date <- gsub("\"\"", "\"", gtm_tests$start_date, fixed = TRUE)
gtm_tests$start_date <- substring(gtm_tests$start_date, 2, 11)
gtm_tests$start_date <- as.Date(gtm_tests$start_date, format = "%Y-%m-%d")
gtm_merge <- merge(gtm_tests, gtm_budgets, by = c('start_date', 'fileName')) 

if(nrow(gtm_merge) != nrow(gtm_tests)){
  print("ERROR: Not all Guatemala tests merged.")
}

gtm_merge$budget = round(gtm_merge$budget)

gtm_merge <- gtm_merge[, .(fileName, correct_bug_sum, budget, expenditure, start_date, data_source.x)]
gtm_merge$country <- "gtm" #For sorting out failed tests later if any. 

failed_budgets_gtm <- gtm_merge[correct_bug_sum != budget, ]
#failed_expenditures_gtm <- gtm_merge[correct_exp_sum != expenditure, ]
failed_tests_gtm = failed_budgets_gtm #Want to rbind with expenditure here. 

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

uga_tests<-fread("J:/Project/Evaluation/GF/resource_tracking/multi_country/gf/testing_budget_numbers/uga_tests.csv")
uga_tests$start_date <- as.Date(uga_tests$start_date)

uga_tests$correct_bug_sum <- gsub("[[:punct:]]", "", uga_tests$correct_bug_sum)
uga_tests$correct_exp_sum <- gsub("[[:punct:]]", "", uga_tests$correct_exp_sum)
uga_tests$correct_bug_sum <- as.numeric(uga_tests$correct_bug_sum)
uga_tests$correct_exp_sum <- as.numeric(uga_tests$correct_exp_sum)

uga_merge <- merge(uga_tests, uga_budgets, by = c('start_date', 'fileName')) 
if(nrow(uga_merge) != nrow(uga_tests)){
  print("ERROR: Not all Uganda tests merged.")
}

uga_merge$budget = round(uga_merge$budget)

uga_merge <- uga_merge[, .(fileName, correct_bug_sum, budget, expenditure, start_date, data_source.x)]
uga_merge$country <- "uga" #For sorting out failed tests later if any. 

#Want to add expenditure here. 
failed_tests_uga <- uga_merge[correct_bug_sum != budget, ]

}

# ------------------
# DRC File prep 
# ------------------
{
dt_drc = file_iterations[loc_name == "cod"]
cod_budgets = check_budgets_pudrs(dt_drc)

# ------------------
# DRC Unit tests 
# ------------------

cod_tests<-fread("J:/Project/Evaluation/GF/resource_tracking/multi_country/gf/testing_budget_numbers/cod_tests.csv")
cod_tests$start_date <- as.Date(cod_tests$start_date, format = "%m/%d/%Y")

cod_tests$correct_bug_sum <- substring(cod_tests$correct_bug_sum, 2)
cod_tests$correct_bug_sum <- gsub("[[:punct:]]", "", cod_tests$correct_bug_sum)
cod_tests$correct_bug_sum <- as.numeric(cod_tests$correct_bug_sum)

cod_merge <- merge(cod_tests, cod_budgets, by = c('start_date', 'fileName')) 
if(nrow(cod_merge) != nrow(cod_tests)){
  print("ERROR: Not all DRC tests merged.")
}

cod_merge$budget = round(cod_merge$budget)

cod_merge <- cod_merge[, .(fileName, correct_bug_sum, budget, expenditure, start_date, data_source.x)]
cod_merge$country <- "cod"

failed_tests_cod <- cod_merge[correct_bug_sum != budget, ]

}

# ------------------------------------
# Print results and summary statistics
# ------------------------------------
{

failed_tests <- rbind(failed_tests_gtm, failed_tests_cod)
  
if (nrow(failed_tests) != 0){
  print("Unit tests failed; review budget calculations.")
  print(paste0(nrow(failed_tests_gtm), " Guatemala tests failed (", round(nrow(failed_tests_gtm)/nrow(gtm_merge)*100, 2), "%)"))
  print(paste0(nrow(failed_tests_uga), " Uganda tests failed (", round(nrow(failed_tests_uga)/nrow(uga_merge)*100, 2), "%)"))
  print(paste0(nrow(failed_tests_cod), " DRC tests failed (", round(nrow(failed_tests_cod)/nrow(cod_merge)*100, 2), "%)"))
  print("...")
  
} else {
  print("All unit tests passed.")
  print("...")
}

uga_filelist <- fread("J:/Project/Evaluation/GF/resource_tracking/uga/grants/uga_budget_filelist.csv")
cod_filelist <- fread("J:/Project/Evaluation/GF/resource_tracking/cod/grants/cod_budget_filelist.csv")
gtm_filelist <- fread("J:/Project/Evaluation/GF/resource_tracking/gtm/grants/gtm_budget_filelist.csv")

gtm_tests_nodup <- gtm_tests[!duplicated(fileName)]
cod_tests_nodup <- cod_tests[!duplicated(fileName)]
uga_tests_nodup <- uga_tests[!duplicated(fileName)]

gtm_tested_grants <- as.vector(unique(gtm_tests[, .(grant_number)]))
cod_tested_grants <- as.vector(unique(cod_tests[, .(grant)]))
uga_tested_grants <- as.vector(unique(uga_tests[, .(grant)]))

print(paste0("Testing ", round(gtm_tests_nodup[format == "pudr", .N]/gtm_filelist[data_source == "pudr", .N]*100, 2), "% of PUDRs and ", 
             round(gtm_tests_nodup[format != "pudr", .N]/gtm_filelist[data_source != "pudr", .N]*100, 2), "% of budgets in Guatemala"))
print(paste0("Testing", nrow(unique(gtm_filelist[grant_status=='active' & grant %in% gtm_tested_grants, .(grant)]))/nrow(unique(gtm_filelist[grant_status=='active', .(grant)])), "% 
             of active grants and % of not active grants Guatemala"))
print("...")

print(paste0("Testing ", round(uga_tests_nodup[type == "pudr", .N]/uga_filelist[data_source == "pudr", .N]*100, 2), "% of PUDRs and ", 
             round(uga_tests_nodup[type != "pudr", .N]/uga_filelist[data_source != "pudr", .N]*100, 2), "% of budgets in Uganda"))
print("Testing % of active grants and % of not active grant Uganda")
print("...")

print(paste0("Testing ", round(cod_tests_nodup[type == "pudr", .N]/cod_filelist[data_source == "pudr", .N]*100, 2), "% of PUDRs and ", 
             round(cod_tests_nodup[type != "pudr", .N]/cod_filelist[data_source != "pudr", .N]*100, 2), "% of budgets in DRC"))
print("Testing % of active grants and % of not active grant DRC")
print("...")


}
