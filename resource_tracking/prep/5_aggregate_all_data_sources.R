# ----------------------------------------------
# AUTHOR: Emily Linebarger, based on code from Irena Chen 
# PURPOSE: Binds together resource tracking prepped data 
#   into six key datasets: 
#   1. Final GF budgets
#   2. Final GF expenditures
#   3. GF budget iterations
#   4. Government health expenditure
#   5. Financing global health actuals
#   6. Financing global health estimates. 
#
# These data can then be used to conduct analysis. 
# DATE: Last updated February 2019.  
# ----------------------------------------------


#---------------------------------------
#To do list for this code: 
# - Get SICOIN and FGH running 
# - Remove 'fill = TRUE' from each Rbind- data should have same column names. 


#---------------------------------------

<<<<<<< HEAD
dir = paste0(j, "/Project/Evaluation/GF/resource_tracking")
cod_prepped <- paste0(dir, "/_gf_files_gos/cod/prepped_data/")
gtm_prepped <- paste0(dir, "/_gf_files_gos/gtm/prepped_data/")
uga_prepped <- paste0(dir, "/_gf_files_gos/uga/prepped_data/")

final_write <- paste0(dir, "/multi_country/mapping/")
=======
cod_prepped = paste0(j, "/Project/Evaluation/GF/resource_tracking/_gf_files_gos/cod/prepped_data/")
gtm_prepped = paste0(j, "/Project/Evaluation/GF/resource_tracking/_gf_files_gos/gtm/prepped_data/")
uga_prepped = paste0(j, "/Project/Evaluation/GF/resource_tracking/_gf_files_gos/uga/prepped_data/")
sen_prepped = paste0(j, "/Project/Evaluation/GF/resource_tracking/_gf_files_gos/sen/prepped_data/")
>>>>>>> develop

final_write = paste0(j, "/Project/Evaluation/GF/resource_tracking/_gf_files_gos/combined_prepped_data/")

# --------------------------------------------
# Load the prepped GOS data - to be used for both 
# final budgets and final expenditures 
#----------------------------------------------
<<<<<<< HEAD
gos_data <- readRDS(paste0(dir, "/_gf_files_gos/gos/prepped_data/prepped_gos_data.rds"))
=======
gos_data = readRDS(paste0(gos_prepped, "prepped_gos_data.rds"))
>>>>>>> develop
            
#-----------------------------------------------------------
# 1. FINAL GF BUDGETS - ONLY HAVE GTM, UGA, AND DRC FOR NOW
#-----------------------------------------------------------
# Merge all 3 countries 
final_budgets_cod = readRDS(paste0(cod_prepped, "final_budgets.rds"))
final_budgets_gtm = readRDS(paste0(gtm_prepped, "final_budgets.rds"))
final_budgets_uga = readRDS(paste0(uga_prepped, "final_budgets.rds"))
final_budgets_sen = readRDS(paste0(sen_prepped, "final_budgets.rds"))

#-------------------------------------------
# Check for duplicate budget quarters - 
# this should also happen in prep process. 
# ------------------------------------------

setDT(final_budgets_cod)
check_qtr_cod = final_budgets_cod[, .(start_date, grant, file_name)]
check_qtr_cod = unique(check_qtr_cod) #Remove duplicate lines in module, intervention, etc. 
#Make sure there are no duplicates in start date and grant number that are coming from different files. 
check_qtr_cod = check_qtr_cod[duplicated(check_qtr_cod, by = c("start_date", "grant")), ]
check_qtr_cod = merge(check_qtr_cod, final_budgets_cod[, .(start_date, grant, file_name)], by=c('start_date', 'grant'), all.x=TRUE)
check_qtr_cod = unique(check_qtr_cod)
if (nrow(check_qtr_cod)!=0){
  print("Warning: There are overlapping budget quarters in DRC. Review data to prevent double-counting.")
  print(check_qtr_cod)
}

setDT(final_budgets_gtm)
check_qtr_gtm = final_budgets_gtm[, .(start_date, grant, file_name)]
check_qtr_gtm = unique(check_qtr_gtm) #Remove duplicate lines in module, intervention, etc. 
#Make sure there are no duplicates in start date and grant number that are coming from different files. 
check_qtr_gtm = check_qtr_gtm[duplicated(check_qtr_gtm, by = c("start_date", "grant")), ]
check_qtr_gtm = merge(check_qtr_gtm, final_budgets_gtm[, .(start_date, grant, file_name)], by=c('start_date', 'grant'), all.x=TRUE)
check_qtr_gtm = unique(check_qtr_gtm)
if (nrow(check_qtr_gtm)!=0){
  print("Warning: There are overlapping budget quarters in Guatemala. Review data to prevent double-counting.")
  print(check_qtr_gtm)
}

setDT(final_budgets_uga)
check_qtr_uga = final_budgets_uga[, .(start_date, grant, file_name)]
check_qtr_uga = unique(check_qtr_uga) #Remove duplicate lines in module, intervention, etc. 
#Make sure there are no duplicates in start date and grant number that are coming from different files. 
check_qtr_uga = check_qtr_uga[duplicated(check_qtr_uga, by = c("start_date", "grant")), ]
check_qtr_uga = merge(check_qtr_uga, final_budgets_uga[, .(start_date, grant, file_name)], by=c('start_date', 'grant'), all.x=TRUE)
check_qtr_uga = unique(check_qtr_uga)
if (nrow(check_qtr_uga)!=0){
  print("Warning: There are overlapping budget quarters in Uganda. Review data to prevent double-counting.")
  print(check_qtr_uga)
}

setDT(final_budgets_sen)
check_qtr_sen = final_budgets_sen[, .(start_date, grant, file_name)]
check_qtr_sen = unique(check_qtr_sen) #Remove duplicate lines in module, intervention, etc. 
#Make sure there are no duplicates in start date and grant number that are coming from different files. 
check_qtr_sen = check_qtr_sen[duplicated(check_qtr_sen, by = c("start_date", "grant")), ]
check_qtr_sen = merge(check_qtr_sen, final_budgets_sen[, .(start_date, grant, file_name)], by=c('start_date', 'grant'), all.x=TRUE)
check_qtr_sen = unique(check_qtr_sen)
if (nrow(check_qtr_sen)!=0){
  print("Warning: There are overlapping budget quarters in Senegal. Review data to prevent double-counting.")
  print(check_qtr_sen)
}

#Bind budgets together
final_budgets = rbind(final_budgets_cod, final_budgets_gtm, final_budgets_uga, final_budgets_sen) 
final_budgets$start_date = as.Date(final_budgets$start_date, "%Y-%m-%d")

#Manually edit grant numbers in GOS to match our labeling - EMILY THIS SHOULD BE DONE BACK IN THE PREP CODE. 
final_budgets[grant == 'GTM-T-UPCOMING', grant:='GTM-T-MSPAS']
final_budgets[grant == 'GTM-M-UPCOMING', grant:='GTM-M-MSPAS']
final_budgets[grant == 'UGD-708-G13-H', grant:='UGA-708-G13-H']

<<<<<<< HEAD
#Check that all grant numbers in GF budgets correlate to GOS grant numbers
fpm_grants = unique(final_budgets[order(grant)]$grant)
gos_grants = unique(gos_data[order(grant)]$grant)
mismatches = fpm_grants[!fpm_grants %in% gos_grants]
print(paste('The following grants are only available in FPM data, not GOS:', paste(mismatches, collapse=', ')))

# Drop any previous grants from the FPM data
final_budgets = final_budgets[grant_period!='2015-2017']

# Identify if GOS has any of the current grants in it
# (some grants have continued from the past with the same name, so grant_period is necessary)
fpm_grant_periods = unique(final_budgets[,c('grant','grant_period')])
gos_grant_periods = unique(gos_data[,c('grant','grant_period')])
overlap = merge(fpm_grant_periods, gos_grant_periods)

# Drop any current grants from the GOS data
if (nrow(overlap)>0) { 
	for(i in seq(nrow(overlap))) {
		gos_data = gos_data[!which(grant==overlap$grant & grant_period==overlap$grant_period)]
	}
}

# Append together FPM and GOS data
final_budgets <- rbind(final_budgets, gos_data, fill = TRUE) 

# Verify data 
na_year <- final_budgets[is.na(year)]
=======
#Wherever there is a grant quarter in the final budgets that doesn't exist in GOS, take that whole grant for the grant 
# period and replace the GOS with the final budgets data. 
gos_data[, start_date:=as.Date(start_date)]

#Bind the files together. 
gos_prioritized_budgets = rbind(final_budgets, gos_data, fill = TRUE) #There are some columns that don't exist in both sources, so fill = TRUE

#See if there are any data gaps or data overlaps in the final file 
budget_overlaps = gos_prioritized_budgets[, .(start_date, grant, file_name)]
budget_overlaps = unique(budget_overlaps) #Remove duplicate lines in module, intervention, etc. 
#Make sure there are no duplicates in start date and grant number that are coming from different files. 
budget_overlaps = budget_overlaps[duplicated(budget_overlaps, by = c("start_date", "grant")), ]
budget_overlaps = merge(budget_overlaps, gos_prioritized_budgets[, .(start_date, grant, file_name)], by=c('start_date', 'grant'), all.x=TRUE)
budget_overlaps = unique(budget_overlaps)
if (nrow(budget_overlaps)!=0){
  print("Warning: There are duplicate budget quarters for the same grant in 'final_budgets'. Review raw data to prevent double-counting.")
  print(budget_overlaps[order(grant, start_date)])
}

#Handle overlaps if they're coming from different data sources - we always want to keep GOS, unless GOS doesn't cover the entire grant period, 
#in which case we replace all data from that grant period with the detaiiled budget. EKL 5/2/19, decision by David Phillips
problem_overlaps = unique(gos_prioritized_budgets[, .(start_date, grant, data_source)]) #Overlaps by data source are not ok. 
problem_overlaps = unique(problem_overlaps) #Remove duplicate lines in module, intervention, etc. 
#Make sure there are no duplicates in start date and grant number that are coming from different files. 
problem_overlaps = problem_overlaps[duplicated(problem_overlaps, by = c("start_date", "grant")), ]
problem_overlaps = merge(problem_overlaps, gos_prioritized_budgets[, .(start_date, grant, data_source)], by=c('start_date', 'grant'), all.x=TRUE)
problem_overlaps = unique(problem_overlaps)
if (nrow(problem_overlaps)!=0){
  print("Warning: There are duplicate problem quarters for the same grant in 'final_problems'. Review raw data to prevent double-counting.")
  print(problem_overlaps[order(grant, start_date)])
  
  #Does GOS cover the entire grant period? 
  #Compare what dates you have covered under budgets vs. GOS
  budget_coverage = unique(final_budgets[grant%in%problem_overlaps$grant, .(start_date, grant, file_name)])
  budget_coverage = budget_coverage[order(file_name, start_date)]
  
  gos_coverage = unique(gos_data[grant%in%problem_overlaps$grant, .(start_date, grant, file_name)])
  gos_coverage = gos_coverage[order(file_name, start_date)]
  gos_coverage[, in_gos:=TRUE]
  gos_coverage = merge(budget_coverage, gos_coverage, all.x=TRUE, by=c('start_date', 'grant'))
  
  #For a given file_name.x in gos_coverage, the condition you need to worry about is when 
  # GOS matches for only part of the grant period. Then, drop GOS and keep the budget (duplicate grant in 'drop_condition' below.)
  drop_condition = unique(gos_coverage[, .(grant, file_name.x, file_name.y, in_gos)])
  drop_condition = drop_condition[duplicated(drop_condition, by=c('grant', 'file_name.x'))]
  
  #Now, grab all the budget quarters for that file in GOS, and drop them. 
  files_overwriting_gos = unique(drop_condition$file_name.x)
  to_drop_gos = unique(gos_coverage[file_name.x%in%files_overwriting_gos, .(grant, start_date)])
  if (nrow(to_drop_gos)!=0){
    print("There are overlaps in GOS and final budgets where GOS does not cover the whole grant period - dropping the following rows for GOS")
    print(to_drop_gos)
    for (i in nrow(to_drop_gos)){
      gos_prioritized_budgets = gos_prioritized_budgets[!(grant==to_drop_gos$grant[i] & start_date==to_drop_gos$start_date[i])]
    }
  }
}

#Look for what might be data gaps between GOS and budget data (EMILY - WOULD BE GOOD TO EXPAND THIS CHECK TO LOOK FOR DATA GAPS IN GENERAL)
gos_in_budgets = gos_data[grant%in%final_budgets$grant, .(start_date, grant)] 
budget_dates = final_budgets[, .(budget_start = min(start_date)), by='grant']
gos_in_budgets = gos_in_budgets[, .(gos_end = max(start_date)), by='grant']
date_check = merge(gos_in_budgets, budget_dates, by=c('grant'))
date_check = date_check[gos_end!=budget_start]
if (nrow(date_check)!=0){
  print("Warning: There are potential reporting gaps between GOS and final budgets. Review output 'Gaps between budgets and GOS' in GOS folder.")
  write.csv(date_check, paste0(dir, "_gf_files_gos/gos/Gaps between budgets and GOS.csv"), row.names=FALSE)
}

# Verify data 
na_year = gos_prioritized_budgets[is.na(year)]
>>>>>>> develop
stopifnot(nrow(na_year)==0)

#Check that you've got the current grants right. 
all_current_grants = unique(gos_prioritized_budgets[current_grant==TRUE, .(grant, grant_period, file_name)])
expected_current_grants = length(current_gtm_grants) + length(current_uga_grants) + length(current_cod_grants) + length(current_sen_grants)
if (nrow(all_current_grants)!=expected_current_grants){
  print("ERROR: Not all current grants are marked with the 'current_grant' flag in budgets.")
}

# Write data 
write.csv(gos_prioritized_budgets, paste0(final_write, "final_budgets.csv"), row.names = FALSE)
saveRDS(gos_prioritized_budgets, paste0(final_write, "final_budgets.rds"))


#----------------------------------
# 2. FINAL GF EXPENDITURES
#----------------------------------
# Merge all 3 countries 
final_expenditures_cod = readRDS(paste0(cod_prepped, "final_expenditures.rds"))
final_expenditures_gtm = readRDS(paste0(gtm_prepped, "final_expenditures.rds"))
final_expenditures_uga = readRDS(paste0(uga_prepped, "final_expenditures.rds"))
final_expenditures_sen = readRDS(paste0(sen_prepped, "final_expenditures.rds"))

#-------------------------------------------
# Hacky fix for duplicate budget quarters -- 
# This should be removed and handled in file prep process!! 
# ------------------------------------------

setDT(final_expenditures_cod)
check_qtr_cod = final_expenditures_cod[, .(start_date, grant, file_name)]
check_qtr_cod = unique(check_qtr_cod) #Remove duplicate lines in module, intervention, etc. 
#Make sure there are no duplicates in start date and grant number that are coming from different files. 
check_qtr_cod = check_qtr_cod[duplicated(check_qtr_cod, by = c("start_date", "grant")), ]
check_qtr_cod = merge(check_qtr_cod, final_expenditures_cod[, .(start_date, grant, file_name)], by=c('start_date', 'grant'), all.x=TRUE)
check_qtr_cod = unique(check_qtr_cod)
if (nrow(check_qtr_cod)!=0){
  print("Warning: There are overlapping expenditure quarters in DRC. Review data to prevent double-counting.")
  print(check_qtr_cod)
}

setDT(final_expenditures_gtm)
check_qtr_gtm = final_expenditures_gtm[, .(start_date, grant, file_name)]
check_qtr_gtm = unique(check_qtr_gtm) #Remove duplicate lines in module, intervention, etc. 
#Make sure there are no duplicates in start date and grant number that are coming from different files. 
check_qtr_gtm = check_qtr_gtm[duplicated(check_qtr_gtm, by = c("start_date", "grant")), ]
check_qtr_gtm = merge(check_qtr_gtm, final_expenditures_gtm[, .(start_date, grant, file_name)], by=c('start_date', 'grant'), all.x=TRUE)
check_qtr_gtm = unique(check_qtr_gtm)
if (nrow(check_qtr_gtm)!=0){
  print("Warning: There are overlapping expenditure categories in Guatemala. Review data to prevent double-counting.")
  print(check_qtr_gtm)
}

setDT(final_expenditures_uga)
check_qtr_uga = final_expenditures_uga[, .(start_date, grant, file_name)]
check_qtr_uga = unique(check_qtr_uga) #Remove duplicate lines in module, intervention, etc. 
#Make sure there are no duplicates in start date and grant number that are coming from different files. 
check_qtr_uga = check_qtr_uga[duplicated(check_qtr_uga, by = c("start_date", "grant")), ]
check_qtr_uga = merge(check_qtr_uga, final_expenditures_uga[, .(start_date, grant, file_name)], by=c('start_date', 'grant'), all.x=TRUE)
check_qtr_uga = unique(check_qtr_uga)
if (nrow(check_qtr_uga)!=0){
  print("Warning: There are overlapping budget quarters in Uganda. Review data to prevent double-counting.")
  print(check_qtr_uga)
}

setDT(final_expenditures_sen)
check_qtr_sen = final_expenditures_sen[, .(start_date, grant, file_name)]
check_qtr_sen = unique(check_qtr_sen) #Remove duplicate lines in module, intervention, etc. 
#Make sure there are no duplicates in start date and grant number that are coming from different files. 
check_qtr_sen = check_qtr_sen[duplicated(check_qtr_sen, by = c("start_date", "grant")), ]
check_qtr_sen = merge(check_qtr_sen, final_expenditures_sen[, .(start_date, grant, file_name)], by=c('start_date', 'grant'), all.x=TRUE)
check_qtr_sen = unique(check_qtr_sen)
if (nrow(check_qtr_sen)!=0){
  print("Warning: There are overlapping budget quarters in sennda. Review data to prevent double-counting.")
  print(check_qtr_sen)
}
#Bind expenditures together
final_expenditures = rbind(final_expenditures_cod, final_expenditures_gtm, final_expenditures_uga, final_expenditures_sen, fill = TRUE) 

#Wherever there is a grant quarter in the final budgets that doesn't exist in GOS, take that whole grant for the grant 
# period and replace the GOS with the final budgets data. 
gos_data[, start_date:=as.Date(start_date)]

#Bind the files together. 
gos_prioritized_expenditures = rbind(final_expenditures, gos_data, fill = TRUE) #There are some columns that don't exist in both sources, so fill = TRUE

#See if there are any data gaps or data overlaps in the final file 
expenditure_overlaps = gos_prioritized_expenditures[, .(start_date, grant, file_name)]
expenditure_overlaps = unique(expenditure_overlaps) #Remove duplicate lines in module, intervention, etc. 
#Make sure there are no duplicates in start date and grant number that are coming from different files. 
expenditure_overlaps = expenditure_overlaps[duplicated(expenditure_overlaps, by = c("start_date", "grant")), ]
expenditure_overlaps = merge(expenditure_overlaps, gos_prioritized_expenditures[, .(start_date, grant, file_name)], by=c('start_date', 'grant'), all.x=TRUE)
expenditure_overlaps = unique(expenditure_overlaps)
if (nrow(expenditure_overlaps)!=0){
  print("Warning: There are duplicate expenditure quarters for the same grant in 'final_expenditures'. Review raw data to prevent double-counting.")
  print(expenditure_overlaps[order(grant, start_date)])
}

#Handle overlaps if they're coming from different data sources - we always want to keep GOS, unless GOS doesn't cover the entire grant period, 
#in which case we replace all data from that grant period with the detaiiled expenditure. EKL 5/2/19, decision by David Phillips
# problem_overlaps = unique(gos_prioritized_expenditures[, .(start_date, grant, data_source)]) #Overlaps by data source are not ok. 
# problem_overlaps = unique(problem_overlaps) #Remove duplicate lines in module, intervention, etc. 
# #Make sure there are no duplicates in start date and grant number that are coming from different files. 
# problem_overlaps = problem_overlaps[duplicated(problem_overlaps, by = c("start_date", "grant")), ]
# problem_overlaps = merge(problem_overlaps, gos_prioritized_expenditures[, .(start_date, grant, data_source)], by=c('start_date', 'grant'), all.x=TRUE)
# problem_overlaps = unique(problem_overlaps)
# if (nrow(problem_overlaps)!=0){
#   print("Warning: There are duplicate problem quarters for the same grant in 'final_problems'. Review raw data to prevent double-counting.")
#   print(problem_overlaps[order(grant, start_date)])
#   
#   #Does GOS cover the entire grant period? 
#   #Compare what dates you have covered under expenditures vs. GOS
#   expenditure_coverage = unique(final_expenditures[grant%in%problem_overlaps$grant, .(start_date, grant, file_name)])
#   expenditure_coverage = expenditure_coverage[order(file_name, start_date)]
#   
#   gos_coverage = unique(gos_data[grant%in%problem_overlaps$grant, .(start_date, grant, file_name)])
#   gos_coverage = gos_coverage[order(file_name, start_date)]
#   gos_coverage[, in_gos:=TRUE]
#   gos_coverage = merge(expenditure_coverage, gos_coverage, all.x=TRUE, by=c('start_date', 'grant'))
#   
#   #For a given file_name.x in gos_coverage, the condition you need to worry about is when 
#   # GOS matches for only part of the grant period. Then, drop GOS and keep the expenditure (duplicate grant in 'drop_condition' below.)
#   drop_condition = unique(gos_coverage[, .(grant, file_name.x, file_name.y, in_gos)])
#   drop_condition = drop_condition[duplicated(drop_condition, by=c('grant', 'file_name.x'))]
#   
#   #Now, grab all the expenditure quarters for that file in GOS, and drop them. 
#   files_overwriting_gos = unique(drop_condition$file_name.x)
#   to_drop_gos = unique(gos_coverage[file_name.x%in%files_overwriting_gos, .(grant, start_date)])
#   if (nrow(to_drop_gos)!=0){
#     print("There are overlaps in GOS and final expenditures where GOS does not cover the whole grant period - dropping the following rows for GOS")
#     print(to_drop_gos)
#     for (i in nrow(to_drop_gos)){
#       gos_data = gos_data[!(grant==to_drop_gos$grant[i] & start_date==to_drop_gos$start_date[i])]
#     }
#   }
# }

#Look for what might be data gaps between GOS and expenditure data (EMILY - WOULD BE GOOD TO EXPAND THIS CHECK TO LOOK FOR DATA GAPS IN GENERAL)
gos_in_expenditures = gos_data[grant%in%final_expenditures$grant, .(start_date, grant)] 
expenditure_dates = final_expenditures[, .(expenditure_start = min(start_date)), by='grant']
gos_in_expenditures = gos_in_expenditures[, .(gos_end = max(start_date)), by='grant']
date_check = merge(gos_in_expenditures, expenditure_dates, by=c('grant'))
date_check = date_check[gos_end!=expenditure_start]
if (nrow(date_check)!=0){
  print("Warning: There are potential reporting gaps between GOS and final expenditures. Review output 'Gaps between expenditures and GOS' in GOS folder.")
  write.csv(date_check, paste0(dir, "_gf_files_gos/gos/Gaps between expenditures and GOS.csv"), row.names=FALSE)
}

# Verify data 
na_year = gos_prioritized_expenditures[is.na(year)]
stopifnot(nrow(na_year)==0)

#Check that you've got the current grants right. 
all_current_grants = unique(gos_prioritized_expenditures[current_grant==TRUE, .(grant, grant_period, file_name)])
expected_current_grants = length(current_gtm_grants) + length(current_uga_grants) + length(current_cod_grants) + length(current_sen_grants)
if (nrow(all_current_grants)!=expected_current_grants){
  print("ERROR: Not all current grants are marked with the 'current_grant' flag in expenditures.")
}

# Write data 
# write.csv(gos_prioritized_expenditures, paste0(final_write, "final_expenditures.csv"), row.names = FALSE)
saveRDS(gos_prioritized_expenditures, paste0(final_write, "final_expenditures.rds"))

#----------------------------------
# 3. GF FILE ITERATIONS
#----------------------------------
all_gf_cod = readRDS(paste0(dir, "_gf_files_gos/cod/prepped_data/budget_pudr_iterations.rds"))
all_gf_uga = readRDS(paste0(dir, "_gf_files_gos/uga/prepped_data/budget_pudr_iterations.rds"))  
all_gf_gtm = readRDS(paste0(dir, "_gf_files_gos/gtm/prepped_data/budget_pudr_iterations.rds"))
all_gf_sen = readRDS(paste0(dir, "_gf_files_gos/sen/prepped_data/budget_pudr_iterations.rds"))

all_files = list(all_gf_cod, all_gf_gtm, all_gf_uga, all_gf_sen)
all_gf_files = rbindlist(all_files, use.names = TRUE, fill = TRUE)

#Write data 
saveRDS(all_gf_files, paste0(final_write, "budget_pudr_iterations.rds"))
write.csv(all_gf_files, paste0(final_write, "budget_pudr_iterations.csv"), row.names = FALSE)

#----------------------------------
# 4. GOVERNMENT HEALTH EXPENDITURE
#----------------------------------
# sicoin_data = data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/prepped_sicoin_data.csv"
#                                    ,fileEncoding="latin1"))
# 
# ##change the start dates from factors to dates: 
# sicoin_data$start_date = as.Date(sicoin_data$start_date,"%Y-%m-%d")
# totalGtm$start_date = as.Date(totalGtm$start_date,"%Y-%m-%d")
# sicoin_data$grant_period = year(sicoin_data$start_date)


#----------------------------------
# 5. FGH ACTUALS 
#----------------------------------
# fgh_data = data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/total_prepped_fgh_total.csv", 
#                                 fileEncoding = "latin1"))
# 
# ##change the dates into date format: 
# fgh_data$start_date = as.Date(fgh_data$start_date, "%Y-%m-%d")
# fgh_data$end_date = as.Date(fgh_data$end_date, "%Y-%m-%d")
# 
# #remove "total" value
# fgh_data = fgh_data[!grepl("total", module)]
# fgh_data$grant_period = year(fgh_data$start_date)



#----------------------------------
# 6. FGH ESTIMATES
#----------------------------------

                             
# --------------------------------------------
#Check for duplicates (the GOS data has one row that is duplicated twice, but it's fine to just aggregate them)
# --------------------------------------------
# dups=totalData[duplicated(totalData) | duplicated(totalData, fromLast=TRUE)]
# 
# ##aggregate duplicates: 
# byVars = names(totalData)[!names(totalData)%in%c('budget', 'disbursement', 'expenditure')]
# totalData= totalData[, list(budget=sum(na.omit(budget)), disbursement=sum(na.omit(disbursement)),expenditure=sum(na.omit(expenditure))), by=byVars]
# 
# # --------------------------------------------
# ##export to correct folder: 
# # --------------------------------------------
# ## date variables can get messed up if we export them, so change them to 'character'
# totalData$start_date = as.character(totalData$start_date)
# totalData$end_date = as.character(totalData$end_date)
# 
# totalData$loc_name = ifelse(totalData$loc_name  == "Uganda", "uga", ifelse(totalData$loc_name == "Guatemala", "gtm", ifelse(totalData$loc_name == 'Congo (Democratic Republic)', "cod", tolower(totalData$loc_name))))
# 
# totalData$budget = as.numeric(totalData$budget)
# totalData$expenditure = as.numeric(totalData$expenditure)
# totalData$disbursement = as.numeric(totalData$disbursement)
# 
# totalData$year = as.Date(totalData$year, "%Y") #To check- where are we getting NA in year?? 
# 
# write.csv(totalData, "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/total_resource_tracking_data.csv", row.names = FALSE)
# 
# # --------------------------------------------
# # This produces a dataset that prioritizes FPM data (drops GOS/PUDR is it overlap)
# # --------------------------------------------
# 
# ##pudrs overlap with the FPM budgets - drop this so we don't double count 
# fpmGtm = totalGtm[!(data_source=="pudr")] ##all of the PUDRs we have correspond to available FPM 
# fpmUga = totalUga[!(data_source=="pudr"&year>2015)] ##we have a lot of recent PUDRs that overlap with FPM 
# fpmCod =  totalCod[!(data_source=="pudr")] #all of the PUDRs we have correspond to available FPM 
# 
# cleanData = rbind(fpmGtm, fpmUga, fpmCod)
# cleanData[,end_date:=start_date+period-1]
# 
# ## some of the FPM data is missing so we'll fill it in w/ the GOS data 
# 
# gos_cod= gos_data[country=="Congo (Democratic Republic)"]
# gos_uga = gos_data[country=="Uganda"]
# gos_gtm = gos_data[country=="Guatemala"]
# 
# ##as we get more FPM data, make sure to check that these years/diseases are still true: 
# ## (realistically, we're probably not going to get any historical data - pre 2015) 
# gos_cod = gos_cod[(disease=="hss")|(disease=="hiv"&year<2012|year%in%c(2013, 2014))|(disease=="malaria"&(year<=2014))|(disease=="tb"&grant!="COD-T-MOH")]
# gos_uga = gos_uga[(disease=="hiv"&(year<2011|year==2014))|(disease=="malaria"&(year%in%c(2013,2014)|year<2012))|(disease=="tb"&(year<2012|year%in%c(2013,2014)))]
# gos_gtm = gos_gtm[(disease=="hiv"&year<2011)|(disease=="malaria"&year<2011)|(disease=="tb"&(year<2011|year==2015))]
# 
# 
# totalGos = rbind(gos_uga, gos_cod, gos_gtm)
# 
# cleanData = rbind(cleanData, totalGos, fill = TRUE)
# 
# ## add in a field that distinguishes between actual numbers and forecasted numbers (FGH)
# cleanData$fin_data_type = "actual"
# 
# # --------------------------------------------
# #  Since FGH data is not at grant level (or disease level), we might as well include it in this data
# ##since there is no danger of double-counting when summing by disease/grant
# # --------------------------------------------
# cleanData = rbind(cleanData, fgh_data, fill = TRUE)
# 
# 
# ## date variables can get messed up if we export them, so change them to 'character'
# cleanData$start_date = as.character(cleanData$start_date)
# cleanData$end_date = as.character(cleanData$end_date)
# 
# # --------------------------------------------
# # check for duplicates again: 
# # --------------------------------------------
# dups=cleanData[duplicated(cleanData) | duplicated(cleanData , fromLast=TRUE)]
# 
# byVars = names(cleanData )[!names(cleanData )%in%c('budget', 'disbursement', 'expenditure')]
# cleanData = cleanData[, list(budget=sum(na.omit(budget)), disbursement=sum(na.omit(disbursement)),expenditure=sum(na.omit(expenditure))), by=byVars]
# 
# # --------------------------------------------
# ##export to correct folder: 
# # --------------------------------------------
# 
# 
# write.csv(cleanData, "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/cleaned_total_data.csv", row.names = FALSE)

