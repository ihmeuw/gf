# ----------------------------------------------
# AUTHOR: Emily Linebarger, based on code from Irena Chen 
# PURPOSE: Binds together resource tracking prepped data 
#   into four key datasets.
#   1. Final budgets 
#   2. Final expenditures, with quarter-overlap removed
#   3. Absorption, by PUDR version
#   4. All bound files (for unit-testing)
#
# These data can then be used to conduct analysis. 
# DATE: Last updated June 2019.  
# ----------------------------------------------


#---------------------------------------
#To do list for this code: 
# - Get SICOIN and FGH running 
# - Remove 'fill = TRUE' from each Rbind- data should have same column names. 


#---------------------------------------

cod_prepped = paste0(j, "/Project/Evaluation/GF/resource_tracking/_gf_files_gos/cod/prepped_data/")
gtm_prepped = paste0(j, "/Project/Evaluation/GF/resource_tracking/_gf_files_gos/gtm/prepped_data/")
uga_prepped = paste0(j, "/Project/Evaluation/GF/resource_tracking/_gf_files_gos/uga/prepped_data/")
sen_prepped = paste0(j, "/Project/Evaluation/GF/resource_tracking/_gf_files_gos/sen/prepped_data/")

final_write = paste0(j, "/Project/Evaluation/GF/resource_tracking/_gf_files_gos/combined_prepped_data/")

# --------------------------------------------
# Load the prepped GOS data - to be used for both 
# final budgets and final expenditures 
#----------------------------------------------
gos_data = readRDS(paste0(gos_prepped, "prepped_gos_data.rds"))
            
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
check_qtr_cod = final_budgets_cod[, .(start_date, grant, grant_period, file_name)]
check_qtr_cod = unique(check_qtr_cod) #Remove duplicate lines in module, intervention, etc. 
#Make sure there are no duplicates in start date and grant number that are coming from different files. 
check_qtr_cod = check_qtr_cod[duplicated(check_qtr_cod, by = c("start_date", "grant", 'grant_period')), ]
check_qtr_cod = merge(check_qtr_cod, final_budgets_cod[, .(start_date, grant, grant_period, file_name)], by=c('start_date', 'grant', 'grant_period'), all.x=TRUE)
check_qtr_cod = unique(check_qtr_cod)
if (nrow(check_qtr_cod)!=0){
  print("Warning: There are overlapping budget quarters in DRC. Review data to prevent double-counting.")
  print(check_qtr_cod)
}

setDT(final_budgets_gtm)
check_qtr_gtm = final_budgets_gtm[, .(start_date, grant, grant_period, file_name)]
check_qtr_gtm = unique(check_qtr_gtm) #Remove duplicate lines in module, intervention, etc. 
#Make sure there are no duplicates in start date and grant number that are coming from different files. 
check_qtr_gtm = check_qtr_gtm[duplicated(check_qtr_gtm, by = c("start_date", "grant", 'grant_period')), ]
check_qtr_gtm = merge(check_qtr_gtm, final_budgets_gtm[, .(start_date, grant, grant_period, file_name)], by=c('start_date', 'grant', 'grant_period'), all.x=TRUE)
check_qtr_gtm = unique(check_qtr_gtm)
if (nrow(check_qtr_gtm)!=0){
  print("Warning: There are overlapping budget quarters in Guatemala. Review data to prevent double-counting.")
  print(check_qtr_gtm)
}

setDT(final_budgets_uga)
check_qtr_uga = final_budgets_uga[, .(start_date, grant, grant_period, file_name)]
check_qtr_uga = unique(check_qtr_uga) #Remove duplicate lines in module, intervention, etc. 
#Make sure there are no duplicates in start date and grant number that are coming from different files. 
check_qtr_uga = check_qtr_uga[duplicated(check_qtr_uga, by = c("start_date", "grant", 'grant_period')), ]
check_qtr_uga = merge(check_qtr_uga, final_budgets_uga[, .(start_date, grant, grant_period, file_name)], by=c('start_date', 'grant', 'grant_period'), all.x=TRUE)
check_qtr_uga = unique(check_qtr_uga)
if (nrow(check_qtr_uga)!=0){
  print("Warning: There are overlapping budget quarters in Uganda. Review data to prevent double-counting.")
  print(check_qtr_uga)
}

setDT(final_budgets_sen)
check_qtr_sen = final_budgets_sen[, .(start_date, grant, grant_period, file_name)]
check_qtr_sen = unique(check_qtr_sen) #Remove duplicate lines in module, intervention, etc. 
#Make sure there are no duplicates in start date and grant number that are coming from different files. 
check_qtr_sen = check_qtr_sen[duplicated(check_qtr_sen, by = c("start_date", "grant", 'grant_period')), ]
check_qtr_sen = merge(check_qtr_sen, final_budgets_sen[, .(start_date, grant, grant_period, file_name)], by=c('start_date', 'grant', 'grant_period'), all.x=TRUE)
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

#Wherever there is a grant quarter in the final budgets that doesn't exist in GOS, take that whole grant for the grant 
# period and replace the GOS with the final budgets data. 
gos_data[, start_date:=as.Date(start_date)]

#Drop unneeded variables 
gos_data$lfa_exp_adjustment<-NULL

#Bind the files together. 
gos_prioritized_budgets = rbind(final_budgets, gos_data, fill = TRUE) #There are some columns that don't exist in both sources, so fill = TRUE

#Check for overlapping grant periods in recent data. 
grant_period_mat = unique(gos_prioritized_budgets[, .(data_source, grant, grant_period, file_name, start_date)])
grant_period_mat[, min_date:=min(start_date), by=c('file_name', 'grant', 'grant_period', 'data_source')]
grant_period_mat[, max_date:=max(start_date), by=c('file_name', 'grant', 'grant_period', 'data_source')]
grant_period_mat = unique(grant_period_mat[, .(min_date, max_date, grant, grant_period, data_source)])
grant_period_mat = dcast.data.table(grant_period_mat, grant+grant_period~data_source, value.var = c("min_date", "max_date"))

#Reorder this data table. 
grant_period_mat = grant_period_mat[, .(grant, grant_period, min_date_fpm, max_date_fpm, min_date_gos, max_date_gos)]

#I only care about cases where we have the same data source reporting for the same grant period/grant, so drop NAs. 
grant_period_mat = grant_period_mat[!(is.na(min_date_fpm) & is.na(max_date_fpm))]
grant_period_mat = grant_period_mat[!(is.na(min_date_gos) & is.na(max_date_gos))]

#Do these sources conflict? 
grant_period_mat[max_date_gos>min_date_fpm, conflict:=TRUE]
grant_period_mat = grant_period_mat[conflict==TRUE]
if (nrow(grant_period_mat)>0){
  print("Warning: Duplicate dates present in GOS and final budgets files.")
  print(grant_period_mat)
  
  #Build up the list of grant quarters you need to drop from GOS
  drop_gos = data.table()
  for (i in 1:nrow(grant_period_mat)){
    quarters = unique(gos_prioritized_budgets[data_source == 'gos' & grant==grant_period_mat$grant[i] & grant_period==grant_period_mat$grant_period[i] & 
                                                start_date>=grant_period_mat$min_date_fpm[i], 
                                .(grant, grant_period, start_date)])
    drop_gos = rbind(drop_gos, quarters, fill=TRUE)
  }
  print("Dropping the following quarters from GOS data")
  print(drop_gos)
  for (i in 1:nrow(drop_gos)){
    gos_prioritized_budgets = gos_prioritized_budgets[!(
      data_source=='gos' & 
      grant==drop_gos$grant[i] & 
      grant_period==drop_gos$grant_period[i] & 
      start_date==drop_gos$start_date[i]
    )]
  }
  
}

#Look for what might be data gaps between GOS and budget data (EMILY - WOULD BE GOOD TO EXPAND THIS CHECK TO LOOK FOR DATA GAPS IN GENERAL)
gos_in_budgets = gos_prioritized_budgets[data_source=="gos" & grant%in%final_budgets$grant, .(start_date, grant, grant_period)] 
budget_dates = gos_prioritized_budgets[data_source=="fpm", .(budget_start = min(start_date)), by=c('grant', 'grant_period')]
gos_in_budgets = gos_in_budgets[, .(gos_end = max(start_date)), by=c('grant', 'grant_period')]
date_check = merge(gos_in_budgets, budget_dates, by=c('grant', 'grant_period'))
date_check = date_check[gos_end!=budget_start]
if (nrow(date_check)!=0){
  print("Warning: There are potential reporting gaps between GOS and final budgets. Review output 'Gaps between budgets and GOS' in GOS folder.")
  print("This check depends on accurate entry of 'grant_period' variable.")
  write.csv(date_check, paste0(dir, "_gf_files_gos/gos/Gaps between budgets and GOS.csv"), row.names=FALSE)
}

# Verify data 
na_year = gos_prioritized_budgets[is.na(year)]
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
final_expenditures_cod[, loc_name:='cod']
final_expenditures_gtm = readRDS(paste0(gtm_prepped, "final_expenditures.rds"))
final_expenditures_gtm[, loc_name:='gtm']
final_expenditures_uga = readRDS(paste0(uga_prepped, "final_expenditures.rds"))
final_expenditures_uga[, loc_name:='uga']
final_expenditures_sen = readRDS(paste0(sen_prepped, "final_expenditures.rds"))
final_expenditures_sen[, loc_name:='sen']

#Bind expenditures together
final_expenditures = rbind(final_expenditures_cod, final_expenditures_gtm, final_expenditures_uga, final_expenditures_sen, fill = TRUE) 
final_expenditures[, year:=year(start_date)]
final_expenditures[, data_source:='pudr']

#Drop unneeded variables, and generate a few needed ones
gos_expenditures = gos_data[, -c('activity_description', 'budget', 'country', 'current_grant', 'file_name', 'includes_rssh', 'orig_module',
                                 'orig_intervention', 'quarter', 'year', 'lfa_exp_adjustment')]
gos_expenditures[, data_source:='gos']
gos_expenditures[, end_date:=(start_date%m+%months(3))-1] #All of the GOS data is at the quarter-level. 

final_expenditures = final_expenditures[, -c('pudr_grant_year', 'year')]

#Append datasets 
gos_prioritized_expenditures = rbind(gos_expenditures, final_expenditures, use.names=TRUE, fill=TRUE)

#Check for overlapping grant periods in recent data - follow the same process for budgets of 
#   prioritizing PUDRs over GOS when we have both. 
grant_period_mat = unique(gos_prioritized_expenditures[, .(data_source, grant, grant_period, start_date, end_date)])
grant_period_mat[, start_date:=as.Date(start_date, format='%Y-%m-%d')]
grant_period_mat[, end_date:=as.Date(end_date, format='%Y-%m-%d')]
grant_period_mat[, min_date:=min(start_date), by=c('grant', 'grant_period', 'data_source')]
grant_period_mat[, max_date:=max(end_date), by=c('grant', 'grant_period', 'data_source')] #Have to format this code slightly differently than budgets b/c budgets are at quarter-level!
grant_period_mat = unique(grant_period_mat[, .(min_date, max_date, grant, grant_period, data_source)])
grant_period_mat = dcast.data.table(grant_period_mat, grant+grant_period~data_source, value.var = c("min_date", "max_date"))

#Reorder this data table. 
grant_period_mat = grant_period_mat[, .(grant, grant_period, min_date_pudr, max_date_pudr, min_date_gos, max_date_gos)]

#I only care about cases where we have the same data source reporting for the same grant period/grant, so drop NAs. 
grant_period_mat = grant_period_mat[!(is.na(min_date_pudr) & is.na(max_date_pudr))]
grant_period_mat = grant_period_mat[!(is.na(min_date_gos) & is.na(max_date_gos))]

#Do these sources conflict? 
grant_period_mat[max_date_gos>min_date_pudr, conflict:=TRUE]
grant_period_mat = grant_period_mat[conflict==TRUE]
if (nrow(grant_period_mat)>0){
  print("Warning: Duplicate dates present in GOS and final expenditures files.")
  print(grant_period_mat)
  
  #Build up the list of grant quarters you need to drop from GOS
  #GOS is still at the quarter-level even though expenditure data is not, so this check will work
  # as adapted from budget combination code -EL 7.9.2019
  drop_gos = data.table()
  for (i in 1:nrow(grant_period_mat)){
    quarters = unique(gos_prioritized_expenditures[data_source == 'gos' & grant==grant_period_mat$grant[i] & grant_period==grant_period_mat$grant_period[i] & 
                                                start_date>=grant_period_mat$min_date_pudr[i], 
                                              .(grant, grant_period, start_date)])
    drop_gos = rbind(drop_gos, quarters, fill=TRUE)
  }
  print("Dropping the following quarters from GOS data")
  print(drop_gos)
  for (i in 1:nrow(drop_gos)){
    gos_prioritized_expenditures = gos_prioritized_expenditures[!(
      data_source=='gos' & 
        grant==drop_gos$grant[i] & 
        grant_period==drop_gos$grant_period[i] & 
        start_date==drop_gos$start_date[i]
    )]
  }
}

#Look for what might be data gaps between GOS and expenditure data (EMILY - WOULD BE GOOD TO EXPAND THIS CHECK TO LOOK FOR DATA GAPS IN GENERAL)
gos_in_expenditures = gos_prioritized_expenditures[data_source=="gos" & grant%in%final_expenditures$grant, .(start_date, grant, grant_period)] 
expenditure_dates = gos_prioritized_expenditures[data_source=="pudr", .(expenditure_start = min(start_date)), by=c('grant', 'grant_period')]
gos_in_expenditures = gos_in_expenditures[, .(gos_end = max(start_date)), by=c('grant', 'grant_period')]
date_check = merge(gos_in_expenditures, expenditure_dates, by=c('grant', 'grant_period'))
date_check = date_check[gos_end!=expenditure_start]
if (nrow(date_check)!=0){
  print("Warning: There are potential reporting gaps between GOS and final expenditures. Review output 'Gaps between expenditures and GOS' in GOS folder.")
  print("This check depends on accurate entry of 'grant_period' variable.")
  write.csv(date_check, paste0(dir, "_gf_files_gos/gos/Gaps between expenditures and GOS.csv"), row.names=FALSE)
}

# Write data 
write.csv(gos_prioritized_expenditures, paste0(final_write, "final_expenditures.csv"), row.names = FALSE)
saveRDS(gos_prioritized_expenditures, paste0(final_write, "final_expenditures.rds"))

#----------------------------------
# 3. ABSORPTION
#----------------------------------
# absorption_cod = readRDS(paste0(dir, "_gf_files_gos/cod/prepped_data/absorption_cod.rds"))
# absorption_uga = readRDS(paste0(dir, "_gf_files_gos/uga/prepped_data/absorption_uga.rds"))  
# absorption_gtm = readRDS(paste0(dir, "_gf_files_gos/gtm/prepped_data/absorption_gtm.rds"))
# absorption_sen = readRDS(paste0(dir, "_gf_files_gos/sen/prepped_data/absorption_sen.rds"))
# 
# all_files = list(absorption_cod, absorption_gtm, absorption_uga, absorption_sen)
# absorption_files = rbindlist(all_files, use.names = TRUE, fill = TRUE)
# 
# #Write data 
# saveRDS(absorption_files, paste0(final_write, "absorption.rds"))
# write.csv(absorption_files, paste0(final_write, "absorption.csv"), row.names = FALSE)

#----------------------------------
# 4. GF FILE ITERATIONS
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

print("Step D: Aggregate GF files completed. Files saved in combined_prepped folder.")
