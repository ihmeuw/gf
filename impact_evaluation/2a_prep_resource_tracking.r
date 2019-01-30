# ---------------------------------------------------------
# AUTHOR: Audrey Batzel and Emily Linebarger
# PURPOSE: Prepare resource tracking data for merge 
#          with activities and outputs.  
# DATE: Last updated January 2019. 
# INSTRUCTIONS: The current working directory should be the root of this repo (set manually by user)
# ----------------------------------------------------------

#------------------------------------
# TO-DO: 
# -Do we want to include any GHE data? 
# - Make this a general function that can be subset to any type of country and disease. 
#Should I keep data source in here to match 

#------------------------------------


#------------------------------------
#Read in previously prepped datasets 
#------------------------------------

#Read in final budget and expenditure data from RT database, and bind together. 
final_budgets <- readRDS(budgetFile)
#final_expenditures <- readRDS(expendituresFile)
fgh <- fread(fghFile)

#------------------------------------
# Validate data 
#------------------------------------

#Make sure we don't have duplicate files for the same start date in the same grant. #Emily need to add this check into the general RT prep code. 
check_file_duplicates <- unique(final_budgets[, .(fileName, start_date, grant_number)])
check_file_duplicates[, file_count:=sequence(.N), by=.(grant_number, start_date)]
grant_with_dup_files <- unique(check_file_duplicates[file_count>1, .(grant_number, start_date)])
for(i in 1:nrow(grant_with_dup_files)){
  if(i == 1){
    duplicate_files = check_file_duplicates[grant_number==grant_with_dup_files$grant_number[i] & start_date==grant_with_dup_files$start_date[i], ]
  } else {
    tmp_data = check_file_duplicates[grant_number==grant_with_dup_files$grant_number[i] & start_date==grant_with_dup_files$start_date[i], ]
    duplicate_files = rbind(tmp_data, duplicate_files)
  }
}

#Emily right now all of these are coming from Guatemala. Need to sort out, but we can ignore for DRC malaria. 
#stopifnot(nrow(duplicate_files)==0)

#------------------------------------
# Subset data and prep for merge
#------------------------------------

#Subset to only the columns we want from resource tracking database and impact evaluation map 
budget_subset = final_budgets[country == "Congo (Democratic Republic)" & (disease == "malaria" | disease == "hss"), .(budget, start_date, code, loc_name, disease, module, intervention)]
other_dah = fgh[fin_data_type == 'actual' & financing_source != 'The Global Fund' & country == 'Congo (Democratic Republic)' & (disease == 'malaria' | disease == 'hss'), 
                .(other_dah = sum(disbursement, na.rm=TRUE)), by=.(sda_activity, year, loc_name, disease, code, module, intervention)]

#Split other_dah into quarters
n_years <- (2018-1990)+1 #This is the range we have data for. 
quarters <- data.table(year=rep(1990:2018, 4))[order(year)]
quarters[, quarter:=rep(1:4, n_years)]

other_dah = merge(quarters, other_dah, by='year', all.x = TRUE, allow.cartesian=TRUE)
other_dah[, other_dah:=other_dah/4]

#Generate variables for budget subset 
budget_subset[, quarter:=quarter(start_date)]
budget_subset[, year:=year(start_date)]
budget_subset[, start_date:=NULL]

#Merge budget and fgh data together
prepped_rt <- merge(budget_subset, other_dah, by=c('year', 'quarter', 'code', 'loc_name', 'disease', 'module', 'intervention'), all=TRUE)

#------------------------------------
# Merge data with map of indicator codes
#------------------------------------

#Map to intervention and indicator using the map for DRC malaria. 
prepped_rt <- merge(prepped_rt, drc_mal_map, by = c('code', 'module', 'intervention'), allow.cartesian = TRUE)

#Do different global fund modules and interventions map to different codes here?
check_unique_codes <- unique(prepped_rt[, .(code, module, intervention)])


#-----------------------------------------------------------
# Make sure data are uniquely identified in the way you want 
#-----------------------------------------------------------

#Make sure resource tracking data is uniquely identified by year, quarter, module, intervention, and indicator 
prepped_rt = prepped_rt[, .(budget=sum(budget, na.rm=TRUE), other_dah=sum(other_dah, na.rm=TRUE)), by=.(year, quarter, module, intervention, indicator, indicator_type, code)]
stopifnot(nrow(unique(prepped_rt[, .(year, quarter, module, intervention, indicator)]))==nrow(prepped_rt))

saveRDS(prepped_rt, outputFile2a)
