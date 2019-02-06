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


# 1. rectangularize the RT data so that every module/intervention (at least those with any output data)
# appears for every quarter for the entire range of the data
# 2. reshape module/interventions wide so that each row is uniquely identified 
# by quarter and there's a column for every financing source/intervention
#it's true, we don't care about modules and interventions that don't have a corresponding activity/output variable,
#so go ahead and drop those from the data prior to merging (edited) 
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

#Reset loc_name to match iso code. 
final_budgets[loc_name == 'Congo (Democratic Republic)', loc_name:='cod']
final_budgets[loc_name == 'Guatemala', loc_name:='gtm']
final_budgets[loc_name == 'Uganda', loc_name:='uga']

#------------------------------------
# Subset data and prep for merge
#------------------------------------

#Subset to only the columns we want from resource tracking database and impact evaluation map 
budget_subset = final_budgets[country == "Congo (Democratic Republic)" & (disease == "malaria" | disease == "hss"), .(budget, start_date, code, loc_name, disease, gf_module, gf_intervention)]
setnames(budget_subset, old=c("gf_module","gf_intervention"), new=c("module", "intervention"))
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

#Match budget and FGH data to codes to only keep relevant modules/interventions (all = TRUE)
drc_mal_map_codes = drc_mal_map[, .(code, indicator, indicator_type)]
budget_subset = merge(budget_subset, drc_mal_map_codes, by=c('code'), allow.cartesian = TRUE)
other_dah = merge(other_dah, drc_mal_map_codes, by=c('code'), allow.cartesian = TRUE)

print(paste0("Codes kept in budget data: ", unique(budget_subset[, .(code)])))
print(paste0("Codes kept in FGH data: ", unique(other_dah[, .(code)])))

#Create date variable
budget_subset[, quarter:=(quarter/4)-0.25] #Q1 should be .00, Q2 should be .25, etc. 
budget_subset[, date:=year+quarter]
other_dah[, quarter:=(quarter/4)-0.25] #Q1 should be .00, Q2 should be .25, etc. 
other_dah[, date:=year+quarter]

#Cast data wide 
budget_wide = dcast(budget_subset, date~code, value.var=c('budget'), fun.aggregate = sum)
frame = data.table(date=seq(1990, 2020, by=.25))
budget_wide = merge(frame, budget_wide, by='date', all.x=TRUE)
for(v in names(budget_wide)) budget_wide[is.na(get(v)), (v):=0]

other_dah_wide = dcast(other_dah, date~code, value.var=c('other_dah'), fun.aggregate = sum)
frame = data.table(date=seq(1990, 2020, by=.25))
other_dah_wide = merge(frame, other_dah_wide, by='date', all.x=TRUE)
for(v in names(other_dah_wide)) other_dah_wide[is.na(get(v)), (v):=0]

#Set column names for merge 
names = colnames(budget_wide[, 2:ncol(budget_wide)])
names <- paste("budget", names, sep = "_")
colnames(budget_wide) <- c('date', names)

names = colnames(other_dah_wide[, 2:ncol(other_dah_wide)])
names <- paste("other_dah", names, sep = "_")
colnames(other_dah_wide) <- c('date', names)

#Merge both files together 
rt_wide <- merge(other_dah_wide, budget_wide, by=c('date'))

#Save output file
saveRDS(rt_wide, outputFile2a)
