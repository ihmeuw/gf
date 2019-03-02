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
#final_budgets <- readRDS(budgetFile)
final_expenditures <- readRDS(expendituresFile)
fgh <- fread(fghFile)
ghe <- fread(paste0(j, "/Project/Evaluation/GF/resource_tracking/multi_country/mapping/total_prepped_fgh.csv"))

#------------------------------------
# Validate data 
#------------------------------------

#Make sure we don't have duplicate files for the same start date in the same grant. #Emily need to add this check into the general RT prep code. 
# check_file_duplicates <- unique(final_expenditures[, .(fileName, start_date, grant_number)])
# check_file_duplicates[, file_count:=sequence(.N), by=.(grant_number, start_date)]
# grant_with_dup_files <- unique(check_file_duplicates[file_count>1, .(grant_number, start_date)])
# for(i in 1:nrow(grant_with_dup_files)){
#   if(i == 1){
#     duplicate_files = check_file_duplicates[grant_number==grant_with_dup_files$grant_number[i] & start_date==grant_with_dup_files$start_date[i], ]
#   } else {
#     tmp_data = check_file_duplicates[grant_number==grant_with_dup_files$grant_number[i] & start_date==grant_with_dup_files$start_date[i], ]
#     duplicate_files = rbind(tmp_data, duplicate_files)
#   }
# }

#Emily right now all of these are coming from Guatemala. Need to sort out, but we can ignore for DRC malaria. 
#stopifnot(nrow(duplicate_files)==0)

#Reset loc_name to match iso code. 
final_expenditures[loc_name == 'Congo (Democratic Republic)', loc_name:='cod']
final_expenditures[loc_name == 'Guatemala', loc_name:='gtm']
final_expenditures[loc_name == 'Uganda', loc_name:='uga']

#------------------------------------
# Subset data and prep for merge
#------------------------------------

#Subset to only the columns we want from resource tracking database and impact evaluation map 
exp_subset = final_expenditures[country == "Congo (Democratic Republic)" & (disease == "malaria" | disease == "hss"), .(expenditure, start_date, code, loc_name, disease, gf_module, gf_intervention)]
setnames(exp_subset, old=c("gf_module","gf_intervention"), new=c("module", "intervention"))
other_dah = fgh[fin_data_type == 'actual' & (financing_source != 'The Global Fund' & financing_source != 'ghe') & country == 'Congo (Democratic Republic)' & (disease == 'malaria' | disease == 'hss'), 
                .(other_dah = sum(disbursement, na.rm=TRUE)), by=.(sda_activity, year, loc_name, disease, code, module, intervention)]
ghe = ghe[fin_data_type == "mean" & financing_source == 'public' & country == "Congo (Democratic Republic)", .(ghe = sum(disbursement, na.rm = TRUE)), 
        by = .(year, loc_name, disease)]

#Split other_dah and ghe into quarters
n_years <- (2018-1990)+1 #This is the range we have data for. 
quarters <- data.table(year=rep(1990:2018, 4))[order(year)]
quarters[, quarter:=rep(1:4, n_years)]

other_dah = merge(quarters, other_dah, by='year', all.x = TRUE, allow.cartesian=TRUE)
other_dah[, other_dah:=other_dah/4]
ghe = merge(quarters, ghe, by='year', all.x = TRUE, allow.cartesian = TRUE)
ghe[, ghe:=ghe/4]

#Generate variables for budget subset 
exp_subset[, quarter:=quarter(start_date)]
exp_subset[, year:=year(start_date)]
exp_subset[, start_date:=NULL]

#Merge budget and fgh data together, and create date variable
prepped_rt <- rbind(exp_subset, other_dah, fill = TRUE) #Note - we still have observations with 'na' for code at this point. 

#------------------------------------
# Merge data with map of indicator codes
#------------------------------------

#Map to intervention and indicator using the map for DRC malaria. 
drc_mal_map_codes = drc_mal_map[, .(code, indicator, indicator_type)]
prepped_rt <- merge(prepped_rt, drc_mal_map_codes, by = c('code'), allow.cartesian = TRUE)

#Create date variable 
prepped_rt[, quarter:=(quarter/4)-0.25] #Q1 should be .00, Q2 should be .25, etc. 
prepped_rt[, date:=year+quarter]

#Should have no missing module, intervention, code, or date at this point! 
stopifnot(nrow(prepped_rt[is.na(module)|is.na(intervention)|is.na(code)])!= 0)
stopifnot(nrow(prepped_rt[is.na(date)])!= 0)

#Cast data wide 
rt_wide = dcast(prepped_rt, date~code, value.var=c('expenditure','other_dah'), fun.aggregate = sum)
frame = data.table(date=seq(1990, 2020, by=.25))
rt_wide = merge(frame, rt_wide, by='date', all.x=TRUE)
for(v in names(rt_wide)) rt_wide[is.na(get(v)), (v):=0]

#Add on total GHE spending for each quarter
ghe[, quarter:=(quarter/4)-0.25] #Q1 should be .00, Q2 should be .25, etc. 
ghe[, date:=year+quarter]
ghe = ghe[, .(date, ghe)]
rt_wide = merge(rt_wide, ghe, by='date')
#-----------------------------------------------------------
# Make sure data are uniquely identified in the way you want 
#-----------------------------------------------------------
# 
# #Make sure resource tracking data is uniquely identified by year, quarter, module, intervention, and indicator 
# rt_wide = rt_wide[, .(budget=sum(budget, na.rm=TRUE), other_dah=sum(other_dah, na.rm=TRUE)), by=.(year, quarter, module, intervention, sda_activity, indicator, indicator_type, code, loc_name, disease)]
# stopifnot(nrow(unique(rt_wide[, .(year, quarter, module, intervention, sda_activity, indicator, indicator_type, loc_name, disease)]))==nrow(rt_wide))
# stopifnot(nrow(unique(rt_wide[, .(year, quarter, code, indicator, indicator_type,loc_name, disease)]))==nrow(rt_wide))

saveRDS(rt_wide, outputFile2a)
