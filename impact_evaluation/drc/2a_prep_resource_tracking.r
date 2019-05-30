# ---------------------------------------------------------
# AUTHOR: Audrey Batzel and Emily Linebarger
# PURPOSE: Prepare resource tracking data for merge 
#          with activities and outputs.  
# DATE: Last updated March 2019. 
# INSTRUCTIONS: The current working directory should be the root of this repo (set manually by user)
# ----------------------------------------------------------

#------------------------------------
# TO-DO: 
# - Make this a general function that can be subset to any type of country and disease. 
#Should I keep data source in here to match 


#------------------------------------


#Temporary prep binding together gos data and final expenditures. EKL 3/25/19
gos_data = readRDS("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/gos/prepped_data/prepped_gos_data.rds")
setDT(gos_data)
gos_data = gos_data[country=="Congo (Democratic Republic)"]
gos_data[, loc_name:='cod']
gos_grants_to_keep = unique(gos_data[disease=='malaria', .(grant)])
gos_grants_to_keep = gos_grants_to_keep[!is.na(grant)]
gos_data = gos_data[grant%in%gos_grants_to_keep$grant]
gos_data[, start_date:=as.Date(start_date)]
gos_data[, end_date:=as.Date(end_date)]

#Expand GOS to be at the quarter-level; the same as the final expenditures 
totals_check = gos_data[, .(budget=sum(budget, na.rm = TRUE), expenditure=sum(expenditure, na.rm=TRUE)), by=c('grant', 'grant_period')][order(grant, grant_period)]
gos_data[, time_diff:=end_date-start_date]
gos_data[, num_quarters:=as.numeric(round(time_diff/90))] #90 days in each period

#Expand data by num_quarters, and generate a variable to iterate over
gos_data <- expandRows(gos_data, "num_quarters")
byVars = names(gos_data)
gos_data[, seq:=seq(0, 100, by=1), by=byVars]

#Increment the start date, and split up budget and expenditure. 
gos_data[, start_date:=start_date + months(3*seq)]
gos_data[, time_diff:=as.numeric(round(time_diff/90))]
gos_data[, budget:=budget/time_diff]
gos_data[, expenditure:=expenditure/time_diff]

#Make sure you haven't changed any budget/expenditure numbers, and clean up
totals_check2 = gos_data[, .(budget=sum(budget, na.rm = TRUE), expenditure=sum(expenditure, na.rm=TRUE)), by=c('grant', 'grant_period')][order(grant, grant_period)]
for (i in 1:nrow(totals_check)){
  stopifnot(totals_check$budget[i]==totals_check2$budget[i] | totals_check$expenditure[i]==totals_check2$expenditure[i])
}
gos_data = gos_data[, -c('time_diff', 'seq', 'end_date')]

#Read in final expenditure data 
cod_data = readRDS("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/cod/prepped_data/final_expenditures.rds")
setDT(cod_data)
cod_grants_to_keep = unique(cod_data[disease=='malaria', .(grant)])
cod_grants_to_keep = cod_grants_to_keep[!is.na(grant)]
cod_data = cod_data[grant%in%cod_grants_to_keep$grant]

#Expand expenditure data to be at the quarter-level (hacky fix adding in end dates here; need to be added in prep function)
totals_check = cod_data[, .(budget=sum(budget, na.rm = TRUE), expenditure=sum(expenditure, na.rm=TRUE)), by=c('grant', 'grant_period')][order(grant, grant_period)]
cod_data[file_name=='COD-M-MOH_PU 30 Juin2018_Révision du 29 Oct 2018.xlsx', end_date:=as.Date("2018-06-30")]
cod_data[file_name=='COD-M-SANRU - PU 30 june 2018 - Révision Oct2018 (final)_IHME.xlsx', end_date:=as.Date("2018-06-30")]
cod_data[file_name=='Final LFA_COD-M-MOH_PUDR_S2_201631032017.xlsx', end_date:=as.Date("2018-12-31")]

cod_data[, time_diff:=end_date-start_date]
cod_data[, num_quarters:=as.numeric(round(time_diff/90))] #90 days in each period

#Expand data by num_quarters, and generate a variable to iterate over
cod_data <- expandRows(cod_data, "num_quarters")
byVars = names(cod_data)
cod_data[, seq:=seq(0, 100, by=1), by=byVars]

#Increment the start date, and split up budget and expenditure. 
cod_data[, start_date:=start_date + months(3*seq)]
cod_data[, time_diff:=as.numeric(round(time_diff/90))]
cod_data[, budget:=budget/time_diff]
cod_data[, expenditure:=expenditure/time_diff]

#Make sure you haven't changed any budget/expenditure numbers, and clean up
totals_check2 = cod_data[, .(budget=sum(budget, na.rm = TRUE), expenditure=sum(expenditure, na.rm=TRUE)), by=c('grant', 'grant_period')][order(grant, grant_period)]
for (i in 1:nrow(totals_check)){
  stopifnot(totals_check$budget[i]==totals_check2$budget[i] | totals_check$expenditure[i]==totals_check2$expenditure[i])
}
cod_data = cod_data[, -c('time_diff', 'seq', 'end_date')]

final_expenditures = rbind(gos_data, cod_data, fill=TRUE)

#------------------------------------
#Read in previously prepped datasets 
#------------------------------------

#Read in final budget and expenditure data from RT database, and bind together. 
#final_budgets <- readRDS(budgetFile)
#final_expenditures <- readRDS(expendituresFile)
fgh <- readRDS(fghFile)
who <- readRDS(whoFile)
oop <- readRDS(gheMalFile)

fgh = fgh[, .(sda_activity, year, loc_name, disease, code, module_eng, intervention_eng, fin_data_type, financing_source, disbursement)]
setnames(fgh, old=c('module_eng', 'intervention_eng'), new=c('module', 'intervention'))

#For OOP, take only the data from the most recent year of reporting. 
oop = oop[value_code == "fs_malaria_domestic_private_oop"]
oop[order(year_id)]
oop = oop[!((year_id == 2012 & report_year == 2015) | (year_id == 2013 & report_year == 2015) | (year_id == 2014 & report_year == 2016))]
setnames(oop, old=c('year_id', 'value'), new=c( 'year', 'oop'))
oop$report_year <- NULL
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

# quick fixes that shouldn't be necessary once earlier code is fixed
fgh[sda_activity=='mal_comm_con_dah_17', code:='M2_3']

#------------------------------------
# Subset data and prep for merge
#------------------------------------

#Subset to only the columns we want from resource tracking database and impact evaluation map 
exp_subset = final_expenditures[loc_name == 'cod' & (disease == "malaria" | disease == "hss" | disease == 'rssh'), .(expenditure, start_date, code, loc_name, disease, gf_module, gf_intervention)]
setnames(exp_subset, old=c("gf_module","gf_intervention"), new=c("module", "intervention"))
other_dah = fgh[fin_data_type == 'actual' & (financing_source != 'The Global Fund' & financing_source != 'ghe') & loc_name=='COD' & (disease == 'malaria' | disease == 'hss' | disease == 'rssh'), 
                .(other_dah = sum(disbursement, na.rm=TRUE)), by=.(sda_activity, year, loc_name, disease, code, module, intervention)]
ghe = who[loc_name == 'cod' & indicator=='domestic_ghe_malaria', .(ghe = sum(expenditure, na.rm = TRUE)), 
        by = .(year)]

#Split other_dah and ghe into quarters
n_years <- (2018-1990)+1 #This is the range we have data for. 
quarters <- data.table(year=rep(1990:2018, 4))[order(year)]
quarters[, quarter:=rep(1:4, n_years)]

#Split each data set out by quarter
other_dah = merge(quarters, other_dah, by='year', all.x = TRUE, allow.cartesian=TRUE)
other_dah[, other_dah:=other_dah/4]

ghe = merge(quarters, ghe, by='year', all.x = TRUE, allow.cartesian = TRUE)
ghe[, ghe:=ghe/4]
ghe[, quarter:=(quarter/4)-0.25] #Q1 should be .00, Q2 should be .25, etc. 
ghe[, date:=year+quarter]

oop = merge(quarters, oop, by='year', all.x = TRUE, allow.cartesian = TRUE)
oop[, oop:=oop/4]
oop[, quarter:=(quarter/4)-0.25] #Q1 should be .00, Q2 should be .25, etc. 
oop[, date:=year+quarter]

exp_subset[, quarter:=quarter(start_date)]
exp_subset[, year:=year(start_date)]
exp_subset[, start_date:=NULL]

#Match exp and FGH data to codes to only keep relevant modules/interventions (all = TRUE)
drc_mal_map_codes = drc_mal_map[, .(code, indicator, indicator_type)]
exp_subset = merge(exp_subset, drc_mal_map_codes, by=c('code'), allow.cartesian = TRUE)
other_dah = merge(other_dah, drc_mal_map_codes, by=c('code'), allow.cartesian = TRUE)

print(paste0("Codes kept in exp data: ", unique(exp_subset[, .(code)])))
print(paste0("Codes kept in FGH data: ", unique(other_dah[, .(code)])))

#Create date variable
exp_subset[, quarter:=(quarter/4)-0.25] #Q1 should be .00, Q2 should be .25, etc. 
exp_subset[, date:=year+quarter]
other_dah[, quarter:=(quarter/4)-0.25] #Q1 should be .00, Q2 should be .25, etc. 
other_dah[, date:=year+quarter]

#Cast data wide 
exp_wide = dcast(exp_subset, date~code, value.var=c('expenditure'), fun.aggregate = sum)
frame = data.table(date=seq(1990, 2020, by=.25))
exp_wide = merge(frame, exp_wide, by='date', all.x=TRUE)
for(v in names(exp_wide)) exp_wide[is.na(get(v)), (v):=0]

other_dah_wide = dcast(other_dah, date~code, value.var=c('other_dah'), fun.aggregate = sum)
frame = data.table(date=seq(1990, 2020, by=.25))
other_dah_wide = merge(frame, other_dah_wide, by='date', all.x=TRUE)
for(v in names(other_dah_wide)) other_dah_wide[is.na(get(v)), (v):=0]

#Set column names for merge 
names = colnames(exp_wide[, 2:ncol(exp_wide)])
names <- paste("exp", names, sep = "_")
colnames(exp_wide) <- c('date', names)

names = colnames(other_dah_wide[, 2:ncol(other_dah_wide)])
names <- paste("other_dah", names, sep = "_")
colnames(other_dah_wide) <- c('date', names)

#Merge both files together 
rt_wide <- merge(other_dah_wide, exp_wide, by=c('date'))

#Add on GHE and OOP as control variables 
ghe = ghe[, .(date, ghe)]
rt_wide = merge(rt_wide, ghe, by='date', all.x = TRUE)
oop = oop[, .(date, oop)]
rt_wide = merge(rt_wide, oop, by='date', all.x = TRUE)

#Save output file
saveRDS(rt_wide, outputFile2a)
