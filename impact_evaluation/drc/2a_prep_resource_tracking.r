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


#Final Expenditures (includes GOS)
final_expenditures = readRDS("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/combined_prepped_data/final_expenditures_cod.rds")

#------------------------------------
#Read in previously prepped datasets 
#------------------------------------

#Read in final budget and expenditure data from RT database, and bind together. 
#final_budgets <- readRDS(budgetFile)
#final_expenditures <- readRDS(expendituresFile)
odah <- readRDS(odahFile)
who <- readRDS(whoFile)
oop <- readRDS(oopMalFile)

odah = odah[, .(activity_description, year, loc_name, disease, code, gf_module, gf_intervention, channel_agg, disbursement)]
setnames(odah, old=c('gf_module', 'gf_intervention'), new=c('module', 'intervention'))

#For OOP, take only the data from the most recent year of reporting. 
oop[order(year)]
setnames(oop, old=c('value'), new=c('oop'))
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

# quick fixes that shouldn't be necessary once earlier code is fixed
odah[activity_description=='mal_comm_con_dah_17', code:='M2_3']

#------------------------------------
# Subset data and prep for merge
#------------------------------------

#Subset to only the columns we want from resource tracking database and impact evaluation map 
exp_subset = final_expenditures[, .(expenditure, start_date, code, loc_name, disease, gf_module, gf_intervention)] #This data has already been subset down to DRC malaria in prep. 
setnames(exp_subset, old=c("gf_module","gf_intervention"), new=c("module", "intervention"))
other_dah = odah[(channel_agg != 'The Global Fund' & channel_agg != 'GHE') & loc_name=='COD' & (disease == 'malaria' | disease == 'hss' | disease == 'rssh'), 
                .(other_dah = sum(disbursement, na.rm=TRUE)), by=.(activity_description, year, loc_name, disease, code, module, intervention)]
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
keep_codes = unique(drc_mal_map$code)
exp_subset = exp_subset[code%in%keep_codes]
check_m1_1 = exp_subset[year==2016 & code=="M1_1", .(sum(expenditure, na.rm=T))]
stopifnot(check_m1_1 == 58438309)
other_dah = other_dah[code%in%keep_codes]

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
saveRDS(rt_wide, paste0("J:/Project/Evaluation/GF/impact_evaluation/cod/prepped_data/archive/prepped_resource_tracking", Sys.Date(), ".rds"))
