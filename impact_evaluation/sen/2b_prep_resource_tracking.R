# ----------------------------------------------------------
# AUTHOR: Francisco RIos-Casas
# PURPOSE: To prep resource tracking data to merge with outputs, outcomes
# DATE: August 14 2019
# ----------------------------------------------------------

# Set up
library(data.table)

#------------------------------------
#Read in previously prepped datasets 
#------------------------------------

final_expenditures <- readRDS('J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/sen/prepped_data/final_expenditures.RDS')
fgh <- readRDS('J:/Project/Evaluation/GF/resource_tracking/_fgh/prepped_data/other_dah_actuals_all_sen.RDS')
who <- readRDS('J:/Project/Evaluation/GF/resource_tracking/_ghe/who/prepped_data/who_prepped.RDS')

fgh = fgh[, .(year, loc_name, disease, code, gf_module, gf_intervention, channel_agg, source, disbursement)]
setnames(fgh, old=c('gf_module', 'gf_intervention'), new=c('module', 'intervention'))

#------------------------------------
# Subset data and prep for merge
#------------------------------------

# Subset to only the columns we want from resource tracking database 
exp_subset = final_expenditures[disease %in% c("rssh", "tb"), .(expenditure, start_date, code, disease, gf_module, gf_intervention)] # kept only TB & RSSH funding
setnames(exp_subset, old=c("gf_module","gf_intervention"), new=c("module", "intervention"))

other_dah = fgh[(source != 'The Global Fund' & source != 'ghe') & loc_name=='SEN' & (disease == 'tb' | disease == 'hss' | disease == 'rssh'), 
                .(other_dah = sum(disbursement, na.rm=TRUE)), by=.(year, loc_name, disease, code, module, intervention)]
ghe = who[loc_name == 'sen' & indicator=='domestic_ghe_tb', .(ghe = sum(expenditure, na.rm = TRUE)), 
          by = .(year)]

# Split data into quarters
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

exp_subset[, quarter:=quarter(start_date)]
exp_subset[, year:=year(start_date)]
exp_subset[, start_date:=NULL]
###############################
# Limitation: wasn't sure wich variables to keep, .i.e., which are the relevant ones for the model yet
# also what is the code from the map
#Match exp and FGH data to codes to only keep relevant modules/interventions (all = TRUE)
# keep_codes = unique(drc_mal_map$code)
###############################

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

###############################
# Limitation: not sure what these datasets refer to
#############################

#Add on GHE and OOP as control variables 
ghe = ghe[, .(date, ghe)]
rt_wide = merge(rt_wide, ghe, by='date', all.x = TRUE)
# oop = oop[, .(date, oop)]
# rt_wide = merge(rt_wide, oop, by='date', all.x = TRUE)

#Save output file
# outputFile2a will eventually move code below to "set-up" file

# switch J for portability to cluster
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/')
ieDir = paste0(dir, 'impact_evaluation/sen/prepped_data/')
outputFile2a = paste0(ieDir, 'prepped_resource_tracking.RDS')
saveRDS(rt_wide, outputFile2a)
