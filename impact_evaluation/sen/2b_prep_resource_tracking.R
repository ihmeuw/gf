# AUTHOR: Francisco RIos-Casas
# PURPOSE: To prep resource tracking data to merge with outputs, outcomes
# DATE: August 14 2019

# Set up
library(data.table)

# read in previously prepped data on GF expenditure in Senegal
final_expenditures <- readRDS('J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/sen/prepped_data/final_expenditures.rds')

# Missing other health assistance in Senegal

#Subset to only the columns we want from resource tracking database and impact evaluation map 
exp_subset = final_expenditures[disease %in% c("rssh", "tb"), .(expenditure, start_date, code, disease, gf_module, gf_intervention)] # kept only TB/RSSH funding 
setnames(exp_subset, old=c("gf_module","gf_intervention"), new=c("module", "intervention"))

#Split data into quarters
n_years <- (2018-2012)+1 #This is the range we have data for. 
quarters <- data.table(year=rep(2012:2018, 4))[order(year)]
quarters[, quarter:=rep(1:4, n_years)]

#Split each data set out by quarter
#other_dah = merge(quarters, other_dah, by='year', all.x = TRUE, allow.cartesian=TRUE)
#other_dah[, other_dah:=other_dah/4]
# will eventually add this back in


exp_subset[, quarter:=quarter(start_date)]
exp_subset[, year:=year(start_date)]
exp_subset[, start_date:=NULL]

#Create date variable
exp_subset[, quarter:=(quarter/4)-0.25] #Q1 should be .00, Q2 should be .25, etc. 
exp_subset[, date:=year+quarter]

#Cast data wide 
exp_wide = dcast(exp_subset, date~code, value.var=c('expenditure'), fun.aggregate = sum)
frame = data.table(date=seq(2012, 2020, by=.25))
exp_wide = merge(frame, exp_wide, by='date', all.x=TRUE)
for(v in names(exp_wide)) exp_wide[is.na(get(v)), (v):=0]

#Set column names for merge 
names = colnames(exp_wide[, 2:ncol(exp_wide)])
names <- paste("exp", names, sep = "_")
colnames(exp_wide) <- c('date', names)

#
# Limitation: wasn't sure wich variables to keep, .i.e., which are the relevant ones for the model yet
# 

#names = colnames(other_dah_wide[, 2:ncol(other_dah_wide)])
#names <- paste("other_dah", names, sep = "_")
#colnames(other_dah_wide) <- c('date', names)

#Merge both files together 
# rt_wide <- merge(other_dah_wide, exp_wide, by=c('date'))
rt_wide <- exp_wide


#Save output file
# outputFile2a will eventually move code below to "set-up" file

# switch J for portability to cluster
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/')
ieDir = paste0(dir, 'impact_evaluation/sen/prepped_data/')
outputFile2a = paste0(ieDir, 'prepped_resource_tracking.RDS')
saveRDS(rt_wide, outputFile2a)
