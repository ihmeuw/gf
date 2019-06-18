# ----------------------------------------------
# Audrey Batzel
#
# 5/16/19 
# final prep for MI (used to be in prep_for_MI.R)
setwd('C:/local/gf/')
# ----------------------------------------------

# --------------------
# Set up R / install packages
rm(list=ls())
library(data.table)
library(reshape2)
library(stringr)
library(RColorBrewer)
library(ggplot2)
library(lubridate)
library(readxl)
library(stats)
library(rlang)
library(zoo)
library(tidyr)
library(dplyr)
library(parallel)
# --------------------

# ----------------------------------------------
# Overview - Files and Directories
# ---------------------------------------------- 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# data directory
dir = paste0(root, "/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/")

# input files
dt_for_dup_removal = "outliers/ameliaDT_with_index_for_dup_removal.rds"
dup_matrix_final = "outliers/final_pnlp_duplicates_matrix.rds"
dt_outliers_labelled = "outliers/pnlp_outliers_labeled.rds"

# output files
output_dt = "PNLP_dt_forMI_updated_6_10_19.rds"
# ----------------------------------------------   

# ----------------------------------------------
# Read in the data
# ---------------------------------------------- 
dt_ids = readRDS( paste0(dir, dt_for_dup_removal) ) 
dups = readRDS(paste0(dir, dup_matrix_final))
dt_outliers = readRDS(paste0(dir, dt_outliers_labelled))
# ----------------------------------------------   

# ----------------------------------------------
# Remove duplicates using the final_dup_matrix generated from screen_duplciates.R and further prepped/prcoessed in process_duplicates.R
# ---------------------------------------------- 
# manually remove duplicates where value = 10,428 for RDTs used under 5. --> duplicate found during outlier detection, but missed because
# of our method for dups detection (which required 80% of non-missing columns, and in this case it was only 50%.... which shows us there 
# will be some false negatives with the rule requiring 80% duplication because these are definitely duplicates).
# we used frequency tables to check for other examples like this in a variety of variables, and did not find them. 

id_vars <- c("dps", "health_zone", "date", "donor", "operational_support_partner", "population", "id")
inds = names(dt_ids)[!names(dt_ids) %in% id_vars]

# get ids where RDTs completed under 5 = 10,428
manual_remove_ids = dt_ids[RDT_completedUnder5 == 10428, ]
# get the variables that are the same/are duplicated among those rows/ids 
i = manual_remove_ids[1, id]
j = manual_remove_ids[2, id]
vars_to_remove = names(dt_ids[, inds, with = FALSE])[dt_ids[i, inds, with=FALSE] == dt_ids[j, inds, with=FALSE]] 
vars_to_remove = vars_to_remove[!is.na(vars_to_remove)]
# some of the vars in vars_to_remove not true duplicates across all rows (manual visual check), so take those out:
vars_to_remove = vars_to_remove[ !vars_to_remove %in% c("ArtLum_used", "ArtLum_received", "malariaDeaths_5andOlder", "totalDeathsAllDiseases_5andOlder") ]
manual_remove_ids = manual_remove_ids$id
# set the vars in vars_to_remove for the rows in manual_remove_ids to be NA
dt_ids[ id %in% manual_remove_ids, (names(dt_ids)[names(dt_ids) %in% vars_to_remove]) := NA]

# loop through and remove data that is duplicated and NOT 0 in the dups_matrix pairs of ids...
# because some have more than 1 duplicate, we won't be able to set duplicates to NA and continue looping through rows
# because then future comparisons will show that the rows aren't dupilcated.  Instead, loop through rows of dups
# and grab both ids, compare those ids, and in the long formatted data--This also gives us a built in way to check! (should be roughly mean(dups$identical_not_zero) * 1228 = 50,162 values)
dt_ids_long = melt.data.table(dt_ids, id.vars = id_vars)
dt_ids_long[, duplicate := FALSE]
na_before_dups_remove = sum(is.na(dt_ids_long$value))

for(row in 1:nrow(dups)){
  i = dups[row, i]
  j = dups[row, j]
  
  vars_to_remove = names(dt_ids[, inds, with = FALSE])[dt_ids[i, inds, with=FALSE] == dt_ids[j, inds, with=FALSE]] 
  vars_to_remove = vars_to_remove[!is.na(vars_to_remove)]
  
  dt_ids_long[variable %in% vars_to_remove & id %in% c(i, j) & value != 0 , duplicate := TRUE ]
}
dt_ids_long[duplicate==TRUE, .N]
dt_ids_long[duplicate==TRUE, value := NA]
rm(dt_ids)
# ----------------------------------------------

# ----------------------------------------------
# Remove outliers
# don't remove outliers in RDTs
# ---------------------------------------------- 
# merge outliers dt with dt that had duplicates removed
id_vars = id_vars[!id_vars %in% "id"]
id_vars = c(id_vars, "variable")
dt_ids_long[, date := as.Date(date)]
dt = merge(dt_ids_long, dt_outliers, all = TRUE, by = id_vars)
dt[ , c("duplicate", "value.y"):= NULL]
setnames(dt, "value.x", "value")

# set RDTs, SSC variables, and stock outs to be excluded from outliers
dt[ grepl(variable, pattern = "RDT", ignore.case = TRUE), outlier := FALSE ]
dt[ grepl(variable, pattern = "SSC", ignore.case = TRUE), outlier := FALSE ]
dt[ grepl(variable, pattern = "RDT", ignore.case = TRUE), outlier := FALSE ]

# dt[ outlier == TRUE, .N] #648 down to 602
dt[ outlier == TRUE, value := NA]
dt[, outlier := NULL]
# (sum(is.na(dt$value)) / nrow(dt)) * 100 = 51.3% missing

# cast back wide
dt = dcast.data.table(dt, dps + health_zone + date + donor + operational_support_partner + population + id ~ variable)
  # intermediate save
  # saveRDS(dt, paste0(dir, output_dt))
# ---------------------------------------------- 

# CANT DO THIS FOR WIDE DATA:
# # ----------------------------------------------
# # remove rectangularization for SSC variables 
# # iCCM had not started before 2015
# # ----------------------------------------------
# dt = dt[!(grepl(variable, pattern = "SSC", ignore.case = TRUE) & date < '2015-01-01' ), ]
# # ---------------------------------------------- 

# ---------------------------------------------- 
# Deterministically impute total health facilities and convert the number reporting to a proportion over the total, then drop the original variable for number reporting and impute
# the proportion so that we can back calculate the number reporting and it will always be less than the total number of facilities.
# ----------------------------------------------   
dt[, year:= year(date)]

dt$healthFacilities_total <- as.numeric(dt$healthFacilities_total)
dt$healthFacilities_numReported <- as.numeric(dt$healthFacilities_numReported)
# keep track of what the original values are before we change them:
dt[, healthFacilities_total_orig := healthFacilities_total]
dt[, healthFacilities_numReported_orig := healthFacilities_numReported]

# Number of health facilities reporting should not be greater than the total number of health facilities but there are 165 instances of this.
# ---------------- 
# NOTE: commented out 01/19 because I think we would want to handle this problem AFTER taking max of total fac and set the num reported rather than the total
# # when health facilities reporting is greater than total health facilities, change health facilities total to = health facilities reporting
# dt[healthFacilities_numReported > healthFacilities_total, healthFacilities_total:=healthFacilities_numReported]
# ----------------
# NOTE: commented out 3/11 because I don't think this is the right approach either.. might be best to set both to NA and then just deterministically impute health facilities
# total the same way
# # when the number of health facilities reporting is greater than total health facilities:
#   # if the number of health facilities reporting is equal to the max of health facilities total by health zone/year, then set
#     # health facilities total = health facilities max
#   dt <- dt[, healthFacilities_max := max(healthFacilities_total, na.rm=TRUE), by=c("dps", "health_zone", "year")]
#   dt <- dt[healthFacilities_max == "-Inf", healthFacilities_max:=NA]  # result was -Inf where all values were missing by unique group
#   dt[healthFacilities_numReported == healthFacilities_max, healthFacilities_total:= healthFacilities_max]
#   # if health facilities total 
# ----------------  
# set both health facilities reporting and health facilities total to missing when reporting > total
dt[healthFacilities_numReported > healthFacilities_total, c('healthFacilities_total', 'healthFacilities_numReported') := NA]

# when healthFacilities_total variable is missing for a given year set it to be the same as the following year (since it is mostly earlier years missing)
test <- dt[, .(healthFacilities_max = max(healthFacilities_total, na.rm=TRUE)), by=c("dps", "health_zone", "year")]
test <- test[healthFacilities_max == "-Inf", healthFacilities_max:=NA]  # result was -Inf where all values were missing by group
# order by year descending for use of na.locf
test <- test[order(dps, health_zone, -year),]

# warning - na.locf won't work if all health facilities total data is missing in most recent year
# manually set mabalako 2017 so that it works
test[health_zone=="mabalako" & year==2017, healthFacilities_max:=28]
#na.locf replaces an NA with the most recent non NA prior to it
test[, healthFacilities_max:= na.locf(healthFacilities_max), by=health_zone]

# TO DO - go back and add this in?? Ask David (1/22/19)         
# # in some cases the mode facilities occurs 7 of 8 times and there's one random outlier value.  We want to change those
# test[ , healthFacilities_mode := getmode(healthFacilities_max), by= c("dps", "health_zone")]
# test2 <- test[healthFacilities_max == healthFacilities_mode, .(num_yrs_equal_to_mode = .N), by= c("dps", "health_zone")]
# test <- merge(test, test2, by=c("dps", "health_zone"), all=TRUE)

# merge test back to dt
dt <- merge(dt, test, all.x=TRUE, by=c("dps", "health_zone", "year"))

# TO DO - clarify this is what we wanted with David - also what would the direction of bias be here?    

# if health facilities total is missing, set it to be health facilities max (the max # of facilites in that year?)
dt[is.na(healthFacilities_total), healthFacilities_total := healthFacilities_max] 
# This created 66 new cases where num reporting > total facilities - in all of these cases, the original total facilities was NA
# set these to be NA too:
dt[ healthFacilities_numReported > healthFacilities_total, healthFacilities_numReported := NA]
# 1/22/19 changed this to divide reporting facilities by total facilities... make sure this is what we should do. 
dt[, healthFacilitiesProportion := ( healthFacilities_numReported / healthFacilities_total )]
if ( dt[healthFacilitiesProportion > 1, .N] > 0 ) stop("Proportion of health facilities reporting must be 1.0 or less")
  # intermediate save
  # saveRDS(dt, paste0(dir, output_dt))
# ----------------------------------------------   

# ----------------------------------------------  
# health zones showing up over time - adjust in data based on how we determined to handle them
# in troubleshooting_hz_matching.R
# ----------------------------------------------  
# Haut Katanga - Kashobwe
# Solution: do not impute data for Kashobwe before 2012; after imputation add to Kasenga where necessary. 
# Nord Kivu - Alimbongo
# Solution: do not impute Alimbongo before 2012; after imputation add to Lubero where necessary. 
# Nord Kivu - Kibirizi 
# Solution: do not impute Kibirizi before 2016; after imputation add to Rutshuru where necessary. 
# Nord Kivu - Mabalako
# Solution: do not impute Mabalako before 2013; after imputation add to Beni where necessary. 
# Nord Kivu - Kamango
# Solution: do not impute Kamango before 2012; after imputation add Kamango to Mutwanga or Oicha.


# Nord Kivu - Bambo, Itebero, Kibua, Nyiragongo, Kalungata
# Solution: They will be treated as missing, and imputed.
# Rectangularize the above health zones:

hzs <- unique(dt[health_zone %in% c("bambo", "itebero", "kibua", "nyiragongo", "kalunguta"), .(dps, health_zone)])
dates <- unique(dt$date)

rect <- hzs[rep(1:nrow(hzs), length(unique(dates))) ]
rect[, date:=rep(dates, each=nrow(hzs))]

dt <- merge(dt, rect, by=c("date", "health_zone", "dps"), all=TRUE)

#now, need to redo id variable
dt[, id := .I]
# ---------------------------------------------- 

# ----------------------------------------------  
# remove vars not needed for imputation - we will impute the proportion of health facilities reporting, and then use that to back-calculate the number reporting
# ----------------------------------------------  
dt[, c("healthFacilities_total_orig", "healthFacilities_numReported_orig", "healthFacilities_numReported", "healthFacilities_max", 
       "healthFacilities_numReportedWithinDeadline"):=NULL]
# ---------------------------------------------- 

# ----------------------------------------------  
# stock out outliers - get rid of any above 31
# ----------------------------------------------
id_vars = c("dps", "health_zone", "date", "year", "donor", "operational_support_partner", "population", "id")
dt_long = melt.data.table(dt, id.vars = id_vars, variable.factor = FALSE)
stockOut_vars = unique( dt_long$variable[ grepl(dt_long$variable, pattern = "stock") ] )
total = 0
for (var in stockOut_vars){
  n = dt_long[ variable == var & value > 31, .N]
  print(n)
  total_not_na = dt_long[ variable == var & !is.na(value), .N]
  print(paste0(round((n / total_not_na) * 100, 2), "% of non missing data for ", var, " is over 31 days"))
  dt_long[ variable == var & value > 31, set_to_na := TRUE]
  total = total+n
}
print(total)
if( dt_long[ set_to_na == TRUE, .N] == total) dt_long[ set_to_na == TRUE, value := NA ] 
if( dt_long[ set_to_na == TRUE, .N] != total) stop("something went wrong: set_to_na is not equal to total outliers in stock out data")
print(paste0(total, " data points set to NA in stock out data"))

dt_long[, set_to_na := NULL ]

dt = dcast.data.table(dt_long, id + dps + health_zone + date + year + donor + operational_support_partner + population ~ variable)
# ---------------------------------------------- 

# ----------------------------------------------  
# last data prep steps:
# ---------------------------------------------- 
dt[, dps := gsub(" ", "-", dps) ]
# ---------------------------------------------- 

# ----------------------------------------------  
# save final data preped for imputation
# ----------------------------------------------  
saveRDS(dt, paste0(dir, output_dt))
# ---------------------------------------------- 
