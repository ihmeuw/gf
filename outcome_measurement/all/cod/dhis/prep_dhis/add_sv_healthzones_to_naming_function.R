
# working directory should be the root of the repository
setwd("") 

#-------------------------------------
# Audrey Batzel 
# The names in supervisions data are not consistent with health zone names in other places in the data 
# so this script adds those to the naming function spreadsheet
# May 2019
rm(list=ls())
#-------------------------------------

#--------------------------------------------------
# Set up R, and read in data 
#--------------------------------------------------
library(data.table)

# directories:
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')
dir = paste0(j, "Project/Evaluation/GF/outcome_measurement/cod/dhis_data/pre_prep/")

# input files
supervisionsFile = "supervisions/Supervisions_2017_2018.rds"
sscFile = "ssc/SSC_data_2017_2018.rds"

hzNamesFile = './core/standardized_hzs_final_inc_pnlt.csv'
dpsNamesFile = './core/alternate_hz_spellings.csv'

# output files
hzNamesFixed = './core/hz_renaming_file.csv'
dpsNamesFixed = './core/dps_renaming_file.csv'

# functions
source("./core/standardizeHZNames.R")
source("./core/standardizeDPSNames.R")
#--------------------------------------------------

#--------------------------------------------------
# load data
#--------------------------------------------------
hzNames = fread(hzNamesFile, header=TRUE, stringsAsFactors = FALSE)
dpsNames = fread(dpsNamesFile, header=TRUE, stringsAsFactors = FALSE)
sv = as.data.table(readRDS(paste0(dir, supervisionsFile)))
#--------------------------------------------------

#--------------------------------------------------
# add sv names to spreadsheet of different names
#--------------------------------------------------
names(sv) = tolower(names(sv))

dt = unique(sv[, .(dps, hz)])

# save original dps and hz names in sv
dt[, dps_sv := dps]
dt[, hz_sv := hz]

# manually standardize dps and hz names in sv
dt$health_zone1 = unlist(lapply(strsplit(dt$hz, " "), "[", 2))
dt$health_zone2 = unlist(lapply(strsplit(dt$hz, " "), "[", 3))
dt$health_zone3 = unlist(lapply(strsplit(dt$hz, " "), "[", 4))
dt[!is.na(health_zone2) & !is.na(health_zone3), health_zone := paste(health_zone1, health_zone2, health_zone3) ]
dt[!is.na(health_zone2) & is.na(health_zone3),  health_zone := paste(health_zone1, health_zone2)]
dt[is.na(health_zone2) & is.na(health_zone3),  health_zone := health_zone1]
dt[ , c('health_zone1', 'health_zone2', 'health_zone3', 'hz') := NULL]

dt$dps1 = unlist(lapply(strsplit(dt$dps, " "), "[", 2))
dt$dps2 = unlist(lapply(strsplit(dt$dps, " "), "[", 3))
dt[!is.na(dps2),  dps := paste(dps1, dps2)]
dt[is.na(dps2),  dps := dps1]
dt[ , c('dps1', 'dps2') := NULL]

# note, before running these lines again, would need to switch the naming functions' files back to orig files
dt[, health_zone := standardizeHZNames(health_zone)]
dt[, dps := standardizeDPSNames(dps)]

hzNames[, V1:= NULL ]
hzNames = merge(hzNames, dt, all = TRUE, by = c("dps", "health_zone"))

dpsNames = merge(dpsNames, dt, all = TRUE, by = c("dps", "health_zone"))

write.csv(hzNames, hzNamesFixed)
write.csv(dpsNames, dpsNamesFixed)
#--------------------------------------------------

#--------------------------------------------------
# test -- changed files read into functions
#--------------------------------------------------
# source functions again:
source("./core/standardizeHZNames.R")
source("./core/standardizeDPSNames.R")
sv = as.data.table(readRDS(paste0(dir, supervisionsFile)))

sv[, health_zone := standardizeHZNames(HZ)]
sv[, dps := standardizeDPSNames(Dps)]

ssc = as.data.table(readRDS(paste0(dir, sscFile)))

ssc[, health_zone := standardizeHZNames(HZ)]
ssc[, dps := standardizeDPSNames(Dps)]
#--------------------------------------------------


