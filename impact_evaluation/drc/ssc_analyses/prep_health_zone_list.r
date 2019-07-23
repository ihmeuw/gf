# ------------------------------------------------------------------------------
# David Phillips
# 
# 6/19/2019
# Prep lists of SSC health zones
# The current workign directory should be the root of this repo
# ------------------------------------------------------------------------------


# -------------------
# Set up R
rm(list=ls())
library(data.table)
library(readxl)
library(zoo)
# -------------------


# ---------------------------------------------------------------------------------------
# Files and directories

# root directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/')

# input file
inFile = paste0(dir, '/impact_evaluation/cod/prepped_data/second_half_data_pre_model.rdata')

# file listing health zones
hzFileUNICEF = paste0(dir, '/mapping/cod/ssc_lists/Unicef HZ extraction_BH.csv')
hzFileSANRU = paste0(dir, '/mapping/cod/ssc_lists/Cartographie des SSC FM.XLSX')

# output files
outFile = paste0(dir, '/mapping/cod/ssc_lists/prepped_hz_list.csv')

# function that standardizes health zone names
source('./core/standardizeHZNames.R')
# ---------------------------------------------------------------------------------------


# --------------------------------------------------------------------------
# Load/prep sanru data

# load
sanruList = data.table(read_excel(hzFileSANRU))

# rename
newNames = c('dps','health_zone','coverage_2018','gap','coverage_2019')
setnames(sanruList, names(sanruList)[1:5], newNames)

# drop blank rows and columns
sanruList = sanruList[, newNames, with=FALSE]
sanruList = sanruList[!is.na(health_zone)]

# drop subtotals
sanruList = sanruList[!dps %in% c('DPS','S/Tot','TOTAL GEN')]

# carry down dps names
sanruList[, dps:=na.locf(dps)]

# standardize names
sanruList[health_zone=='Bena (Manguredjipa)', health_zone:='biena']
sanruList[, health_zone:=gsub("\\s*\\([^\\)]+\\)","", health_zone)]
sanruList[, health_zone:=standardizeHZNames(health_zone)]
# --------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
# Compare to unicef file

# load
unicefList = fread(hzFileUNICEF)

# compare
length(sanruList$health_zone[!sanruList$health_zone %in% unicefList$health_zone])
unicefList$health_zone[!unicefList$health_zone %in% sanruList$health_zone]

# identify unicef health zones
unicefList[, unicef_supported:=1]
sanruList = merge(sanruList, unicefList[,-'dps',with=F], by='health_zone', all.x=TRUE)
sanruList[is.na(unicef_supported), unicef_supported:=0]
# ---------------------------------------------------------------------------------------


# --------------------------------------------
# Save
write.csv(sanruList, outFile, row.names=FALSE)
# --------------------------------------------
