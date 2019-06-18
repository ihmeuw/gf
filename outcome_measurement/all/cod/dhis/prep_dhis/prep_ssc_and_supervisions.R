setwd("C:/local/gf/") 
# working directory should be the root of the repository

#-------------------------------------
# AUTHOR: Emily Linebarger / Audrey Batzel (modified 5/7/19 to combine, simplify, and fix duplicates)
# PURPOSE: Prep SSC and supervisions data from PNLS
# DATE: April 2019
rm(list=ls())
#-------------------------------------

#--------------------------------------------------
# Set up R, and read in data 
#--------------------------------------------------
library(data.table)

# directories:
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')
dir = paste0(j, "Project/Evaluation/GF/outcome_measurement/cod/dhis_data/")

# input files
inFile = "pre_prep/ssc_supervisions/ssc_supervisions_data_01_2017_04_2019.xls"
cat = "catalogues/data_elements_cod.csv"
  
# output files
outFile = 'prepped/ssc_supervisions_prepped.rds'

# functions
source('./core/standardizeDPSNames.R')
source('./core/standardizeHZNames.R')

# load data
dt = as.data.table(read_excel(paste0(dir, inFile)))
catalog = fread(paste0(dir, cat))
#--------------------------------------------------

#--------------------------------------------------
# use meta data to get dps names matched to dps codes
#--------------------------------------------------
# meta data for getting names
facilities = data.table(readRDS(paste0(dir, 'meta_data/master_facilities.rds')))
# get dps names merged to completeness data based on the dps code in the health zone name
dps_code_matching = unique(facilities[, .(health_zone, dps)])
dps_code_matching$dps_code = unlist(lapply(strsplit(dps_code_matching$dps, " "), "[", 1))
dps_code_matching = unique(dps_code_matching[, .(dps, dps_code)])

dps_code_matching$dps1 = unlist(lapply(strsplit(dps_code_matching$dps, " "), "[", 2))
dps_code_matching$dps2 = unlist(lapply(strsplit(dps_code_matching$dps, " "), "[", 3))
dps_code_matching[dps2 != 'Province', dps:=paste(dps1, dps2) ]
dps_code_matching[dps2=='Province', dps:=dps1]
dps_code_matching[ , c('dps1', 'dps2'):=NULL]
dps_code_matching = dps_code_matching[ !is.na(dps)]
if(nrow(dps_code_matching) != 26) stop("something went wrong, there should be 26 DPS!")

dps_code_matching$dps = standardizeDPSNames(dps_code_matching$dps)
#--------------------------------------------------

#--------------------------------------------------
# Standardize variable names  
#--------------------------------------------------
setnames(dt, "periodname", "date")
setnames(dt, "organisationunitname", "health_zone")

# edit dates to be usable in the format we want - depends on whether input file is quarterly or monthly
dt = melt.data.table(dt, id.vars = c("date", "health_zone"), variable.name = "element", value.name = "value", variable.factor = FALSE)
dt[, value := as.numeric(value)]

# clean up health zone and dps names
dt$dps_code = unlist(lapply(strsplit(dt$health_zone, " "), "[", 1))

# standardize health zone names
dt$health_zone1 = unlist(lapply(strsplit(dt$health_zone, " "), "[", 2))
dt$health_zone2 = unlist(lapply(strsplit(dt$health_zone, " "), "[", 3))
dt$health_zone3 = unlist(lapply(strsplit(dt$health_zone, " "), "[", 4))
dt[health_zone3 != 'Zone' & health_zone2 != 'Zone', health_zone:=paste(health_zone1, health_zone2, health_zone3) ]
dt[health_zone3=='Zone', health_zone:=paste(health_zone1, health_zone2)]
dt[health_zone2=='Zone', health_zone:=health_zone1]
dt[ , c('health_zone1', 'health_zone2', 'health_zone3'):=NULL]
dt[, health_zone := standardizeHZNames(health_zone)]

# merge dps names onto dt
dt = merge(dt, dps_code_matching, by = "dps_code")
# ----------------------------------------------

#--------------------------------------------------
# Merge with element catalog 
#--------------------------------------------------
dt = merge(dt, catalog[, .(element, element_eng, element_id)], by='element', all.x=TRUE) 
#--------------------------------------------------

#--------------------------------------------------
# Create other variable names  
#--------------------------------------------------
dt[grepl(element, pattern = "B 10.2|B 7.1|B 7.2|B 8.1|B 9.2"), data_set := 'secondaires'] 
dt[grepl(element, pattern = "F 1.4"), data_set := 'supervisions']
dt[grepl(element, pattern = "A 4.1"), data_set := 'base']

dt[, level:="health_zone"]
#--------------------------------------------------

#--------------------------------------------------
# need to sum over health zones because the renaming functions combines two sets of two different health zones 
# unique idenitifiers test will fail without this
#--------------------------------------------------
id_vars = names(dt)[!names(dt) %in% "value"]
dt = dt[, .(value = sum(value)), by = id_vars]
#--------------------------------------------------

#--------------------------------------------------
# check unique ids
#--------------------------------------------------
# test unique identifiers:
if (nrow(dt) != nrow( unique( dt[, .(date, health_zone, dps, element)]))) {stop('Unique identifiers do not uniquely identify rows in ssc')}
#--------------------------------------------------

#--------------------------------------------------
# fix dates
#--------------------------------------------------
# ssc dates
dt[, c("month", "year"):= tstrsplit(date, " ")]
mos = unique(dt[, .(month, year)])
mos = mos[seq(dim(mos)[1],1),]
mos[, month_number := c(seq(1:12), seq(1:12), seq(1:4))]
mos[, date := as.Date(as.yearmon(paste(year, month_number, sep = "-")))]
dt = merge(dt, mos, by = c("year", "month"), all = TRUE)
setnames(dt, "date.y", "date")
dt[, c("year", "month", "dps_code", "date.x") := NULL]
#--------------------------------------------------

#--------------------------------------------------
# Save the prepped data sets
#--------------------------------------------------
saveRDS(dt, paste0(dir, outFile))
#--------------------------------------------------
