# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 4/23/2018
# Multiple imputation for the Uganda Viral Load Dashboard
# ----------------------------------------------
# Set up R

rm(list=ls())
library(data.table)
library(rgeos)
library(raster)
library(ggplot2)
library(rgdal)
library(tibble)
library(dplyr)
library(RColorBrewer)
library(Amelia)
library(MASS)


# set input/output directory
# ----------------------------------------------
dir <- 'J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard'

# upload the data with month, year, sex
uganda_vl <- readRDS(paste0(dir, "/sex_data.rds"))

# start from scale up and continue to febrary 2018
uganda_vl <- uganda_vl[!(month==3 & year==2018)]
uganda_vl <- uganda_vl[year==2016 | year==2017 | year==2018]


# ----------------------------------------------
# prep the data for imputation 


# --------------
# create a list of the facility ids - change to include names
f_ids <- unique(uganda_vl$facility_id)
length(f_ids)

sexes <- unique(uganda_vl$sex)
dates <- seq(from=min(uganda_vl$date), to=max(uganda_vl$date), by='month')

expanded_data <- data.table(expand.grid(f_ids, dates, sexes))
setnames(expanded_data, c('facility_id', 'date','sex'))

# --------------------

# merge in the blank rows for facility_id (by date, sex)
uganda_vl <- merge(uganda_vl, expanded_data, by=c('facility_id', 'date','sex'), all=TRUE)

# --------------------

# add facility names
facilities <- data.table(facility_id=uganda_vl$facility_id, facility_name=uganda_vl$facility_name)
facilities <- facilities[is.na(facility_name)==FALSE]
facilities <- facilities[ , .(facility_id, facility_name), by=.(facility_id, facility_name)]
facilities <- facilities[ ,.(facility_id, facility_name)]

uganda_vl <- merge(uganda_vl, facilities, by='facility_id', all.x=TRUE)

# make facility_name.y the facility name and drop out x (contains missing values)
uganda_vl[,facility_name.x:=NULL]
setnames(uganda_vl, "facility_name.y", "facility_name")

# --------------------
# add districts
# districts <- data.table(district_id=uganda_vl$district_id, dist_name=uganda_vl$dist_name)
# districts <- districts[is.na(dist_name)==FALSE]
# districts <- districts[ , .(district_id, dist_name), by=.(district_id, dist_name)]
# districts <- districts[ ,.(district_id, dist_name)]
# 
# districts[(!district_id %in% uganda_vl$district_id), district_id]
# uganda_vl[(!district_id %in% districts$district_id) & !is.na(district_id), district_id]
# 
# # glitchy fix later
# uganda_vl <- merge(uganda_vl, districts, by='district_id', all.x=TRUE)
# 
# uganda_vl[is.na(facility_name.y), facility_id] # all ids matched with facility names
# 
# setnames(uganda_vl, "facility_name.y", "facility_name")


# ----------------------------------------------
# amelia  test
uvl <- uganda_vl[ ,.(facility_id, facility_name, district_id, sex, date,
                    patients_received, samples_received,  dbs_samples, total_results,
                    rejected_samples, valid_results, ratio=(suppressed/valid_results))]             


#Completed_data<-amelia(data,m=3,ts="year", cs="country", p2s=0, ords="polity", noms="signed", idvars=c("year","country"))

# should I include sex bc i want it in the moel
idVars <- c("facility_id", "facility_name", "district_id", "sex", "date")

#do I need ts?

imputed_data <- amelia(uvl, m=2, idvars=idVars)
View(imputed_data$imp1)









lm(logit(ratio)~log(samples received), data=dt)