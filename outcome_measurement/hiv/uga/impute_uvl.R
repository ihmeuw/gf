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
# split the unknowns by sex ratio

uganda_vl[sex=='Unknown', length(facility_id)]

uganda_vl[sex=='Male', .(male_patients:=sum(patients_received)), by=.(facility_id, date)]
uganda_vl[sex=='Female', male_patients:=0]
uganda_vl[sex=='Unknown', male_patients:=0]

uganda_vl[sex=='Female', female_patients:=sum(patients_received)]
uganda_vl[sex=='Male', female_patients:=0]
uganda_vl[sex=='Unknown', female_patients:=0]


uganda_vl[sex!='Unknown', known_patients:=sum(patients_received)]


uganda_vl[ ,.((total=sum(male_patients+female_patients, na.rm=T)), known_patients), by=(date)]


uganda_vl[  , sex_ratio:=
            by=.(facility_id, date)]





# ----------------------------------------------
# prep the data for imputation 

# --------------
# create a list of the facility ids - change to include names

# store list of unique faciltiies, sexes and dates
f_ids <- unique(uganda_vl$facility_id)
length(f_ids)
sexes <- unique(uganda_vl$sex)
dates <- seq(from=min(uganda_vl$date), to=max(uganda_vl$date), by='month')

# make a "fully rectangularized" dataset with all months for each facility-sex
expanded_data <- data.table(expand.grid(f_ids, dates, sexes))
setnames(expanded_data, c('facility_id', 'date','sex'))
# --------------------

# merge in the blank rows for facility_id (by date, sex)
uganda_vl <- merge(uganda_vl, expanded_data, by=c('facility_id', 'date','sex'), all=TRUE)

#----------------------------------
# descriptive statistics when females are missing and males are in the data
uganda_vl[, combine2:= paste0(facility_id, '_', date)]

missing_fems <- uganda_vl[(sex=='Female' & is.na(combine)), .(combine2) ]

uvl_males <- uganda_vl[sex=="Male"]
uvl_males <- merge(missing_fems, uvl_males, by='combine2')
uvl_males <- uvl_males[!is.na(patients_received)]

#----------------------------------
missing_lads <- uganda_vl[(sex=='Male' & is.na(combine)), .(combine2) ]

uvl_females <- uganda_vl[sex=="Female"]
uvl_females <- merge(missing_lads, uvl_females, by='combine2')
uvl_females <- uvl_females[!is.na(patients_received)]


#----------------------------------

# collapse to facility-sex-date level to ensure unique identifiers
sumVars = c("patients_received", "samples_received", "dbs_samples", "total_results", "rejected_samples", "valid_results", "suppressed")

#uganda_vl <- uganda_vl[, lapply(.SD, sum, na.rm=TRUE), by=c('facility_id','sex','date'), .SDcols=sumVars]

#  test unique identifiers
#test = nrow(uganda_vl[duplicated(uganda_vl[,c('facility_id','sex','date'),with=F])])
if (test>0) stop('Facility-sex-hub-date does not uniquely identify rows! This is necessary for Amelia to do cs/ts operations!') 

#---------------------

# check for duplicates
# create a unique identifier (char) of facilityid_date_sex
uganda_vl[sex=="Female", sex1:=1 ]
uganda_vl[sex=="Male", sex1:=2]
uganda_vl[sex=="Unknown", sex1:=3]

uganda_vl[ ,combine1:= paste0(facility_id, '_', date, '_', sex1)]
uganda_vl[,length(unique(combine1))] 

uganda_vl[duplicated(combine1)] # no duplicates
# ---------------




# ----------------------------------------------
# amelia  test
           
uvl <- uganda_vl[ ,.(facility_id, sex, date, 
                    patients_received, samples_received,  dbs_samples, total_results,
                    rejected_samples, valid_results, ratio=(suppressed/valid_results))] 

uvl[, random:=runif(nrow(uvl))]

# make cs variable
uvl[, cs_variable:=paste0(facility_id, sex)]
uvl$facility_id <- NULL
uvl$sex <- NULL

# run imputation
imputed_data <- amelia(uvl, m=2, cs='cs_variable', ts='date', lags='ratio')


# View(imputed_data$imp1)

# graph one test case to see how it looks
cstmp = sample(unique(uvl$cs_variable),1)
merged = merge(uvl, imputed_data[[1]]$imp1, by=c('cs_variable','date'), all=T)
ggplot(merged[cs_variable==cstmp], aes(y=ratio.y, x=date)) + 
	geom_point(color='red') + 
	geom_point(aes(y=ratio.x), color='black') + 
	labs(title=paste('Facility-sex:', cstmp))




# lm(logit(ratio)~log(samples received), data=dt)





# --------------------

# # add facility names
# facilities <- data.table(facility_id=uganda_vl$facility_id, facility_name=uganda_vl$facility_name)
# facilities <- facilities[is.na(facility_name)==FALSE]
# facilities <- facilities[ , .(facility_id, facility_name), by=.(facility_id, facility_name)]
# facilities <- facilities[ ,.(facility_id, facility_name)]

# uganda_vl <- merge(uganda_vl, facilities, by='facility_id', all.x=TRUE)

# # make facility_name.y the facility name and drop out x (contains missing values)
# uganda_vl[,facility_name.x:=NULL]
# setnames(uganda_vl, "facility_name.y", "facility_name")

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

