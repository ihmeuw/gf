# ----------------------------------------------
# Caitlin O'Brien-Carelli

# 5/1/2018
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
# create a list of the facility ids

# store list of unique faciltiies, sexes and dates
f_ids <- unique(uganda_vl$facility_id)
length(f_ids)
sexes <- unique(uganda_vl$sex)
dates <- seq(from=min(uganda_vl$date), to=max(uganda_vl$date), by='month')

# make a "fully rectangularized" dataset with all months for each facility-sex
expanded_data <- data.table(expand.grid(f_ids, dates, sexes))
setnames(expanded_data, c('facility_id', 'date', 'sex'))
# --------------------

# merge in the blank rows for facility_id (by date, sex)
uganda_vl <- merge(uganda_vl, expanded_data, by=c('facility_id', 'date', 'sex'), all=TRUE)

#----------------------------------
# create a for loop that fills in missing values

ids <- uganda_vl[!is.na(facility_name), .(facility_id=unique(facility_id)),  
          by=.(facility_name, level, dhis2name, district_id, district_name)]

for (f in ids$facility_id) {

uganda_vl <- uganda_vl[is.na(facility_name) & facility_id==f, level:=ids[facility_id==f]$level]  
uganda_vl <- uganda_vl[is.na(facility_name) & facility_id==f, dhis2name:=ids[facility_id==f]$dhis2name]   
uganda_vl <- uganda_vl[is.na(facility_name) & facility_id==f, district_id:=ids[facility_id==f]$district_id]   
uganda_vl <- uganda_vl[is.na(facility_name) & facility_id==f, district_name:=ids[facility_id==f]$district_name] 
uganda_vl <- uganda_vl[is.na(facility_name) & facility_id==f, facility_name:=ids[facility_id==f]$facility_name]

print(f)
}

View(uganda_vl)
uganda_vl[is.na(facility_name)]

 
#---------------------

# check for duplicates
# create a unique identifier (char) of facilityid_date_sex
uganda_vl[sex=="Female", sex1:=1 ]
uganda_vl[sex=="Male", sex1:=2]
uganda_vl[sex=="Unknown", sex1:=3]

uganda_vl[ ,combine1:= paste0(facility_id, '_', date, '_', sex1)]
uganda_vl[,length(unique(combine1))] 

uganda_vl[duplicated(combine1)] # no duplicates
uganda_vl[,combine1:=NULL] # then delete the identifier

# ----------------------------------------------
# prep the data for the imputation

#------------------------
# change integers to doubles
Vars <- c("patients_received", "samples_received", "rejected_samples","dbs_samples", "plasma_samples",
          "total_results",  "valid_results", "suppressed")


uganda_vl <- uganda_vl[,lapply(.SD, as.double), by=c('facility_id', 'facility_name', 'dhis2name', 
                                               'district_id', 'district_name', 'sex', 'month', 
                                               'year', 'date'), .SDcols=Vars]

# keep track of zeroes
uganda_vl[, valid_results_zero:=valid_results==0]

# transform the 0s to the 1st percentile
uganda_vl[patients_received==0, patients_received:=1]
uganda_vl[samples_received==0, samples_received:=1]
uganda_vl[rejected_samples==0, rejected_samples:=1]
uganda_vl[dbs_samples==0, dbs_samples:=1]
uganda_vl[plasma_samples==0, plasma_samples:=1]
uganda_vl[total_results==0, total_results:=1]
uganda_vl[valid_results==0, valid_results:=1]

# create the suppression ratio
uganda_vl[ , ratio:=(suppressed/valid_results)]

# compute district average ratio among nonzeroes
uganda_vl[valid_results_zero==FALSE, district_ratio:=mean(ratio, na.rm=TRUE), by='district_id']
uganda_vl[, district_ratio:=mean(ratio, na.rm=TRUE), by='district_id']

# replace ratio to district average when the number of samples was zero
uganda_vl[valid_results_zero==TRUE, ratio:=district_ratio]

# log all the variables to impute
uganda_vl <- uganda_vl[,lapply(.SD, log), by=c('ratio', 'facility_id', 'facility_name', 'dhis2name', 
                                               'district_id', 'district_name', 'sex', 'month', 
                                               'year', 'date'), .SDcols=Vars]


# ---------------
# create a data set to use for imputation
# merge in facility names, etc. after imputation
# leave out numerator (suppressed); include only ratio and denominator

uvl <- uganda_vl[  ,.( patients_received, samples_received, rejected_samples, dbs_samples, plasma_samples,
                      total_results, valid_results, ratio), 
                      by=.(facility_id, sex, date)]


# ---------------
# amelia  test

# make cs variable
uvl[, cs_variable:=paste0(facility_id, sex)]
uvl$facility_id <- NULL
uvl$sex <- NULL

# run imputation
imputed_data <- amelia(uvl, m=2, cs='cs_variable', ts='date', lags='ratio')




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

sumVars = c("patients_received", "samples_received", "rejected_samples","dbs_samples", 
            "total_results",  "valid_results", "suppressed")

uganda_vl[, lapply(.SD, sum, na.rm=TRUE), by=c('facility_id','sex','date'), .SDcols=sumVars]




uganda_vl[ ,.(patients_received=sum(patients_received), 
              samples_received=sum(samples_received),
              rejected_samples=sum(rejected_samples),
              dbs_samples=sum(dbs_samples), 
              total_results=sum(total_results),
              valid_results=sum(valid_results),
              suppressed=sum(suppressed))]


#----------------------------------

# collapse to facility-sex-date level to ensure unique identifiers
# sumVars = c("patients_received", "samples_received", "dbs_samples", "total_results", "rejected_samples", "valid_results", "suppressed")
# 
# #uganda_vl <- uganda_vl[, lapply(.SD, sum, na.rm=TRUE), by=c('facility_id','sex','date'), .SDcols=sumVars]
# 
# #  test unique identifiers
# #test = nrow(uganda_vl[duplicated(uganda_vl[,c('facility_id','sex','date'),with=F])])
# if (test>0) stop('Facility-sex-hub-date does not uniquely identify rows! This is necessary for Amelia to do cs/ts operations!')
