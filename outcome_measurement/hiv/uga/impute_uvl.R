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

# store list of unique faciltiies, sexes and dates
f_ids <- unique(uganda_vl$facility_id)
length(f_ids)
sexes <- c('Female', 'Male')
dates <- seq(from=min(uganda_vl$date), to=max(uganda_vl$date), by='month')

# make a "fully rectangularized" dataset with all months for each facility-sex
expanded_data <- data.table(expand.grid(f_ids, dates, sexes))
setnames(expanded_data, c('facility_id', 'date','sex'))
# --------------------

# merge in the blank rows for facility_id (by date, sex)
uganda_vl <- merge(uganda_vl, expanded_data, by=c('facility_id', 'date','sex'), all.y=TRUE)

#----------------------------------

missing_fems <- uganda_vl[(sex=='Female' & is.na(patients_received)), .(facility_id, date) ]

males_test<- uganda_vl[(sex == "Male" & facility_id %in% missing_fems$facility_id & date %in% missing_fems$date ),
                       .(facility_id, date, patients_received)]

males_test[ facility_id==2, .(sum(patients_received, na.rm=T))]


#----------------------------------

# collapse to facility-sex-date level to ensure unique identifiers
sumVars = c("patients_received", "samples_received", "dbs_samples", "total_results", "rejected_samples", "valid_results", "suppressed")
uganda_vl <- uganda_vl[, lapply(.SD, sum, na.rm=TRUE), by=c('facility_id','sex','date'), .SDcols=sumVars]

#  test unique identifiers
test = nrow(uganda_vl[duplicated(uganda_vl[,c('facility_id','sex','date'),with=F])])
if (test>0) stop('Facility-sex-hub-date does not uniquely identify rows! This is necessary for Amelia to do cs/ts operations!') 


# ----------------------------------------------
# amelia  test
           
uvl <- uganda_vl[ ,.(facility_id, sex, date,
                    patients_received, samples_received,  dbs_samples, total_results,
                    rejected_samples, valid_results, ratio=(suppressed/valid_results))]             

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

