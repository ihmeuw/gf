# ----------------------------------------------
# Caitlin O'Brien- Carelli
# 6/5/2019
#
# Format SIGSA HIV testing data
# Rewrite later as a function - many overlapping processes
# ----------------------------------------------

#-----------------------------------------------
# Load packages 
# ----------------------------------------------
rm(list=ls())
library(lubridate)
library(data.table)
library(ggplot2)
library(Hmisc)
library(stringr)
library(readxl)
library(XLConnect)
#---------------------------------------
# Set up directories 
#----------------------------------------

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set working and output directories
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/hiv/')
setwd(paste0(dir, 'sigsa/'))

# to output prepped files
out_dir = paste0(dir, 'prepped/sigpro/')

#-----------------------------------------
# load in the file 

dt = data.table(read.xlsx(paste0(dir,'sigsa/Patient Level Data/Solicitud 0593-2018 SIGSA SIDA 1.2 años 2014 al 2017.xlsx'), sheet=1))

#-----------------------------------------
# format the column names and drop unecessary variables 

# create a list of variable names
varNames = as.character(dt[1, ])
varAlt = as.character(dt[2, ])
names = data.table(cbind(varNames, varAlt))
names[is.na(varNames), varNames:=varAlt]
names = names$varNames

# replace with the variable names
setnames(dt, names)

# drop rows 1 and 2 (include variable names)
dt = dt[-(1:2)]

# drop the last three rows - just summary information
dt = head(dt, -3)

#subset to the hiv-related variables
dt = dt[ ,c(1:9, 12:19, 21:30, 38)]

#-----------------------------------------
# rename the variables in english 

new_names = c("year", "month", "health_area", "health_district", "health_service",
              "service_type", "muni", "date", "cui", "birthday_day",
              "birthday_month", "birthday_year", "nationality", "sex", "residence_dept",
              "residence_muni", "sexual_orientation", "linguistic_community",
              "risk_condition", "reason_for_orientation", 
              "pregnancy_post_partum", "pre_test", "test_completed", "hiv", "result",
              "confirmatory_test", "result", "referral")

setnames(dt, new_names)

#-----------------------------------------
# excel date origin is 1899

# format activity date
dt[ , date:=as.Date(as.numeric(date), origin='1899-12-30')]

# format birthdate
dt[as.numeric(birthday_day) < 10, birthday_day:=paste0('0', birthday_day)]
dt[as.numeric(birthday_month) < 10, birthday_month:=paste0('0', birthday_month)]
dt[ ,dob:=ymd(paste0(birthday_year, birthday_month, birthday_day))]

# calculate age
dt[ ,age:=as.numeric(round(((date - dob) / 365.25), 0))]

# drop excess variables
dt[ ,c('year', 'month', 'birthday_day', 
       'birthday_year', 'birthday_month', 'dob'):=NULL]

#-----------------------------------------
# format depatments


# format nationalities








