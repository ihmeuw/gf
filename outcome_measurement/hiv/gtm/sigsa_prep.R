# ----------------------------------------------
# Caitlin O'Brien- Carelli
# 6/5/2019
#
# Format SIGSA HIV testing data
# To combine with SIGPRO
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
library(xlsx)
library(XLConnect)
library(tools)
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

dt = data.table(read.csv(paste0(dir,'sigsa/Patient Level Data/Solicitud 0593-2018 Sigsa 6 mensual producción TB y VIH 2014 al 2017_csv.csv'), skip=3,
                         stringsAsFactors = FALSE))
#-----------------------------------------
# format the column names and drop unecessary variables 

# create a list of variable names
varNames = as.character(names(dt))
varAlt = as.character(dt[1])
names = data.table(cbind(varNames, varAlt))
names[grep("^X", varNames), varNames:=varAlt]
names = names$varNames

# replace with the variable names
setnames(dt, names)

# drop rows 1 and 2 (include variable names)
dt = dt[-1]

# drop the last three rows - just summary information/often blank
dt = head(dt, -3)

#subset to the hiv-related variables
dt = dt[ ,c(1:9, 12:19, 21:30, 38)]

#-----------------------------------------
# rename the variables in english 

new_names = c("year", "month", "health_area", "health_district", "health_service",
              "service_type", "muni", "date", "cui", "birthday_day",
              "birthday_month", "birthday_year", "nationality", "gender", "residence_dept",
              "residence_muni", "sexual_orientation", "linguistic_community",
              "risk_condition", "reason_for_orientation", 
              "pregnancy_post_partum", "pre_test", "test_completed", "hiv", "result",
              "confirmatory_test", "confirmatory_result", "referral")

setnames(dt, new_names)

#-----------------------------------------
# format dates 

# format activity date
dt[ , date:=as.Date(date, format='%m/%d/%Y')]

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
# format the data 

# replace missing data 
vars = names(dt)[names(dt)!='date' & names(dt)!='age']
for (v in vars) dt[get(v)=="-", (v):=NA]

# replace si and no values with logicals
dt[!is.na(pre_test), pre_test1:=(pre_test=='Si')]
dt[!is.na(test_completed), test_completed1:=(test_completed=='Si')]
dt[!is.na(hiv), hiv1:=(hiv=='Si')]
dt[!is.na(result), result1:=(result=='Si')]
dt[!is.na(confirmatory_test), confirmatory_test1:=(confirmatory_test=='Si')]
dt[!is.na(confirmatory_result), confirmatory_result1:=(confirmatory_result=='Si')]

# delete the character variables in favor of the logicals
dt[ ,c("pre_test", "test_completed", "hiv", "result",
      "confirmatory_test", "confirmatory_result"):=NULL]

# use the new variables
setnames(dt, c("pre_test1", "test_completed1", "hiv1", "result1",
               "confirmatory_test1", "confirmatory_result1"),
              c("pre_test", "test_completed", "hiv", "result",
               "confirmatory_test", "confirmatory_result"))

#-----------------------------------------
# format gender

dt[gender=="Femenino", gender:="Female"]
dt[gender=="Masculino", gender:="Male"]
#-----------------------------------------
# format the department and municipalities

names = c(names(dt)[1:2], names(dt)[12:length(dt)])


dt = dt[ ,lapply(.SD, tolower), by=names, .SDcols=c(3:5, 10:11)]
# dt = dt[ ,lapply(.SD, toTitleCase), by=names, .SDcols=c(1:3, 10:11)]

#-----------------------------------------
# save a prepped version 

saveRDS(dt, paste0(dir, 'prepped/sigsa/prepped_sigsa_data.rds'))

#-----------------------------------------





