# ----------------------------------------------
# Caitlin O'Brien- Carelli / Audrey Batzel
# 6/5/2019
#
# Format SIGSA HIV testing data
# To combine with SIGPRO
setwd("C:/local/gf")
# ----------------------------------------------

#--------------------
# Load packages 
#--------------------
rm(list=ls())
library(lubridate)
library(data.table)
library(ggplot2)
library(Hmisc)
library(stringr)
library(xlsx)
library(XLConnect)
library(tools)
#--------------------

#-----------------------------------------
# Set up directories 
#-----------------------------------------
# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set working and output directories
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/hiv/')

# to output prepped files
out_dir = paste0(dir, 'prepped/sigsa/')

# input files
sigsaFile = paste0(dir, 'sigsa/Patient Level Data/Solicitud 0593-2018 Sigsa 6 mensual producción TB y VIH 2014 al 2017_csv.csv')
#-----------------------------------------

#-----------------------------------------
# load in the file 
#-----------------------------------------
dt = data.table(read.csv(sigsaFile, skip=3, stringsAsFactors = FALSE))
#-----------------------------------------

#-----------------------------------------
# format the column names and drop unecessary variables 
#-----------------------------------------
#subset to the hiv-related variables
dt = dt[ , -c(31:37)]

# create a list of variable names
varNames = as.character(names(dt))
varAlt = as.character(dt[1])
names = data.table(cbind(varNames, varAlt))
names[grep("^X", varNames), varNames:=varAlt]

# rename the variables in english 
names[, new_names := c("year", "month", "health_area", "health_district", "health_service",
              "service_type", "muni", "date", "person_code", "birthplace_dept", "birthplace_muni",
              "birthday_day", "birthday_month", "birthday_year", "nationality", "gender", "residence_dept",
              "residence_muni", "sexual_orientation", "pueblo", "linguistic_community",
              "risk_condition", "orientation_reason", 
              "pregnancy_postPartum", "pre_test", "test_done", "hiv_test", "hiv_testResult",
              "hiv_confirmatoryTest", "hiv_confirmatoryTestResult", "reference")]

setnames(dt, names$new_names)

# drop rows 1 and 2 (include variable names)
dt = dt[-1]

# drop the last three rows - just summary information/often blank
dt = head(dt, -3)
#-----------------------------------------

#-----------------------------------------
# format dates 
#-----------------------------------------
# format activity date
dt[ , date:=as.Date(date, format='%m/%d/%Y')]

# format birthdate
dt[as.numeric(birthday_day) < 10, birthday_day:=paste0('0', birthday_day)]
dt[as.numeric(birthday_month) < 10, birthday_month:=paste0('0', birthday_month)]
dt[ ,dob:=ymd(paste0(birthday_year, birthday_month, birthday_day))]

# calculate age
dt[ , age:=as.numeric(round(((date - dob) / 365.25), 0))]

# drop excess variables
dt[ ,c('birthday_day', 'birthday_year', 'birthday_month', 'dob'):=NULL]
#-----------------------------------------

#-----------------------------------------
# format the data 
#-----------------------------------------
# where there is missing data - replace '-' with NA
vars = names(dt)[! names(dt) %in% c('age', 'date', 'year', 'month')]
for (v in vars) dt[get(v)=="-", (v):=NA]

# replace si and no values with logicals
dt[pre_test == "Si", pre_test := "1"]
dt[pre_test == "No", pre_test := "0"]
dt[hiv_test == "Si" | hiv_test == "Reactivo", hiv_test := "1"]
dt[hiv_test == "No", hiv_test := "0"] 
dt[test_done == "Si", test_done := "1"]
dt[test_done == "No", test_done := "0"]
dt[hiv_confirmatoryTest == "Si", hiv_confirmatoryTest := "1"]
dt[hiv_confirmatoryTest == "No", hiv_confirmatoryTest := "0"]
dt[hiv_confirmatoryTestResult == "Reactivo", hiv_confirmatoryTestResult := "1"]
dt[hiv_confirmatoryTestResult == "No Reactivo", hiv_confirmatoryTestResult := "0"]
dt[hiv_testResult == "Reactivo", hiv_testResult := "1"]
dt[hiv_testResult == "No Reactivo", hiv_testResult := "0"]
#-----------------------------------------

#-----------------------------------------
# format gender
#-----------------------------------------
dt[gender=="Femenino", gender:="Female"]
dt[gender=="Masculino", gender:="Male"]
#-----------------------------------------

#-----------------------------------------
# format the department and municipalities
#-----------------------------------------
sd_cols = c("health_area", "health_district", "health_service", "muni", "birthplace_dept", "birthplace_muni", "residence_dept", "residence_muni")
vars = names(dt)[ ! names(dt) %in% sd_cols ]
dt = dt[ , lapply(.SD, tolower), by = vars, .SDcols=sd_cols]
#-----------------------------------------

#-----------------------------------------
# add monthly date for graphs and a pregnancy binary
#-----------------------------------------
dt[ , month_date:=as.Date(paste0(year(date), '-', month(date), '-01'), '%Y-%m-%d')]
dt[ , c("year", "month") := NULL ]

# pregnancy binary
dt[orientation_reason=='Embarazo' | pregnancy_postPartum %in% c("Primer Trimestre (01 - 12 Semanas)", "Segundo Trimestre (13 - 28 Semanas)", "Tercer Trimestre (29 - 40 Semanas)"), pregnant:=TRUE]
dt[ orientation_reason == "Pareja de Embarazada" & gender == "Female", pregnant:=TRUE]
dt[is.na(pregnant), pregnant:=FALSE]
#-----------------------------------------

#-----------------------------------------
# save a prepped version 
#-----------------------------------------
saveRDS(dt, paste0(dir, 'prepped/sigsa/prepped_sigsa_data.rds'))
#-----------------------------------------





