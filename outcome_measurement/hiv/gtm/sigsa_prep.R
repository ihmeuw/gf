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
library(readxl)
#--------------------

#-----------------------------------------
# Set up directories 
#-----------------------------------------
# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set working and output directories
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/hiv/')

# to output prepped files
out_dir = paste0(dir, 'prepped/intermediate_prepped/sigsa/')

# input files
f1 = paste0(dir, 'raw_data/sigsa/Patient Level Data/Solicitud 0593-2018 Sigsa 6 mensual producción TB y VIH 2014 al 2017_csv.csv')
f2 = paste0(dir, 'raw_data/sigsa/updated_data_11_19/Solicitud 1383-2019 SIGSA SIDA 1.2 año 2017.xlsx')
f3 = paste0(dir, 'raw_data/sigsa/updated_data_11_19/Solicitud 1383-2019 SIGSA SIDA 1.2 año 2018.xlsx')
f4 = paste0(dir, 'raw_data/sigsa/updated_data_11_19/Solicitud 1383-2019 SIGSA SIDA 1.2 enero a octubre 2019.xlsx')
#-----------------------------------------

#-----------------------------------------
# load in the file 
#-----------------------------------------
dt1 = data.table(read.csv(f1, skip=3, stringsAsFactors = FALSE))
dt2 = data.table(read_xlsx(f2, skip=3))
dt3 = data.table(read_xlsx(f3, skip=3))
dt4 = data.table(read_xlsx(f4, skip=3))
#-----------------------------------------

#-----------------------------------------
# format the column names and drop unecessary variables - original data
#-----------------------------------------
#subset to the hiv-related variables
dt1 = dt1[ , -c(31:37)]

# create a list of variable names
varNames = as.character(names(dt1))
varAlt = as.character(dt1[1])
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

setnames(dt1, names$new_names)

# drop row 1 (has variable names)
dt1 = dt1[-1]

# drop the last three rows - just summary information/often blank
dt1 = head(dt1, -3)
#-----------------------------------------

#-----------------------------------------
# format the column names and drop unecessary variables - new data
#-----------------------------------------
#subset to the hiv-related variables
dt2 = dt2[ , -c(31:37)]
dt3 = dt3[ , -c(31:37)]
dt4 = dt4[ , -c(31:37)]

# create a list of variable names
varNames = as.character(names(dt2))
names = data.table(varNames)

# rename the variables in english 
names[, new_names := c("health_area", "health_district", "muni", "health_service", "service_type", "date", 
                       "person_code", "birthplace_dept", "birthplace_muni", "birthday_day", "birthday_month", "birthday_year", 
                       "nationality", "gender", "residence_dept", "residence_muni", "sexual_orientation", "marital_status", "education_status",
                       "pueblo", "linguistic_community", "risk_condition", "orientation_reason", 
                       "pregnancy_postPartum", "pre_test", "test_done", "hiv_test", "hiv_testResult",
                       "hiv_confirmatoryTest", "hiv_confirmatoryTestResult", "reference")]

setnames(dt2, names$new_names)
setnames(dt3, names$new_names)
setnames(dt4, names$new_names)

# drop the last three rows - just summary information/often blank
dt2 = head(dt2, -3)
dt3 = head(dt3, -3)
dt4 = head(dt4, -3)
#-----------------------------------------

#-----------------------------------------
# format dates 
#-----------------------------------------
# format activity date
format_dates = function(dt, date.form = NULL){
  if(is.null(date.form)){stop('must pass date format')}
  dt[ , date:=as.character(date)]
  dt[ , birthday_day:=as.character(birthday_day)]
  dt[ , birthday_month:=as.character(birthday_month)]
  dt[ , birthday_year:=as.character(birthday_year)]
  
  dt[ , date:=as.Date(date, format=date.form)]
  
  # format birthdate
  dt[as.numeric(birthday_day) < 10, birthday_day:=paste0('0', birthday_day)]
  dt[as.numeric(birthday_month) < 10, birthday_month:=paste0('0', birthday_month)]
  dt[ ,dob:=ymd(paste0(birthday_year, birthday_month, birthday_day))]
  
  # calculate age
  dt[ , age:=as.numeric(round(((date - dob) / 365.25), 0))]
  
  # drop excess variables
  dt[ ,c('birthday_day', 'birthday_year', 'birthday_month', 'dob'):=NULL]
}

format_dates(dt1, '%m/%d/%Y')
format_dates(dt2, '%Y-%m-%d')
format_dates(dt3, '%Y-%m-%d')
format_dates(dt4, '%Y-%m-%d')
#-----------------------------------------

#-----------------------------------------
# combine years of data
#-----------------------------------------
dt1 = dt1[year(date)!= 2017] # Guillermo said use 2017 from new SIGSA data he sent, instead of this older file but we still need 2014-2016 from the old file
dt = rbindlist(list(dt1, dt2, dt3, dt4), use.names = TRUE, fill = TRUE)
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
dt[test_done == "Si", test_done := "1"]
dt[test_done == "No", test_done := "0"]

dt[hiv_test == "Si" | hiv_test == "Reactivo", hiv_test := "1"]
dt[hiv_test == "No", hiv_test := "0"] 
dt[hiv_testResult == "Reactivo", hiv_testResult := "1"]
dt[hiv_testResult == "No Reactivo", hiv_testResult := "0"]

dt[hiv_confirmatoryTest == "Si", hiv_confirmatoryTest := "1"]
dt[hiv_confirmatoryTest == "No", hiv_confirmatoryTest := "0"]
dt[hiv_confirmatoryTestResult == "Reactivo", hiv_confirmatoryTestResult := "1"]
dt[hiv_confirmatoryTestResult == "No Reactivo", hiv_confirmatoryTestResult := "0"]
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
dt[orientation_reason == "Pareja de Embarazada" & gender == "Female", pregnant:=TRUE]
dt[is.na(pregnant), pregnant:=FALSE]
#-----------------------------------------

#-----------------------------------------
# save a prepped version 
#-----------------------------------------
saveRDS(dt, paste0(out_dir, 'prepped_sigsa_data.rds'))
#-----------------------------------------





