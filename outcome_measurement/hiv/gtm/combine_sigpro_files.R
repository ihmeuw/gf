# ----------------------------------------------
# Caitlin O'Brien- Carelli
# Combine the SIGPRO Testing data sets

# ----------------------------------------------

#-----------------------------------------------
# Install packages 
# ----------------------------------------------
rm(list=ls())
library(lubridate)
library(data.table)
library(openxlsx)
library(ggplot2)
library(Hmisc)
library(stringr)
library(XLConnect)
#---------------------------------------
# Set up directories 
#----------------------------------------

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set working and output directories
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/hiv/')

#----------------------------------------
# load sigpro testing data 

f1 = readRDS(paste0(dir, 'prepped/sigpro_f1_completo 2018.xlsx_prepped.RDS'))
f2 = readRDS(paste0(dir, 'prepped/sigpro_f2_completo 2018.xlsx_prepped.RDS'))
f3 = readRDS(paste0(dir, 'prepped/sigpro_f3_completo 2018.xlsx_prepped.RDS'))
f4 = readRDS(paste0(dir, 'prepped/sigpro_f4_completo 2018.xlsx_prepped.RDS'))
f5 = readRDS(paste0(dir, 'prepped/sigpro_f4_JanNov2018 - PB_TVC.csv_prepped.RDS'))

#----------------------------------------
# combine the data sets - f3 and f4
# patient level data

# subset and combine similar data sets
vars = c("set", "sr_code", "department", "muni", "date",  
         "theme", "pop", "subpop", "gender", 
         "result", "informed_of_result", "flag")

f3 = f3[, vars, with=FALSE]
f4 = f4[, vars, with=FALSE]

# bind and add variable to fit with f2
f = rbind(f3, f4)
f[ , sr:=NA]

#---------------------------
# combine the data sets - f2 with f3 and f4

# subset and combine the patient level data
vars2 = c("set", "sr_code", "sr", "department", "muni", "date",  
          "pop", "subpop", "gender", 
         "result", "informed_of_result", "flag")

# subset and add theme
f2 = f2[ ,vars2, with=FALSE]
f2[ ,theme:=NA]

# bind it in
f = rbind(f, f2)

#---------------------------
# add sr names based on codes and names in other data sets
# 
# f[sr_code==401, sr:='otrans']
# f[sr_code==402, sr:='fma']
# f[sr_code==403, sr:='conevih']
# f[sr_code==404, sr:='gp']
# f[sr_code==405, sr:='ffi']
# f[sr_code==406, sr:='gente nueva']
# f[sr_code==407, sr:='idei']
# f[sr_code==408, sr:='proyecto vida']
# f[sr_code==409, sr:='idei']

#---------------------------
# add values - patients level so each row is 1
f[ ,value:=1]

#---------------------------
# drop erroneous values

# check unique values for erroneous values
f[ ,unique(sr_code), by=sr]
f[ ,unique(department)]
f[ ,unique(muni)]

#---------------------------
# fix outlier dates 

# test = f[ ,.(test_completed=sum(test_completed)), by=date]
# 
# ggplot(test, aes(x=date, y=test_completed)) +
#   geom_point() +
#   geom_line()

f[year(date) < 2013, date:=NA] # reporting appears to begin in 2013
f['2018-08-01' < date, date:=NA]

#-------------------------------------------------------
# sum the final results 

# drop informed of result - early data only has positive or negative
# no information in f1 on informing patients
f = f[ ,.(value=sum(value)), by=.(set, sr, sr_code,
        department, muni, date, theme, pop, subpop,
        gender, result)]

#---------------------------
# aggregate to the sr, muni, department level

# fix the age categories
# drop out 'total' age groups
f1 = f1[age!="total"]

# update the age
f1[!grepl("-", age) & !grepl("<", age) & !grepl("\\+", age), age:=NA]

#---------------------------
# keep only hiv-related data 

f1 = f1[grep("vih", result)]

# format the test results to resemble the other data
f1[grep("negativo", result), result:="nonreactive"]
f1[grep("positivo", result), result:="reactive"]
f1[grep("presuntivo", result), result:="test not done"]

#---------------------------
# create subpopulations 

# reset variable name
setnames(f1, "category", "subpop")
f1[subpop=='mujeres' | subpop=='hombres' | subpop=='trans', subpop:=NA] # captured in gender

#---------------------------
# drop unecessary variables and create variables for rbind

f1[ ,c("flag", "pregnant"):=NULL]

# add variables in f
f1[ ,department:=NA]
f1[ ,muni:=NA]
f1[ ,theme:=NA]

# finalize shaped long
setnames(f1, "total", "value")

#---------------------------
# sum over the age categories

otherVars = names(f1)[names(f1)!='value' & names(f1)!='age']
f1 = f1[ ,.(value=sum(value)), by=otherVars]

#---------------------------
# bind the data together

f = rbind(f, f1)

#---------------------------
# add f5

f5 = readRDS(paste0(dir, 'prepped/sigpro_f4_JanNov2018 - PB_TVC.csv_prepped.RDS'))

# format it




#--------------------------------------------------------
# save the product

saveRDS(f, paste0(dir, 'prepped/sigpro_october_2018_transfer_prepped.RDS'))

#--------------------------------------------------------



