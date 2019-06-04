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
library(ggplot2)
library(stringr)
library(tools)
#---------------------------------------
# Set up directories 
#----------------------------------------

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set working and output directories
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/hiv/')

#----------------------------------------
# load sigpro testing data 

f1 = readRDS(paste0(dir, 'prepped/sigpro/sigpro_f1_completo 2018.xlsx_prepped.RDS'))
f2 = readRDS(paste0(dir, 'prepped/sigpro/sigpro_f2_completo 2018.xlsx_prepped.RDS'))
f3 = readRDS(paste0(dir, 'prepped/sigpro/sigpro_f3_completo 2018.xlsx_prepped.RDS'))
f4 = readRDS(paste0(dir, 'prepped/sigpro/sigpro_f4_completo 2018.xlsx_prepped.RDS'))
f5 = readRDS(paste0(dir, 'prepped/sigpro/sigpro_f4_JanNov2018 - PB_TVC.csv_prepped.RDS'))

#----------------------------------------
# Remove errant values in all five data sets

# f1 errors
f1 = f1[age!='total']
f1[age=="41913" | age=="43348", age:=NA] # months over 12 already have missing dates

# f2 errors 
f2[year(date)!=2014 & year(date)!=2015 & year(date)!=2016, date:=NA]

# f3 errors - impossible dates; no errors in f4
f3[year(date) < 2016, date:=NA]

# f5 errors - values after the extraction
f5['2019-02-01' < date, date:=NA]

#--------------------------------------------------------------------------

#--------------------------------------------------------------------------
# FORMAT SETS AND COMBINE 

#------------------------------------------
# combine the patient level data sets - f3, f4, and f5

# subset and combine similar data sets
vars = c("set", "sr_code", "department", "muni", "date",  
         "theme", "pop", "subpop", "gender", 
         "result", "informed_of_result")

f3 = f3[, vars, with=FALSE]
f4 = f4[, vars, with=FALSE]
f5 = f5[,vars, with=FALSE]
f = rbind(f3, f4, f5)

# add an SR category to merge with f2
f[ , sr:=NA]

#---------------------------
# combine the data sets - f2 with f3, f4, f5

# subset and combine the patient level data
vars2 = c("set", "sr_code", "sr", "department", "muni", "date",  
          "pop", "subpop", "gender", 
         "result", "informed_of_result")

# subset and add theme
f2 = f2[ ,vars2, with=FALSE]
f2[ , theme:=NA]

# bind it in
# keep informed of result - only NA if the test was not done 
# f now includes all patient level data 
f = rbind(f, f2) # all patient level data not in a single data set

#---------------------------
# aggregate patient level data to the SR level 
# add values - patients level so each row is 1

f[ ,value:=1]

# fix sr names with poor formatting
f[ , sr:=trimws(sr)]

#-------------------------------------------------------
# COMBINE WITH SR LEVEL DATA 

# sum to the sr/gender level
# drop informed of result - early data only has positive or negative
# no information in f1 on informing patients
f = f[ ,.(value=sum(value)), by=.(set, sr, sr_code,
        department, muni, date, theme, pop, subpop,
        gender, result)]

#---------------------------
# aggregate to the sr, muni, department level

# fix the age categories
f1[!grepl("-", age) & !grepl("<", age) & !grepl("\\+", age), age:=NA]

#---------------------------
# keep only hiv-related data, drop syphilis

f1 = f1[grep("vih", result)]

# format the test results to contain the same variables as the other data
f1[grep("negativo", result), result:="nonreactive"]
f1[grep("positivo", result), result:="reactive"]
# classify presumptive as test not done 
f1[grep("presuntivo", result), result:="test not done"] 

#---------------------------
# create subpopulations 

# reset variable name
setnames(f1, "category", "subpop")

# finalize shaped long
setnames(f1, "total", "value")

#---------------------------
# drop unecessary variables and create variables for rbind

f1[ ,c("flag", "pregnant", "numeroInforme"):=NULL] # subpop captures pregnancy

# add variables that occur in the patient level data captured in f
f1[ , department:=NA]
f1[ , muni:=NA]
f1[ , theme:=NA]

#---------------------------
# sum over the age categories

byVars = names(f1)[names(f1)!='value' & names(f1)!='age']
f1 = f1[ ,.(value=sum(value)), by=byVars]

#---------------------------
# bind the data together

f = rbind(f, f1)
#-------------------------------------------------
# FORMAT THE FULLY COMBINED DATA 

#------------------------------
# create monthly dates
f[ ,month:=as.character(month(date))]
f[month!="10" & month!="11" & month!="12", month:=paste0("0", month)]
f[ , month_date:=paste0(year(date), '-', month, '-01')]
f[ , month_date:=ymd(month_date)]
f[!is.na(date), date:=month_date] # accounts for missing dates, warning is ok

# drop unecessary variables
f[ ,c("month_date", "month"):=NULL]

# sum to monthly totals (rather than daily)
byVars = names(f)[names(f)!='value']
f = f[ ,.(value=sum(value)), by=byVars]

#------------------------------

#------------------------------
# format geographic information 

# remove errors from department names
f[department=='totonicapã¡n', department:='totonicapan']
f[department=='quichã©', department:='quiche']
f[department=='suchitepã©quez', department:='suchitepequez']
f[department=='sacatepã©quez', department:='sacatepequez']
f[department=='petã©n', department:='peten']
f[department=='sololã¡', department:='solola']
f[ ,department:=toTitleCase(department)]

# remove errors from municipalities 

#--------------------------------------------------------
# save the product

saveRDS(f, paste0(dir, 'prepped/sigpro_testing_transfer_prepped.RDS'))

#--------------------------------------------------------



