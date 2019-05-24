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

#----------------------------------------
# combine the data sets

# subset and combine similar data sets
vars = c("set", "sr_code", "department", "muni", "date",  
         "theme", "pop", "subpop", "gender", "test_completed",
         "result", "informed_of_result", "flag")

f3 = f3[, vars, with=FALSE]
f4 = f4[, vars, with=FALSE]

# bind and add variable to fit with f2
f = rbind(f3, f4)
f[ , sr:=NA]

#---------------------------
# subset and combine the patient level data
vars2 = c("set", "sr_code", "sr", "department", "muni", "date",  
          "pop", "subpop", "gender", "test_completed",
         "result", "informed_of_result", "flag")

# subset and add theme
f2 = f2[ ,vars2, with=FALSE]
f2[ ,theme:=NA]

# bind it in
f = rbind(f, f2)

# drop informed of result - early data only has pos and neg
f = f[ ,.(tests=sum(test_completed)), by=.(set, sr, sr_code,
        department, muni, date, theme, pop, subpop,
        gender, result)]

dcast(data = f, set+sr+sr_code+department+muni+date+theme+pop+subpop+gender~result, value.var='tests')


#---------------------------
# aggregate to the sr, muni, department level

f1 = f1[grep('vih', diagnosis)]
f1 = f1[age!='total']

setnames(f1, "category", "pop")


dcast(f1, sr+sr_code+pop+diagnosis+age+total+gender+pregnant+flag+date+set~diagnosis, value.var='total')



