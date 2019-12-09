#----------------------------------------
# Caitlin O'Brien- Carelli / Audrey Batzel
# 5/31/2019
#
# Format SIGPRO HIV testing data
# Rewrite later as a function - many overlapping processes
#----------------------------------------

#-----------------------
# Install packages 
# ----------------------
rm(list=ls())
library(lubridate)
library(data.table)
library(openxlsx)
library(ggplot2)
library(Hmisc)
library(stringr)
library(XLConnect)
# ----------------------

#----------------------------------------
# Set up directories 
#----------------------------------------
# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set working and output directories
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/hiv/')

# to output prepped files
out_dir = paste0(dir, 'prepped/intermediate_prepped/sigpro/')
#----------------------------------------

#----------------------------------------
# read in the data 
#----------------------------------------
file2 = 'sigpro_f2_completo 2018.xlsx'
file3 = 'sigpro_f3_completo 2018.xlsx'
file4 = 'sigpro_f4_JanNov2018 - PB_TVC.csv'

# import the files into distinct data tables
f2 = data.table(read.xlsx(paste0(dir, 'sigpro/october_transfer_2018/',file2), sheet=2))
f3 = data.table(read.xlsx(paste0(dir, 'sigpro/october_transfer_2018/', file3), sheet=2))
# f4 = data.table(read.xlsx(paste0(dir, 'sigpro/october_transfer_2018/sigpro_f4_completo 2018.xlsx'), sheet=2))
f4 = data.table(read.csv(paste0(dir, 'sigpro/', file4)))
             
# check that there is not overlapping combination of codigo unico and date of test (we are assuming someone doesn't 
  # get more than one test on the same day, and would consider that a duplicate)
# f2[ , date := as.Date(as.numeric(fechaPrueba), origin='1899-12-30')]
# f3[ , date := as.Date(as.numeric(fechareal), origin='1899-12-30')]
# 
# dt = f2[, .(date, cui, file = 'f2')]
# dt = rbind(dt, f3[, .(date, cui = codigounico, file = 'f3')])
# dt = dt[!is.na(date)]
# dt[duplicated(dt[, .(date, cui)])] # all duplicated cui/date pairs are within a given file... hmmm. remove during normal prep.
#----------------------------------------

#----------------------------------------
# prep f2
#----------------------------------------
# drop unecessary variables (usually codes associated with values)
f2[ ,c('codactiv', 'codgrupo', 'codsubgrupo', 
       'codpais', 'codigoResultado'):=NULL]

#translate the variables 
setnames(f2, c('sr', 'sr_code', 'year', 'month', 'numeroInforme', 'cui', 
               'pop', 'subpop', 'muni_code', 'muni', 'department',
              'condoms_delivered', 'female_condoms_delivered', 'lube_tubes_delivered',
              'lube_packets_delivered', 'pamphlets_delivered', 'date', 'test_completed',
              'pre_test_completed', 'post_test_completed', 'result',
              'informed_of_result', 'referred'))
         
# put the columns into lower case for reformatting
f2 = f2[ ,lapply(.SD, tolower), .SDcols=c(1:length(f2))]

# format the dates 
# excel date origin is 1899
f2[ , date := as.Date(as.numeric(date), origin='1899-12-30')] # checked with Caitlin - the 22 values set to NA are fine to set to NA because they are messed up dates in the original file
f2[date < "2014-01-01" | date > "2017-12-31", date := NA] # 138 values outside the range we want, so these can also be set to NA; 2018-07-09 is the max date in f4; 160 NAs in total

# match the sr codes
f2[ ,sr_code:=gsub("nac0", "", sr_code)]

# format sr names
f2[!grep('cas|peniten', sr) , sr:=sapply(strsplit(sr, '-'), '[', 3)]
f2[grep('cas', sr), sr:=sapply(strsplit(sr, '-'), '[', 2)]
f2[grep('peniten', sr), sr:=sapply(strsplit(sr, '-'), '[', 2)]
f2[ , sr:=trimws(sr)]

# convert yes and no to 1s and 0s
vars = c('test_completed', 'pre_test_completed', 'post_test_completed', 'informed_of_result', 'referred')
for (v in vars) f2[get(v)=="s", (v):="1"]
for (v in vars) f2[get(v)=="n", (v):="0"]

# format and create a marker for positive tests
f2[result=='reactivo', result:='reactive']
f2[result=='no reactivo', result:='nonreactive']
f2[result=='indeterminado', result:='indeterminate']

f2[ ,flag:=(year(date)!=2014 & year(date)!=2015 & year(date)!=2016)]
f2[is.na(date), flag:=TRUE]
f2[ , set:=file2]

#remove duplicates
f2[, X:=.I] # add ID var
f2 = f2[!is.na(date)] # remove where date is na
dups = f2[duplicated(f2[,.(cui, date, subpop, muni, test_completed, result)])]
remove_ids = dups$X
f2 = f2[!X %in% remove_ids,]
f2[, X := NULL]

# save the file 
saveRDS(f2, paste0(out_dir, 'f2_prepped.rds'))
#----------------------------------------

#----------------------------------------
# format f3
#----------------------------------------
# keep and rename these variables
# month and year show no association with date - multiple years of data but only one code
f3 = f3[ ,.(cui=codigounico, pre_test_completed=prePruebaVIH, test_completed=pruebaVIH,  
            post_test_completed=postPruebaVIH,
            informed_of_result=conoceResultadoVIH, 
            informed_of_sif_result=conoceResultadoSif, 
            condoms=condonesMasculinos, female_condoms=condonesFemeninos, 
            flavored_condoms=condonesSabores, 
            lube_packets=lubriSachet, lube_tubes=lubriTubo ,
            pop=grupo, subpop=subgrupo, result=resultadoVIH, pqExtendido, 
            sr_code=codejecutor,            
            activity=tipoActividad, sif_result=resultadoSif, unmovil, 
            date=fechareal,  department=departamento,
            muni=municipio, theme=tema)]

# put the columns into lower case for reformatting
f3 = f3[ ,lapply(.SD, tolower), .SDcols=c(1:length(f3))]

# format the dates 
f3[ ,date:=as.Date(as.numeric(date), origin='1899-12-30')]
f3[date < "2014-01-01", date := NA] # 5 values

# match the sr codes
f3[ ,sr_code:=gsub("nac0", "", sr_code)]

# convert yes and no to 1s and 0s
vars = c('test_completed', 'pre_test_completed', 'post_test_completed', 'informed_of_result', 'informed_of_sif_result', 'pqExtendido')
for (v in vars) f3[get(v)=="s", (v):="1"]
for (v in vars) f3[get(v)=="n", (v):="0"]

# format and create a marker for positive tests
f3[result=='reactivo', result:='reactive']
f3[result=='no reactivo', result:='nonreactive']
f3[result=='indeterminado', result:='indeterminate']
f3[result=='prueba no realizada', result:='test_not_done']

# label the data set
f3[ , set:=file3]

# only a single data entry error
f3[ , flag:=(2016 < year(date))]

#remove duplicates
f3[, X:=.I] # add ID var
f3 = f3[!is.na(date)] # remove where date is na
dups = f3[duplicated(f3[,.(cui, date, muni, pop, result, test_completed)])]
remove_ids = dups$X
f3 = f3[!X %in% remove_ids,]
f3[, X := NULL]

# save the file 
saveRDS(f3, paste0(out_dir, 'f3_prepped.RDS'))
#----------------------------------------

#----------------------------------------
# format f4
#----------------------------------------
f4[, X := NULL]
# keep and rename these variables
# month and year show no association with date - multiple years of data but only one code
f4 = f4[ ,.(cui=codigounico, referral=refervih, pre_test_completed=prePruebaVIH, 
            test_completed=pruebaVIH,  post_test_completed=postPruebaVIH, 
            informed_of_result=conoceResultadoVIH, informed_of_sif_result=conoceResultadoSif, 
            condoms=condonesMasculinos, female_condoms=condonesFemeninos, 
            flavored_condoms=condonesSabores, lube_packets=lubriSachet, lube_tubes=lubriTubo ,
            pop=grupo, subpop=subgrupo, result=resultadoVIH, sr_code=codejecutor,            
            activity=tipoActividad, sif_result=resultadoSif, unmovil, date=fechareal, 
            department=departamento, muni=municipio, theme=tema)]

# put the columns into lower case for reformatting
f4 = f4[ ,lapply(.SD, tolower), .SDcols=c(1:length(f4))]
   
f4[date == '', date := NA]

# match the sr codes
f4[ ,sr_code:=gsub("nac0", "", sr_code)]

# convert yes and no to 1s and 0s
vars = c('test_completed', 'pre_test_completed', 'post_test_completed', 'informed_of_result', 'informed_of_sif_result')
for (v in vars) f4[get(v)=="s", (v):="1"]
for (v in vars) f4[get(v)=="n", (v):="0"]

# format and create a marker for positive tests
f4[result=='reactivo', result:='reactive']
f4[result=='no reactivo', result:='nonreactive']
f4[result=='indeterminado', result:='indeterminate']
f4[result=='prueba no realizada', result:='test_not_done']

# label the data set
f4[ , set:=file4]

#remove duplicates
f4[, X:=.I] # add ID var
f4 = f4[!is.na(date)] # remove where date is na
dups = f4[duplicated(f4[,.(cui, date, muni, subpop, result, test_completed)])]
remove_ids = dups$X
f4 = f4[!X %in% remove_ids,]
f4[, X := NULL]

# save the file 
saveRDS(f4, paste0(out_dir, 'f4_prepped.RDS'))
#----------------------------------------



