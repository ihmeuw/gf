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
out_dir = paste0(dir, 'prepped/sigpro/')
#----------------------------------------

#----------------------------------------
# read in the data 
#----------------------------------------
# list existing files
files = list.files(paste0(dir, 'sigpro/october_transfer_2018/'), recursive=TRUE)

# import the files into distinct data tables
f2 = data.table(read.xlsx(paste0(dir, 'sigpro/october_transfer_2018/', files[[2]]), sheet=2))
f3 = data.table(read.xlsx(paste0(dir, 'sigpro/october_transfer_2018/', files[[3]]), sheet=2))
f4 = data.table(read.xlsx(paste0(dir, 'sigpro/october_transfer_2018/', files[[4]]), sheet=2))
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
f2[date < "2014-01-01" | date > "2017-12-31", date := NA] # 138 values outside the range we want, so these can also be set to NA; 2018-07-09 is the max date in f4

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
f2[ , set:=files[[2]]]

# save the file 
saveRDS(f2, paste0(out_dir, files[[2]], '_prepped.rds'))
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
f3[ , set:=files[[3]]]

# only a single data entry error
f3[ , flag:=(2016 < year(date))]

# save the file 
saveRDS(f3, paste0(out_dir, files[[3]], '_prepped.RDS'))
#----------------------------------------

#----------------------------------------
# format f4
#----------------------------------------
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

# format the dates 
f4[date=='null', date:=NA]
f4[ ,date:=as.Date(as.numeric(date), origin='1899-12-30')]
# f4[date < "2014-01-01", .N ] # 0 values
   
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
f4[ , set:=files[[4]]]

# only a single data entry error
f4[, flag:=(is.na(date))]

# save the file 
saveRDS(f4, paste0(out_dir, files[[4]], '_prepped.RDS'))
#----------------------------------------



