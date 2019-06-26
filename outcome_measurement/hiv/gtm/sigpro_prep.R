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
f2[ , date_check := as.Date(as.numeric(date), origin='1899-12-30')]

# match the sr codes
f2[ ,sr_code:=gsub("nac0", "", sr_code)]

# format sr names
f2[ , sr:=sapply(strsplit(sr, '-'), '[', 3)]
f2[grep('cas', sr), sr:=sapply(strsplit(sr, '-'), '[', 2)]
f2[grep('peniten', sr), sr:=sapply(strsplit(sr, '-'), '[', 2)]
f2[ , sr:=trimws(sr)]

# convert yes and no to a logical 
f2[ , test_completed:=(test_completed=='s')]
f2[ , pre_test_completed:=(pre_test_completed=='s')]
f2[ , post_test_completed:=(post_test_completed=='s')]
f2[ , informed_of_result:=(informed_of_result=='s')]
f2[ , referred:=(referred=='s')]

# format and create a marker for positive tests
f2[result=='reactivo', result:='reactive']
f2[result=='no reactivo', result:='nonreactive']
f2[result=='indeterminado', result:='indeterminate']

# create a gender category
f2[ ,gender:=str_sub(cui, 1, 1)]
f2[gender=='m', gender:='Male']
f2[gender=='f', gender:='Female']
f2[gender=='t', gender:='Trans']

# overwrite inaccurate genders
f2[pop=='hsh', gender:='Male']
f2[pop=='trans', gender:='Trans']
f2[pop=='mts' & gender!='Trans', gender:='Female'] # female sex workers can be female or trans
f2[grep('mujer', subpop), gender:='Female'] # some ppl mujeres are listed as male
f2[grep('hom', subpop), gender:='Male']

# translate populations 
f2[pop=='hsh', pop:='msm']
f2[pop=='mts', pop:='fsw']
f2[pop=='ppl', pop:='prisoners'] # personas privadas de la libertad
f2[grep('trans', subpop), pop:='trans']

# update subpopulations
f2[subpop=='trans trabajadora sexual', pop:='fsw']
f2[subpop=='hsh trabajador sexual', pop:='msm']

# flag the errant entries - only date in this case
# name the data set for the combination 

f2[ ,flag:=(year(date)!=2014 & year(date)!=2015 & year(date)!=2016)]
f2[is.na(date), flag:=TRUE]
f2[ , set:=files[[2]]]

# save the file 
saveRDS(f2, paste0(dir, 'prepped/', files[[2]], '_prepped.RDS'))
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

# match the sr codes
f3[ ,sr_code:=gsub("nac0", "", sr_code)]


# convert yes and no to a logical 
f3[ , pre_test_completed:=(pre_test_completed=='s')]
f3[ , test_completed:=(test_completed=='s')]
f3[ , post_test_completed:=(post_test_completed=='s')]
f3[ , informed_of_result:=(informed_of_result=='s')]
f3[ , informed_of_sif_result:=(informed_of_sif_result=='s')]
f3[ , pqExtendido:=(pqExtendido=='s')]

# format and create a marker for positive tests
f3[result=='reactivo', result:='reactive']
f3[result=='no reactivo', result:='nonreactive']
f3[result=='indeterminado', result:='indeterminate']
f3[result=='prueba no realizada', result:='test not done']

# create a gender category
# no category for PV
f3[ , gender:=str_sub(cui, 1, 1)]
f3[pop=='trans', gender:='t']
f3[(gender=='t' | gender=='f') & pop=='hsh', gender:='t'] # trans women classified as hsh moved to trans

# format sexes 
f3[gender=='m', gender:='Male']
f3[gender=='f', gender:='Female']
f3[gender=='t', gender:='Trans']

# translate populations 
f3[pop=='hsh', pop:='msm']
f3[pop=='mts', pop:='fsw']
f3[subpop=='trans privadas de libertad', pop:='prisoners']

# label the data set
f3[ , set:=files[[3]]]

# only a single data entry error
f3[ , flag:=(2016 < year(date))]

# save the file 
saveRDS(f3, paste0(dir, 'prepped/', files[[3]], '_prepped.RDS'))
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

# match the sr codes
f4[ ,sr_code:=gsub("nac0", "", sr_code)]

# convert yes and no to a logical 
f4[ , pre_test_completed:=(pre_test_completed=='s')]
f4[ , test_completed:=(test_completed=='s')]
f4[ , post_test_completed:=(post_test_completed=='s')]
f4[ , informed_of_result:=(informed_of_result=='s')]
f4[ , informed_of_sif_result:=(informed_of_sif_result=='s')]

# format and create a marker for positive tests
f4[result=='reactivo', result:='reactive']
f4[result=='no reactivo', result:='nonreactive']
f4[result=='indeterminado', result:='indeterminate']
f4[result=='prueba no realizada', result:='test not done']

# create a gender category
f4[ ,gender:=str_sub(cui, 1, 1)]
f4[gender=='t' & pop!='pv', pop:='trans'] # genders are mostly accurate

# formate gender names
f4[gender=='m', gender:='Male']
f4[gender=='f', gender:='Female']
f4[gender=='t', gender:='Trans']

# translate populations 
f4[pop=='hsh', pop:='msm']

f4[subpop=='trans privadas de libertad', pop:='prisoners']

# label the data set
f4[ , set:=files[[4]]]

# only a single data entry error
f4[, flag:=(is.na(date))]

# save the file 
saveRDS(f4, paste0(dir, 'prepped/', files[[4]], '_prepped.RDS'))
#----------------------------------------



