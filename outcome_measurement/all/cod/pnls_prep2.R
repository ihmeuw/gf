# Prep the COD DHIS2 PNLS data 
# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 11/7/2018
# ----------------------------------------------

# --------------------
# Set up R

rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(ggplot2)
library(dplyr)
library(stringr) # to extract meta data from file names
# --------------------

# shell script
# sh /share/singularity-images/rstudio/shells/rstudio_qsub_script.sh -p 1247 -s 10 -P snis_download

# --------------------
# set working directories

# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

#-------------------------------------------

# load the original prepped data
# source()
# x = readRDS(paste0(dir, 'prepped/pnls_drc_01_2017_07_2018_prepped.rds'))
# pnls_subset(x)
# saveRDS(x, paste0(dir, 'prepped/pnls_drc_01_2017_07_2018_prepped_subset.rds'))

#---------------------------------------
# load the file that represents a subset (no sex or )

readRDS(pnls, paste0(dir, 'prepped/pnls_drc_01_2017_07_2018_prepped_subset.rds'))


#--------------------------------------
# classify by type of variable

# create classes using the second word
pnls$second = unlist(lapply(strsplit(pnls$element1, "-"), "[", 2))
pnls[ ,unique(second)]
setnames(pnls, 'second', 'type')

# translate groupings
pnls[type=='cdv', type:='vct']
pnls[type=='ist', type:='sti']
pnls[type=='ptme', type:='pmtct']


#--------------------------------------
# create subpopulations

pnls[grep('prisonniers', element1), subpop:='prisoner']
pnls[grep('udi', element1), subpop:='idu']
pnls[grep('tg', element1), subpop:='trans']
pnls[grep('routier', element1), subpop:='trucker']
pnls[grep('minier', element1), subpop:='miner']
pnls[grep('discordant', element1), subpop:='serodisc']
pnls[grep('masc', element1), subpop:='msm']
pnls[grep('hsh', element1), subpop:='msm']

pnls[grep('enceintes', element1), subpop:='plw']
pnls[grep('allaitantes', element1), subpop:='plw']

pnls[grep('svs', element1), subpop:='svs']
pnls[grep('ps', element1), subpop:='csw']
pnls[grep('client', element1), subpop:='csw_client']
pnls[grep('enfants de rue', element1), subpop:='street_hildren']

pnls[grep('autres', element1), subpop:='other']
pnls[grep('deplaces', element1), subpop:='idp']
pnls[grep('refug', element1), subpop:='refugee']
pnls[grep('pecheurs', element1), subpop:='fisher']

pnls[grep('uniforme', element1), subpop:='uniform']
pnls[grep('hu', element1), subpop:='uniform']
pnls[grep('enfants', element1), subpop:='exposed_infant']
pnls[grep('eev', element1), subpop:='exposed_infant']
pnls[grep('handicap', element1), subpop:='disabled']

#--------------------------------------
# prep age and sex categories

# create a searchable category variable
pnls[ ,category1:=tolower(category)]
pnls[ ,category1:=fix_diacritics(category1)]

# create a sex category
pnls[grep('feminin', category1), sex:='Female']
pnls[grep('femme', category1), sex:='Female']
pnls[grep('masc', category1), sex:='Male']
pnls[subpop=='plw', sex:='Female']
pnls[subpop=='msm', sex:='Male']

#---------------------
# old and new cases
pnls[grep('NC', category), case:='new']
pnls[grep('AC', category), case:='old']

#---------------------
# create an age category

# create additional age categories
pnls$age1 = unlist(lapply(strsplit(pnls$category1, " "), "[", 1))
pnls$age2 = unlist(lapply(strsplit(pnls$category1, " "), "[", 2))
pnls$age3 = unlist(lapply(strsplit(pnls$category1, " "), "[", 3))
pnls$age4 = unlist(lapply(strsplit(pnls$category1, " "), "[", 4))
pnls$age5 = unlist(lapply(strsplit(pnls$category1, " "), "[", 5))

# replace old and new cases and fix spelling of years and plus
pnls[age5=='nc' | age5=='ac', age5:=NA]
pnls[age1=='nc' | age1=='ac', age1:=NA]

pnls[age5=='plus,', age5:='plus']
pnls[age5=='ans,', age5:='ans']

pnls[age1=='moins', age:=paste(age1, age2, age3, age4)]
pnls[age1=='feminin,' | age1=='masculin,' | age1=="sa/pp" | age1=='cpn,', age:=paste(age2, age3, age4, age5)]

# if the age category begins with a numeric, use the existing category
pnls[grep('^[0-9]', category1), age:=category1]

# delete NAs from age categories and fix spellings
pnls$age = unlist(lapply(pnls$age, function(x) gsub("NA", "", x)))

pnls[grep('nc', age), age:=NA]
pnls[grep('ac', age), age:=NA]
pnls[grep('un an', age), age:='moins dun an']

# drop the age categories that consist of spaces
pnls[grep("^\\s", age), age:=NA]

# delete the interim outputs
pnls[ , c("age1", "age2", "age3", "age4", "age5"):=NULL]

# anc visit binary 
pnls[grep('sa/pp', category1), maternity:='SA/PP']
pnls[grep('cpn', category1), maternity:='CPN']

#-----------------------------
# category for drug stock
drugs = c('Entrée', 'Stock Initial', 'Nbr de jours RS', 
          'Sortie', 'Stock disponible utilisable')

pnls[category %in% drugs, stock_category:=category]

#-------------------------------------
# categories that are not captured by age, sex, case
pnls[type=='drug', unique(category)]



#-------------------------------------------------------------

#--------------------------------------
# normalize the variables 

pnls[type=='vct', unique(element)]

pnls[grep('', element1), variable:=]

#substring the element
























