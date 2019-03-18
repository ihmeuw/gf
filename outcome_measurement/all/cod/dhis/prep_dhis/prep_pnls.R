# Prep the COD DHIS2 PNLS data 
# Final prep file for usable data 
# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 1/25/19
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr) 
# --------------------

# shell script for working on the cluster
# sh /share/singularity-images/rstudio/shells/rstudio_qsub_script.sh -p 1247 -s 2 

# --------------------
# set working directories

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')
setwd(dir)

#---------------------------------------
# load the file that represents a subset (no sex or )

dt = readRDS(paste0(dir, 'pre_prep/merged/pnls_subset_2014_11_01_2018_12_01.rds'))

# subset to 2017 on 
dt = dt[2017 <=  year(date) ]

#----------------------
# function to eliminate diacritical marks

fix_diacritics = function(x) {
  replacement_chars = list('S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A',
                           'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                           'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N',
                           'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                           'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a',
                           'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                           'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i',
                           'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                           'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 
                           'þ'='b', 'ÿ'='y')
  
  replace_me = paste(names(replacement_chars), collapse='')
  replace_with = paste(replacement_chars, collapse = '')
  return(chartr(replace_me, replace_with, x))
  
}

#-----------------------------
# convert factor variables to characters and value to numeric
dt[ , value:=as.numeric(as.character(value))]
dt[ , category:=as.character(category)]

#-----------------------------
# create an element that is easier to grep
dt[ , element1:=fix_diacritics(tolower(element))]

#--------------------------------------
# create subpopulations from the elements

# run other first in case more specific populations are included
dt[grep('autres', element1), subpop:='others']
dt[grep('prisonniers', element1), subpop:='prisoner']
dt[grep('udi', element1), subpop:='idu']
dt[grep('tg', element1), subpop:='trans']
dt[grep('routier', element1), subpop:='trucker']
dt[grep('minier', element1), subpop:='miner']

# male partners
dt[grep('masc', element1), subpop:='male_partners']

# some serodiscordant couples include male partners 
dt[grep('discordant', element1), subpop:='serodisc']

# msm
dt[grep('hsh', element1), subpop:='msm']
dt[grep('msm', element1), subpop:='msm']

# pregnant and lactating women
dt[grep('enceintes', element1), subpop:='plw']
dt[grep('allaitantes', element1), subpop:='plw']
dt[grep('partum', element1), subpop:='plw']

# survivors of sexual violence 
dt[grep('svs', element1), subpop:='svs']
dt[grep('survivants', element1), subpop:='svs'] 

# commercial sex workers and clients
dt[grep('ps', element1), subpop:='csw']
dt[grep('client', element1), subpop:='csw_client']

# refugees and idps
dt[grep('deplaces', element1), subpop:='idp']
dt[grep('intern', element1), subpop:='idp']
dt[grep('refug', element1), subpop:='refugee']

# men in uniform
dt[grep('uniforme', element1), subpop:='uniform']
dt[grep('hu', element1), subpop:='uniform']
dt[grep('dependants des hu', element1), subpop:='unform_dependents']

# fisherpeople
dt[grep('pecheurs', element1), subpop:='fisher']
dt[grep('pêcheurs', element1), subpop:='fisher']

# other key and vulnerable populations
dt[grep('enfants de rue', element1), subpop:='street_children']
dt[grep('enfants', element1), subpop:='exposed_infant']
dt[grep('eev', element1), subpop:='exposed_infant']
dt[grep('handicap', element1), subpop:='disabled']

#------------------------------------------------------------
# generate age and sex categories

# create a searchable category variable
dt[ ,category1:=tolower(category)]
dt[ ,category1:=fix_diacritics(category1)]

#------------------------------
# maternal health categorical
dt[subpop=='plw', maternity:='pregant_or_lactating']
dt[grep('sa/pp', category1), maternity:='delivery_post_partum']
dt[grep('cpn', category1), maternity:='anc']
dt[grep('option', category1), maternity:='option_b']

#------------------------
# create a sex category
dt[grep('feminin', category1), sex:='Female']
dt[grep('femme', category1), sex:='Female']
dt[!is.na(maternity), sex:='Female']

dt[grep('masc', category1), sex:='Male']
dt[subpop=='msm', sex:='Male']
dt[subpop=='male_partners', sex:='Male']

#---------------------
# old and new cases
dt[grep('NC', category), case:='new']
dt[grep('AC', category), case:='old']

#---------------------------------------------------
# create an age category

# create an interim variable to subset to age categories
dt[ , age_test:=category1]

# eliminate all the unnecessary words
dt[ , age_test:=gsub(",", "", age_test)]
dt[ , age_test:=gsub("feminin", "", age_test)]
dt[ , age_test:=gsub("masculin", "", age_test)]
dt[ , age_test:=gsub("sa/pp", "", age_test)]
dt[ , age_test:=gsub("cpn", "", age_test)]
dt[ , age_test:=gsub("ac", "", age_test)]
dt[ , age_test:=gsub("nc", "", age_test)]

# trim white space
dt[ , age_test:=trimws(age_test, which='both')]

# create an age variable
dt[grep('moins', age_test), age:=age_test]
dt[grep('^[0-9]', age_test), age:=age_test]

# drop the interim output
dt[ , age_test:=NULL]

#-------------------------------
# translate the age categories to english

dt[age=="moins de 25 ans", age:='<25']
dt[age=="25 ans et plus", age:='25 and over']
dt[age=="10 et 14 ans", age:='10-14']
dt[age=="15 et 19 ans", age:='15-19']
dt[age=="moins de 10 ans", age:='<10']
dt[age=="25 et 49 ans", age:='25-49']
dt[age=="20 et 24 ans", age:='20-24']
dt[age=="50 ans et plus", age:='50 and over']
dt[age=="1 et 4 ans", age:='1-4']
dt[age=="moins d'un an", age:='<1']
dt[age=="5 et 9 ans", age:='5-9']
dt[age=="moins de 15 ans", age:='<15']
dt[age=="15 et 24 ans", age:='15-24']
dt[age=="moins de 14 ans", age:='<14']

#-----------------------------
# create a category for drug stock

drugs = c('Entrée', 'Stock Initial', 'Nbr de jours RS', 
          'Sortie', 'Stock disponible utilisable')

dt[category %in% drugs, stock_category:=category]

#-------------------------------------
# create a tb binary 
dt[grep('tb', element1), tb:=TRUE]
dt[!grep('tb', element1), tb:=FALSE]
#------------------------------------
#------------------------------------
# delete excess variables
dt[ , c('data_set', 'element1','category1'):=NULL]

#------------------------------------
# collapse on category
dt[ , value:=as.numeric(as.character(value))]
dt = dt[ ,.(value=sum(value, na.rm=T)), 
         by=c("set", "org_unit_id", "element_id", "org_unit", "date", 
              "element_eng",  "org_unit_type", "level", "dps", 
              "health_zone", "health_area", "mtk", "element", "subpop",
              "maternity", "sex", "case", "age", "stock_category", "tb")]

#------------------------------------
# fix some english translations and abbreviations

#  drop the hyphen in co-infected in order to strsplit 
dt[    , element:= gsub('co-infectés', 'coinfectes', element)]
dt[    , element:= gsub('co-infectes', 'coinfectes', element)]

# fix english translations of abbreviations for major abbreviations
dt[ ,element_eng:= gsub('VIH', 'HIV', element_eng)]
dt[ ,element_eng:= gsub('PLWHA', 'PLHIV', element_eng)]
dt[ ,element_eng:= gsub('INH', 'IPT', element_eng)]

# fix co-infected
dt[ ,element_eng:= gsub('co-infectes', 'coinfected', element_eng)]


#-------------------------------------
# save a single data set 
saveRDS(dt, paste0(dir, 'prepped/pnls_sets/pnls_clean_all_sets.rds'))

#--------------------------------------
# save each data set as a distinct RDS file
sets = dt[ , unique(set)]

# arguments for the save
min = dt[ , min(date)]
min = gsub('-', '_', min)
max = dt[ , max(date)]
max = gsub('-', '_', max)

# save the sets 
for (s in sets) {
  y = tolower(s)
  saveRDS(dt[set==s], paste0(dir, 'prepped/pnls_sets/pnls_',
                             y,'_', min, '_', max,'.rds'))
}

#----------------------------------------