# Prep the COD DHIS2 PNLS data 
# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 10/15/2018
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

# --------------------
# set working directories

# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

pnls = readRDS(paste0(dir, 'prepped/pnls_drc_01_2017_07_2018_prepped.rds'))

#-------------------------------------------
#  subset the pnls data set to only the relevant elements for analysis

# drop unecessary variables
pnls[ , c('type', 'drug', 'tableau', 'coordinates', 'opening_date', 'last_update', 'org_unit_type'):=NULL]

# drop support elements - duplicates
pnls[grep('soutien', element), support:=TRUE]
pnls[is.na(support), support:=FALSE]
pnls = pnls[support==F]
pnls[ ,support:=NULL]

# drop out elements that end in 'sex' (remainder include sex and age)
pnls[grep('sex', element), sex_element:=TRUE]
pnls[is.na(sex_element), sex_element:=FALSE]
pnls = pnls[sex_element==FALSE]
pnls[ ,sex_element:=NULL]

#------------------
# classify the elements

# create an element easier to grep 
pnls[ , element1:=tolower(element)]


# drop diacritical marks
fix_diacritics <- function(x) {
  replacement_chars = list('S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                           'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                           'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                           'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                           'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y')
  
  replace_me <- paste(names(replacement_chars), collapse='')
  replace_with <- paste(replacement_chars, collapse = '')
  return(chartr(replace_me, replace_with, x))
  
}

pnls[ , element1:=fix_diacritics(element1)]

#--------------------------------------
# classify by type of variable
pnls[grep('cdv', element1), type:='vct']
pnls[grep('arv', element1), type:='art']
pnls[grep('drug', element1), type:='drug']
pnls[grep('ist', element1), type:='sti']
pnls[grep('ptme', element1), type:='pmtct']
pnls[grep('com', element1), type:='com']

#--------------------------------------
# create subpopulations

pnls[grep('prisonniers', element1), subpop:='prisoner']
pnls[grep('udi', element1), subpop:='idu']
pnls[grep('tg', element1), subpop:='transgender']
pnls[grep('routier', element1), subpop:='trucker']
pnls[grep('minier', element1), subpop:='miner']
pnls[grep('discordant', element1), subpop:='serodisc']
pnls[grep('masc', element1), subpop:='msm']
pnls[grep('hsh', element1), subpop:='msm']

pnls[grep('enceintes', element1), subpop:='plw']
pnls[grep('allaitantes', element1), subpop:='plw']

pnls[grep('svs', element1), subpop:='svs']
pnls[grep('ps', element1), subpop:='csw']
pnls[grep('client', element1), subpop:='client']
pnls[grep('enfants de rue', element1), subpop:='street']

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
drugs = c('Entrée', 'Stock Initial', 'Nbr de jours RS', 'Sortie', 'Stock disponible utilisable')
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
























