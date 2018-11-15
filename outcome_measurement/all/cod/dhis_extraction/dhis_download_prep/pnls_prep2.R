# Prep the COD DHIS2 PNLS data 
# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 11/14/2018
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
# eliminate the unecessary elements from the original prepped data
# drop anything ending in sex, age, or

# # source the function that cleans the original prepped data
#  source(paste0(dir, "pnls_function.r"))
# 
# # load the original prepped data
#  x = readRDS(paste0(dir, 'prepped/pnls_drc_01_2017_07_2018_prepped.rds'))
#  
#  pnls_subset(x)
# 
# # save the cleaned output
#  saveRDS(x, paste0(dir, 'prepped/pnls_drc_01_2017_07_2018_prepped_subset.rds'))

#---------------------------------------
# load the file that represents a subset (no sex or )

pnls = readRDS(paste0(dir, 'prepped/pnls_drc_01_2017_07_2018_prepped_subset.rds'))

#----------------------
# function to eliminate diacritical marks

fix_diacritics <- function(x) {
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
  
  replace_me <- paste(names(replacement_chars), collapse='')
  replace_with <- paste(replacement_chars, collapse = '')
  return(chartr(replace_me, replace_with, x))
  
}

#-----------------------------
# convert the elements to characters from a factor
pnls[ , element:=as.character(element)]

#-----------------------------
# drop sex element with a typo 

pnls = pnls[element!='pnls-com-personnes orientees vers fosa-survivants vs-ex']

#--------------------------------------
# classify variables by type

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

# male partners
pnls[grep('masc', element1), subpop:='male_partners']

# some serodiscordant couples include male partners 
pnls[grep('discordant', element1), subpop:='serodisc']

pnls[grep('hsh', element1), subpop:='msm']
pnls[grep('msm', element1), subpop:='msm']

pnls[grep('enceintes', element1), subpop:='plw']
pnls[grep('allaitantes', element1), subpop:='plw']

pnls[grep('svs', element1), subpop:='svs']
pnls[grep('survivants vs', element1), subpop:='svs'] 

pnls[grep('ps', element1), subpop:='csw']
pnls[grep('client', element1), subpop:='csw_client']
pnls[grep('enfants de rue', element1), subpop:='street_children']

pnls[grep('autres', element1), subpop:='other']
pnls[grep('deplaces', element1), subpop:='idp']
pnls[grep('refug', element1), subpop:='refugee']
pnls[grep('pecheurs', element1), subpop:='fisher']

pnls[grep('uniforme', element1), subpop:='uniform']
pnls[grep('hu', element1), subpop:='uniform']
pnls[grep('dependants des hu', element1), subpop:='dependants des hu']

pnls[grep('enfants', element1), subpop:='exposed_infant']
pnls[grep('eev', element1), subpop:='exposed_infant']
pnls[grep('handicap', element1), subpop:='disabled']

#--------------------------------------
# prep age and sex categories

# create a searchable category variable
pnls[ ,category1:=tolower(category)]
pnls[ ,category1:=fix_diacritics(category1)]

#------------------------------
# maternal health categorical
pnls[subpop=='plw', maternity:='Pregnant or lactating woman']
pnls[grep('sa/pp', category1), maternity:='SA/PP']
pnls[grep('cpn', category1), maternity:='CPN']
pnls[grep('option', category1), maternity:='Option B+']

#------------------------

# create a sex category
pnls[grep('feminin', category1), sex:='Female']
pnls[grep('femme', category1), sex:='Female']
pnls[!is.na(maternity), sex:='Female']

pnls[grep('masc', category1), sex:='Male']
pnls[subpop=='msm', sex:='Male']
pnls[subpop=='male_partners', sex:='Male']

#---------------------
# old and new cases
pnls[grep('NC', category), case:='new']
pnls[grep('AC', category), case:='old']

#---------------------
# create an age category

# create an interim variable to subset to age categories
pnls[ , age_test:=category1]

# eliminate all the unnecessary words
pnls[ , age_test:=gsub(",", "", age_test)]
pnls[ , age_test:=gsub("feminin", "", age_test)]
pnls[ , age_test:=gsub("masculin", "", age_test)]
pnls[ , age_test:=gsub("sa/pp", "", age_test)]
pnls[ , age_test:=gsub("cpn", "", age_test)]
pnls[ , age_test:=gsub("ac", "", age_test)]
pnls[ , age_test:=gsub("nc", "", age_test)]

# trim white space
pnls[ , age_test:=trimws(age_test, which='both')]

# create an age variable
pnls[grep('moins', age_test), age:=age_test]
pnls[grep('^[0-9]', age_test), age:=age_test]

# drop the interim output
pnls[ , age_test:=NULL]

#-----------------------------
# create a category for drug stock
drugs = c('Entrée', 'Stock Initial', 'Nbr de jours RS', 
          'Sortie', 'Stock disponible utilisable')

pnls[category %in% drugs, stock_category:=category]

#-------------------------------------
# create a tb binary 

pnls[grep('TB', element), tb:=TRUE]
pnls[!grep('TB', element), tb:=FALSE]
#------------------------------------
# Collapse the data elements 
# create variables that do not include age, sex, subpop stratifications

#  drop the hyphen in co-infected in order to strsplit 
pnls[    , element:= gsub('co-infectés', 'coinfectes', element)]
pnls[    , element1:= gsub('co-infectes', 'coinfectes', element1)]

# separate out the data elements from their codes
pnls$variable = unlist(lapply(strsplit(pnls$element, "-"), "[", 3))
pnls$add = unlist(lapply(strsplit(pnls$element, "-"), "[", 4))

# for drugs, add the text after the second hyphen
pnls[type=='drug' & !is.na(add) , variable:=paste0(variable, add)]


#-----------------------------------
# delete excess variables
# leave element and element id and clean the data sets individually to collapse

pnls[ , c('element1', 'element_eng', 'category', 'category1',  'data_set', 'add'):=NULL]

#-------------------------------------
# save a single data set 

saveRDS(pnls, paste0(dir, 'prepped/pnls_sets/pnls_clean.rds'))

# save each type of variable as a distinct RDS file 
files = pnls[ ,unique(type)]

for (f in files) {
  saveRDS(pnls[type==f], paste0(dir, 'prepped/pnls_sets/pnls_', f, '.rds'))
}

#----------------------------------------





