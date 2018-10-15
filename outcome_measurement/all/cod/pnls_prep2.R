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
library(xlsx)
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

pnls[is.na(type), unique(element)]

#--------------------------------------
# create subpopulations

pnls[grep('prisonniers', element1), subpop:='prisoner']
pnls[grep('udi', element1), subpop:='idu']
pnls[grep('tg', element1), subpop:='transgender']
pnls[grep('routier', element1), subpop:='trucker']
pnls[grep('minier', element1), subpop:='miner']
pnls[grep('discordants', element1), subpop:='serodisc']
pnls[grep('masc', element1), subpop:='msm']
pnls[grep('hsh', element1), subpop:='msm']
pnls[grep('enceintes', element1), subpop:='plw']
pnls[grep('allaitantes', element1), subpop:='plw']
pnls[grep('svs', element), subpop:='svs']
pnls[grep('client', element), subpop:='client']
pnls[grep('enfants de rue', element1), subpop:='street']
pnls[grep('autres', element1), subpop:='other']
pnls[grep('deplaces', element1), subpop:='idp']
pnls[grep('refug', element1), subpop:='refugee']
pnls[grep('pecheurs', element1), subpop:='fisher']
pnls[grep('ps', element1), subpop:='csw']

#--------------------------------------
# prep age and sex categories

# create a searchable category variable
pnls[ ,category1:=tolower(category)]
pnls[ ,category1:=fix_diacritics(category1)]

# create a sex category
pnls[grep('feminin', category1), sex:='Female']
pnls[grep('masc', category1), sex:='Male']
pnls[subpop=='plw', sex:='Female']
pnls[subpop=='msm', sex:='Male']


# age category 





#--------------------------------------
# normalize the variables

pnls[type=='vct', unique(element)]

#substring the element
























