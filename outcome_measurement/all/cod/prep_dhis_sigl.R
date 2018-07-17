# Prep the COD DHIS2 SIGL data - SIGL1 and SIGL2
# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 7/16/2018
#
# Upload the RDS data from DHIS2 and merge with the meta data 
# prep the data sets for analysis and the Tableau Dashboard

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

#--------------------
# Initial cleaning after download
# Import SIGL data sets and convert to a data table

sigl <- readRDS(paste0(dir, 'pre_merge/sigl/sigl_drc_01_2015_05_2018.rds'))
sigl <- data.table(sigl)
#-----------------------------------------

#------------------------------
# subset to only the relevant elements
# subset before further cleaning if working on a home machine
# data set is too large for data table manipulation (crashes RStudio)

# import the id #s of the relevant elements and create a vector
elements <- fread(paste0(dir, 'catalogues/data_elements_cod.csv'))

# subset to only base services
elements_sigl <- elements[data_set_id=='s6yd0w2KXWa' | data_set_id=='pePpSnKtAh3']   
elements_sigl <- elements_sigl[keep==1]
elements_sigl <- unique(elements_sigl$element_id)

# subset sigl1 & sigl2 to only the relevant elements
sigl[ , data_element_ID:=as.character(data_element_ID)]
sigl <- sigl[data_element_ID %in% elements_sigl]

#-------------------

#------------------------------
# merge the SIGL data with the meta data

# import the meta data for the merge
data_sets<- data.table(readRDS(paste0(dir, 'meta_data/data_sets.rds'))) # not necessary for the merge
org_units <- data.table(readRDS(paste0(dir, 'meta_data/org_units_list.rds')))
data_elements <- data.table(readRDS(paste0(dir, 'meta_data/updated_data_elements.rds')))
data_elements_categories <- data.table(readRDS(paste0(dir, 'meta_data/data_elements_categories.rds')))
org_units_description <- data.table(readRDS(paste0(dir, 'meta_data/org_units_description.rds')))

# change the names of the ID variables in element categories and descriptions to match for the merge
data_elements[ , element_name:=displayName]
data_elements[ , displayName:=NULL]

data_elements_categories <- data_elements_categories[ ,.(category=ID, category_name=displayName)]
org_units_description <- org_units_description[ ,.(org_unit_ID = id, coordinates, opening_date, parent_id)]

#------------------------
# merge in the names of the objects in the data set

# merge on org_unit_ID to get names and descriptions of organisational units
sigl <- merge(sigl, org_units, by='org_unit_ID', all.x=TRUE)
sigl  <- merge(sigl, org_units_description, by='org_unit_ID', all.x=TRUE)

sigl [ ,length(unique(org_unit_ID))] # print the number of organisational units

# merge on data element ID to get data sets and data sets name
sigl  <- merge(sigl , data_elements, by='data_element_ID', all.x=TRUE)

# merge on category id to get age and sex categories for the data elements
sigl  <- merge(sigl , data_elements_categories, by='category', all.x=TRUE)
setnames(sigl , c('category', 'category_name'), c('category_id', 'category'))

# drop unnecessary urls
sigl [ , org_unit_url:=NULL]
sigl [ , url_list:=NULL]
#------------------------

#------------------------
# error in coordinates - drop temporarily until glitch fix
sigl[ ,coordinates:=NULL]

#------------------------
# rename the variables and rearrange in an intuitive order 

sigl <- sigl[ , .(data_set=as.character(datasets_name), element=as.character(element_name), category=as.character(category),
                  period=period,value=as.numeric(value),
                  org_unit=as.character(org_unit_name), group=group,
                  opening_date=opening_date, last_update=last_update,
                  data_set_id=as.character(datasets_ID), element_id=as.character(data_element_ID),
                  org_unit_id=as.character(org_unit_ID))]

#-----------------------------------------------
# create a date variable from period
sigl[ , per:= as.character(period)]
sigl[ , year:=substr(per, 1, 4)]
sigl[ , month:=substr(per, 5, 6)]
sigl[ , per:=NULL]
sigl[ , period:=NULL]

sigl[ , date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]

#-----------------------------------------------
# change last_update and opening_date to date variables
sigl[ , last_update:=as.character(last_update)]
sigl[ ,opening_date:=as.character(opening_date)]

sigl$last_update <- unlist(lapply(strsplit(sigl$last_update, "T"), "[", 1))
sigl$opening_date <- unlist(lapply(strsplit(sigl$opening_date, "T"), "[", 1))

#-----------------------------------------------

#-----------------------------------------------
# merge in the english names for the data elements and element type

elements_eng <- read.csv(paste0(dir, 'catalogues/data_elements_cod.csv'), stringsAsFactors=F)
elements_eng <- data.table(elements_eng)
elements_eng <- elements_eng[ ,.(element_id, element_eng=element, type, drug, tableau)]

sigl <- merge(sigl, elements_eng, by='element_id', all.x=TRUE)

# change the default name of elements to english
setnames(sigl, c('element', 'element_eng'), c('element_fr', 'element'))

#-----------------------------------------------

#--------------------------------------
# add a DPS variable 
sigl[ , dist:=(substr(sigl$org_unit, 1, 2))]
sigl[ , dist:=tolower(dist)]

sigl[dist=='bu' ,province:='Bas-Uele']
sigl[dist=='eq' ,province:='Equateur']
sigl[dist=='hk' ,province:='Haut-Katanga']
sigl[dist=='hl' ,province:='Haut-Lomami']
sigl[dist=='hu' ,province:='Haute-Uele']
sigl[dist=='it' ,province:='Ituri']

sigl[dist=='kn' ,province:='Kinshasa']
sigl[dist=='kc' ,province:='Kongo Central'] # checked against facility inventory

# Kasai, Kasai Central, Kasai Oriental, and Kongo Central
sigl[dist=='ks' ,province:='Kasai'] # checked
sigl[dist=='ke' ,province:='Kasai Oriental'] # on the list - checked - definitely east
sigl[dist=='kr' ,province:='Kasai Central'] # checked 

sigl[dist=='ki' ,province:='Haut-Lomami'] # mistake - only one facility
sigl[dist=='kg' ,province:='Kwango'] 
sigl[dist=='kl' ,province:='Kwilu'] 

sigl[dist=='lm' ,province:='Lomami'] 
sigl[dist=='ll' ,province:='Lualaba'] #checked
# typo - says 'll' but with space in front 
sigl[dist==' l' ,province:='Lualaba'] 

sigl[dist=='md' ,province:='Mai-Ndombe']
sigl[dist=='mg' ,province:='Mongala'] # checked 
sigl[dist=='mn' ,province:='Maniema'] # checked 
sigl[dist=='nk' ,province:='Nord-Kivu'] 
sigl[dist=='nu' ,province:='Nord-Ubangi']

sigl[dist=='sn' ,province:='Sankuru'] # checked 
sigl[dist=='su' ,province:='Sud-Ubangi'] # checked
sigl[dist=='sk' ,province:='Sud-Kivu'] 
sigl[dist=='tn' ,province:='Tanganyika']
sigl[dist=='tp' ,province:='Tshopo'] #checked
sigl[dist=='tu' ,province:='Tshuapa'] #checked

# mistaken names - checked
sigl[org_unit=="Im Kabinda Hôpital Général de Référence", dist:='lm']
sigl[org_unit=="Im Kabinda Hôpital Général de Référence", province:='Lomami']

# fix these districts
sigl[dist=='im' , province:='Unknown'] # possibly real - has 25?
sigl[dist=='ii' , province:='Unknown'] # ?mistake only 9 facilities

sigl[ ,dist:=NULL]

#-----------------------------------------------
# add a variable to demarcate the provincial approach provinces
sigl[province=='Maniema' | province=='Tshopo' | province=="Kinshasa", mtk:='Yes']
sigl[is.na(mtk), mtk:='No']

#-----------------------------------------------
# Level of reporting or type of facility (levels of organizational units)

# create organisational unit name in lower case
sigl[ ,org_unit1:=tolower(org_unit)]

# Clinics - add spaces to eliminate polyclinics
# This code should be first, since many facility have both clinic and another classification
clinic <- grep(pattern="\\sclinique", x=sigl$org_unit1)
sigl[clinic, level:='Clinic']

# health centers
health_center <- grep(pattern="centre de santé", x=sigl$org_unit1)
sigl[health_center, level:='Health Center']
health_center <- grep(pattern="centre de sante", x=sigl$org_unit1)
sigl[health_center, level:='Health Center']

# reference health center
ref_hc <- grep(pattern="centre de santé de référence", x=sigl$org_unit1)
sigl[ref_hc, level:='Reference Health Center']

# Health posts
health_post <- grep(pattern="poste de santé", x=sigl$org_unit1)
sigl[health_post, level:='Health Post']
health_post1 <- grep(pattern="poste de sante", x=sigl$org_unit1)
sigl[health_post1, level:='Health Post']

#  Medical centers
medical_center <-  grep(pattern="centre médical", x=sigl$org_unit1)
sigl[medical_center, level:='Medical Center']
medical_center <-  grep(pattern="centre medical", x=sigl$org_unit1)
sigl[medical_center, level:='Medical Center']
medical_center <-  grep(pattern="centre de médical", x=sigl$org_unit1)
sigl[medical_center, level:='Medical Center']
medical_center <-  grep(pattern="centre de medical", x=sigl$org_unit1)
sigl[medical_center, level:='Medical Center']

#  Hospitals
hospital <- grep(pattern="\\shôpital", x=sigl$org_unit1)
sigl[hospital, level:='Hospital']
hospital <- grep(pattern="\\shopital", x=sigl$org_unit1)
sigl[hospital, level:='Hospital']

# Reference hospitals
hospital <- grep(pattern="hopital général de référence", x=sigl$org_unit1)
sigl[hospital, level:='General Reference Hospital']
hgr <- grep(pattern="hgr", x=sigl$org_unit1)
sigl[hgr, level:="General Reference Hospital"]

# Hospital center
hospital_c <- grep(pattern="centre hôspitalier", x=sigl$org_unit1)
sigl[hospital_c, level:='Hospital Center']
hospital_c1 <- grep(pattern="centre hospitalier", x=sigl$org_unit1)
sigl[hospital_c1, level:='Hospital Center']

# Dispensaries
dispensary <- grep(pattern="dispensaire", x=sigl$org_unit1)
sigl[dispensary, level:='Dispensary']

# Polyclinics
polyclinic <- grep(pattern="polyclinique", x=sigl$org_unit1)
sigl[polyclinic, level:='Polyclinic']

# Medical-surgical centers
m_c  <- grep(pattern="centre médico-chirurgical", x=sigl$org_unit1)
sigl[m_c, level:='Medical-Surgical Center']

# Health areas
health_area<- grep(pattern="aire de santé", x=sigl$org_unit1)
sigl[health_area, level:="Health Area"]
health_area <- grep(pattern="aire de sante", x=sigl$org_unit1)
sigl[health_area, level:="Health Area"]

# Health zones
zone <- grep(pattern="zone de sante", x=sigl$org_unit1)
sigl[zone, level:="Health Zone"]
zone1 <- grep(pattern="zone de santé", x=sigl$org_unit1)
sigl[zone1, level:="Health Zone"]

# if the level is missing, add 'other'
sigl[is.na(level), level:='Other']

#fix the 58 facilities with typos
sigl[ ,.(length(unique(org_unit))), by=level]
#--------------------------------------------

#----------------------------------------------
# put the variables in an intuitive order 

sigl <- sigl[ ,.(data_set, element, date, type, value, org_unit, level, dps=province,
                      mtk, opening_date, last_update, drug, element_fr, element_id, 
                      org_unit_id, data_set_id, month, year)]

#----------------------------------------------
# save the data sets

saveRDS(sigl, paste0(dir, 'sigl.rds'))

#----------------------------------------------

