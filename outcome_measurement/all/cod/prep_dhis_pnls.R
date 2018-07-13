# Prep the COD DHIS2 PNLS data 
# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 7/12/2018
#
# Upload the RDS data from DHIS2 and merge with the meta data 
# Prep the data sets for analysis and the Tableau Dashboard

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

#--------------------
# Initial merge after download
# Import PNLS downloads and convert the merged data sets to a data table

#  read in the pnls data sets and merge them - jan. through april 2018
pnls1 <- readRDS(paste0(dir, 'pre_merge/pnls/pnls_drc_02_2018_04_2018.rds'))
pnls2 <- readRDS(paste0(dir, 'pre_merge/pnls/pnls_drc_01_2018_02_2018.rds'))

pnls1 <- data.table(pnls1)
pnls2 <- data.table(pnls2)

# merge them to create a 2018 dataset
pnls3 <- merge(pnls1, pnls2, by=c('group', 'data_element_ID', 'period',
                                  'org_unit_ID', 'value', 'category', 'last_update'),
                                   all=TRUE)

# merge in the 2017 data
pnls4 <- readRDS(paste0(dir, 'pre_merge/pnls/pnls_drc_01_2017_12_2017.rds'))
pnls4 <- data.table(pnls4)

pnls <- merge(pnls3, pnls4, by=c('group', 'data_element_ID', 'period',
                       'org_unit_ID', 'value', 'category', 'last_update'),
                             all=TRUE)

#------------------------
# save the preppred file
saveRDS(pnls, paste0(dir, 'pre_merge/pnls_merged_01_2017_04_2018.rds'))

# #------------------------

#--------------------
# Initial cleaning after download
# Import pnls data set and convert to a data table - 2017 - 4/2018
# for future downloads, merge with this data set

pnls <- readRDS(paste0(dir, 'pre_merge/pnls_merged_01_2017_04_2018.rds'))
pnls <- data.table(pnls)
#-----------------------------------------

#------------------------------
# merge the pnls data with the meta data

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
pnls <- merge(pnls, org_units, by='org_unit_ID', all.x=TRUE)
pnls <- merge(pnls, org_units_description, by='org_unit_ID', all.x=TRUE)
pnls[ ,length(unique(org_unit_ID))] # print the number of organisational units

# merge on data element ID to get data sets and data sets name
pnls <- merge(pnls, data_elements, by='data_element_ID', all.x=TRUE)

# merge on category id to get age and sex categories for the data elements
pnls <- merge(pnls, data_elements_categories, by='category', all.x=TRUE)
setnames(pnls, c('category', 'category_name'), c('category_id', 'category'))

# drop unnecessary urls
pnls[ , org_unit_url:=NULL]
pnls[ , url_list:=NULL]
#------------------------

#------------------------
# rename the variables to be more intuitive

# put the data set in a more intuitive order and change variable types
pnls <- pnls[ , .(data_set=as.character(datasets_name), element=as.character(element_name), category=as.character(category),
                  period=period,value=as.numeric(as.character(value)),
                  org_unit=as.character(org_unit_name), group=group,
                  coordinates=coordinates, opening_date=opening_date, last_update=last_update,
                  data_set_id=as.character(datasets_ID), element_id=as.character(data_element_ID),
                  org_unit_id=as.character(org_unit_ID))]

#-----------------------------------------------
# create a date variable from period
pnls[ , per:= as.character(period)]
pnls[ , year:=substr(per, 1, 4)]
pnls[ , month:=substr(per, 5, 6)]
pnls[ , per:=NULL]
pnls[ , period:=NULL]

pnls[ , date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]
#-----------------------------------------------
# change last_update and opening_date to date variables
pnls[ , last_update:=as.character(last_update)]
pnls[ ,opening_date:=as.character(opening_date)]

pnls$last_update <- unlist(lapply(strsplit(pnls$last_update, "T"), "[", 1))
pnls$opening_date <- unlist(lapply(strsplit(pnls$opening_date, "T"), "[", 1))

#--------------------------------------
# add a district variable 
pnls[ , dist:=(substr(pnls$org_unit, 1, 2))]
pnls[ , dist:=tolower(dist)]

pnls[dist=='bu' ,province:='Bas-Uele']
pnls[dist=='eq' ,province:='Equateur']
pnls[dist=='hk' ,province:='Haut-Katanga']
pnls[dist=='hl' ,province:='Haut-Lomami']
pnls[dist=='hu' ,province:='Haute-Uele']
pnls[dist=='it' ,province:='Ituri']

pnls[dist=='kn' ,province:='Kinshasa']

pnls[dist=='kc' ,province:='Kongo Central'] 

pnls[dist=='ks' ,province:='Kasai'] # checked
pnls[dist=='ke' ,province:='Kasai Oriental'] # on the list - checked - definitely east
pnls[dist=='kr' ,province:='Kasai Central'] 

pnls[dist=='ki' ,province:='Haut-Lomami'] # mistake - only one facility

pnls[dist=='kg' ,province:='Kwango'] 
pnls[dist=='kl' ,province:='Kwilu'] 

pnls[dist=='lm' ,province:='Lomami'] 
pnls[dist=='ll' ,province:='Lualaba'] #checked
# typo - says 'll' but with space in front 
pnls[dist==' l' ,province:='Lualaba'] 

pnls[dist=='md' ,province:='Mai-Ndombe']
pnls[dist=='mg' ,province:='Mongala'] # checked 
pnls[dist=='mn' ,province:='Maniema'] # checked 
pnls[dist=='nk' ,province:='Nord-Kivu'] 
pnls[dist=='nu' ,province:='Nord-Ubangi']

pnls[dist=='sn' ,province:='Sankuru'] # checked 
pnls[dist=='su' ,province:='Sud-Ubangi'] # checked
pnls[dist=='sk' ,province:='Sud-Kivu'] 
pnls[dist=='tn' ,province:='Tanganyika']
pnls[dist=='tp' ,province:='Tshopo'] #checked
pnls[dist=='tu' ,province:='Tshuapa'] #checked

# one code is a mistake - three facilities; appears to be a typo of lm
pnls[dist=='im' , province:='Lomami'] 
pnls[ ,dist:=NULL]

# rename province dps 
setnames(pnls, 'province', 'dps')

#-----------------------------------------------
# add a variable to demarcate the provincial approach provinces
pnls[dps=='Maniema' | dps=='Tshopo' | dps=="Kinshasa", mtk:='Yes']
pnls[is.na(mtk), mtk:='No']

#---------------------------------------------------
# export a list of french and english elements, with ids, side by side
# 
# elements <- pnls[ ,.(unique(element), unique(element_eng), unique(element_id))]
# write.csv(elements, paste0(dir, 'pnls_elements_eng_fr.csv'))

#-----------------------------------------------
# type of facility

# create organisational unit name in lower case
pnls[ , org_unit1:=tolower(org_unit)]

# Clinics - add spaces to eliminate polyclinics
# This code should be first, since many facility have both clinic and another classification
clinic <- grep(pattern="\\sclinique", x=pnls$org_unit1)
pnls[clinic, level:='Clinic']

# health centers
health_center <- grep(pattern="centre de santé", x=pnls$org_unit1)
pnls[health_center, level:='Health Center']
health_center <- grep(pattern="centre de sante", x=pnls$org_unit1)
pnls[health_center, level:='Health Center']

# reference health center
ref_hc <- grep(pattern="centre de santé de référence", x=pnls$org_unit1)
pnls[ref_hc, level:='Reference Health Center']

# Health posts
health_post <- grep(pattern="poste de santé", x=pnls$org_unit1)
pnls[health_post, level:='Health Post']
health_post1 <- grep(pattern="poste de sante", x=pnls$org_unit1)
pnls[health_post1, level:='Health Post']

#  Medical centers
medical_center <-  grep(pattern="centre médical", x=pnls$org_unit1)
pnls[medical_center, level:='Medical Center']
medical_center <-  grep(pattern="centre medical", x=pnls$org_unit1)
pnls[medical_center, level:='Medical Center']
medical_center <-  grep(pattern="centre de médical", x=pnls$org_unit1)
pnls[medical_center, level:='Medical Center']
medical_center <-  grep(pattern="centre de medical", x=pnls$org_unit1)
pnls[medical_center, level:='Medical Center']

#  Hospitals
hospital <- grep(pattern="\\shôpital", x=pnls$org_unit1)
pnls[hospital, level:='Hospital']
hospital <- grep(pattern="\\shopital", x=pnls$org_unit1)
pnls[hospital, level:='Hospital']

# Reference hospitals
hospital <- grep(pattern="hopital général de référence", x=pnls$org_unit1)
pnls[hospital, level:='General Reference Hospital']
hgr <- grep(pattern="hgr", x=pnls$org_unit1)
pnls[hgr, level:="General Reference Hospital"]

# Hospital center
hospital_c <- grep(pattern="centre hôspitalier", x=pnls$org_unit1)
pnls[hospital_c, level:='Hospital Center']
hospital_c1 <- grep(pattern="centre hospitalier", x=pnls$org_unit1)
pnls[hospital_c1, level:='Hospital Center']

# Dispensaries
dispensary <- grep(pattern="dispensaire", x=pnls$org_unit1)
pnls[dispensary, level:='Dispensary']

# Polyclinics
polyclinic <- grep(pattern="polyclinique", x=pnls$org_unit1)
pnls[polyclinic, level:='Polyclinic']

# Medical-surgical centers
m_c  <- grep(pattern="centre médico-chirurgical", x=pnls$org_unit1)
pnls[m_c, level:='Medical-Surgical Center']

# Health areas
health_area<- grep(pattern="aire de santé", x=pnls$org_unit1)
pnls[health_area, level:="Health Area"]
health_area <- grep(pattern="aire de sante", x=pnls$org_unit1)
pnls[health_area, level:="Health Area"]

# Health zones - there should be no health zones in pnls services
health_zone <- grep(pattern="zone de sante", x=pnls$org_unit1)
pnls[health_zone, level:="Health Zone"]
health_zone1 <- grep(pattern="zone de santé", x=pnls$org_unit1)
pnls[health_zone1, level:="Health Zone"]

# until the other facilities are fixed - other category
pnls[is.na(level), level:='Other']
#--------------------------------------------

#-----------------------------------------------
# merge english translations for data elements into the data

elements_eng <- read.csv(paste0(dir, 'translations/pnls_elements_eng_fr.csv'))
elements_eng <- data.table(elements_eng)

# convert elements to character strings in order to merge
elements_eng[ , element_eng:=as.character(element_eng)]
elements_eng[ , element_id:=as.character(element_id)]

pnls <- merge(pnls, elements_eng, by='element_id', all.x=T)
#-----------------------------------------------
# put the data set in an intuitive order

pnls <- pnls[ ,.(data_set, element, element_eng, category, value, org_unit, level, date, dps, mtk, 
          coordinates, opening_date, last_update,
         data_set_id, element_id, org_unit_id)]


#---------------------------------
# save the prepped data as an RDS
saveRDS(pnls, paste0(dir, 'prepped_data/pnls.rds'))
 
#-------------------------------------

