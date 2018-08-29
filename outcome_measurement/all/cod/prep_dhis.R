# Services de Base - Prep the COD DHIS2 data from Basic Services
# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 8/28/2018
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
library(xlsx)
library(stringr) 
# --------------------

# --------------------
# set working directories

# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

#--------------------
# upload the data set and convert it to a data table

# change the file pathway to match your data table 
dt <- paste0(dir, 'merged/base_services_drc_01_2015_07_2018.rds')


#-----------------------------------------

#------------------------------
# merge the base services data with the meta data

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
dt <- merge(dt, org_units, by='org_unit_ID', all.x=TRUE)
dt <- merge(dt, org_units_description, by='org_unit_ID', all.x=TRUE)
dt[ ,length(unique(org_unit_ID))] # print the number of organisational units

# merge on data element ID to get data sets and data sets name
dt <- merge(dt, data_elements, by='data_element_ID', all.x=TRUE)

# merge on category id to get age and sex categories for the data elements
dt <- merge(dt, data_elements_categories, by='category', all.x=TRUE)
setnames(dt, c('category', 'category_name'), c('category_id', 'category'))

# drop unnecessary urls
dt[ , org_unit_url:=NULL]
dt[ , url_list:=NULL]
#------------------------

#------------------------
# rename the variables to be more intuitive

# put the data set in a more intuitive order and change variable types
# convert all factor variables to character string; then convert value to numeric 

# this code will produce a warning from changing the values to characters - this is OK
dt <- dt[ , .(data_set=as.character(datasets_name), element=as.character(element_name), category=as.character(category),
                  period=period, value=as.numeric(as.character(value)),
                  org_unit=as.character(org_unit_name),
                  coordinates=coordinates, opening_date=opening_date, last_update=last_update,
                  data_set_id=as.character(datasets_ID), element_id=as.character(data_element_ID),
                  org_unit_id=as.character(org_unit_ID))]

#----------------------------------------

#-----------------------------------------------
# create a date variable from period
dt[ , period:= as.character(period)]
dt[ , year:=substr(period, 1, 4)]
dt[ , month:=substr(period, 5, 6)]
dt[ , date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]

#-----------------------------------------------
# merge in the english names for the data elements and element type

elements_dt <- read.csv(paste0(dir, 'catalogues/data_elements_cod.csv'), stringsAsFactors=F)
elements_dt <- data.table(elements_dt)
elements_dt <- elements_dt[data_set_id=='pMbC0FJPkcm']

setnames(elements_dt, 'element', 'element_eng')
elements_dt[ ,data_set_id:=NULL]
elements_dt[ ,data_set_fr:=NULL]
elements_dt[ ,element_fr:=NULL]
elements_dt[ ,data_set:=NULL]

# merge in the english names for the data elements and element type
dt <- merge(dt, elements_dt, by='element_id', all.x=TRUE )

#-----------------------------------------------
# subset to only the variables you want to keep
dt <- dt[keep==1]

#-----------------------------------------------
# change last_update and opening_date to date variables
dt[ , last_update:=as.character(last_update)]
dt[ ,opening_date:=as.character(opening_date)]

dt$last_update <- unlist(lapply(strsplit(dt$last_update, "T"), "[", 1))
dt$opening_date <- unlist(lapply(strsplit(dt$opening_date, "T"), "[", 1))


#-----------------------------------------------
#create age and sex variables
# the majority of categories say only "default"
# keep these categories for tableau

# over 5 years of age
dt[category==">5 ans" , age:='5 +']
dt[category=="Féminin, 5 ans et plus", age:='5 +']
dt[category=="Masculin, 5 ans et plus" , age:='5 +']

# under 5 years of age
dt[category=="<5 ans" , age:='Under 5']
dt[category=="Féminin, Moins de 5 ans", age:='Under 5']
dt[category=="Masculin, Moins de 5 ans", age:='Under 5']

# create a sex variable
dt[category=="Féminin, 5 ans et plus" | category=="Féminin, Moins de 5 ans", sex:='Female']
dt[category=="Masculin, 5 ans et plus" | category=="Masculin, Moins de 5 ans", sex:='Male']

#--------------------------------------
# create an svs variable in order to subset to svs data
dt[type=='svs', svs:='Yes']
dt[type!='svs', svs:='No']

#--------------------------------------
# add a district variable 
dt[ , dist:=(substr(dt$org_unit, 1, 2))]
dt[ , dist:=tolower(dist)]

dt[dist=='bu' ,province:='Bas-Uele']
dt[dist=='eq' ,province:='Equateur']
dt[dist=='hk' ,province:='Haut-Katanga']
dt[dist=='hl' ,province:='Haut-Lomami']
dt[dist=='hu' ,province:='Haute-Uele']
dt[dist=='it' ,province:='Ituri']

dt[dist=='kn' ,province:='Kinshasa']

dt[dist=='kc' ,province:='Kongo Central'] 

dt[dist=='ks' ,province:='Kasai'] # checked
dt[dist=='ke' ,province:='Kasai Oriental'] # on the list - checked - definitely east
dt[dist=='kr' ,province:='Kasai Central'] 

dt[dist=='ki' ,province:='Haut-Lomami'] # mistake - only one facility
dt[dist=='kg' ,province:='Kwango'] 
dt[dist=='kl' ,province:='Kwilu'] 

dt[dist=='lm' ,province:='Lomami'] 
dt[dist=='ll' ,province:='Lualaba'] #checked
# typo - says 'll' but with space in front 
dt[dist==' l' ,province:='Lualaba'] 

dt[dist=='md' ,province:='Mai-Ndombe']
dt[dist=='mg' ,province:='Mongala'] # checked 
dt[dist=='mn' ,province:='Maniema'] # checked 
dt[dist=='nk' ,province:='Nord-Kivu'] 
dt[dist=='nu' ,province:='Nord-Ubangi']

dt[dist=='sn' ,province:='Sankuru'] # checked 
dt[dist=='su' ,province:='Sud-Ubangi'] # checked
dt[dist=='sk' ,province:='Sud-Kivu'] 
dt[dist=='tn' ,province:='Tanganyika']
dt[dist=='tp' ,province:='Tshopo'] #checked
dt[dist=='tu' ,province:='Tshuapa'] #checked

# fix these districts
dt[dist=='im' , province:='Unknown'] # possibly real - has 25?
dt[dist=='ii' , province:='Unknown'] # ?mistake only 9 facilities

dt[ ,dist:=NULL]

#-----------------------------------------------
# add a variable to demarcate the provincial approach provinces
dt[province=='Maniema' | province=='Tshopo' | province=="Kinshasa", mtk:='Yes']
dt[is.na(mtk), mtk:='No']

# rename province dps
setnames(dt, 'province', 'dps')
#-----------------------------------------------
# type of facility

# create organisational unit name in lower case
dt[ , org_unit1:=tolower(org_unit)]

# Clinics - add spaces to eliminate polyclinics
# This code should be first, since many facility have both clinic and another classification
clinic <- grep(pattern="\\sclinique", x=dt$org_unit1)
dt[clinic, level:='Clinic']

# health centers
health_center <- grep(pattern="centre de santé", x=dt$org_unit1)
dt[health_center, level:='Health Center']
health_center <- grep(pattern="centre de sante", x=dt$org_unit1)
dt[health_center, level:='Health Center']

# reference health center
ref_hc <- grep(pattern="centre de santé de référence", x=dt$org_unit1)
dt[ref_hc, level:='Reference Health Center']

# Health posts
health_post <- grep(pattern="poste de santé", x=dt$org_unit1)
dt[health_post, level:='Health Post']
health_post1 <- grep(pattern="poste de sante", x=dt$org_unit1)
dt[health_post1, level:='Health Post']

#  Medical centers
medical_center <-  grep(pattern="centre médical", x=dt$org_unit1)
dt[medical_center, level:='Medical Center']
medical_center <-  grep(pattern="centre medical", x=dt$org_unit1)
dt[medical_center, level:='Medical Center']
medical_center <-  grep(pattern="centre de médical", x=dt$org_unit1)
dt[medical_center, level:='Medical Center']
medical_center <-  grep(pattern="centre de medical", x=dt$org_unit1)
dt[medical_center, level:='Medical Center']

#  Hospitals
hospital <- grep(pattern="\\shôpital", x=dt$org_unit1)
dt[hospital, level:='Hospital']
hospital <- grep(pattern="\\shopital", x=dt$org_unit1)
dt[hospital, level:='Hospital']

# Secondary hospitals
hospital2 <- grep(pattern="\\shôpital secondaire", x=dt$org_unit1)
dt[hospital2, level:='Secondary Hospital']
hospital2 <- grep(pattern="\\shopital secondaire", x=dt$org_unit1)
dt[hospital2, level:='Secondary Hospital']

# Reference hospitals
hgr <- grep(pattern="hopital général de référence", x=dt$org_unit1)
dt[hgr, level:='General Reference Hospital']
hgr <- grep(pattern="hôpital général de référence", x=dt$org_unit1)
dt[hgr, level:="General Reference Hospital"]
hgr <- grep(pattern="hôpital général de réference", x=dt$org_unit1)
dt[hgr, level:="General Reference Hospital"]
hgr <- grep(pattern="hopital général de reference", x=dt$org_unit1)
dt[hgr, level:="General Reference Hospital"]

# Hospital center
hospital_c <- grep(pattern="centre hôspitalier", x=dt$org_unit1)
dt[hospital_c, level:='Hospital Center']
hospital_c1 <- grep(pattern="centre hospitalier", x=dt$org_unit1)
dt[hospital_c1, level:='Hospital Center']

# Dispensaries
dispensary <- grep(pattern="dispensaire", x=dt$org_unit1)
dt[dispensary, level:='Dispensary']

# Polyclinics
polyclinic <- grep(pattern="polyclinique", x=dt$org_unit1)
dt[polyclinic, level:='Polyclinic']

# Medical-surgical centers
m_c  <- grep(pattern="centre médico-chirurgical", x=dt$org_unit1)
dt[m_c, level:='Medical-Surgical Center']

# Health areas
health_area<- grep(pattern="aire de santé", x=dt$org_unit1)
dt[health_area, level:="Health Area"]
health_area <- grep(pattern="aire de sante", x=dt$org_unit1)
dt[health_area, level:="Health Area"]

# Health zones - there should be no health zones in dt services
health_zone <- grep(pattern="zone de sante", x=dt$org_unit1)
dt[health_zone, level:="Health Zone"]
health_zone1 <- grep(pattern="zone de santé", x=dt$org_unit1)
dt[health_zone1, level:="Health Zone"]

# until the other facilities are fixed - other category
dt[is.na(level), level:='Other']
#--------------------------------------------

# --------------------
# organize the data table 
dt <- dt[ ,.(data_set, element, date, category, element_eng,
                 age, sex, value, org_unit, level, dps, mtk,
                 type, tableau, drug, keep,
                 coordinates, opening_date, last_update, element_id, 
                 org_unit_id, month, year)]

#------------------------
# save the preppred file - original data set
# for future data sets, save only the variables you want to keep
#saveRDS(dt, paste0(dir, 'prepped_data/full/dt_02_2015_04_2018.rds'))

#------------------------
# save only the elements we plan to use for analysis in dt
dt <- dt[keep==1]

# upload the most recent full data set to merge into
dt_og <- readRDS(paste0(dir, 'prepped_data/dt.rds'))

# check the date range
dt_og[, range(date)]
dt[, range(date)]

dt_og <- dt_og[year==2017 | year==2018]

# bind the data tables together to create the most recent data set
dt <- rbind(dt_og, dt)

#-------------------------
# save the final data set

saveRDS(dt, paste0(dir, 'prepped_data/dt.rds'))

#------------------------
# create a tableau-specific data set and save it

# check that dt has all the correct tableau elements
dt[tableau==1, unique(element_eng)]

# create a data table just for the tableau elements and save it
tabl_dt <- dt[tableau==1]
tabl_dt <- tabl_dt[year==2017 | year==2018]

saveRDS(tabl_dt, paste0(dir, 'tableau/tabl_dt.rds'))

#------------------------
# test graph of tableau elements to confirm it worked 
try <- tabl_dt[ ,.(count=sum(value)), by=.(element_eng, date)]

ggplot(try, aes(x=date, y=count)) +
  geom_point() +
  geom_line() +
  facet_wrap(~element_eng) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)

#----------------------

