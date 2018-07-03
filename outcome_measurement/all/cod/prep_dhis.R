# Prep the COD DHIS2 data 
# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 6/15/2018
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
# Import base services data set and convert to a data table

base <- readRDS(paste0(dir, 'pre_merge/base_services_drc_01_2015_04_2018.rds'))
base <- data.table(base)
#-----------------------------------------

#------------------------------
# subset to only the relevant elements
# subset before further cleaning if working on a home machine
# data set is too large for data table manipulation (crashes RStudio)

# import the id #s of the relevant elements and create a vector
elements <- fread(paste0(dir, 'elements_catalogue.csv'))

# subset to only base services
elements_base <- elements[datasets_ID=='pMbC0FJPkcm']   
elements_base <- elements_base[keep==1]

# subset to only the relevant ids
element_ids <- as.character(elements_base$data_element_ID)

# subset base services to only the relevant elements
base[ , data_element_ID:=as.character(data_element_ID)]
base <- base[data_element_ID %in% element_ids]

#-------------------

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
base <- merge(base, org_units, by='org_unit_ID', all.x=TRUE)
base <- merge(base, org_units_description, by='org_unit_ID', all.x=TRUE)
base[ ,length(unique(org_unit_ID))] # print the number of organisational units

# merge on data element ID to get data sets and data sets name
base <- merge(base, data_elements, by='data_element_ID', all.x=TRUE)

# merge on category id to get age and sex categories for the data elements
base <- merge(base, data_elements_categories, by='category', all.x=TRUE)
setnames(base, c('category', 'category_name'), c('category_id', 'category'))

# drop unnecessary urls
base[ , org_unit_url:=NULL]
base[ , url_list:=NULL]
#------------------------

#------------------------
# rename the variables to be more intuitive

# put the data set in a more intuitive order and change variable types
base <- base[ , .(data_set=as.character(datasets_name), element=as.character(element_name), category=as.character(category),
                    period=period,value=as.numeric(value),
                    org_unit=as.character(org_unit_name), group=group,
                    coordinates=coordinates, opening_date=opening_date, last_update=last_update,
                    data_set_id=as.character(datasets_ID), element_id=as.character(data_element_ID),
                    org_unit_id=as.character(org_unit_ID))]

#-----------------------------------------------
# create a date variable from period
base[ , per:= as.character(period)]
base[ , year:=substr(per, 1, 4)]
base[ , month:=substr(per, 5, 6)]
base[ , per:=NULL]
base[ , period:=NULL]

base[ , date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]

#-----------------------------------------------
# change last_update and opening_date to date variables
base[ , last_update:=as.character(last_update)]
base[ ,opening_date:=as.character(opening_date)]

base$last_update <- unlist(lapply(strsplit(base$last_update, "T"), "[", 1))
base$opening_date <- unlist(lapply(strsplit(base$opening_date, "T"), "[", 1))

#-----------------------------------------------
# merge in the english names for the data elements and element type
elements_base <- elements_base[ ,.(element_id=data_element_ID, element_eng=displayName, type=type, drug=drug)]
base <- merge(base, elements_base, by='element_id', all.x=TRUE )

# change the default name of elements to english
setnames(base, c('element', 'element_eng'), c('element_fr', 'element'))

#-----------------------------------------------

#------------------------
#create age and sex variables
# the majority of categories say only "default"

# over 5 years of age
base[category==">5 ans" , age:='5 +']
base[category=="Féminin, 5 ans et plus", age:='5 +']
base[category=="Masculin, 5 ans et plus" , age:='5 +']

# under 5 years of age
base[category=="<5 ans" , age:='Under 5']
base[category=="Féminin, Moins de 5 ans", age:='Under 5']
base[category=="Masculin, Moins de 5 ans", age:='Under 5']

# create a sex variable
base[category=="Féminin, 5 ans et plus" | category=="Féminin, Moins de 5 ans", sex:='Female']
base[category=="Masculin, 5 ans et plus" | category=="Masculin, Moins de 5 ans", sex:='Male']

#set missing age values to 'Unknown'
base[is.na(age), age:='Unknown']
base[is.na(sex), sex:='Unknown']

#--------------------------------------
# create an svs variable in order to subset to svs data
base[type=='svs', svs:=1]
base[type!='svs', svs:=0]

#--------------------------------------
# add a district variable 
base[ , dist:=(substr(base$org_unit, 1, 2))]
base[ , dist:=tolower(dist)]

base[dist=='bu' ,province:='Bas-Uele']
base[dist=='eq' ,province:='Equateur']
base[dist=='hk' ,province:='Haut-Katanga']
base[dist=='hl' ,province:='Haut-Lomami']
base[dist=='hu' ,province:='Haute-Uele']
base[dist=='it' ,province:='Ituri']

base[dist=='kn' ,province:='Kinshasa']

base[dist=='kc' ,province:='Kongo Central'] 

base[dist=='ks' ,province:='Kasai'] # checked
base[dist=='ke' ,province:='Kasai Oriental'] # on the list - checked - definitely east
base[dist=='kr' ,province:='Kasai Central'] 

base[dist=='ki' ,province:='Haut-Lomami'] # mistake - only one facility
base[dist=='kg' ,province:='Kwango'] 
base[dist=='kl' ,province:='Kwilu'] 

base[dist=='lm' ,province:='Lomami'] 
base[dist=='ll' ,province:='Lualaba'] #checked
# typo - says 'll' but with space in front 
base[dist==' l' ,province:='Lualaba'] 

base[dist=='md' ,province:='Mai-Ndombe']
base[dist=='mg' ,province:='Mongala'] # checked 
base[dist=='mn' ,province:='Maniema'] # checked 
base[dist=='nk' ,province:='Nord-Kivu'] 
base[dist=='nu' ,province:='Nord-Ubangi']

base[dist=='sn' ,province:='Sankuru'] # checked 
base[dist=='su' ,province:='Sud-Ubangi'] # checked
base[dist=='sk' ,province:='Sud-Kivu'] 
base[dist=='tn' ,province:='Tanganyika']
base[dist=='tp' ,province:='Tshopo'] #checked
base[dist=='tu' ,province:='Tshuapa'] #checked

# mistaken names - checked
base[org_unit=="Im Kabinda Hôpital Général de Référence", dist:='lm']
base[org_unit=="Im Kabinda Hôpital Général de Référence", province:='Lomami']

# fix these districts
base[dist=='im' , province:='Unknown'] # possibly real - has 25?
base[dist=='ii' , province:='Unknown'] # ?mistake only 9 facilities

base[ ,dist:=NULL]

#-----------------------------------------------
# add a variable to demarcate the provincial approach provinces
base[province=='Maniema' | province=='Tshopo' | province=="Kinshasa", mtk:=1]
base[is.na(mtk), mtk:=0]

#-----------------------------------------------
# type of facility

# create organisational unit name in lower case
base[ , org_unit1:=tolower(org_unit)]

# Clinics - add spaces to eliminate polyclinics
# This code should be first, since many facility have both clinic and another classification
clinic <- grep(pattern="\\sclinique", x=base$org_unit1)
base[clinic, level:='Clinic']

# health centers
health_center <- grep(pattern="centre de santé", x=base$org_unit1)
base[health_center, level:='Health Center']
health_center <- grep(pattern="centre de sante", x=base$org_unit1)
base[health_center, level:='Health Center']

# reference health center
ref_hc <- grep(pattern="centre de santé de référence", x=base$org_unit1)
base[ref_hc, level:='Reference Health Center']

# Health posts
health_post <- grep(pattern="poste de santé", x=base$org_unit1)
base[health_post, level:='Health Post']
health_post1 <- grep(pattern="poste de sante", x=base$org_unit1)
base[health_post1, level:='Health Post']

#  Medical centers
medical_center <-  grep(pattern="centre médical", x=base$org_unit1)
base[medical_center, level:='Medical Center']
medical_center <-  grep(pattern="centre medical", x=base$org_unit1)
base[medical_center, level:='Medical Center']
medical_center <-  grep(pattern="centre de médical", x=base$org_unit1)
base[medical_center, level:='Medical Center']
medical_center <-  grep(pattern="centre de medical", x=base$org_unit1)
base[medical_center, level:='Medical Center']

#  Hospitals
hospital <- grep(pattern="\\shôpital", x=base$org_unit1)
base[hospital, level:='Hospital']
hospital <- grep(pattern="\\shopital", x=base$org_unit1)
base[hospital, level:='Hospital']

# Reference hospitals
hospital <- grep(pattern="hopital général de référence", x=base$org_unit1)
base[hospital, level:='General Reference Hospital']
hgr <- grep(pattern="hgr", x=base$org_unit1)
base[hgr, level:="General Reference Hospital"]

# Hospital center
hospital_c <- grep(pattern="centre hôspitalier", x=base$org_unit1)
base[hospital_c, level:='Hospital Center']
hospital_c1 <- grep(pattern="centre hospitalier", x=base$org_unit1)
base[hospital_c1, level:='Hospital Center']

# Dispensaries
dispensary <- grep(pattern="dispensaire", x=base$org_unit1)
base[dispensary, level:='Dispensary']

# Polyclinics
polyclinic <- grep(pattern="polyclinique", x=base$org_unit1)
base[polyclinic, level:='Polyclinic']

# Medical-surgical centers
m_c  <- grep(pattern="centre médico-chirurgical", x=base$org_unit1)
base[m_c, level:='Medical-Surgical Center']

# Health areas
health_area<- grep(pattern="aire de santé", x=base$org_unit1)
base[health_area, level:="Health Area"]
health_area <- grep(pattern="aire de sante", x=base$org_unit1)
base[health_area, level:="Health Area"]

# Health zones - there should be no health zones in base services
health_zone <- grep(pattern="zone de sante", x=base$org_unit1)
base[health_zone, level:="Health Zone"]
health_zone1 <- grep(pattern="zone de santé", x=base$org_unit1)
base[health_zone1, level:="Health Zone"]

# until the other facilities are fixed - other category
base[is.na(level), level:='Other']
#--------------------------------------------

# --------------------
# organize the data table 
base <- base[ ,.(data_set, element, date, category, age, sex, type, value, org_unit, level, province,
                mtk, coordinates, opening_date, last_update, drug, svs, element_fr, element_id, 
                org_unit_id, group, data_set_id, month, year)]

#------------------------
# save the preppred file
saveRDS(base, paste0(dir, 'base.rds'))

#------------------------
