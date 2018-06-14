# Prep the DHIS2 data for COD
# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 6/14/2018
#
# upload the RDS files scraped from DHIS2
#  merge in the meta data 
# prep the data sets for analysis and Tableau Dashboard

# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(ggplot2)
library(stringr) # to extract meta data from file names
# --------------------

# --------------------
# set working directories

# detect if operating on windows or on the cluster 

if (Sys.info()[1] == 'Windows') {
  username <- "ccarelli"
  root <- "J:/"
} else {
  username <- Sys.getenv("USER")
  root <- "/home/j/"
}

# set the directory for input and output
dir <- paste0(root, 'Project/Evaluation/GF/outcome_measurement/cod/dhis/')

# --------------------
# Import baser services data set and convert to a data table

base <- readRDS(paste0(dir, 'pre_merge/base_services_drc_01_2015_04_2018.rds'))
base <- data.table(base)
#-----------------------------------------


#------------------------------
# merge the base services data with the meta data

# import the meta data for the merge
data_sets<- data.table(readRDS(paste0(dir, 'meta_data/data_sets.rds'))) # not necessary for the merge 
org_units <- data.table(readRDS(paste0(dir, 'meta_data/org_units_list.rds')))
data_elements <- data.table(readRDS(paste0(dir, 'meta_data/updated_data_elements.rds')))
data_elements_categories <- data.table(readRDS(paste0(dir, 'meta_data/data_elements_categories.rds')))
org_units_description <- data.table(readRDS(paste0(dir, 'meta_data/org_units_description.rds')))

# change the names of ID variables in categories and descriptions to match for the merge
data_elements[ , element_name:=displayName]
data_elements[ , displayName:=NULL]
data_elements_categories <- data_elements_categories[ ,.(category=ID, category_name=displayName)]
org_units_description <- org_units_description[ ,.(org_unit_ID = id, coordinates, opening_date, parent_id)]

#------------------------
# merge in the names of the objects in the data set

# merge on org_unit_ID to get namesand descriptions of organisational units and
base <- merge(base, org_units, by='org_unit_ID', all.x=TRUE)
base <- merge(base, org_units_description, by='org_unit_ID', all.x=TRUE)
base[ ,length(unique(org_unit_ID))] # print the number of organisational units

# merge on data element ID to get data sets and data sets name
base <- merge(base, data_elements, by='data_element_ID', all.x=TRUE)

# drop unnecessary urls
base[ , org_unit_url:=NULL]
base[ , url_list:=NULL]
#------------------------


#------------------------
# temporary save 
#saveRDS(base, paste0(dir, 'base.rds'))

#------------------------


#-------------------
# rename the variables to be more intuitive 

# put the data set in a more intuitive over and change variable types
base <- base[ , .(data_set=as.character(datasets_name), element=as.character(element_name), category=as.character(category), 
                    period=period,value=as.numeric(value), 
                    org_unit=as.character(org_unit_name), group=group,
                    coordinates=coordinates, opening_date=opening_date, last_update=last_update,
                    data_set_id=as.character(datasets_ID), element_id=as.character(data_element_ID),
                    org_unit_id=as.character(org_unit_ID))]


#------------------------------
# subset to only the relevant elements

# import the id #s of the relevant elements and create a vector
elements <- fread(paste0(dir, 'element_ids.csv'))
elements <- elements$ids

# subset base services to only the relevant elements
base <- base[element_id %in% elements]

# now that the data set is smaller, merge on category id 
# provides age and sex categories for the data elements
base <- merge(base, data_elements_categories, by='category', all.x=TRUE)
setnames(base, c('category', 'category_name'), c('category_id', 'category'))

#-----------------------------------------------
# create a date variable from period
base[ , per:= as.character(period)]
base[ , year:=substr(per, 1, 4)]
base[ , month:=substr(per, 5, 6)]
base[ , per:=NULL]

base[ , date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]

x <- substr(base$per, 1, 4)

base[ ,per2:=substr(per, 1, 4)]

base[ , month:=(as.numeric(period) %% 100) ]





#---------------
# separate out the number from facility name to get facility level
uganda_vl[, facility_name1:=paste(facility_name, '_')]

level2 <- grep(pattern="\\sII\\s", x=uganda_vl$facility_name1) 
uganda_vl[level2, unique(facility_name)]

level3 <- grep(pattern="III", x=uganda_vl$facility_name1)
uganda_vl[level3, unique(facility_name)]

level4 <- grep(pattern="IV", x=uganda_vl$facility_name1)
uganda_vl[level4, unique(facility_name)]

levelh <-  grep(pattern="Hospital", x=uganda_vl$facility_name1)
uganda_vl[levelh, unique(facility_name)]

uganda_vl[level2, level:=2]
uganda_vl[facility_id==2335, level:=2] #contains Level II in the name but no space after (typo)
uganda_vl[level3, level:=3]
uganda_vl[level4, level:=4]
uganda_vl[levelh, level:=5]

uganda_vl[is.na(level), unique(facility_name)]
uganda_vl[is.na(level), length(unique(facility_name))] #206 facilities do not contain level in the name

#--------------------------------













# create a severe malaria data set
severe_malaria <- base[element_name=='A 1.4 Paludisme grave']

severe_malaria <- severe_malaria[  , .(cases = sum(as.numeric(value))), by=.(period, category, element_name)]

severe_malaria$category_name <- factor(severe_malaria$category_name, 
                                       levels=c(">5 ans", "<5 ans"), 
                                       labels=c("Over 5", "Under 5"))

name <- unique(severe_malaria$element_name)
colors <- c('#de2d26', '#3182bd' )

ggplot(severe_malaria, aes(x=period, y=cases, color=category, group=category)) + 
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_color_manual(values=colors) +
  labs (title = paste(name, '-', 'severe malaria cases'),
        x='Period', y="Number of severe malaria cases",
        caption='Source: DHIS2', color="Age Category") 

ggplot(severe_malaria, aes(x=period, y=cases, color=category, group=category)) + 
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_color_manual(values=colors) +
  labs (title = paste(name, '-', 'severe malaria cases in 1,000 health facilities'),
        x='Period', y="Number of severe malaria cases",
        caption='Source: DHIS2', color="Age Category") 


