# Final cleaning on PNLS
# Use this to manually aggregate the final variables
# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr) 
library(openxlsx)
# --------------------

# shell script for working on the cluster
# sh /share/singularity-images/rstudio/shells/rstudio_qsub_script.sh -p 1247 -s 2 

# --------------------
# set working directories

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
# dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

# local directory
dir = "C:/Users/ccarelli/Documents/pnls_data/"

#---------------------------------------
# load the file that represents a subset (no sex, age, or support)
# dt = data.table(readRDS(paste0(dir, 'prepped/pnls_sets/pnls_vct_2017_01_01_2019_02_01.rds')))

# load local data table
dt = data.table(readRDS(paste0(dir, 'pnls_vct_2017_01_01_2019_02_01.rds')))

# set the name of the set to clean
set_name = dt[ ,tolower(unique(pnls_set))]
# #---------------------------------------
# 
# # import the translated elements
# new_elements = data.table(read.xlsx(paste0(dir, 'meta_data/translate/pnls_elements_translations_', set_name, '.xlsx' )))

# import the translated elements locally
new_elements = data.table(read.xlsx(paste0(dir, 'pnls_elements_translations_', set_name, '.xlsx' )))

# drop out french elements for the merge 
new_elements[ ,element:=NULL]

# check to make sure you didn't include additional spaces
new_elements[, element_eng:=trimws(element_eng)]
#---------------------------------------

#---------------------------------------------------------------------
# Merge the data and the new English elements together 

# drop the previous english elements out the data for the corrected elements
dt[ ,c("maternity", "case","stock_category", "tb"):=NULL] # all of these elements are missing

dt = merge(dt, new_elements, by='element_id', all.x = TRUE)
#---------------------------------------------------------------------
# check the final translations 

# export only the french elements and their translations
# export_file = dt[,.(elements=unique(element)), by=.(element_eng, subpop)]
# write.xlsx(export_file, paste0(dir, 'meta_data/translate/pnls_elements_vct_check_translations.xlsx'))

#---------------------------------------------------------------------
# drop the useless element (these two elements have unique ids but identical variable names)

dt = dt[element_eng!='DROP' & element_eng!='Patients admitted to the hospital']

#---------------------------------------------------------------------
# fill in couples and patients for the missing sub-populations

dt[grepl("couple", tolower(element)) & is.na(subpop), subpop:='couple']
dt[grepl("malade", tolower(element)) & is.na(subpop), subpop:='patient']
dt[subpop=='serodisc', subpop:='couple'] # count serodiscordant among couples

#---------------------------------------------------------------------
# collapse to the appropriate values 

byVars = names(dt)[names(dt)!='element' & names(dt)!='element_id' & names(dt)!='last_update' & names(dt)!='coordinates' & names(dt)!='org_unit_type']
dt = dt[ ,.(value=sum(value)), by=byVars]

# ----------------------------------
# subset to only 2017 and 2018

dt = dt[date < '2019-01-01']

#---------------------------------------------------------------------
# split kinshasa between funders

# split global fund and pepfar dps
dt[dps=='Haut Katanga' | dps=='Lualaba', funder:='PEPFAR']
dt[!(dps=='Haut Katanga' | dps=='Lualaba' | dps=='Kinshasa'), funder:='The Global Fund']

# for kinshasa, split the city by health zone
gf_zones = c('Barumbu', 'Gombe','Kasa Vubu', 
             'Kintambo', 'Police', 'Selembao', 'Biyela', 'Bumbu', 
             'Kalamu 1', 'Kalamu 2', 'Kisenso', 'Lemba', 'Makala', 
             'Mont Ngafula 2')

# set the funders in kinshasa by health zone
dt[health_zone %in% gf_zones, funder:='The Global Fund']
dt[is.na(funder), funder:='PEPFAR']

#---------------------------------------------------------------------
# format the data set according to plan

# change the variable name to the english versions
setnames(dt, 'element_eng', 'variable')

# re-arrange variables and drop set
dt = dt[ ,.(org_unit_id, org_unit, date, sex, age, subpop, variable, value, 
            facility_level, dps, health_zone, health_area, funder)]

# change the translation of client
dt[subpop=='customer', subpop:='client']

#---------------------------------------------------------------------

#------------------------------------
# minor outlier screen

dt = dt[value!=769776 & value!=29841 & value!=10000 & value!=6985 & !(variable=='HIV+' & value==510)]

# outliers noticed
dt = dt[!(subpop=='msm' & value=='2757')] # one wonderful facility keeps reporting 2757 MSM

# ensure value is numeric
dt[ ,value:=as.numeric(value)]

# round the values to integers
dt[ ,value:=round(value)]

# some values are dropped in qr
dt = dt[!is.na(value)]

#--------------------------------------
# CREATE GRAPH LABELS

#------------------------------------
# factor sub populations for graphs 

dt$subpop = factor(dt$subpop, 
                   c("prisoner", "trans", "idu", "trucker",  "uniform", "msm", "csw_customer", 
                     "fisher", "miner", "other_groups", "couple", "csw", "client", "patient"),    
                   c("Prisoners", "Trans people", "IDUs", "Truckers", "Uniformed personnel",
                     "MSM", "CSW Clients", "Fisher people", "Miners", 
                     "Other groups", "Couples", "CSWs", "Clients", "Patients")) 

#----------------------------------
# equality constraints check on testing and positive
# if there are more HIV cases reported than tests, remove the value
# remove the value if one is missing and the other is not 

check = dt[variable=='Tested and received the results' | variable=='HIV+']
check = check[ ,.(value=sum(value)), by = .(org_unit_id, date, variable, sex, age, subpop)]
check = dcast(check, org_unit_id+sex+age+subpop+date~variable)
setnames(check, c('org_unit_id', 'sex', 'age', 'subpop', 'date', 'hiv', 'tests'))
check[ , eq:=(hiv > tests)]
check[ , missing_one:=(is.na(hiv) | is.na(tests))]
check = check[eq==T]

check[ , check_var:=paste0(org_unit_id, sex, age, subpop, date)]
dt[ , check_var:=paste0(org_unit_id, sex, age, subpop, date)]
dt = dt[!(check_var %in% check$check_var)]
dt[ ,check_var:=NULL]

#----------------------------------
# bizarre month where people reported hiv+ trans people but no tests

dt = dt[!(date=='2017-07-01' & subpop=='Trans people' & variable=='HIV+')]

#----------------------------------
# create smaller health facility groupings for graphs 

dt[grep('hospital',facility_level), next_level:='Hospitals']
dt[facility_level=='reference_health_center', next_level:='Reference health centers']
dt[facility_level=='health_center' | facility_level=='health_post' | facility_level=='dispensary', next_level:='Health centers, posts, and dispensaries']
dt[is.na(next_level), next_level:='Other types of facilities']

# factor facility level for graphs
dt$facility_level = factor(dt$facility_level, 
                           rev(c("health_center", "reference_health_center", "health_post", "hospital", 
                                 "general_reference_hospital", "hospital_center", "medical_center",
                                 "clinic", "secondary_hospital",  "dispensary","polyclinic", "medical_surgical_center")),
                           rev(c("Health Center", "Reference Health Center", "Health Post", "Hospital", 
                                 "General Reference Hospital", "Hospital Center", "Medical Center",
                                 "Clinic", "Secondary Hospital",  "Dispensary","Polyclinic", "Medical surgical center")))

#------------------------------------------------------------------
# bind in he client variables so clients are not reported separately

dt[variable=='Clients counseled', variable:='Counseled']
dt[variable=='Clients tested', variable:='Tested'] # only in clients
dt[variable=='Clients tested and received the results', variable:='Tested and received the results']
dt[variable=='Clients enrolled in case management', variable:='Enrolled in case management'] # only in clients

dt[variable=='Clients HIV+', variable:='HIV+']
dt[variable=='Clients HIV+ and informed of the results', variable:='HIV+ and informed of the results']
dt[variable=='Clients with indeterminate status', variable:='Indeterminate status'] # only in clients

#---------------------------------------------------------------------
# export the final data 

saveRDS(dt, paste0(dir, "pnls_vct_final_labels.rds"))
#---------------------------------------------------------------------



