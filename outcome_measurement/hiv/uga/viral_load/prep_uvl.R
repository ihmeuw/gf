# ----------------------------------------------
# Caitlin O'Brien-Carelli
# Prep UVL data for analysis
# 12/24/2018
#
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(ggplot2)
library(stringr) 
library(plyr)
library(data.table)
# --------------------

# --------------------
# detect if operating on windows or on the cluster 

root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set working directory
dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard')
setwd(dir)
#--------------------------------------------------------
# import the merged data
dt = readRDS(paste0(dir, '/merged/vl_2014_2018.rds'))

# ----------------------------------------------
# drop out 55 patients in 'facility left blank'

dt = dt[facility!='Facility Left Blank']

#---------------------------------------------
# some facilities are associated with multiple district ids
# using the health facility inventory, choose a single district id

dt[facility_id==8335, district_id:=7]
dt[facility_id==8336, district_id:=68]
dt[facility_id==8337, district_id:=97]
dt[facility_id==8338, district_id:=31]
dt[facility_id==8339, district_id:=84]
dt[facility_id==8340, district_id:=101]
dt[facility_id==8341, district_id:=20]

dt[facility_id==8344, district_id:=97]
dt[facility_id==8345, district_id:=97]
dt[facility_id==8346, district_id:=86]
dt[facility_id==8347, district_id:=29]
dt[facility_id==8348, district_id:=31]

dt[facility_id==8350, district_id:=5]
dt[facility_id==8351, district_id:=85]
dt[facility_id==8352, district_id:=15]
dt[facility_id==8353, district_id:=64]
dt[facility_id==8354, district_id:=86]
dt[facility_id==8355, district_id:=30]
dt[facility_id==8356, district_id:=69] # not in inventory, majority in Mukono
dt[facility_id==8357, district_id:=35] # not in inventory, chose Kampala
dt[facility_id==8358, district_id:=39]
dt[facility_id==8359, district_id:=7]

#-----------------------------------------
# drop out the 'facility left blank' facility with no district information
# contains only 55 patients
dt = dt[district_id!=121]

#-------------------------------------------
# change district names from new 2016/17 districts to match the shape file

merge_new_dists = function(x) {
  x[district=="Bunyangabu", district:="Kabarole"]
  x[district=="Butebo", district:="Pallisa"]
  x[district=="Kagadi", district:="Kibaale"]
  x[district=="Kakumiro", district:="Kibaale"]
  x[district=="Kyotera", district:="Rakai"]
  x[district=="Namisindwa", district:="Manafwa" ]
  x[district=="Omoro", district:="Gulu"]
  x[district=="Pakwach", district:="Nebbi"]
  x[district=="Rubanda", district:= "Kabale"]
  x[district=="Rukiga", district:="Kabale"]
  x[district=="Luwero", district:="Luweero"]
  x[district=="Sembabule", district:="Ssembabule"]
  
}

# run the function on your data set
# there should be 113 districts - 112 plus one missing
merge_new_dists(dt)
length(unique(dt$district))

# -------------------------
# rename sex
dt[sex=='m', sex:='Male']
dt[sex=='f', sex:='Female']
dt[sex=='x', sex:='Unknown']

# ------------------------
# combine the duplicates into single entries

# 42 duplicate entries
dt[ ,combine:=paste0(date,'_', facility_id,'_', sex)]
dt[duplicated(combine)]

# full data table of all duplicate entries as single entries
# drop out hub
dt = dt[ , .(patients_received=sum(patients_received),
                samples_received=sum(samples_received),  
                rejected_samples=sum(rejected_samples), 
                dbs_samples=sum(dbs_samples), 
                samples_tested=sum(total_results),
                suppressed=sum(suppressed), valid_results=sum(valid_results)),
            by=.(facility_id, facility, district_id, district, dhis2_name,
                 sex, date)]


# -------------------------------------------
# determine facility levels from facility name

# separate out the number from facility name to get facility level
dt[, facility_1:=paste(facility, '_')] # creates a space after the name
dt[, facility_1:=tolower(facility_1)]

dt[grepl(pattern="\\sii\\s", facility_1), level:='HC II'] 
dt[grepl(pattern="\\sii", facility_1) & grepl(pattern="\\siii", facility_1), level:='2'] 
dt[grepl(pattern="iii", facility_1), level:='HC III']
dt[grepl(pattern="\\siv",facility_1), level:='HC IV']
dt[grepl(pattern="hospital", facility_1) & !grepl(pattern='military hospital', facility_1), level:='Hospital']
dt[grepl(pattern="military hospital", facility_1), level:='Military Hospital']

# TASO unique health centers
dt[grepl(pattern="taso", facility_1), level:='TASO']

#-----------------------------------------------
# use dhis2name to get facility levels not contained in the udt dashboard name
dt[!is.na(dhis2_name), dhis2_name1:=paste(dhis2_name, '_')] # creates a space after the name
dt[!is.na(dhis2_name), dhis2_name1:=tolower(dhis2_name1)]

dt[grepl(pattern="\\sii\\s", dhis2_name1), level2:='HC II'] 
dt[grepl(pattern="\\sii", dhis2_name1) & grepl(pattern="\\siii", dhis2_name1), level2:='2'] 
dt[grepl(pattern="iii", dhis2_name1), level2:='HC III']
dt[grepl(pattern="\\siv",dhis2_name1), level2:='HC IV']
dt[grepl(pattern="hospital", dhis2_name1) & !grepl(pattern='military hospital', dhis2_name1), level2:='Hospital']
dt[grepl(pattern="military hospital", dhis2_name1), level2:='Military Hospital']

# replace missing levels from facility names with levels from dhis2names
dt[is.na(level), level:=level2]

# facility name and dhis2name disagree
dt[facility_id==2335, level:='HC II'] 

# number of facilities without level
dt[is.na(level), length(unique(facility))]

#--------------------------------
# create a variable to identify prisons
dt[grep(pattern="prison", facility_1), prison:='Yes']
dt[grep(pattern="prison", dhis2_name1), prison:='Yes'] # using dhis2name captures a remand center
dt[is.na(prison), prison:='No']

# delete excess variables
dt[ , c("facility_1", "dhis2_name1", 'level2'):=NULL]

#--------------------------------
# convert integers to numerics 
vars = c('facility_id', 'facility', 'district_id', 'district', 
          'dhis2_name', 'sex', 'date', 'level', 'prison')

dt = dt[ ,lapply(.SD, as.numeric), .SDcols=8:14, by=vars]

#--------------- 

#-----------------------------------------------
# assign unknown sex using mean imputation

# create a data set to calculate the sex ratio by district
vars = c('district', 'sex')
ratio = dt[sex!='Unknown' & !is.na(district), lapply(.SD, sum), .SDcols=10:16, by=vars]

# create a data table of totals
total = ratio[ ,lapply(.SD, sum), .SDcols=3:9, by=district]
total[ , sex:='Total']

# bind them together and drop the males
total = rbind(ratio, total)
total = total[order(district)]
total = total[sex!='Male']

#-------------------------------

#-------------------------------
# replace the female values with the ratio for every variable

total[, patients_received:=(patients_received/shift(patients_received, type='lead'))]
total[, samples_received:=(samples_received/shift(samples_received, type='lead'))]
total[, rejected_samples:=(rejected_samples/shift(rejected_samples, type='lead'))]
total[, dbs_samples:=(dbs_samples/shift(dbs_samples, type='lead'))]
total[, samples_tested:=(samples_tested/shift(samples_tested, type='lead'))]
total[, suppressed:=(suppressed/shift(suppressed, type='lead'))]
total[, valid_results:=(valid_results/shift(valid_results, type='lead'))]

#-------------------------------
# replace the total values with the male ratio

# create a data set of the female ratio
females = total[sex=='Female']

# calculate the male ration
male_fun = function(x) {
  x = (1 - x) 
}

# create a male ratio data set
total = total[sex=='Female']
males = total[ ,lapply(.SD, male_fun), .SDcols=3:9, by=.(district, sex)]
males[ ,sex:='Male']

#-------------------------------
# rbind them together and apply

# create a single ratio data set
ratio = rbind(females, males)

# reset the names for the merge
setnames(ratio, c("patients_received", "samples_received",  "rejected_samples",  "dbs_samples",   
                  "samples_tested", "suppressed", "valid_results"),
                  c("patients_received1", "samples_received1",  "rejected_samples1",  "dbs_samples1",   
                   "samples_tested1", "suppressed1", "valid_results1"))


# if a ratio is missing, assign the patients received ratio
ratio[is.na(samples_received1), samples_received1:=patients_received1]
ratio[is.na(rejected_samples1), rejected_samples1:=patients_received1]
ratio[is.na(dbs_samples1), dbs_samples1:=patients_received1]
ratio[is.na(samples_tested1), samples_tested1:=patients_received1]
ratio[is.na(suppressed1), suppressed1:=patients_received1]
ratio[is.na(valid_results1), valid_results1:=patients_received1]

#-------------------------------
# create a data set of unknowns - double in size to apply ratios
fems = dt[sex=='Unknown']
men = dt[sex=='Unknown']
fems[ , sex:='Female']
men[ ,sex:='Male']
people = rbind(fems, men)

#-------------------------------
# merge in the ratios

unknowns = merge(people, ratio, by=c('district', 'sex'), all.x=T)

# calculate the new values
unknowns[!is.na(district), patients_received:=(patients_received*patients_received1)]
unknowns[!is.na(district), samples_received:=(samples_received*samples_received1)]
unknowns[!is.na(district), rejected_samples:=(rejected_samples*rejected_samples1)]
unknowns[!is.na(district), dbs_samples:=(dbs_samples*dbs_samples1)]
unknowns[!is.na(district), samples_tested:=(samples_tested*samples_tested1)]
unknowns[!is.na(district), suppressed:=(suppressed*suppressed1)]
unknowns[!is.na(district), valid_results:=(valid_results*valid_results1)]

# drop out the people with unknown district
unknowns = unknowns[!is.na(district)]

# drop unnecessary variables
unknowns[ , c("patients_received1", "samples_received1",  "rejected_samples1",  "dbs_samples1",   
            "samples_tested1", "suppressed1", "valid_results1"):=NULL]

# round to single digit
vars = c( 'facility_id', 'district_id', 'facility', 'dhis2_name',  
          'district', 'sex', 'date','level', 'prison')

unknowns = unknowns[ ,lapply(.SD, round, 1), .SDcols=10:16, by=vars]

#-------------------------------
# merge in new values

dt = dt[sex!='Unknown']
dt = rbind(dt, unknowns)

# sum over values to ensure new females and males are incorporated
dt = dt[ ,lapply(.SD, sum), .SDcols=10:16, by=vars]

#-------------------------------
# final quality checks 

# check for missing data - show all be 0 rows
dt[is.na(patients_received)]
dt[is.na(samples_received)]
dt[is.na(rejected_samples)]
dt[is.na(dbs_samples)]
dt[is.na(samples_tested)]
dt[is.na(suppressed)]
dt[is.na(valid_results)]

#--------------------------------------------
# check equality constraints
# if the variables violate equality constraints, alter them

# samples received can be greater than patients received
dt[samples_received < rejected_samples, rejected_samples:=samples_received]
dt[samples_received < dbs_samples, dbs_samples:=samples_received]
dt[samples_received < samples_tested, samples_tested:=samples_received]
dt[samples_received < valid_results, valid_results:=samples_received]
dt[samples_tested < valid_results, valid_results:=samples_tested]
dt[valid_results < suppressed, suppressed:=valid_results]

# -------------------------------------------
# calculate plasma samples
dt[ , plasma_samples:=(samples_received - dbs_samples)]

# change to the correct order
dt = dt[ ,.(patients_received, samples_received, rejected_samples, dbs_samples, 
            plasma_samples, samples_tested, valid_results, suppressed), 
            by=.(facility_id, district_id, dhis2_name, level, prison,
                 facility, district, sex, date)]

#--------------------------------------------
# final equality constraints check
dt[samples_received < rejected_samples]
dt[samples_received < dbs_samples]
dt[samples_received < plasma_samples ]
dt[samples_received < samples_tested]
dt[samples_received < valid_results]
dt[samples_tested < valid_results]
dt[valid_results < suppressed]

#-------------------------------
# add a year variable
dt[ , year:=year(date)]

#-------------------------------
# save the final data as an RDS

saveRDS(dt, file= paste0(dir, "/prepped_data/sex_data.rds"))

#-------------------------------



