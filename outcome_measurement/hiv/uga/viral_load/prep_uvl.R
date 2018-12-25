# ----------------------------------------------
# Caitlin O'Brien-Carelli
# Prep UVL data for analysis
# 12/24/2018
#
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(stringr) 
library(plyr)
library(gtools)
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
# import districts for facilities without names

# some samples are associated with a district or a hub 
dt[is.na(facility), sum(samples_received)]

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
# calculate plasma samples
dt[ , plasma_samples:=(samples_received - dbs_samples)]

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

dt = dt[ ,lapply(.SD, as.numeric), .SDcols=8:15, by=vars]

#--------------- 

#-----------------------------------------------
# assign unknown sex using mean imputation

# create a data set to calculate the sex ratio by district
vars = c('district', 'sex')
ratio = dt[sex!='Unknown' & !is.na(district), lapply(.SD, sum), .SDcols=10:17, by=vars]

# create a data table of totals
total = ratio[ ,lapply(.SD, sum), .SDcols=3:10, by=district]
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
total[, plasma_samples:=(plasma_samples/shift(plasma_samples, type='lead'))]

#-------------------------------
# replace the total values with the male ratio

# create a data set of the female ratio
females = total[sex=='Female']

# calculate the male ratio (1 - females)
total[, patients_received:=(1 - shift(patients_received, type='lead'))]
total[, samples_received:=(1 - shift(samples_received, type='lead'))]
total[, rejected_samples:=(1 - shift(rejected_samples, type='lead'))]
total[, dbs_samples:=(1 - shift(dbs_samples, type='lead'))]

total[, samples_tested:=(1 - shift(samples_tested, type='lead'))]
total[, suppressed:=(1 - shift(suppressed, type='lead'))]
total[, valid_results:=(1 - shift(valid_results, type='lead'))]
total[, plasma_samples:=(1 - shift(plasma_samples, type='lead'))]

# create a data set of only males
males = total[sex=='Total']
males[ , sex:='Male']

#-------------------------------
# rbind them together and apply

# create a single ratio data set
ratio = rbind(females, males)

# reset the names for the merge
setnames(ratio, c("patients_received", "samples_received",  "rejected_samples",  "dbs_samples",   
                  "samples_tested", "suppressed", "valid_results", "plasma_samples"),
                  c("patients_received1", "samples_received1",  "rejected_samples1",  "dbs_samples1",   
                   "samples_tested1", "suppressed1", "valid_results1", "plasma_samples1"))


# create a data set of unknowns
fems = dt[sex=='Unknown']
men = dt[sex=='Unknown']
fems[ , sex:='Female']
men[ ,sex:='Male']
unknowns = rbind(fems, men)







