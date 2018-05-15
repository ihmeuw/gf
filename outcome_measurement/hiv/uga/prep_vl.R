# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 5/11/2018
#
# Combine the downloaded Uganda VL data w filters month, year, sex
# Merge in the names of the districts and facilities
# Collapse districts to match most recent shape files
# Prep the data for analysis
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(ggplot2)
library(stringr) # to help extract meta ata from file names

# --------------------

# ----------------------------------------------
# set files and directories for the viral load data

# change directory to files where data w sex filter is kept
setwd("J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape_agg/sex")

# list existing files
files <- list.files('./', recursive=TRUE)
files

# --------------------

# ----------------------------------------------
# add identifying variables to the existing data tables using file names
# add year, month, sex

# loop over existing files
i = 1
for(f in files) {
  
  #Load the RDs file
  jsonData = readRDS(f)
  
  # pull out relevant table
  current_data = data.table(jsonData$f_numbers)
  
  # skip to next if there was no data for this combination
  if (length(current_data)==0) next

  #to check the position of variables: 
  #outFile = paste0(dir, '/facilities_suppression_', m,'_','20', y,'_',s,'_','.rds')
  # positions: year = 4; month = 3; sex=5
  
  # extract meta data from file name
  meta_data = strsplit(f, '_')[[1]]
  current_data[, year:=as.numeric(substr(meta_data[4],1,4))]
  current_data[, month:=as.numeric(substr(meta_data[3],1,2))]
  current_data[, sex:=(meta_data[5])]

  # append to the full data 
  if(i==1) full_data = current_data
  if(i>1) full_data = rbind(full_data, current_data)
  i = i+1
}

# view the final product of full_data
str(full_data)

# ----------------------------------------------

#  stats to check if the sex disaggregated data downloaded correctly 

# run some stats to check that the data downloaded correctly
full_data[, sum(samples_received), by=year]
full_data[month==1, sum(samples_received), by=year] # should be no samples in jan 2014

# check that no data downloaded for jan - july 2014 or after march 2018
# both should be 0
full_data[year==2014 & month!=8 & month!=9 & month!=10 & month!=11 & month!=12, sum(samples_received)]
full_data[year==2018 & month!=1 & month!=2 & month!=3, sum(samples_received)]

full_data[year==2014, sum(samples_received), by=.(month, sex)]
full_data[year==2014, sum(samples_received), by=month]
full_data[year==2018, sum(samples_received), by=.(month, sex)]

# check for substantial variability in the number of samples
full_data[, .(total_samples=sum(samples_received)), by=.(sex, year)]

# check for missing data in the unknown sex category
full_data[sex=='x', .(total_sup=sum(samples_received)), by=.(month, year)]
full_data[sex=='x', .(total_sup=sum(suppressed)), by=.(month, year)]

# ----------------------------------------------

# upload the facilities names, ids and check for disparate values

# reset working directory to main folder
setwd("J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard")
dir <- "J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard"

facilities <- readRDS(paste0(dir,"/facilities/facilities.rds"))
str(facilities)

# list unique facility ids in the full data
full_data [,facility_id, by=facility_id] # 2042 values
full_data[is.na(facility_id)] # no missing facility ids

# print a list of the facility ids in full_data that are not on the names list
# 34 facilities, 692 patients in those facilities
full_data[!full_data$facility_id %in% facilities$facility_id, .(length(unique(facility_id)), 
                                                                sum(patients_received))]

# ----------------------------------------------
# merge the facility and district names into the full data set
# additional to code to identify merge mismatches and downloading errors

#------------------------
# merge in the facilities
uvl_sex <- merge(full_data, facilities, by='facility_id', all.x=TRUE)

# 34 facilities are missing a name, containing 692 patients
uvl_sex[is.na(facility_name), length(unique(facility_id))]
uvl_sex[is.na(facility_name), .(sum(patients_received))]

# create a placeholder for missing facility names for 34 facilities
uvl_sex [is.na(facility_name), facility_name:=paste0('Facility #',facility_id)]


# ----------------------------------------------
# additional to code to identify merge mismatches and downloading errors

# 2 facilities contain Rakai in the name
uvl_sex[facility_id==228 | facility_id==175, district_id:=80]

# looked up in the health facility inventory; majority are Kyotera (134) and Rakai(80)
uvl_sex[facility_id==255, district_id:=80]
uvl_sex[facility_id==502, district_id:=80]
uvl_sex[facility_id==1351, district_id:=80]
uvl_sex[facility_id==1526, district_id:=80]
uvl_sex[facility_id==1575 , district_id:=80]
uvl_sex[facility_id==2058, district_id:=89]

uvl_sex[facility_id== 8335, district_id:=7]
uvl_sex[facility_id==8350, district_id:=5]
uvl_sex[facility_id== 8359, district_id:=7]
uvl_sex[facility_id== 8352, district_id:=15]
uvl_sex[facility_id== 8341, district_id:=20]
uvl_sex[facility_id== 8348, district_id:=31]

uvl_sex[facility_id==8347 , district_id:=29]
uvl_sex[facility_id==8355 , district_id:=30]
uvl_sex[facility_id==8338, district_id:=31]
uvl_sex[facility_id==8358, district_id:=39]
uvl_sex[facility_id==8340, district_id:=101]
uvl_sex[facility_id==8357, district_id:=35] # not in inventory, chose Kampala
uvl_sex[facility_id==8351, district_id:=85]
uvl_sex[facility_id==8345, district_id:=97]

uvl_sex[facility_id==8353, district_id:=64]
uvl_sex[facility_id==8354, district_id:=86]
uvl_sex[facility_id==8344, district_id:=97]
uvl_sex[facility_id==8336, district_id:=68]
uvl_sex[facility_id==8356, district_id:=69] # not in inventory, majority in Mukono
uvl_sex[facility_id==8346, district_id:=86]
uvl_sex[facility_id==8339, district_id:=84]
uvl_sex[facility_id==8337, district_id:=97]


# facility ids that are associated with multiple district ids
# 29 facilities are associated with two district ids (always two)
# check for duplicates after the change
dups <- uvl_sex[ , .(dup=length(unique(district_id))), by=.(facility_id, facility_name)]
dups <- dups[dup>1, .(facility_id, facility_name)]
dups # should be an empty data table


#------------------------
# merge in the districts
districts <- readRDS(paste0(dir,"/facilities/districts.rds"))
uvl_sex <- merge(uvl_sex, districts, by='district_id', all.x=TRUE)

# ---------------

# change district names from new 2016/17 districts to previous districts
# allows for map making (shape files not yet updated)

#create a function that merges new districts into previous districts to match shape file
merge_new_dists <- function(x) {
  x[district_name=="Bunyangabu", district_name:="Kabarole"]
  x[district_name=="Butebo", district_name:="Pallisa"]
  x[district_name=="Kagadi", district_name:="Kibaale"]
  x[district_name=="Kakumiro", district_name:="Kibaale"]
  x[district_name=="Kyotera", district_name:="Rakai"]
  x[district_name=="Namisindwa", district_name:="Manafwa" ]
  x[district_name=="Omoro", district_name:="Gulu"]
  x[district_name=="Pakwach", district_name:="Nebbi"]
  x[district_name=="Rubanda", district_name:= "Kabale"]
  x[district_name=="Rukiga", district_name:="Kabale"]
  
}

# run the function on your data set
# there should be 113 districts - 112 plus one missing
merge_new_dists(uvl_sex)
length(unique(uvl_sex$district_name))

# Change spelling of Luwero=Luweero and Sembabule=Ssembabule
uvl_sex[district_name=="Luwero", district_name:="Luweero"]
uvl_sex[district_name=="Sembabule", district_name:="Ssembabule"]

# 55 patients in district 121, or "district left blank"
# exclude the 55 patients in district left blank
uvl_sex[district_id==121, .(sum(patients_received), sum(suppressed), month, year), by=.(facility_name, district_name)]
uvl_sex <- uvl_sex[district_id!=121]

# ---------------
# rename sex
uvl_sex[ , .(class(sex)) ]
uvl_sex[sex=='m', sex:='Male']
uvl_sex[sex=='f', sex:='Female']
uvl_sex[sex=='x', sex:='Unknown']

# ---------------
# add date 
uvl_sex[, date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]


# ---------------
# combine the duplicates into single entries

# full data table of all duplicate entries as single entries
uvl_sex <- uvl_sex[ , .(patients_received=sum(patients_received), samples_received=sum(samples_received),  
                        rejected_samples=sum(rejected_samples), 
                        dbs_samples=sum(dbs_samples), total_results=sum(total_results),
                        suppressed=sum(suppressed), valid_results=sum(valid_results)),
                        by=.(facility_id, facility_name, district_id, district_name, dhis2name,
                        sex, date, month, year)]

# simple check for duplicates
unique(duplicated(uvl_sex))

# ---------------
# extensive check for duplicate entries
# create a unique identifier (char) of facilityid_date_sex for single rows in the data table
uvl_sex[sex=="Female", sex1:=1 ]
uvl_sex[sex=="Male", sex1:=2]
uvl_sex[sex=="Unknown", sex1:=3]

uvl_sex[ ,combine:= paste0(facility_id, '_', date, '_', sex1)]
uvl_sex[,length(unique(combine))] 

uvl_sex[duplicated(combine)] # should be an empty data table (no duplicate entries)


# ---------------
# check equality constraints are met 
# commented out checks for equality constraints

# samples received is always >= patients_received
#uvl_sex[samples_received < patients_received, .(samples_received, patients_received), by=.(facility_id, sex, month, year)]

#in one case rejected and dbs samples are both 4, while samples_received is 3; samples received must be >= both
# both samples tested and valid results are 0, so sample size is not very important
#uvl_sex[rejected_samples > samples_received, .(samples_received, patients_received, 
     #  rejected_samples, dbs_samples, total_results, valid_results, suppressed), 
      # by=.(facility_id, sex, month, year)]

uvl_sex[facility_id==3238 & sex=='Male' & month==8 & year==2015, samples_received:=4]

# if samples received is < dbs_samples, change to match dbs_samples (samples received must be greater than or equal to dbs)
# there are 133 instances in which samples_received is < dbs_samples
# if you substract from dbs_samples to match samples_received, valid results is sometimes > samples received
# error is in samples received
#uvl_sex[samples_received < dbs_samples, .(patients_received, samples_received, dbs_samples, valid_results ), by=.(facility_id, sex, month, year)]
#uvl_sex[valid_results > samples_received]
uvl_sex[samples_received < dbs_samples, samples_received:=dbs_samples]

# once samples_received is always >= dbs_samples, valid_results is always =< samples_received

# rejected samples is a subset of samples received
# total results refers to "samples tested" - must be < samples received 
# rejected samples are sometimes tested, sometimes not tested (no clear relationship)

# ---------------
# add a variable for plasma samples and organize the data set intuitively
uvl_sex[ , plasma_samples:=(samples_received - dbs_samples)]
uvl_sex[plasma_samples < 0, plasma_samples:=0]

# change the name of total_results to samples_tested
uvl_sex <- uvl_sex[ , .(samples_received=sum(samples_received),  patients_received=sum(patients_received),
                        rejected_samples=sum(rejected_samples), plasma_samples=sum(plasma_samples),
                        dbs_samples=sum(dbs_samples), samples_tested=sum(total_results),
                        valid_results=sum(valid_results), suppressed=sum(suppressed)),
                       by=.(facility_id, facility_name, district_id, district_name, dhis2name,
                         sex, date, month, year)]

# ---------------
# rename uvl_sex uganda_vl

uganda_vl <- uvl_sex

# ---------------
# check for equality constraints
uganda_vl[patients_received > samples_received]
uganda_vl[rejected_samples > samples_received]
uganda_vl[dbs_samples > samples_received]
uganda_vl[plasma_samples > samples_received]
uganda_vl[valid_results > samples_received]
uganda_vl[suppressed > valid_results]


#---------------
# add date back in
#uganda_vl[, date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]

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
# do the same for dhis2name
uganda_vl[, dhis2name1:=paste(dhis2name, '_')]

level2d <- grep(pattern="\\sII\\s", x=uganda_vl$dhis2name1) 
uganda_vl[level2d, unique(dhis2name)]

level3d <- grep(pattern="III", x=uganda_vl$dhis2name1)
uganda_vl[level3d, unique(dhis2name)]

level4d <- grep(pattern="IV", x=uganda_vl$dhis2name1)
uganda_vl[level4d, unique(dhis2name)]

levelhd <-  grep(pattern="Hospital", x=uganda_vl$dhis2name1)
uganda_vl[levelhd, unique(dhis2name)]

uganda_vl[level2d, level_d:=2]
uganda_vl[level3d, level_d:=3]
uganda_vl[level4d, level_d:=4]
uganda_vl[levelhd, level_d:=5]

# replace level with dhis2 level when level is missing 
uganda_vl[is.na(level) & !is.na(level_d), level:=level_d]

#number of facilities now that do not have level
uganda_vl[is.na(level), .(unique(facility_name))] #126 facilities do not contain level in the name
# replace unknown level with "level 0"
uganda_vl[is.na(level), level:=0]

#---------------
# run a missing data check
uganda_vl[is.na(patients_received)]
uganda_vl[is.na(samples_received)]
uganda_vl[is.na(rejected_samples)]
uganda_vl[is.na(plasma_samples)]
uganda_vl[is.na(dbs_samples)]
uganda_vl[is.na(samples_tested)]
uganda_vl[is.na(suppressed)]
uganda_vl[is.na(valid_results)]
#--------------- 

#--------------- 
# replace the unknow sex values with the facility sex ratio of patients received

# create a total sex ratio for each facility for the entire data set
test1 <- uganda_vl[sex=='Male' | sex=='Female', .(total_pts=sum(patients_received)), by=facility_id]
test2 <- uganda_vl[sex=='Female', .(female_pts=sum(patients_received)), by=facility_id]
test <- merge(test1, test2, by='facility_id', all.x=T)

test[is.na(female_pts), female_pts:=0]
test[ , fem_ratio:=(female_pts/total_pts)]
test[ , male_ratio:=(1 - fem_ratio)]
test[ , c('total_pts', 'female_pts'):=NULL]

# print any facilities that have only unknown sex - there is one, #2846
for (f in unique(uganda_vl$facility_id)) {
if (!f %in% test$facility_id) {
  print(f)
}}

uganda_vl <- uganda_vl[facility_id!=2846]

# merge the male and female mean ratios into the main data set
uganda_vl <- merge(uganda_vl, test, by='facility_id', all.x=T)

# apply the ratios to the unknown sex values


unknowns <- uganda_vl[sex=='Unknown']

fems <- unknowns
fems[ , patients_received:=(patients_received*fem_ratio)]
fems[ , samples_received:=(samples_received*fem_ratio)]
fems[ , rejected_samples:=(rejected_samples*fem_ratio)]
fems[ , plasma_samples:=(plasma_samples*fem_ratio)]
fems[ , dbs_samples:=(dbs_samples*fem_ratio)]
fems[ , samples_tested:=(samples_tested*fem_ratio)]
fems[ , suppressed:=(suppressed*fem_ratio)]
fems[ , valid_results:=(valid_results*fem_ratio)]

males <- unknowns
males[ , patients_received:=(patients_received*male_ratio)]
males[ , samples_received:=(samples_received*male_ratio)]
males[ , rejected_samples:=(rejected_samples*male_ratio)]
males[ , plasma_samples:=(plasma_samples*male_ratio)]
males[ , dbs_samples:=(dbs_samples*male_ratio)]
males[ , samples_tested:=(samples_tested*male_ratio)]
males[ , suppressed:=(suppressed*male_ratio)]
males[ , valid_results:=(valid_results*male_ratio)]

new_peeps <- merge(females, males, by=)




fem_func <- function(x) {
  x=(x*unknowns$fem_ratio)
}

unkVars <- c('facility_id', 'facility_name', 'level', 'dhis2name', 'district_id', 'dist_name', 'sex', 'month', 'year', 'date')

fems <- unknowns[ ,lapply(.SD, fem_func), by=unkVars]

#--------------------------------------------

# final version with only necessary variables in useful order
# rename district_name dist_name for shape file merge 
uganda_vl <- uganda_vl[ ,.( facility_id, facility_name, level, dhis2name, district_id, dist_name=district_name, sex,
                            month, year, date, patients_received, samples_received, rejected_samples,
                            plasma_samples, dbs_samples, samples_tested, suppressed, valid_results)]


#save the final data as an RDS
saveRDS(uganda_vl, file= paste0(dir, "/sex_data.rds"))

# ----------------------------------------------


