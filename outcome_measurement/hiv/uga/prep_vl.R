# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 5/4/2018
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

# ------------------------------------
# reassign the unknown sex values to an annual sex ratio per facility
 
# subset out unknowns into their own data set
unknowns <- uganda_vl[sex=='Unknown']
uganda_vl <- uganda_vl[sex!='Unknown']

# ---------------
# total patients (annual)

# create annual total patients, samples etc. per facility
Vars <- c("samples_received", "patients_received", "rejected_samples","dbs_samples", "plasma_samples",
          "samples_tested",  "valid_results", "suppressed")

uvl <- uganda_vl[,lapply(.SD, sum), by=c('facility_id', 'year'), .SDcols=Vars]

# rename the variables to indicate annual totals
Vars1 <- c("samples_received1", "patients_received1", "rejected_samples1","dbs_samples1", "plasma_samples1",
          "samples_tested1",  "valid_results1", "suppressed1")
setnames(uvl, Vars, Vars1)

# ---------------
# total female patients (annual)

# create annual total patients, samples etc. per facility
Vars <- c("patients_received", "samples_received", "rejected_samples","dbs_samples", "plasma_samples",
          "samples_tested",  "valid_results", "suppressed")

uvl_f <- uganda_vl[sex=='Female']
uvl_f <- uvl_f[,lapply(.SD, sum), by=c('facility_id','year'), .SDcols=Vars]

# rename the variables to indicate annual totals
VarsF <- c("patients_received_f", "samples_received_f", "rejected_samples_f","dbs_samples_f", "plasma_samples_f",
           "samples_tested_f",  "valid_results_f", "suppressed_f")

setnames(uvl_f, Vars, VarsF)

#------------------------
# facilities with males and no females 

males_only <- NULL
for (f in unique(uvl$facility_id)) {
  if (!f %in% unique(uvl_f$facility_id)) {
    a <- uganda_vl[facility_id==f, .(facility_id, year)]
    males_only <- rbind(males_only, a)
  }}

males_only$facility_id %in% uvl$facility_id
males_only$facility_id %in% uvl_f$facility_id # only males

# create annual rows for males only, with female values set to 0
uvl_f <- merge(uvl_f, males_only, by=c('facility_id', 'year'), all=TRUE)

# for the male entries, set all the values to 0 (0 females)
uvl_f[is.na(samples_received_f), samples_received_f:=0]
uvl_f[is.na(patients_received_f), patients_received_f:=0]
uvl_f[is.na(rejected_samples_f), rejected_samples_f:=0]
uvl_f[is.na(plasma_samples_f), plasma_samples_f:=0]
uvl_f[is.na(dbs_samples_f), dbs_samples_f:=0]
uvl_f[is.na(samples_tested_f),samples_tested_f:=0]
uvl_f[is.na(valid_results_f), valid_results_f:=0]
uvl_f[is.na(suppressed_f), suppressed_f:=0]


# merge female ratios into main data set of totals
uvl_f <- merge(uvl, uvl_f, by=c('facility_id', 'year'))

# no missing values
uvl_f[is.na(patients_received_f)]
uvl_f[is.na(samples_received_f)]
uvl_f[is.na(plasma_samples_f)]
uvl_f[is.na(dbs_samples_f)]
uvl_f[is.na(samples_tested_f)]
uvl_f[is.na(valid_results_f)]
uvl_f[is.na(suppressed_f)]

uvl_f[is.na(patients_received1)]
uvl_f[is.na(samples_received1)]
uvl_f[is.na(plasma_samples1)]
uvl_f[is.na(dbs_samples1)]
uvl_f[is.na(samples_tested1)]
uvl_f[is.na(valid_results1)]
uvl_f[is.na(suppressed1)]


# ---------------
# calculate the % female for every annual total
 uvl_f <- uvl_f[,.(pt_ratio=(patients_received_f/patients_received1), sam_ratio=(samples_received_f/samples_received1),
  rej_ratio=(rejected_samples_f/rejected_samples1), dbs_ratio=(dbs_samples_f/dbs_samples1),
  plasma_ratio=(plasma_samples_f/plasma_samples1), tested_ratio=(samples_tested_f/samples_tested1),
  valid_ratio=(valid_results_f/valid_results1), sup_ratio=(suppressed_f/suppressed1)),
  by=.(facility_id, year)]


# 0/0 is infinity 
# replacing NaN ratios (0/0) with 0
uvl_f[is.na(rej_ratio), rej_ratio:=0]
uvl_f[is.na(dbs_ratio), dbs_ratio:=0]
uvl_f[is.na(plasma_ratio), plasma_ratio:=0]
uvl_f[is.na(tested_ratio), tested_ratio:=0]
uvl_f[is.na(valid_ratio), valid_ratio:=0]
uvl_f[is.na(sup_ratio), sup_ratio:=0]

# check for missing values
uvl_f[is.na(rej_ratio)]
uvl_f[is.na(dbs_ratio)]
uvl_f[is.na(plasma_ratio)]
uvl_f[is.na(tested_ratio)]
uvl_f[is.na(valid_ratio)]
uvl_f[is.na(sup_ratio)]

# check that there are ratios for all unknown sex facilities
x <- unique(unknowns$facility_id)
uvl_f[ ,.(unique(x %in% uvl_f$facility_id))]

# facility with only uunknowns
# one facility, 2846, has only unknown sex values
for (f in x) {
  if (!f %in% unique(uvl_f$facility_id)) {
  print(f)
  }}

# eliminate this facility
unknowns <- unknowns[facility_id!=2846]

# check to make sure all of the ratio years are in unknown 
uvl_f[,combine:=(paste0(facility_id, '_', year))]
unknowns[,combine:=(paste0(facility_id, '_', year))]

x <- unique(unknowns$combine)

for (f in uvl_f$combine) {
if (!f %in% x) {
  uvl_f[combine==f, drop:=1]
} else { uvl_f[combine==f, drop:=0] }}

uvl_f <- uvl_f[drop==0]

# ---------------
# create a new set of female patients by applying the annual sex ratio to each month in each facility
new_females <- merge(unknowns, uvl_f, by=c('facility_id', 'year'), all.x=TRUE)
 
new_females <- new_females[ ,.(patients_received=(patients_received*pt_ratio), 
                samples_received=(samples_received*sam_ratio),
                rejected_samples=(rejected_samples*rej_ratio), plasma_samples=(plasma_samples*plasma_ratio),
                dbs_samples=(dbs_samples*dbs_ratio), samples_tested=(samples_tested*tested_ratio),
                suppressed=(suppressed*sup_ratio), valid_results=(valid_results*valid_ratio)),
                by=.(facility_id, facility_name, district_id, district_name,
                dhis2name, date, month, year)]
new_females[ , sex:='Female']

# ---------------






# ---------------
# create a new set of male patients by applying the inverse of the annual female ratio

new_males <- merge(unknowns, uvl_f, by=c('facility_id', 'year'))

new_males <- new_males[ ,.(patients_received, patients_received1=(patients_received*pt_ratio), 
                           samples_received, samples_received1=(samples_received*sam_ratio),
                          rejected_samples, rejected_samples1=(rejected_samples*rej_ratio), 
                          plasma_samples, plasma_samples1=(plasma_samples*plasma_ratio),
                           dbs_samples, dbs_samples1=(dbs_samples*dbs_ratio), 
                          samples_tested, samples_tested1=(samples_tested*tested_ratio),
                           suppressed, suppressed1=(suppressed*sup_ratio), 
                          valid_results, valid_results1=(valid_results*valid_ratio)),
                        by=.(facility_id, facility_name, district_id, district_name,
                             dhis2name, date, month, year)]

#calculate the annual male ratio based on the female mean
new_males[, patients_received:=(patients_received - patients_received1)]
new_males[, samples_received:=(samples_received - samples_received1)]
new_males[, rejected_samples:=(rejected_samples - rejected_samples1)]
new_males[, plasma_samples:=(plasma_samples - plasma_samples1)]
new_males[, dbs_samples:=(dbs_samples - dbs_samples1)]
new_males[, samples_tested:=(samples_tested - samples_tested1)]
new_males[, valid_results:=(valid_results - valid_results1)]
new_males[, suppressed:=(suppressed - suppressed1)]


new_males <- new_males[ ,.(patients_received,  samples_received, rejected_samples,
                           plasma_samples, dbs_samples, samples_tested,
                            suppressed, valid_results),
                        by=.(facility_id, facility_name, district_id, district_name,
                             dhis2name, date, month, year)]

new_males[ , sex:='Male']

new_peeps <- merge(new_females, new_males, by=c('facility_id', 'facility_name', 'district_id', 'district_name',
                                                'dhis2name', 'sex', 'date', 'month', 'year',
                                                'patients_received', 'samples_received', 'rejected_samples',
                                                'plasma_samples', 'dbs_samples', 'samples_tested', 
                                                'valid_results', 'suppressed'), all=TRUE)

# artificially introduce equality constraints
new_peeps[patients_received > samples_received, patients_received:=samples_received]
new_peeps[rejected_samples > samples_received, rejected_samples:=samples_received]
new_peeps[dbs_samples > samples_received, dbs_samples:=samples_received]
new_peeps[plasma_samples > samples_received, plasma_samples:=samples_received]
new_peeps[valid_results > samples_received, valid_results:=samples_received]
new_peeps[suppressed > valid_results, suppressed:=valid_results]

# add the new people into the data set
uganda_vl <- merge(uganda_vl, new_peeps, by=c('facility_id', 'facility_name', 'district_id', 'district_name',
                                 'dhis2name', 'sex', 'date', 'month', 'year',
                                 'patients_received', 'samples_received', 'rejected_samples',
                                 'plasma_samples', 'dbs_samples', 'samples_tested', 
                                 'valid_results', 'suppressed'), all=TRUE)

# ---------------
# collapse on sex to add the new females and males into monthly facility totals

#only the variables needed for analysis
uganda_vl <- uganda_vl[ ,.( facility_id, facility_name, dhis2name, district_id, district_name,
                            month, year, sex, samples_received, patients_received, rejected_samples,
                            plasma_samples, dbs_samples, samples_tested, suppressed, valid_results)]

Vars <- c("samples_received", "patients_received", "rejected_samples","dbs_samples", "plasma_samples",
          "samples_tested",  "valid_results", "suppressed")

uganda_vl <- uganda_vl[,lapply(.SD, sum), by=c('facility_id', 'facility_name', 'dhis2name',
                                       'district_id', 'district_name', 'sex', 'month','year'), .SDcols=Vars]


#---------------
# add date back in
uganda_vl[, date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]

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
uganda_vl[is.na(level), length(unique(facility_name))] #205 facilities do not contain level in the name


#---------------
# check equality constraints
# should I alter to reflect equality constraints?

uganda_vl[plasma_samples<0] 
uganda_vl[dbs_samples > samples_received]
uganda_vl[rejected_samples > samples_received]
uganda_vl[plasma_samples > samples_received]


#---------------

# final version with only necessary variables in useful order
uganda_vl <- uganda_vl[ ,.( facility_id, facility_name, level, dhis2name, district_id, district_name, sex,
                            month, year, date, patients_received, samples_received, rejected_samples,
                            plasma_samples, dbs_samples, samples_tested, suppressed, valid_results)]

#---------------

#save the final data as an RDS
saveRDS(uganda_vl, file= paste0(dir, "/sex_data.rds"))

# ----------------------------------------------


