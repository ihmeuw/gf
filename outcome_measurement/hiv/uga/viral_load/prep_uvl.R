# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 7/30/2018
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
library(stringr) 
library(plyr)
# --------------------

# --------------------
# detect if operating on windows or on the cluster 

root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# ----------------------------------------------
# set files and directories for the uganda viral load data

# set the working directory to loop over the downloaded files
setwd(paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape/sex/'))

# list existing files
files <- list.files('./', recursive=TRUE)
length(files)

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
  
  # grab the facility and district ids
  setnames(current_data, '_id', 'id')
  
  district_id <- unlist(current_data$id[[1]])
  district_id <- data.table(district_id)

  hub_id <- unlist(current_data$id[[2]])
  hub_id <- data.table(hub_id)
  
  facility_id <- unlist(current_data$id[[3]])
  facility_id <- data.table(facility_id)
  
  current_data[ ,id:=NULL]
  
  current_data <- cbind(current_data, district_id)
  current_data <- cbind(current_data, hub_id)
  current_data <- cbind(current_data, facility_id)
  
  
  # skip to next if there was no data for this combination
  if (length(current_data)==0) next
  

  #to check the position of variables: 
  # outFile = paste0(dir, '/facilities_suppression_', m,'_','20', y,'_',s,'_', t,'_','.rds')
  # positions: year = 4; month = 3; sex=5, tb_status=6
  
  # extract meta data from file name
  meta_data = strsplit(f, '_')[[1]]
  current_data[, year:=as.numeric(substr(meta_data[4],1,4))]
  current_data[, month:=as.numeric(substr(meta_data[3],1,2))]
  current_data[, sex:=(meta_data[5])]
  current_data[ , sex:=(substr(current_data$sex, 1, 1))] # to remove .rds
  
  # add if tb status is included 
  # current_data[, tb:=gsub('tb', '', meta_data[6])]
  # current_data[, tb:=gsub('.rds', '', tb)]

  # append to the full data 
  if(i==1) full_data = current_data
  if(i>1) full_data = rbind(full_data, current_data)
  i = i+1
}

# view the final product of full_data
str(full_data)

# ----------------------------------------------
# reset working directory to main folder
setwd(paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard'))
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard')

facilities <- readRDS(paste0(dir,"/facilities/facilities.rds"))
str(facilities)

# ----------------------------------------------
#  stats to check if the sex disaggregated data downloaded correctly 

# run some stats to check that the data downloaded correctly
full_data[, sum(samples_received), by=year]
full_data[month==1, sum(samples_received), by=year] # should be no samples in jan 2014

# ----------------------------------------------
# drop out the current month - change to reflect current month to accomodate data lags

full_data[year==2018, range(month)]
full_data <- full_data[!(year==2018 & month==7)]

# ----------------------------------------------
# upload the facilities names, ids and check for disparate values

# print a list of the facility ids in full_data that are not on the list of facility names
# 66 facilities, 1234 patients in those facilities

full_data[!full_data$facility_id %in% facilities$facility_id, .(length(unique(facility_id)), 
                                                                sum(patients_received))]

# ----------------------------------------------
# merge the facility and district names into the full data set
# identify merge mismatches and downloading errors

#------------------------
# merge in the facility names

# eliminate hub id and use hub id in meta data 
full_data[ ,hub_id:=NULL]

# merge on facility id
uvl <- merge(full_data, facilities, by='facility_id', all.x=TRUE)

# 66 facilities are still missing
uvl[is.na(facility_name), length(unique(facility_id))]

#-------------------------
# if the facility information is missing, districts do not merge
# full data also contains district ids 
# replace missing district info using full data (to supplement info in meta data)

# create a vector of districts only
districts <- facilities[ ,.(district_name1=unique(district_name)), by=district_id]
districts <- districts[!is.na(district_id)]

# rename the variable to join on 
setnames(uvl, 'district_id.x', 'district_id')

#-------------------------------------------------------------------------
# some facilities are associated with multiple district ids
# using the health facility inventory, choose a single district id
uvl[facility_id==8335, district_id:=7]
uvl[facility_id==8336, district_id:=68]
uvl[facility_id==8337, district_id:=97]
uvl[facility_id==8338, district_id:=31]
uvl[facility_id==8339, district_id:=84]
uvl[facility_id==8340, district_id:=101]
uvl[facility_id==8341, district_id:=20]

uvl[facility_id==8344, district_id:=97]
uvl[facility_id==8345, district_id:=97]
uvl[facility_id==8346, district_id:=86]
uvl[facility_id==8347, district_id:=29]
uvl[facility_id==8348, district_id:=31]

uvl[facility_id==8350, district_id:=5]
uvl[facility_id==8351, district_id:=85]
uvl[facility_id==8352, district_id:=15]
uvl[facility_id==8353, district_id:=64]
uvl[facility_id==8354, district_id:=86]
uvl[facility_id==8355, district_id:=30]
uvl[facility_id==8356, district_id:=69] # not in inventory, majority in Mukono
uvl[facility_id==8357, district_id:=35] # not in inventory, chose Kampala
uvl[facility_id==8358, district_id:=39]
uvl[facility_id==8359, district_id:=7]

#----------------------------------------------------------------------
# merge the data sets
# use the meta data district ids as primary 
uvl <- join(uvl, districts, by='district_id', type='left')

# replace missing district names
uvl[is.na(district_name), district_name:=district_name1]
uvl[ ,district_name1:=NULL]

# eliminate the district id from the full data, use the meta data (district_id.y)
# when the meta data district id is midding, replace with full data district id
uvl[is.na(district_id.y), district_id.y:=as.numeric(district_id)]
uvl[ ,district_id:=NULL]
setnames(uvl, 'district_id.y', 'district_id')

# create a placeholder for missing facility names for 34 facilities
uvl[is.na(facility_name), facility_name:=paste0('Facility #',facility_id)]

# drop out the 'facility left blank' facility with no district information
# contains only 55 patients
uvl <- uvl[district_id!=121]

#-------------------------------------------
# check for facility ids associated with multiple district ids after the change
dups <- uvl[ , .(dup=length(unique(district_id))), by=.(facility_id, facility_name)]
dups <- dups[dup>1, .(facility_id, facility_name)]

uvl[facility_id %in% dups$facility_id, .(facility_name, district_id)]

#-------------------------------------------
# change district names from new 2016/17 districts to previous districts
# allows for map making (shape files not yet updated)

#create a function that merges new districts into previous districts to match shape file
# before 2016, there were 112 districts
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
merge_new_dists(uvl)
length(unique(uvl$district_name))

# Change spelling of Luwero=Luweero and Sembabule=Ssembabule
uvl[district_name=="Luwero", district_name:="Luweero"]
uvl[district_name=="Sembabule", district_name:="Ssembabule"]

# ---------------
# rename sex
uvl[ , .(class(sex)) ]
uvl[sex=='m', sex:='Male']
uvl[sex=='f', sex:='Female']
uvl[sex=='x', sex:='Unknown']

# ---------------
# add date 
uvl[, date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]

# ---------------
# combine the duplicates into single entries

# 35 duplicate entries
uvl[ ,combine:=paste0(year,'_', month, '_', facility_id,'_',sex)]
uvl[duplicated(combine)]

# full data table of all duplicate entries as single entries
uvl <- uvl[ , .(patients_received=sum(patients_received),
                samples_received=sum(samples_received),  
                rejected_samples=sum(rejected_samples), 
                dbs_samples=sum(dbs_samples), samples_tested=sum(total_results),
                suppressed=sum(suppressed), valid_results=sum(valid_results)),
                by=.(facility_id, facility_name, district_id, district_name, dhis2_name,
                sex, date, month, year)]

# ---------------
# check equality constraints are met 
# commented out checks for equality constraints

# check for equality constraints
uvl[patients_received > samples_received] # should be 0
uvl[rejected_samples > samples_received] # should be 0
uvl[dbs_samples > samples_received]

# if samples received is < dbs_samples, change to match dbs_samples (samples received must be greater than or equal to dbs)
# uvl[samples_received < dbs_samples, samples_received:=dbs_samples]

# once samples_received is always >= dbs_samples, valid_results is always =< samples_received
uvl[samples_tested > samples_received] # should be 0
uvl[valid_results > samples_received] # should be 0
uvl[suppressed > valid_results] # should be 0

# ---------------
# add a variable for plasma samples and organize the data set intuitively
uvl[ , plasma_samples:=(samples_received - dbs_samples)]
uvl[plasma_samples > samples_received] # should be no values in this category

#---------------
# facility levels 

# separate out the number from facility name to get facility level
# create spaces after II to avoid being included in III
uvl[, facility_name1:=paste(facility_name, '_')] # creates a space after the name
uvl[, facility_name1:=tolower(facility_name1)]

uvl[grepl(pattern="\\sii\\s", facility_name1), level:='HC II'] 
uvl[grepl(pattern="\\sii", facility_name1) & grepl(pattern="\\siii", facility_name1), level:='2'] 
uvl[grepl(pattern="iii", facility_name1), level:='HC III']
uvl[grepl(pattern="\\siv",facility_name1), level:='HC IV']
uvl[grepl(pattern="hospital", facility_name1) & !grepl(pattern='military hospital', facility_name1), level:='Hospital']
uvl[grepl(pattern="military hospital", facility_name1), level:='Military Hospital']

# TASO unique health centers
uvl[grepl(pattern="taso", facility_name1), level:='TASO']

uvl[is.na(level), unique(facility_name)]
uvl[is.na(level), length(unique(facility_name))] #239 facilities do not contain level in the name

# use dhis2name to get facility levels not contained in the uvl dashboard name
uvl[!is.na(dhis2_name), dhis2_name1:=paste(dhis2_name, '_')] # creates a space after the name
uvl[!is.na(dhis2_name), dhis2_name1:=tolower(dhis2_name1)]

uvl[grepl(pattern="\\sii\\s", dhis2_name1), level2:='HC II'] 
uvl[grepl(pattern="\\sii", dhis2_name1) & grepl(pattern="\\siii", dhis2_name1), level2:='2'] 
uvl[grepl(pattern="iii", dhis2_name1), level2:='HC III']
uvl[grepl(pattern="\\siv",dhis2_name1), level2:='HC IV']
uvl[grepl(pattern="hospital", dhis2_name1) & !grepl(pattern='military hospital', dhis2_name1), level2:='Hospital']
uvl[grepl(pattern="military hospital", dhis2_name1), level2:='Military Hospital']

# replace missing levels from facility names with levels from dhis2names
uvl[is.na(level), level:=level2]

# facility name and dhis2name disagree
uvl[facility_id==2335, level:='HC II'] #contains Level II in the name but no space after (typo)

# print the number of facility without level
uvl[is.na(level), length(unique(facility_name))]

#--------------------------------
# create a variable to identify prisons
uvl[grep(pattern="prison", facility_name1), prison:='Yes']
uvl[grep(pattern="prison", dhis2_name1), prison:='Yes'] # using dhis2name captures a remand center
uvl[prison!='Yes', prison:='No']

uvl[ , c("facility_name1", "dhis2_name1"):=NULL]

#---------------
# run a missing data check
uvl[is.na(patients_received)]
uvl[is.na(samples_received)]
uvl[is.na(rejected_samples)]
uvl[is.na(plasma_samples)]
uvl[is.na(dbs_samples)]
uvl[is.na(samples_tested)]
uvl[is.na(suppressed)]
uvl[is.na(valid_results)]
#--------------- 

#--------------- 
# replace the unknown sex values with the facility sex ratio of patients received

# create a total sex ratio for each facility for the entire data set
pts1 <- uvl[sex=='Male' | sex=='Female', .(total_pts=sum(patients_received)), by=facility_id]
pts2 <- uvl[sex=='Female', .(female_pts=sum(patients_received)), by=facility_id]
pts2[ ,fem_only:=1]

# create a mean sex ratio per facility
pts <- merge(pts1, pts2, by='facility_id', all.x=T)
pts[is.na(fem_only), fem_only:=0]
pts[is.na(female_pts), female_pts:=0]
pts[ , fem_ratio:=(female_pts/total_pts)]
pts[ , male_ratio:=(1 - fem_ratio)]

pts[ , c('total_pts', 'female_pts', 'fem_only'):=NULL]

#--------------- 
# print any facilities that have only unknown sex - there is one, #2846
for (f in unique(uvl$facility_id)) {
if (!f %in% pts$facility_id) {
  print(f)
}}

# assign it to the mean sex ratio for the entire data set
y <- uvl[ ,sum(patients_received)]
x <- uvl[sex=='Female',sum(patients_received)]
x/y
n2846 <- data.table(facility_id=2846, fem_ratio=0.654702, male_ratio=(1 - 0.654702))
pts <- rbind(pts, n2846)
#---------------  

# merge the male and female mean ratios into the main data set
uvl <- merge(uvl, pts, by='facility_id', all.x=T)

# create new female patients
fems <- uvl[sex=='Unknown']
fems[ , patients_received:=(patients_received*fem_ratio)]
fems[ , samples_received:=(samples_received*fem_ratio)]
fems[ , rejected_samples:=(rejected_samples*fem_ratio)]
fems[ , plasma_samples:=(plasma_samples*fem_ratio)]
fems[ , dbs_samples:=(dbs_samples*fem_ratio)]
fems[ , samples_tested:=(samples_tested*fem_ratio)]
fems[ , valid_results:=(valid_results*fem_ratio)]
fems[ , suppressed:=(suppressed*fem_ratio)]

males <- uvl[sex=='Unknown']
males[ , patients_received:=(patients_received*male_ratio)]
males[ , samples_received:=(samples_received*male_ratio)]
males[ , rejected_samples:=(rejected_samples*male_ratio)]
males[ , plasma_samples:=(plasma_samples*male_ratio)]
males[ , dbs_samples:=(dbs_samples*male_ratio)]
males[ , samples_tested:=(samples_tested*male_ratio)]
males[ , valid_results:=(valid_results*male_ratio)]
males[ , suppressed:=(suppressed*male_ratio)]

# drop unnecessary variables and label sex
males[,c("fem_ratio", "male_ratio"):=NULL]
fems[,c("fem_ratio", "male_ratio"):=NULL]
males[ , sex:='Male']
fems[ , sex:='Female']

# create a set of new patients and check if it matches the number of unknown patients
allVars <- c("facility_id", "facility_name", "dhis2name", "level",  "prison", "district_id", "district_name", "sex",             
            "month", "year", "date","patients_received", "samples_received", "rejected_samples",  "plasma_samples", 
            "dbs_samples", "samples_tested", "suppressed", "valid_results")

new_pts <- merge(fems, males, by=allVars, all.x=T, all.y=T)

# these should have the same output - unknown is equal to the new sex values
new_pts[ , .(sum(patients_received, na.rm=T), sum(samples_received, na.rm=T), sum(suppressed, na.rm=T), sum(valid_results, na.rm=T))]
uvl[sex=='Unknown', .(sum(patients_received, na.rm=T), sum(samples_received, na.rm=T), sum(suppressed, na.rm=T), sum(valid_results, na.rm=T))]

# drop out the unknown values 
uvl <- uvl[sex!='Unknown']
uvl[,c("fem_ratio", "male_ratio"):=NULL]
new_pts[ ,.N] + uvl[ ,.N] # total number of rows that should be in the data set after the merge

# create unique identifiers and merge
uvl[ , check:=0]
new_pts[ , check:=1]
allVars1 <- c("facility_id", "facility_name", "dhis2name", "level",  "prison", "district_id", "district_name", "sex",             
             "month", "year", "date","patients_received", "samples_received", "rejected_samples",  "plasma_samples", 
             "dbs_samples", "samples_tested", "suppressed", "valid_results", "check")

uvl <- merge(uvl, new_pts, by=allVars1, all=T)
uvl[,.N]
uvl[ ,check:=NULL]

# sum over the new patients to combine them with other variables
sumVars <- c('patients_received', 'samples_received', 'rejected_samples',
    'plasma_samples', 'dbs_samples', 'samples_tested', 'suppressed', 'valid_results')

uvl <- uvl[ ,lapply(.SD, sum), by=c('facility_id', 'facility_name', 'dhis2name', 'level', 'prison', 'district_id', 'district_name',
        'sex', 'month', 'year', 'date'),.SDcols=sumVars]




#---------------
# run a final check for missing data and violations of equality constraints

# check for missing data
uvl[is.na(patients_received)]
uvl[is.na(samples_received)]
uvl[is.na(rejected_samples)]
uvl[is.na(plasma_samples)]
uvl[is.na(dbs_samples)]
uvl[is.na(samples_tested)]
uvl[is.na(suppressed)]
uvl[is.na(valid_results)]

# check equality constraints 
  uvl[samples_received < patients_received]
  uvl[samples_received < dbs_samples]
  uvl[samples_received < rejected_samples]
  uvl[samples_received < samples_tested]
  uvl[samples_received < valid_results]
  uvl[samples_tested < valid_results]
  uvl[valid_results < suppressed]
  
  
#-----------------
# plasma samples violates equality constraints because of rounding - re-calculate
uvl[ , plasma_samples:=NULL]  
  uvl[ , plasma_samples:=(samples_received - dbs_samples)]  

# change level to be called platform 
names(uvl)[names(uvl) == "level"] <- "platform"
  
  
#--------------- 

#--------------------------------------------

# final version with only necessary variables in useful order
# rename district_name dist_name for shape file merge 
uvl <- uvl[ ,.(facility_id, facility_name, dhis2name, platform, prison, district_id, dist_name=district_name, sex,
                            month, year, date, patients_received, samples_received, rejected_samples,
                            dbs_samples, plasma_samples, samples_tested, valid_results, suppressed)]


#save the final data as an RDS
saveRDS(uvl, file= paste0(dir, "/sex_data.rds"))

# ----------------------------------------------

# optional - export a CSV of the prepped data set

write.csv(uvl, paste0(root, 'Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/uvl_data.csv'))


# ----------------------------------------------
# archived code
# this code is used to identify facility ids in the full data assoc. w multiple district ids
# this code is not nexcessary after using meta data district ids (no repeats)

# # identify facility ids that are associated with multiple district ids
# 
# # identify acility ids that are associated with multiple district ids
# # 29 facilities are associated with two district ids (always two)
# # check for duplicates after the change
# dups <- uvl[ , .(dup=length(unique(district_id))), by=.(facility_id, facility_name)]
# dups <- dups[dup>1, .(facility_id, facility_name)]
# 
# # 2 facilities contain Rakai in the name
# uvl[facility_id==228 | facility_id==175, district_id:=80]
# 
# # looked up in the health facility inventory; majority are Kyotera (134) and Rakai(80)
# uvl[facility_id==255, district_id:=80]
# uvl[facility_id==502, district_id:=80]
# uvl[facility_id==1351, district_id:=80]
# uvl[facility_id==1526, district_id:=80]
# uvl[facility_id==1575 , district_id:=80]
# uvl[facility_id==2058, district_id:=89]
# 
# uvl[facility_id== 8335, district_id:=7]
# uvl[facility_id==8350, district_id:=5]
# uvl[facility_id== 8359, district_id:=7]
# uvl[facility_id== 8352, district_id:=15]
# uvl[facility_id== 8341, district_id:=20]
# uvl[facility_id== 8348, district_id:=31]
# 
# uvl[facility_id==8347 , district_id:=29]
# uvl[facility_id==8355 , district_id:=30]
# uvl[facility_id==8338, district_id:=31]
# uvl[facility_id==8358, district_id:=39]
# uvl[facility_id==8340, district_id:=101]
# uvl[facility_id==8357, district_id:=35] # not in inventory, chose Kampala
# uvl[facility_id==8351, district_id:=85]
# uvl[facility_id==8345, district_id:=97]
# 
# uvl[facility_id==8353, district_id:=64]
# uvl[facility_id==8354, district_id:=86]
# uvl[facility_id==8344, district_id:=97]
# uvl[facility_id==8336, district_id:=68]
# uvl[facility_id==8356, district_id:=69] # not in inventory, majority in Mukono
# uvl[facility_id==8346, district_id:=86]
# uvl[facility_id==8339, district_id:=84]
# uvl[facility_id==8337, district_id:=97]
# 
# # re-run to check that there are no duplicate entries
# dups <- uvl[ , .(dup=length(unique(district_id))), by=.(facility_id, facility_name)]
# dups <- dups[dup>1, .(facility_id, facility_name, dup)]

#------------------------------------------

