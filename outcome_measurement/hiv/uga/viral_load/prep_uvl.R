# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 12/24/2018
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
library(ggplot2)
library(stringr) 
library(plyr)
# --------------------

# --------------------
# detect if operating on windows or on the cluster 

root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard')
setwd(dir)

#--------------------------------------------------------
# import the merged data
dt = readRDS(paste0(dir, '/merged/vl_2014_2018.rds'))

#----------------------------
# import the facilities and prep for the merge

facilities = readRDS(paste0(dir,"/facilities/facilities.rds"))
setnames(facilities, c("district_id", "facility_id", "facility_name",
                    "dhis2_name", "hub_id", "district_name"),
                    c("district_id", "facility_id", "facility", "dhis2", "hub_id", "district"))

# ----------------------------------------------

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
udt <- merge(full_data, facilities, by='facility_id', all.x=TRUE)

# 66 facilities are still missing
udt[is.na(facility_name), length(unique(facility_id))]

#-------------------------
# if the facility information is missing, districts do not merge
# full data also contains district ids 
# replace missing district info using full data (to supplement info in meta data)

# create a vector of districts only
districts <- facilities[ ,.(district_name1=unique(district_name)), by=district_id]
districts <- districts[!is.na(district_id)]

# rename the variable to join on 
setnames(udt, 'district_id.x', 'district_id')

#-------------------------------------------------------------------------
# some facilities are associated with multiple district ids
# using the health facility inventory, choose a single district id
udt[facility_id==8335, district_id:=7]
udt[facility_id==8336, district_id:=68]
udt[facility_id==8337, district_id:=97]
udt[facility_id==8338, district_id:=31]
udt[facility_id==8339, district_id:=84]
udt[facility_id==8340, district_id:=101]
udt[facility_id==8341, district_id:=20]

udt[facility_id==8344, district_id:=97]
udt[facility_id==8345, district_id:=97]
udt[facility_id==8346, district_id:=86]
udt[facility_id==8347, district_id:=29]
udt[facility_id==8348, district_id:=31]

udt[facility_id==8350, district_id:=5]
udt[facility_id==8351, district_id:=85]
udt[facility_id==8352, district_id:=15]
udt[facility_id==8353, district_id:=64]
udt[facility_id==8354, district_id:=86]
udt[facility_id==8355, district_id:=30]
udt[facility_id==8356, district_id:=69] # not in inventory, majority in Mukono
udt[facility_id==8357, district_id:=35] # not in inventory, chose Kampala
udt[facility_id==8358, district_id:=39]
udt[facility_id==8359, district_id:=7]

#----------------------------------------------------------------------
# merge the data sets
# use the meta data district ids as primary 
udt <- join(udt, districts, by='district_id', type='left')

# replace missing district names
udt[is.na(district_name), district_name:=district_name1]
udt[ ,district_name1:=NULL]

# eliminate the district id from the full data, use the meta data (district_id.y)
# when the meta data district id is midding, replace with full data district id
udt[is.na(district_id.y), district_id.y:=as.numeric(district_id)]
udt[ ,district_id:=NULL]
setnames(udt, 'district_id.y', 'district_id')

# create a placeholder for missing facility names for 34 facilities
udt[is.na(facility_name), facility_name:=paste0('Facility #',facility_id)]

# drop out the 'facility left blank' facility with no district information
# contains only 55 patients
udt <- udt[district_id!=121]

#-------------------------------------------
# check for facility ids associated with multiple district ids after the change
dups <- udt[ , .(dup=length(unique(district_id))), by=.(facility_id, facility_name)]
dups <- dups[dup>1, .(facility_id, facility_name)]

udt[facility_id %in% dups$facility_id, .(facility_name, district_id)]

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
merge_new_dists(udt)
length(unique(udt$district_name))

# Change spelling of Luwero=Luweero and Sembabule=Ssembabule
udt[district_name=="Luwero", district_name:="Luweero"]
udt[district_name=="Sembabule", district_name:="Ssembabule"]

# ---------------
# rename sex
udt[ , .(class(sex)) ]
udt[sex=='m', sex:='Male']
udt[sex=='f', sex:='Female']
udt[sex=='x', sex:='Unknown']

# ---------------
# add date 
udt[, date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]

# ---------------
# combine the duplicates into single entries

# 35 duplicate entries
udt[ ,combine:=paste0(year,'_', month, '_', facility_id,'_',sex)]
udt[duplicated(combine)]

# full data table of all duplicate entries as single entries
udt <- udt[ , .(patients_received=sum(patients_received),
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
udt[patients_received > samples_received] # should be 0
udt[rejected_samples > samples_received] # should be 0
udt[dbs_samples > samples_received]

# if samples received is < dbs_samples, change to match dbs_samples (samples received must be greater than or equal to dbs)
# udt[samples_received < dbs_samples, samples_received:=dbs_samples]

# once samples_received is always >= dbs_samples, valid_results is always =< samples_received
udt[samples_tested > samples_received] # should be 0
udt[valid_results > samples_received] # should be 0
udt[suppressed > valid_results] # should be 0

# ---------------
# add a variable for plasma samples and organize the data set intuitively
udt[ , plasma_samples:=(samples_received - dbs_samples)]
udt[plasma_samples > samples_received] # should be no values in this category

#---------------
# facility levels 

# separate out the number from facility name to get facility level
# create spaces after II to avoid being included in III
udt[, facility_name1:=paste(facility_name, '_')] # creates a space after the name
udt[, facility_name1:=tolower(facility_name1)]

udt[grepl(pattern="\\sii\\s", facility_name1), level:='HC II'] 
udt[grepl(pattern="\\sii", facility_name1) & grepl(pattern="\\siii", facility_name1), level:='2'] 
udt[grepl(pattern="iii", facility_name1), level:='HC III']
udt[grepl(pattern="\\siv",facility_name1), level:='HC IV']
udt[grepl(pattern="hospital", facility_name1) & !grepl(pattern='military hospital', facility_name1), level:='Hospital']
udt[grepl(pattern="military hospital", facility_name1), level:='Military Hospital']

# TASO unique health centers
udt[grepl(pattern="taso", facility_name1), level:='TASO']

udt[is.na(level), unique(facility_name)]
udt[is.na(level), length(unique(facility_name))] #239 facilities do not contain level in the name

# use dhis2name to get facility levels not contained in the udt dashboard name
udt[!is.na(dhis2_name), dhis2_name1:=paste(dhis2_name, '_')] # creates a space after the name
udt[!is.na(dhis2_name), dhis2_name1:=tolower(dhis2_name1)]

udt[grepl(pattern="\\sii\\s", dhis2_name1), level2:='HC II'] 
udt[grepl(pattern="\\sii", dhis2_name1) & grepl(pattern="\\siii", dhis2_name1), level2:='2'] 
udt[grepl(pattern="iii", dhis2_name1), level2:='HC III']
udt[grepl(pattern="\\siv",dhis2_name1), level2:='HC IV']
udt[grepl(pattern="hospital", dhis2_name1) & !grepl(pattern='military hospital', dhis2_name1), level2:='Hospital']
udt[grepl(pattern="military hospital", dhis2_name1), level2:='Military Hospital']

# replace missing levels from facility names with levels from dhis2names
udt[is.na(level), level:=level2]

# facility name and dhis2name disagree
udt[facility_id==2335, level:='HC II'] #contains Level II in the name but no space after (typo)

# print the number of facility without level
udt[is.na(level), length(unique(facility_name))]

#--------------------------------
# create a variable to identify prisons
udt[grep(pattern="prison", facility_name1), prison:='Yes']
udt[grep(pattern="prison", dhis2_name1), prison:='Yes'] # using dhis2name captures a remand center
udt[is.na(prison), prison:='No']

udt[ , c("facility_name1", "dhis2_name1", 'level2'):=NULL]


#--------------------------------

rdt[grep(pattern="15", date), year:='2015']
rdt[grep(pattern="16", date), year:='2016']
rdt[grep(pattern="17", date), year:='2017']
rdt[grep(pattern="18", date), year:='2018']





udt[grep(pattern="prison", dhis2_name1), prison:='Yes'] # using dhis2name captures a remand center
udt[is.na(prison), prison:='No']




#------------------------------------------
# run a missing data check
udt[ ,lapply(.SD, is.na), .SDcols=10:17]
unique(dt) # should be only false values

#--------------------------
# convert integers to numerics 
vars <- c('facility_id', 'facility_name', 'district_id', 'district_name', 
          'dhis2_name', 'sex', 'date', 'month', 'year', 'level', 'prison')

udt <- udt[ ,lapply(.SD, as.numeric), .SDcols=10:17, by=vars]

#--------------- 
# where one sex is present and the other is missing, assign a 0 value to the missing sex
udt[sex!='Unknown', rows:=.N, by=.(facility_name, date)]
opp_sex <- udt[rows==1]

# create a data table of the opposite sex values for values in the data set
opp_sex[sex=='Female', sex1:='Male']
opp_sex[sex=='Male', sex1:='Female']
opp_sex[ , sex:=sex1][ ,'sex1':=NULL]

opp_sex[ , patients_received:=0]
opp_sex[ , samples_received:=0]
opp_sex[ , rejected_samples:=0]
opp_sex[ , plasma_samples:=0]
opp_sex[ , dbs_samples:=0]
opp_sex[ , samples_tested:=0]
opp_sex[ , suppressed:=0]
opp_sex[ , valid_results:=0]

# rbind in the 0 values for the opposite sex
udt <- rbind(udt, opp_sex)

# check that the merge worked correctly
udt[sex!='Unknown', rows:=.N, by=.(facility_name, date)]
udt[sex!='Unknown', unique(rows)] # there should be two values for each facility
udt[sex!='Unknown', .(y=length(unique(sex))), by=.(facility_name, date)][y!=2] # should be empty - two sexes per facility month
udt[ , rows:=NULL]
#---------------------------------------
# save the final data as an RDS

saveRDS(udt, file= paste0(dir, "/prepped_data/sex_data.rds"))

#------------------------------------------------------------




#-------------------------------------------
# replace unknown sex values


#--------------- 
# replace the unknown sex values with the facility sex ratio of patients received

# create a total sex ratio for each facility for the entire data set
pts1 <- udt[sex=='Male' | sex=='Female', .(total_pts=sum(patients_received)), by=facility_id]
pts2 <- udt[sex=='Female', .(female_pts=sum(patients_received)), by=facility_id]
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
for (f in unique(udt$facility_id)) {
  if (!f %in% pts$facility_id) {
    print(f)
  }}

# assign it to the mean sex ratio for the entire data set
y <- udt[ ,sum(patients_received)]
x <- udt[sex=='Female',sum(patients_received)]
x/y
n2846 <- data.table(facility_id=2846, fem_ratio=0.654702, male_ratio=(1 - 0.654702))
pts <- rbind(pts, n2846)
#---------------  

# merge the male and female mean ratios into the main data set
udt <- merge(udt, pts, by='facility_id', all.x=T)

# create new female patients
fems <- udt[sex=='Unknown']
fems[ , patients_received:=(patients_received*fem_ratio)]
fems[ , samples_received:=(samples_received*fem_ratio)]
fems[ , rejected_samples:=(rejected_samples*fem_ratio)]
fems[ , plasma_samples:=(plasma_samples*fem_ratio)]
fems[ , dbs_samples:=(dbs_samples*fem_ratio)]
fems[ , samples_tested:=(samples_tested*fem_ratio)]
fems[ , valid_results:=(valid_results*fem_ratio)]
fems[ , suppressed:=(suppressed*fem_ratio)]

males <- udt[sex=='Unknown']
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
udt[sex=='Unknown', .(sum(patients_received, na.rm=T), sum(samples_received, na.rm=T), sum(suppressed, na.rm=T), sum(valid_results, na.rm=T))]

# drop out the unknown values 
udt <- udt[sex!='Unknown']
udt[,c("fem_ratio", "male_ratio"):=NULL]
new_pts[ ,.N] + udt[ ,.N] # total number of rows that should be in the data set after the merge

# create unique identifiers and merge
udt[ , check:=0]
new_pts[ , check:=1]
allVars1 <- c("facility_id", "facility_name", "dhis2name", "level",  "prison", "district_id", "district_name", "sex",             
              "month", "year", "date","patients_received", "samples_received", "rejected_samples",  "plasma_samples", 
              "dbs_samples", "samples_tested", "suppressed", "valid_results", "check")

udt <- merge(udt, new_pts, by=allVars1, all=T)
udt[,.N]
udt[ ,check:=NULL]

# sum over the new patients to combine them with other variables
sumVars <- c('patients_received', 'samples_received', 'rejected_samples',
             'plasma_samples', 'dbs_samples', 'samples_tested', 'suppressed', 'valid_results')

udt <- udt[ ,lapply(.SD, sum), by=c('facility_id', 'facility_name', 'dhis2name', 'level', 'prison', 'district_id', 'district_name',
                                    'sex', 'month', 'year', 'date'),.SDcols=sumVars]




#---------------
# run a final check for missing data and violations of equality constraints




# check for missing data
udt[is.na(patients_received)]
udt[is.na(samples_received)]
udt[is.na(rejected_samples)]
udt[is.na(plasma_samples)]
udt[is.na(dbs_samples)]
udt[is.na(samples_tested)]
udt[is.na(suppressed)]
udt[is.na(valid_results)]

# check equality constraints 
udt[samples_received < patients_received]
udt[samples_received < dbs_samples]
udt[samples_received < rejected_samples]
udt[samples_received < samples_tested]
udt[samples_received < valid_results]
udt[samples_tested < valid_results]
udt[valid_results < suppressed]


#-----------------
# plasma samples violates equality constraints because of rounding - re-calculate
udt[ , plasma_samples:=NULL]  
udt[ , plasma_samples:=(samples_received - dbs_samples)]  

# change level to be called platform 
names(udt)[names(udt) == "level"] <- "platform"


#--------------- 

#--------------------------------------------

# final version with only necessary variables in useful order
# rename district_name dist_name for shape file merge 
udt <- udt[ ,.(facility_id, facility_name, dhis2name, platform, prison, district_id, dist_name=district_name, sex,
               month, year, date, patients_received, samples_received, rejected_samples,
               dbs_samples, plasma_samples, samples_tested, valid_results, suppressed)]


#save the final data as an RDS
saveRDS(udt, file= paste0(dir, "/sex_data_unknowns_altered.rds"))

# ----------------------------------------------

# optional - export a CSV of the prepped data set

write.csv(udt, paste0(root, 'Project/Evaluation/GF/outcome_measurement/uga/dt_dashboard/udt_data.csv'))
