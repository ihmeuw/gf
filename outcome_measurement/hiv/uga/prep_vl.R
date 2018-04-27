# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 4/26/2018
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
library(stringr) # to help extract meta data from file names

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
                        rejected_samples=sum(rejected_samples), dbs_samples=sum(dbs_samples),
                        total_results=sum(total_results),
                        suppressed=sum(suppressed), valid_results=sum(valid_results)),
                        by=.(facility_id, facility_name, district_id, district_name, dhis2name,
                        sex, date, month, year)]

# ---------------
# check for duplicates
# create a unique identifier (char) of facilityid_date_sex for single rows in the data table
uvl_sex[sex=="Female", sex1:=1 ]
uvl_sex[sex=="Male", sex1:=2]
uvl_sex[sex=="Unknown", sex1:=3]

uvl_sex[ ,combine:= paste0(facility_id, '_', date, '_', sex1)]
uvl_sex[,length(unique(combine))] 

uvl_sex[duplicated(combine)] # should be an empty data table (no duplicate entries)


# ---------------
# rename uvl_sex uganda_vl

uganda_vl <- uvl_sex

# ----------------------------------------------
# split the patients of unknown sex by sex ratio

# create variables indicating the number of patients by sex
ids <- c('facility_id', 'month', 'year')

males <- uganda_vl[sex=='Male', .(male_pts=sum(patients_received)), by=ids]
uganda_vl <- merge(uganda_vl, males, by=ids, all.x=TRUE)
uganda_vl[sex=='Female', male_pts:=0]
uganda_vl[sex=='Unknown', male_pts:=0]

females <- uganda_vl[sex=='Female', .(female_pts=sum(patients_received)), by=ids]
uganda_vl <- merge(uganda_vl, females, by=ids, all.x=TRUE)
uganda_vl[sex=='Male', female_pts:=0]
uganda_vl[sex=='Unknown', female_pts:=0]

unks <- uganda_vl[sex=='Unknown', .(unk_pts=sum(patients_received)), by=ids]
uganda_vl <- merge(uganda_vl, unks, by=ids, all.x=TRUE)
uganda_vl[sex=='Male', unk_pts:=0]
uganda_vl[sex=='Female', unk_pts:=0]


# ----------------
# monthly patients received by sex 
# check to make sure values are correct

test1 <- uganda_vl[ ,.(male_pts=sum(male_pts), female_pts=sum(female_pts),
                       unk_pts=sum(unk_pts)),by=date]

test1 <- melt(test1, id.vars='date')
test1$variable <- factor(test1$variable, levels=c("male_pts", "female_pts", "unk_pts"),
                         labels=c("Male", "Female", "Unknown"))

ggplot(test1, aes(x=date, y=value, color=variable)) +  
  geom_point() + geom_line() + theme_bw() + 
  labs(title="Monthly patients submitting samples by sex", x="Date", y="Count", color="Sex")

uganda_vl[male_pts!=0, unique(female_pts)]
uganda_vl[female_pts!=0, unique(male_pts)]
uganda_vl[male_pts!=0 & female_pts!=0, unique(unk_pts)]


# ----------------
# calculate the sex ratio for each facility in each month

# calculate the ratop of female patients to patients with known sex
known <- uganda_vl[,.(known_pts=sum(male_pts+female_pts)), by=ids]
uganda_vl <- merge(uganda_vl, known, by=ids, all.x=TRUE)

fems <- uganda_vl[ , .(fems=sum(female_pts)), by=ids]
uganda_vl <- merge(uganda_vl, fems, by=ids, all.x=TRUE)

# apply the females ratio to the unknowns
uganda_vl[sex=='Unknown', new_fems:=(unk_pts*(fems/known_pts)) ]
uganda_vl[is.na(new_fems), new_fems:=0]

# males are the remainder
uganda_vl[sex=='Unknown', new_gents:=(unk_pts-new_fems) ]
uganda_vl[is.na(new_gents), new_gents:=0]

# check that the unknown patients make sense
test2 <- uganda_vl[sex=="Unknown", .(all_unk=sum(patients_received),
                                    new_fems=sum(new_fems), new_gents=sum(new_gents)),               
                                    by=.(month, year)]

test2 <- melt(test2, id.vars=c('month', 'year'))

test2$variable <- factor(test2$variable, levels=c("all_unk", "new_fems", "new_gents"),
                         labels=c("Unknown Sex - All", "New Females", "New Males"))


# why are there a bunch of missing values
ggplot(test2, aes(x=factor(month), y=value, color=variable, group=variable)) +  
  geom_point() + geom_line() + theme_bw() + 
  facet_wrap(~year) +
  labs(title="Monthly patients submitting samples by sex", x="Date", y="Count", color="Sex")


#---------------
# add the unknown sex projected females/males into unknowns



#---------------

#save the final data as an RDS
saveRDS(uvl_sex, file= paste0(dir, "/sex_data.rds") )

# ----------------------------------------------


