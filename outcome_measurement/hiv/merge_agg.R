# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 3/26/2018
#
# Combine the downloaded Uganda VL data w filters month, year, sex
# Merge in the names of the districts and facilities
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

#  stats to check if the sex, tb data disaggregated data downloaded correctly 

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

# ----------------------------------------------
# upload the facilities names, ids and check for disparate values

# reset working directory to main folder
setwd("J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard")
dir <- "J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard"

facilities <- readRDS(paste0(dir,"/facilities.rds"))
str(facilities)

# list unique facility ids
full_data [,facility_id, by=facility_id] # 2042 values
facilities[, facility_id, by=facility_id] # 2012 values

# identify mismatches
full_data$facility_id[!full_data$facility_id %in% facilities$facility_id] 
check <- full_data$facility_id[!full_data$facility_id %in% facilities$facility_id] 
unique(check) # 91 facility ids are in the full data but not the facility names list

facilities$facility_id[!facilities$facility_id %in% full_data$facility_id] # 61 names are on the list but not in the data

# ---------------
# check for missing districts 
# 0 missing district ids
full_data$district_id[!full_data$district_id %in% facilities$district_id] 
facilities$district_id[!facilities$district_id %in% full_data$district_id]

# ---------------

# ----------------------------------------------
# merge the facility and district names into the full data set

uvl_sex <- merge(full_data, 
                 facilities,
                 by='facility_id', all.x=TRUE)

# handle missing names
uvl_sex [is.na(facility_name), name:=paste0('Facility #',facility_id)]

# ----------------------------------------------
# additional to code to identify merge mismatches and downloading errors

# check for mismatch in district IDs
uvl_sex[district_id.x!=district_id.y, .(district_id.x, district_id.y, district_name), by=district_name]

# Gomba is under both 75 and 89; Rakai 134 snd 80

uvl_sex[district_id.x!=district_id.y, .(district_id.x, district_id.y, district_name)]

# Kabira HC III GOVT, Kalisizo Hospital, Nabigasa HC III, Mutukula HC III, Kasensero HC II, Kirumba  HC III
uvl_sex[district_id.x==134, .(unique(facility_name)),by=.(district_name, district_id.x, district_id.y)]  
uvl_sex[district_id.y==80, .(unique(facility_name)),by=.(district_name, district_id.x, district_id.y)]

# Kitwe HC II
uvl_sex[district_id.x==75, .(unique(facility_name)),by=.(district_name, district_id.x, district_id.y)]  
uvl_sex[district_id.y==89, .(unique(facility_name)),by=.(district_name, district_id.x, district_id.y)]



# ---------------

#reset hub, district ids to y values; set x values as process districts and hubs
uvl_sex <- uvl_sex[ ,
                    .(facility_id, process_id=district_id.x, process_hub=hub_id.x, samples_received,
                      patients_received, suppressed,  valid_results, rejected_samples,  dbs_samples,
                      total_results, year, month, sex, district_id=district_id.y, facility_name,     
                      hub_id=hub_id.y, district_name)]

# rename sex
uvl_sex[ , .(class(sex)) ]
uvl_sex[sex=='m', sex:='Male']
uvl_sex[sex=='f', sex:='Female']
uvl_sex[sex=='x', sex:='Unknown']


#save the final data as an RDS
saveRDS(uvl_sex, file= paste0(dir, "/sex_data.rds") )

# ----------------------------------------------


