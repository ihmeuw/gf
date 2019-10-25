# ----------------------------------------------
# David, Phillips, Caitlin O'Brien-Carelli
#
# 10/1/2019
# Source from webscrape_uvl.R
# To extract a list of districts and facilities 
# Facilities and districts are stored in separate urls
# From Uganda Viral Load Dashboard: https://vldash.cphluganda.org/
# ----------------------------------------------

# ----------------------------------------------
# Load/prep data
            
# store url
url = 'https://vldash.cphluganda.org/other_data'
              
# load
data = fromJSON(url)

# set the output directory
outDir_facilities = paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/meta_data/')
  
#-----------------------------
# extract the facilities and their associated meta data 

# create a data table that contains only facilities information
facilities_1 = data$facilities

 # create a function that selects the facility names and ids from the list
  select_names = function(i) {
   y = unlist(facilities_1[i])
   y = y[c(2,3,4,5,7)]
   names(y) = c("facility_id", "facility", "dhis2_name", "hub_id", "district_id")
   return(y)
  }
  
  # use lapply to run the select_names function and bind into a data table
  facilities = lapply(1:length(data$facilities), function(x) select_names(x))
  facilities = do.call('rbind', facilities)
  facilities = data.table(facilities)
  
  #destring id #s
  facilities[ , facility_id:=as.numeric(facility_id)]
  facilities[ , hub_id:=as.numeric(hub_id)]
  facilities[ , district_id:=as.numeric(district_id)]
  
  # 147 facilities are missing a district id
  facilities[is.na(district_id), length(unique(facility_id))]
  
#-----------------------------
 # extract the districts and their associated meta data 

# create a data table that contains only district information
districts_1 = data$districts

# create a function that selects the district names and ids
  select_dist = function(i) {
    y = unlist(districts_1[i])
    y = y[c(2:3)]
    names(y) = c("district_id", "district")
    return(y)
  }
  
  # use lapply to run the select_names function and bind into a data table
  districts = lapply(1:length(data$districts), function(x) select_dist(x))
  districts = do.call('rbind', districts)
  districts = data.table(districts)
  
  # convert ids to numerics
  districts[ , district_id:=as.numeric(district_id)]
  
  # drop 'District' from the district names
  districts[, district:=gsub('District','', district)]
  districts[, district:=gsub(' ','', district)]
  
  # eliminate duplicate district ids (error ids for 3 distrits)
  # no facilities listed under mistaken ids
  districts[duplicated(district)]
  districts = districts[!(district_id==127| district_id==128 | district_id==129)]
 
  # one district is named 'district left blank'
  districts[district_id==121, district:=NA]

# ----------------------------------------------
# download and merge meta data for hubs

hubs_1 = data$hubs
  
# create a function that selects the facility names and ids from the list
 select_hubs = function(i) {
    y = unlist(facilities_1[i])
    y = y[c(2:3)]
    names(y) = c("hub_id", "hub")
    return(y)
  }
  
  # use lapply to run the select_names function and bind into a data table
  hubs = lapply(1:length(data$hubs), function(x) select_hubs(x))
  hubs = do.call('rbind', hubs)
  hubs = data.table(hubs)
  hubs[ , hub_id:=as.numeric(hub_id)]
  
# ----------------------------------------------
# export meta data files
  
# save facilities data to merge into downloads from Uganda VL
setnames(facilities, c('hub_id', 'district_id'), c('meta_hub_id', 'meta_district_id') )
saveRDS(facilities, paste0(outDir_facilities, 'facilities.rds'))
  
# save districts data to merge into downloads from Uganda VL
saveRDS(districts, paste0(outDir_facilities, 'districts.rds'))

# save full facilities and data to merge into downloads from Uganda VL
saveRDS(hubs, paste0(outDir_facilities, 'hubs.rds'))
# ----------------------------------------------
