outDir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/eid') # drop out later

# ---------------------------
# download facility names and merge

# store url
url = 'https://edash.cphluganda.org/other_data'

# load
data = fromJSON(url)

# create a data table that contains only facilities information
facilities_1 = data$facilities

# create a function that selects the facility names and ids from the list
select_names = function(i) {
  y = unlist(facilities_1[i])
  y = y[c(1,2,3)]
  names(y) = c("alpha_id", "id", "facility")
  return(y)
}

# use lapply to run the select_names function and bind into a data table
facilities = lapply(1:length(data$facilities), function(x) select_names(x))
facilities = data.table(do.call('rbind', facilities))

#destring  facility ids
facilities[ , id:=as.numeric(id)]

# export the facilities as an RDS file
saveRDS(facilities, paste0(outDir, '/facility_names.rds'))

#--------------------------------------------
districts_1 = data$districts

# create a function that selects the facility names and ids from the list
select_districts = function(i) {
  y = unlist(districts_1[i])
  y = y[c(1,2,3)]
  names(y) = c("alpha_id", "id", "district")
  return(y)
}

# use lapply to run the select_names function and bind into a data table
districts = lapply(1:length(data$districts), function(x) select_districts(x))
districts = data.table(do.call('rbind', districts))

# save the districts from the list
saveRDS(districts, paste0(outDir, '/district_names.rds'))

#------------------------------------------
# download and store viral load meta data 
# test overlap between id numbers

# store url
uvl_url = 'https://vldash.cphluganda.org/other_data'

# load
data2 = fromJSON(uvl_url)

# create a data table that contains only facilities information
facilities_2 = data2$facilities

# create a function that selects the facility names and ids from the list
select_names2 = function(i) {
  y = unlist(facilities_2[i])
  if (length(y)==6) {
    y = y[c(1,2,3,NA,4,6)]
    names(y) = c("alpha_id", "id", "facility", "dhis2_name", "hub_id", "district_id")
    return(y)
  } else {
  y = y[c(1,2,3,4,5,7)]
  names(y) = c("alpha_id", "id", "facility", "dhis2_name", "hub_id", "district_id")
  return(y)
}}

# use lapply to run the select_names function and bind into a data table
v_facilities = lapply(1:length(data2$facilities), function(x) select_names2(x))
v_facilities = do.call('rbind', v_facilities)
v_facilities = data.table(v_facilities)

#destring id #s
v_facilities[ , id:=as.numeric(id)]
v_facilities[ , hub_id:=as.numeric(hub_id)]
v_facilities[ , district_id:=as.numeric(district_id)]

# save the facilities from viral load
saveRDS(v_facilities, paste0(outDir, '/uvl_facilities_names.rds'))
#------------------------------------------
# match on name with viral load data 

names(v_facilities) = c("alpha_id1", "id1", "facility1", "dhis2_name1",  "hub_id1", "district_id1")

facilities[ ,facility2:=tolower(facility)]
facilities[ ,facility2:=trimws(facility2, 'both')]
facilities[ ,facility2:=gsub(" ", "", facility2)]
facilities[ ,facility2:=gsub("/", "", facility2)]

v_facilities[ ,facility2:=tolower(facility1)]
v_facilities[ ,facility2:=trimws(facility2, 'both')]
v_facilities[ ,facility2:=gsub(" ", "", facility2)]
v_facilities[ ,facility2:=gsub("/", "", facility2)]

facilities = merge(facilities, v_facilities, by='facility2')

# subset to relevant variables
# check to make sure uvl district ids are the same districts as eid district id/district pairs


# rename id
setnames(full_data, 'id', 'facility_id')





