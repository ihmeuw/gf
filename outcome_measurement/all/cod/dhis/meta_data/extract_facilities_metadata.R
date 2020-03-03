# DHIS Extraction - extract the meta data for org units
# Runs as a function of extract_meta_data.R
# Extracts associated geographic information for health facilities
#
# Caitlin O'Brien-Carelli
# 3/1/2020
#-------------------------------

#--------------------------------------------
# directories are already set by the source code
# website for bug fixes (use ancestors for higher level units):
# https://www.snisrdc.com/api/organisationUnits/pCfpKXoGBF8.xml

#------------------------------
# testing purposes
extract_all_ancestors = T
org_units = readRDS(paste0(dir, '0_meta_data/org_units.rds'))

#-------------------------------
# do you want to extract only missing geographic info, or re-extract all?
# if extracting all ancestors, be sure to archive contents of 'units' folder

if (extract_all_ancestors==T) UnitDir = paste0(dir,'0_meta_data/units/')
if (extract_all_ancestors==F) UnitDir = paste0(dir,'0_meta_data/new_units/')

#-------------------------------
#extract_dhis_content function
extract_dhis_units = function(base_url, userID, password) {
  print('Making DHIS urls')
  urls = make_dhis_urls(base_url)
  
  #extract information about organisational units: coordinates, data sets, etc.
  print('Extracting units information')
  extracted_org_units = dlply(org_units[extraction_group==g], .(org_unit_ID),
                              function(org_units) {
                                try(extract_org_unit(org_units$url, userID, password))},
                              .progress = 'text')  
}

#-------------------------------
# to extract only the missing information

# preserve original org_units for the reshape
original_units = copy(org_units)

# subset the org units to just the ones missing metadata
if (extract_all_ancestors==F) org_units = missing_units

#-------------------------------
# this code mysteriously breaks if you run it all at once
# however, it does not break as a loop with a pause

# separate the massive facilities list into groups of 1,000
length_units = c(1:round(nrow(org_units)/500, 0))

# repeat each group number 1,000 times (label the groups)
org_units$extraction_group =  rep(length_units, each = 500, length.out = nrow(org_units))

#-------------------------------
# run the extraction 
# loop through each group of 1000 units and download meta data
# save into separate files of 1000 units each

i = 1
for (g in length_units) {
  
# arguments for the file name and print statement
total_groups = length(unique(org_units$extraction_group))
group_number = as.character(g)
next_group = as.numeric(group_number)+1

# extract the meta data and save as a RDS
units = extract_dhis_units(base_url = base_url, userID = userID, password = password)
pause(20) # not sure why this helps, but it does
saveRDS(units, paste0(UnitDir, 'extracted_org_units_',group_number, '.rds'))
print(paste0("Saved ", group_number, " of ", total_groups, " groups!"))

# pause and notify that a new group is starting
pause(90) # like kicking a vending machine
print(paste0("Starting group ", next_group, " of ", total_groups, " groups!"))

if (i == total_groups) print ("All done!")
i = i+1

}

#-------------------------------

#-------------------------------
# rbind the groups together and save

# list files that were extracted
setwd(UnitDir) # either units for all files or new units for new files
files = list.files('./', recursive=TRUE)
length(files)

# bind the files together
i = 1
for(f in files) {
  #load the RDs file
  current_data = readRDS(f)
  current_data = rbindlist(current_data)
  print(paste('Successfully bound: ', f))
  
  # append to the full data 
  if(i==1) full_data = current_data
  if(i>1) full_data = rbind(full_data, current_data)
  i = i+1
}

# ensure the ancestors did not download as a list
full_data[ , ancestors:=unlist(full_data$ancestors)] # ancestors output a list

#---------------------------------
# the full data is a list of ancestors
# add the facility names

setnames(full_data, 'name', 'org_unit')

#---------------------------------
# use the list of org_units to name the ancestors

# subset to the names of ancestors - full list, as you need all units
# ancestor names occur in the full list
names = original_units[ ,.(ancestors = org_unit_ID, ancestor_name = org_unit_name)]

# merge in the units information
full_data = merge(full_data, names, by='ancestors', all.x=T)

#---------------------------------
# set ancestor types 

full_data[grep(pattern="du Congo", ancestor_name),type:='country']
full_data[grep(pattern="Province", ancestor_name), type:='dps']
full_data[grep(pattern="Zone", ancestor_name), type:='health_zone']
full_data[grep(pattern="Aire", ancestor_name), type:='health_area']

# drop ancestor id to shape long
full_data[ , ancestors:=NULL]

# reshape the meta data wide 
# creates a data set of single facilities with associated geographic units
full_data = dcast(full_data, id+opening_date+coordinates+org_unit~type, value.var='ancestor_name')

#---------------------------------

#---------------------------------
# run the prep function 
full_data = prep_facilities(full_data)

#---------------------------------
# put in an intuitive order 

full_data = full_data[ ,.(org_unit_id = id, opening_date, coordinates, org_unit,
                          country, dps, health_zone, health_area, 
                          org_unit_type = type, level)]

#---------------------------------
# save the output

# if only missing facilities were extracted, merge with existing facility meta data 
if (extract_all_ancestors==F) master = rbind(master, full_data)
if (extract_all_ancestors==F) saveRDS(master, paste0(dir, '0_meta_data/master_facilities.rds'))

# if extracted all facilities, new list of facilities meta data
if (extract_all_ancestors==T) saveRDS(full_data, paste0(dir, '0_meta_data/master_facilities.rds'))

#-------------------------------
# delete files in the units or new units folder
# do not delete for all units in case you need the files
if (extract_all_ancestors==F) {
    setwd(UnitDir)
    for (f in files) {
       unlink(f)}}

#-------------------------------
