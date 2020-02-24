# DHIS Extraction - extract the meta data for org units
# Runs as a function of extract_meta_data.R
# Extracts associated geographic information for health facilities
#
# Caitlin O'Brien-Carelli
# 2/20/2020
#-------------------------------

#--------------------------------------------
# directories are already set by the source code
# website for bug fixes (use ancestors for higher level units):
# https://www.snisrdc.com/api/organisationUnits/pCfpKXoGBF8.xml

#-------------------------------

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

#-------------------------------
# this code mysteriously breaks if you run it all at once
# however, it does not break as a loop with a pause

# separate the massive facilities list into groups of 1,000
length_units = c(1:round(nrow(org_units)/1000, 0))

# repeat each group number 1,000 times (label the groups)
org_units$extraction_group =  rep(length_units, each = 1000, length.out = nrow(org_units))

# preserve original org_units just in case
original_units = copy(org_units)

#-------------------------------
# run the extraction 
# loop through each group of 1000 units and download meta data
# save into separate files of 1000 units each

i = 1
for (g in length_units) {
  
# arguments for the file name and print statement
total_groups = length(unique(org_units$extraction_group))
group_number = as.character(g)

# extract the meta data and save as a RDS
units = extract_dhis_units(base_url = base_url, userID = userID, password = password)
saveRDS(units, paste0(dir,'0_meta_data/units/extracted_org_units_',group_number, '.rds'))

# pause and notify that a new group is starting
pause(60)
print(paste0("Starting group ", group_number, " of ", total_groups, " group!"))

if (i == total_groups) print ("All done!")
i = i+1

}

#-------------------------------

#-------------------------------
# rbind the groups together and save

setwd(paste0(dir, '/0_meta_data/units/'))

# list existing files
files = list.files('./', recursive=TRUE)
length(files)

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

# reset variable names for the merge
full_data[ , ancestors:=unlist(full_data$ancestors)] # ancestors output a list
setnames(full_data, 'name', 'org_unit')

#---------------------------------
# use the list of org_units to name the ancestors

# list of org_units
names = original_units[ ,.(ancestors = org_unit_ID, ancestor_name = org_unit_name)]

# merge in the units information
full_data = merge(full_data, names, by='ancestors', all.x=T)

# check that every unit is included in full data
missing_units = original_units[!(original_units$org_unit_ID %in% full_data$id)]
if (0 < length(missing_units )) print("Houston, some ancestors did not extract.")

#---------------------------------
# set ancestor types 

full_data[grep(pattern="du Congo", ancestor_name),type:='country']
full_data[grep(pattern="Province", ancestor_name), type:='dps']
full_data[grep(pattern="Zone", ancestor_name), type:='health_zone']
full_data[grep(pattern="Aire", ancestor_name), type:='health_area']

# units with typos in the names - check these
# full_data[id=='RKN8w566BvC' & ancestors=='U333OaNPIrk', type:='health_area']
# full_data[id=='MvZV4WrjmVW' & ancestors=='o22J1nmzywE', type:='health_area']

# drop ancestor id to shape long
full_data[ , ancestors:=NULL]

# reshape the meta data wide 
# creates a data set of single facilities with associated geographic units
full_data = dcast(full_data, id+opening_date+coordinates+org_unit~type, value.var='ancestor_name')

#---------------------------------
# run checks to determine if the data are processing correctly


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
saveRDS(full_data, paste0(dir, 'meta_data/master_facilities.rds'))

#-------------------------------




