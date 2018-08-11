



#-------------------------------
units <- readRDS(paste0(dir, 'all_units/extracted_org_units.rds'))
org_units_list <- readRDS(paste0(dir, 'all_units/org_units_list.rds'))
org_units_list <- data.table(org_units_list)

#-----------------------------------
# eliminate error messages from the download
# all of the elements in the list should be data frames (T)
# if some are not data frames, they are error messages
# identify which facilities generated error messages and delete them from the list 

errors <- sapply(units, is.data.frame)
unique(errors) # if F is included, there are errors
units <- units[errors==T]

# rbind the data frames in the list together to create a data table
units <- rbindlist(units, fill=true())
units <- data.table(units)

#------------------------------
# subset to only the relevant org_unit names and merge

org_units <- org_units_list[ ,.(id=org_unit_ID, org_unit_name)]
org_units[ , id:=as.character(id)]
units[ , id:=as.character(id)]

setnames(units, 'name', 'org_unit')
#------------------------------
# convert factor variables to characters 

units[ ,c('coordinates', 'opening_date', 'active', 'parent_id'):=NULL]
units[ ,org_unit:=as.character(org_unit)]
units[ , organisationUnit:=as.character(organisationUnit)]
units[ , organisationUnit.1:=as.character(organisationUnit.1)]
units[ , organisationUnit.2:=as.character(organisationUnit.2)]
units[ , organisationUnit.3:=as.character(organisationUnit.3)]

#change the names of the hierarchy to merge on
setnames(units, c('organisationUnit', 'organisationUnit.1', 'organisationUnit.2', 
                  'organisationUnit.3'),
         c('country_id', 'dps_id', 'health_zone_id', 'health_area_id'))

#-------------------------------
# merge in the names for the hierarchy 

# merge in the countries to all units
units <- merge(units, org_units, by.x='country_id', by.y='id', all.X=TRUE)
setnames(units, 'org_unit_name', 'country')

# join the dps to all units
setnames(org_units, 'id', 'dps_id')
units <- join(units, org_units, by='dps_id', type='left')
setnames(units, 'org_unit_name', 'dps')

# join the health zones to all units
setnames(org_units, 'dps_id', 'health_zone_id')
units <- join(units, org_units, by='health_zone_id',type='left')
setnames(units, 'org_unit_name', 'health_zone')

# join the health areas to all units
setnames(org_units, 'health_zone_id', 'health_area_id')
units <- join(units, org_units, by='health_area_id', type='left')
setnames(units, 'org_unit_name', 'health_area')

#---------------------------------
# convert factors to characters

units[ ,country:=as.character(country)]
units[ ,dps:=as.character(dps)]
units[ ,health_zone:=as.character(health_zone)]
units[ ,health_area:=as.character(health_area)]

#----------------------------------
# put the variables in an intuitive order

units <- units[  ,.(org_unit, id, country, dps, health_zone, health_area, 
           country_id, dps_id, health_zone_id, health_area_id)]
             
#----------------------------------
# save the master list of facilities

saveRDS(units, paste0(dir, 'all_units/master_facilities.rds'))

#---------------------------------
# export a csv of health zones by dps

zones <- units[!is.na(health_zone), .(health_zone=unique(health_zone)), by=dps]

zones$health_zone_1 <- strsplit(zones$health_zone, '\\s')
zones$health_zone_1 <- sapply(zones$health_zone_1, '[[', 2)

fwrite(zones, paste0(dir, 'all_units/health_zones_by_dps.csv'))
#-----------------------------------
# merge the list into base services

base <- readRDS(paste0(dir, 'prepped_data/base.rds'))
base[ ,dps:=NULL]

# create a data table of only the variables needed for the merge
base_units <- units[ ,.(org_unit_id=id, dps, health_zone, health_area)]

# join the hierarchical facilities list to the data 
base <- join(base, base_units, by='org_unit_id', type='left')

# save
saveRDS(base, paste0(dir, 'prepped_data/base.rds'))

#--------------------------------------
# merge the list into SIGL1/2
sigl <- readRDS(paste0(dir, 'prepped_data/sigl.rds'))
sigl[ ,dps:=NULL]

# the units list for base services can also be merged with sigl
# join the hierarchical facilities list to the data 
sigl <- join(sigl, base_units, by='org_unit_id', type='left')

#save
saveRDS(sigl, paste0(dir, 'prepped_data/sigl.rds'))

#--------------------------------------





# create a list to merge into the data 




orgs <- units[ ,.(org_unit, id, dps=as.character(dps), 
                  health_zone=as.character(health_zone), 
                  health_area=as.character(health_area))]

orgs[ ,health_zone:=(strsplit(orgs$health_zone, '\\s'))]




orgs$health_zone <- strsplit(orgs$health_zone, '\\s')
orgs$health_zone <- sapply(orgs$health_zone, '[[', 2)



orgs[, health_zone:= strsplit(health_zone, "_", fixed=TRUE)]
dt[, orig:=gsub(v1, '', orig)]
dt$v2=NULL

orgs$dps <- strsplit(orgs$dps, '\\s')
orgs$dps <- sapply(orgs$dps, '[[', 2)







