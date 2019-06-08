#--------------------------------------------------------
# function to remove overlapping dates - keep all dates before dt
# where x is the data table and dt is the first date you want to remove
# delete overlapping dates 
#--------------------------------------------------------
overlap = function(x) {
  
  if(folder=='pnlt' | folder=='tb_pati_v_registered' | folder=='tb_pati_v_result') {
    # pnlt data is quarterly - create date from qurter (start month)
    x[ , period:= as.character(period)]
    x[ , year:=substr(period, 1, 4)]
    x[ , quarter:=substr(period, 5, 6)]
    x[quarter=='Q1', month:='01']
    x[quarter=='Q2', month:='04']
    x[quarter=='Q3', month:='07']
    x[quarter=='Q4', month:='10']
    x[ , date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]
    x[ , c('period', 'year', 'month'):=NULL]
  } else {
    
    # create a date variable 
    x[ , period:= as.character(period)]
    x[ , year:=substr(period, 1, 4)]
    x[ , month:=substr(period, 5, 6)]
    x[ , date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]
    x[ , c('period', 'year', 'month'):=NULL] }
  
  #---------------------
  # eliminate the overlapping dates 
  x[ , extraction_date:=max(date), by=file]
  x[ , max_extraction_date:=max(extraction_date), by=date]
  
  # print the overlapping dates 
  dates_vector = x[extraction_date!=max_extraction_date, unique(date)]
  if (length(dates_vector)==0) { print("No overlapping dates!")} else {
    print(paste('The overlapping dates are:', dates_vector)) }
  
  x = x[extraction_date==max_extraction_date]
  x[ ,c('extraction_date', 'max_extraction_date'):=NULL]
  
  # return the data set with no overlap
  return(x) }

#--------------------------------------------------------

#--------------------------------------------------------
# function that uploads the meta data and merges it into the data table
# includes english translations
#--------------------------------------------------------
merge_meta_data = function(x) { 
  # import the meta data for the merge
  
  # once the master facilities list if updated, read in master facilities
  facilities = data.table(readRDS(paste0(dir, 'meta_data/master_facilities.rds')))
  data_elements = data.table(readRDS(paste0(dir, 'meta_data/updated_data_elements.rds')))
  categories = data.table(readRDS(paste0(dir, 'meta_data/data_elements_categories.rds')))
  
  # drop unecessary variables in meta data data sets
  data_elements[ , c('datasets_url', 'data_element_url'):=NULL]
  categories[ , url_list:=NULL]
  
  # change the names of vars in dt so they match with meta data
  setnames(x, 'org_unit_ID', 'org_unit_id')
  setnames(x, 'data_element_ID', 'data_element_id')
  setnames(x, 'category', 'category_id')
  
  # change the names of vars in meta data so they match with dt
  setnames(categories, 'ID', 'category_id')
  setnames(categories, 'displayName', 'category')
  setnames(data_elements, 'datasets_ID', 'data_set_id')
  
  x[ , group:=NULL]
  x[ , data_element_id:=as.character(data_element_id)]
  x[ , category_id:=as.character(category_id)]
  x[ , org_unit_id :=as.character(org_unit_id)]
  x[ , last_update:=as.character(last_update)]
  data_elements[, data_set_id:=as.character(data_set_id)]
  data_elements[, data_element_id:=as.character(data_element_id)]
  
  # merge in the facilities meta data 
  y = merge(x, facilities, by='org_unit_id', all.x=TRUE)
  
  # merge in the data elements
  # some data elements contain duplicate ids - set if statements for these sets
  if (folder=='pnls') {
    y[ , data_set_id:='wIMw0dzITTs']
    y = merge(y, data_elements, by=c('data_set_id', 'data_element_id'), all.x=TRUE)
  } else if (folder=='base') { 
    y[ , data_set_id:='pMbC0FJPkcm']
    y = merge(y, data_elements, by=c('data_set_id', 'data_element_id'), all.x=TRUE)
  } else { y = merge(y, data_elements, by='data_element_id', all.x=TRUE) }
  
  # merge in the categories
  y = merge(y, categories, by='category_id', all.x=TRUE)
  
  # change last update to be a data variable
  y[ , last_update:=as.Date(last_update)]
  
  # rename variables and place in an intuitive order 
  # check if the data table contains quarterly data 
  setnames(y, "data_element_id", "element_id")
  setnames(y, "datasets_name", "data_set")
  setnames(y, "data_element_name", "element")
  
  y[, c("period", "data_set_id") := NULL]
  
  return(y) }
#--------------------------------------------------------

#--------------------------------------------------------
# prep function
#--------------------------------------------------------
prep_dhis = function(x) {
  
  #--------------------------------------
  # replace the dps/hz with just the name, excluding the code and word 'province'
  # some health zones and provinces have two names before 'province'
  
  # replace dps with the name only
  x$dps1 = unlist(lapply(strsplit(x$dps, " "), "[", 2))
  x$dps2 = unlist(lapply(strsplit(x$dps, " "), "[", 3))
  x[dps2!='Province', dps:=paste(dps1, dps2)]
  x[dps2=='Province', dps:=dps1]
  x[ , c('dps1', 'dps2'):=NULL]
  
  # replace health zone with the name only
  x$health_zone1 = unlist(lapply(strsplit(dt$health_zone, " "), "[", 2))
  x$health_zone2 = unlist(lapply(strsplit(dt$health_zone, " "), "[", 3))
  x$health_zone3 = unlist(lapply(strsplit(dt$health_zone, " "), "[", 4))
  x[health_zone3 != 'Zone' & health_zone2 != 'Zone', health_zone:=paste(health_zone1, health_zone2, health_zone3) ]
  x[health_zone3=='Zone', health_zone:=paste(health_zone1, health_zone2)]
  x[health_zone2=='Zone', health_zone:=health_zone1]
  x[ , c('health_zone1', 'health_zone2', 'health_zone3'):=NULL]
  
  return(x)
}
#--------------------------------------------------------






