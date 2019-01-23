#--------------------------------------------------------
# function to remove overlapping dates - keep all dates before dt
# where x is the data table and dt is the first date you want to remove
# delete overlapping dates 

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

merge_meta_data = function(x) { 
  
  #------------------
  # import the meta data for the merge
  
  # once the master facilities list if updated, read in master facilities
  facilities = data.table(readRDS(paste0(dir, 'meta_data/master_facilities.rds')))
  data_elements = data.table(readRDS(paste0(dir, 'meta_data/data_elements.rds')))
  categories = data.table(readRDS(paste0(dir, 'meta_data/data_elements_categories.rds')))
  
  # drop unecessary variables
  data_elements[ , c('data_set_url', 'element_url'):=NULL]
  categories[ , url_list:=NULL]
  
  #-------------------
  # change the names of the ID variables in elements and categories to match for the merge
  setnames(facilities, 'org_unit_id', 'id')
  setnames(data_elements, 'element_id', 'data_element_id')
  setnames(categories, c('category', 'category_name'))
  
  #-------------------
  # merge in the meta data 
  
  # change the organisational unit id to be called 'id' and change var types for the merge
  setnames(x, 'org_unit_ID', 'id')
  setnames(x, 'data_element_ID', 'data_element_id')
  x[ , group:=NULL]
  x[ , id:=as.character(id)]
  x[ , category:=as.character(category)]
  x[ , last_update:=as.character(last_update)]
  
  # merge in the meta data 
  y = merge(x, facilities, by='id', all.x=T)
  
  y = merge(y, data_elements, by='data_element_id', all.x=T)
  y = merge(y, categories, by='category', all.x=T)
  y = data.table(y)
  
  # drop unecessary variables to simplify
  y[ , c('category', 'last_update', 'file', 'opening_date',
          'data_set_id'):=NULL] 
  
  #-------------------
  # rename variables and place in an intuitive order 
  names_vector = names(y)
  
  if ("quarter" %in% names_vector) {
  
  # rename the variables
  y = y[ ,.(org_unit_id=id, element_id = data_element_id, 
            org_unit, element_eng, quarter, date, category=category_name,
            value, org_unit_type, level, country, dps, health_zone,
            health_area, element, data_set=data_sets, coordinates)]
  } else {print("Add more code")}
  
  
  return(y) }

#--------------------------------------------------------------


