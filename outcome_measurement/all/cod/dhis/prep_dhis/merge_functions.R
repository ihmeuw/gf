#--------------------------------------------------------
# create a function that uploads the meta data and merges it into the data table

merge_meta_data = function(x) { 
  
  #------------------
  # import the meta data for the merge
  
  # once the master facilities list if updated, read in master facilities
  facilities = data.table(readRDS(paste0(dir, 'meta_data/master_facilities.rds')))
  data_elements = data.table(readRDS(paste0(dir, 'meta_data/updated_data_elements.rds')))
  data_elements_categories = data.table(readRDS(paste0(dir, 'meta_data/data_elements_categories.rds')))
  org_units_description = data.table(readRDS(paste0(dir, 'meta_data/org_units_description.rds')))
  org_units_description[ , c('.id', 'active', 'parent_id'):=NULL]
  
  #-------------------
  # change the names of the ID variables in element categories and descriptions to match for the merge
  setnames(data_elements, 'displayName', 'element')
  setnames(data_elements_categories, c('ID', 'displayName'), c('category', 'category_name'))
  
  #-------------------
  # merge in the meta data 
  
  # change the organisational unit id to be called 'id'
  setnames(x, 'org_unit_ID', 'id')
  x[ , group:=NULL]
  
  # create a date variable
  x[ , period:= as.character(period)]
  x[ , year:=substr(period, 1, 4)]
  x[ , month:=substr(period, 5, 6)]
  x[ , date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]
  x[ , period:=NULL]
  
  # merge in the meta data 
  y = merge(x, facilities, by='id', all.x=T)
  y = merge(y, data_elements, by='data_element_ID', all.x=T)
  y = merge(y, data_elements_categories, by='category', all.x=T)
  y = merge(y, org_units_description, by='id', all.x=T)
  y = data.table(y)
  
  # fix names for the prep
  y [ , c('category', 'name', 'datasets_ID'):=NULL] 
  setnames(y, 'category_name', 'category')
  setnames(y, 'datasets_name', 'data_set')
  setnames(y, 'data_element_ID', 'element_id')
  setnames(y, 'type', 'org_unit_type')
  
  return(y) }

#----------------------------------
# create a function to remove overlapping dates - keep all dates before dt
# where x is the data table and dt is the first date you want to remove

overlap = function(x, dt) { 
  
  # function to delete overlapping dates 
  x[ , period1:= as.character(period)]
  x[ , year:=substr(period, 1, 4)]
  x[ , month:=substr(period, 5, 6)]
  x[ , date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]
  
  x = x[date < dt]
  print(unique(x$date))
  x[ , c('period1', 'year', 'month', 'date'):=NULL]
  
  return(x) }

#--------------------