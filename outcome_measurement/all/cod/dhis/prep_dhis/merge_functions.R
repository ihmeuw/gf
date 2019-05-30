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
  
  # merge in the facilities meta data 
  y = merge(x, facilities, by='id', all.x=T)

  # merge in the data elements
  # some data elements contain duplicate ids - set if statements for these sets
  if (folder=='pnls') {
    y[ , data_set_id:='wIMw0dzITTs']
    y = merge(y, data_elements, by=c('data_set_id', 'data_element_id'), all.x=T)
  } else if (folder=='base') { 
    y[ , data_set_id:='pMbC0FJPkcm']
    y = merge(y, data_elements, by=c('data_set_id', 'data_element_id'), all.x=T)
  } else { y = merge(y, data_elements, by='data_element_id', all.x=T) }
  
  # merge in the categories
  y = data.table(merge(y, categories, by='category', all.x=T))
  
  # save last update to analyze lags
  y[ , last_update:=unlist(lapply(str_split(last_update, 'T'), '[', 1))]
  y[ , last_upate:=as.Date(last_update)]
  
  # drop unecessary variables to simplify
  y[ , c('category', 'file', 'opening_date', 'data_set_id'):=NULL] 
  
  #-------------------
  # rename variables and place in an intuitive order 
  # check if the data table contains quarterly data 
  
  names_vector = names(y)
  if ("quarter" %in% names_vector) {
  
  # rename the variables - different variables in quarterly data 
  y = y[ ,.(org_unit_id=id, element_id=data_element_id,
            org_unit, element_eng, quarter, date, category=category_name,
            value, org_unit_type, level, country, dps, health_zone,
            health_area, element, data_set=data_sets, coordinates)]
  } else {
    y = y[ ,.(org_unit_id=id, element_id=data_element_id,
              org_unit, element_eng, date, category=category_name,
              value, org_unit_type, level, country, dps, health_zone,
              health_area, element, data_set=data_sets, coordinates)]
  }

  # return the new data set
  return(y) }


#--------------------------------------------------------------
# prep function for all data sets

prep_dhis = function(x) {
  
  #--------------------------------------
  # replace the dps/hz with just the name, excluding the code and word 'province'
  # some health zones and provinces have two names before 'province'
  
  # replace dps with the name only
  x$dps1 = unlist(lapply(strsplit(dt$dps, " "), "[", 2))
  x$dps2 = unlist(lapply(strsplit(dt$dps, " "), "[", 3))
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
  
  #--------------------------------------
  return(x)
}

#--------------------------------------------------------------
# function to prep and subset pnls 

pnls_subset = function(x) {
  
  #---------------------
  # drop the english element for final prep
  
  x[ , element_eng:=NULL]  
  #---------------------------------------
  # drop out duplicate entries
  
  # create a variable that contains only the last word
  x[ , last:=word(element, -1)]
  
  # drop anything containing soutien as the last word
  # these elements are duplicates or totals of other variables
  x = x[!grep('soutien', last)]
  
  # drop out elements that end in 'sex' or 'age'
  # these variables are stratified only by sex or age, while others include both sex/age
  x = x[!grep('sex', last)]
  x = x[!grep('age', last)]
  
  #---------------------------------------
  # create a data set variable based on element codes
  x[ , set:=(unlist(lapply(strsplit(element, "-"), '[', 2)))]
  
  # translate groupings
  x[set=='CDV', set:='VCT']
  x[set=='IST', set:='STI']
  x[set=='PTME', set:='PMTCT']
  
  #---------------------------------------
  # create an element that is easier to grep
  x[ , element1:=tolower(element)]
  
  # run the function to eliminate diacritical marks
  x[ , element1:=fix_diacritics(element1)]
  
  #---------------------------------------
  # create new variable names without 'PNLS' or set code
  
  # drop the pnls and set codes from the variable names
  # in 'co-infected' elements, there are three hyphens
  x[ , element_new1:=(unlist(lapply(strsplit(element, "-"), '[', 3)))]
  x[ , element_new2:=(unlist(lapply(strsplit(element, "-"), '[', 4)))]
  x[ , element_new3:=(unlist(lapply(strsplit(element, "-"), '[', 5)))]
  
  x[!is.na(element_new2) & !is.na(element_new3), element_new:=paste0(element_new1, "-", element_new2, "-", element_new3)]
  x[!is.na(element_new2) & is.na(element_new3), element_new:=paste0(element_new1, "-", element_new2)]
  x[is.na(element_new2), element_new:=element_new1]
  
  # drop out the variables used to create new elements
  x[ , c( 'last', 'element1','element_new1', 'element_new2', 'element_new3', 'element'):=NULL]
  setnames(x, 'element_new', 'element')
  
  #------------------------
  return(x)
  
}

#---------------------------------------






