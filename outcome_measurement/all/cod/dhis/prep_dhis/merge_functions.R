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
  }
    
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
  data_elements = data.table(readRDS(paste0(dir, 'meta_data/updated_data_elements.rds')))
  categories = data.table(readRDS(paste0(dir, 'meta_data/data_elements_categories.rds')))
  
  # replace dps with the name only
  facilities$dps1 = unlist(lapply(strsplit(facilities$dps, " "), "[", 2))
  facilities$dps2 = unlist(lapply(strsplit(facilities$dps, " "), "[", 3))
  facilities[dps2!='Province', dps:=paste(dps1, dps2)]
  facilities[dps2=='Province', dps:=dps1]
  facilities[ , c('dps1', 'dps2'):=NULL]
  
  # replace health zone with the name only
  facilities$health_zone1 = unlist(lapply(strsplit(facilities$health_zone, " "), "[", 2))
  facilities$health_zone2 = unlist(lapply(strsplit(facilities$health_zone, " "), "[", 3))
  facilities$health_zone3 = unlist(lapply(strsplit(facilities$health_zone, " "), "[", 4))
  facilities[health_zone3 != 'Zone' & health_zone2 != 'Zone', health_zone:=paste(health_zone1, health_zone2, health_zone3) ]
  facilities[health_zone3=='Zone', health_zone:=paste(health_zone1, health_zone2)]
  facilities[health_zone2=='Zone', health_zone:=health_zone1]
  facilities[ , c('health_zone1', 'health_zone2', 'health_zone3'):=NULL]

  # drop unecessary variables
  data_elements[ , c('datasets_url', 'data_element_url'):=NULL]
  categories[ , url_list:=NULL]
  
  #-------------------
  # change the names of the ID variables in elements and categories to match for the merge
  setnames(categories,'displayName', 'category_name')
  setnames(categories,'ID', 'category')
  setnames(x, 'org_unit_ID', 'org_unit_id')
  setnames(x, 'data_element_ID', 'data_element_id')
  
  #-------------------
  # merge in the meta data 
  
  # merge in the facilities meta data 
  y = merge(x, facilities, by='org_unit_id', all.x=T)

  # merge in the data elements
  # some data elements contain duplicate ids - set if statements for these sets
  if (folder=='pnls') y[ , data_set_id:='wIMw0dzITTs']
  if (folder=='base') y[ , data_set_id:='pMbC0FJPkcm']
  
  # subset down the list of elements to the data set being processed
  sub_id = y[ , unique(data_set_id)]
  data_elements = data_elements[datasets_ID==sub_id]
  
  # merge in the variable names 
  y = merge(y, data_elements, by='data_element_id', all.x=T)

  # merge in the categories
  y = data.table(merge(y, categories, by='category', all.x=T))

  # drop unecessary variables to simplify
  y[ , c('category', 'opening_date', 'data_set_id'):=NULL] 
  setnames(y, 'category_name', 'category')
  
  print("Metadata merged; beginning to add translations.")
  
  #-------------------
  # merge in the english translations
  translations = data.table(readRDS(paste0(dir, 'meta_data/data_elements.rds')))
  translations = translations[ ,.(data_set_id, data_element_id=element_id, element_eng)]
  translations = translations[data_set_id==sub_id]
  translations[ ,data_set_id:=NULL]
  y = merge(y, translations, by='data_element_id', all.x=T)
  
  #--------------------------------------

  # return the new data set
  return(y) 
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






