# DHIS Extraction for DRC  - Metadata extraction
# Caitlin O'Brien-Carelli
# 6/4/18

# sh /share/singularity-images/rstudio/shells/rstudio_qsub_script.sh -p 1349 -s 10 -P dhis_download

#----------------------
# determine if the code is being run on the cluster or on hom computer
root <- ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set working directory and print to the console
setwd(paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/'))

# source

dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

#------------------------------

#------------------------------
# install and load the dhisextractr package 

# cluster only: install dhisextractr 

#------------------------------
# import a list of organisational units 

org_units_list <- readRDS(paste0(dir, 'meta_new/org_units_list.rds'))
#---------------------------------------------

#--------------------------------------
country <- 'drc'
base_url <- 'https://www.snisrdc.com'
userID <- 'Bethany_Huntley'
password <- 'Snisrdcongo1'
#--------------------------------------------


#-----------------------------

#extract_dhis_content function
extract_dhis_content <- function(base_url, userID, password) {
  print('Making DHIS urls')
  urls <- make_dhis_urls(base_url)
  
  #-----------------------
  #extract information about organisational units: coordinates, data sets, etc.
  print('Extracting units information')
  extracted_org_units <- dlply(org_units_list, .(org_unit_ID),
                               function(org_units_list) {
                                 try(extract_org_unit(as.character(org_units_list$org_unit_url) ,
                                                      userID, password))},
                               .progress = 'text')  
  
  # #screen for timed out and other faulty org units
  # lengths = sapply(extracted_org_units, length)
  # faults = which(lengths<3)
  # if (length(faults)>0) warning('Warning: at least one org unit didn\'t extract correctly')
  # extracted_org_units <<- extracted_org_units[-faults]

}


#-----------------------
# run the extraction 
 units <- extract_dhis_content(base_url = base_url, userID = userID, password = password)

#-----------------------
# save the contents of the extraction
saveRDS(units, file=paste0(dir, 'meta_new/extracted_org_units.rds')) 

#-----------------------