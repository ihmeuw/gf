# Extract DHIS2 Data

# Caitlin O'Brien-Carelli
# 6/11/2018
# Extract single data sets - code to extract all 19 data sets at the end
# Data will be merged with the meta data to create complete data sets
#-------------------------------------------

#----------------------------------
# To run on the cluster:

# Copy shell script into a qlogin session to open an IDE
# request at least 10 slots for extractions 

# sh /share/singularity-images/rstudio/shells/rstudio_qsub_script.sh -p 1247 -s 20 -P pnls_download

#----------------------------------
# Set the directory to download the data
# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

#------------------------------------
# CHANGE THESE - set the start year, end year, and output directory


# define main directory
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

#----------------------------------
# source the necessary functions to download the data 
source(paste0(dir, 'dhis_extracting_functions.R')) # change to locate code 

# make sure it worked
?extract_all_data

# select the start year and end year for the download
start_year <- '2078'
end_year <- '2018'
start_month <- '01'
end_month <- '12'

# change the update year to before the data begins
update_year <- '2018'

#identify the data set(s) you want to download 


#------------------------------------------------


#----------------------------------
# set the country, base_url, username, and password 
country <- 'drc'
base_url <- 'https://www.snisrdc.com'
userID <- 'Bethany_Huntley'
password <- 'Snisrdcongo1'

#------------------------------------------------
# import the necessary meta data for the download
# you will need organizational units and data sets 

dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')


data_sets <- readRDS(paste0(dir, "meta_data/data_sets.rds")) 
data_sets <- data.table(data_sets)
org_units <- readRDS(paste0(dir, "meta_data/org_units_list.rds")) 

#-----------------------------------------------


#------------------------
# to export a list of regularly reported data sets
data_sets_ids <- c("ktBWTI6yjTB", "iriO2vCt72x", "OeWrFwkFMvf", "s6yd0w2KXWa", 
              "cKVdn82G240", "Fo5ux0Ja21i", "maDtHIFrSHx", "pePpSnKtAh3",
              "pMbC0FJPkcm", "EbG2JnCIPKD", "ycHbewznGao", "mV0r6yDCZy3")

data_sets[datasets_ID %in% data_sets_ids]

reported_elements <- elements[datasets_ID %in% data_sets_ids]

write.csv(reported_elements, paste0(dir, 'elements_catalogue.csv'))


#------------------------------
# import the meta data for the merge (after the download)

# org_units is already uploaded - change into a data table
org_units <- data.table(org_units)

# upload the data elements
data_elements <- readRDS(paste0(dir, 'meta_data/updated_data_elements.rds')) 
data_elements <- data.table(data_elements)
data_elements[ , element_name:=displayName]
data_elements[ , displayName:=NULL]

# upload the categories for the data elements
data_elements_categories <- readRDS(paste0(dir, 'meta_data/data_elements_categories.rds')) 
data_elements_categories <- data.table(data_elements_categories)
data_elements_categories <- data_elements_categories[ ,.(category=ID, category_name=displayName)]

# upload the information about the organisational units - coordinates, opening date, etc.
org_units_description <- readRDS(paste0(dir, 'meta_data/org_units_description.rds'))
org_units_description <- data.table(org_units_description)
org_units_description <- org_units_description[ ,.(org_unit_ID = id, coordinates, opening_date, parent_id)]

#-----------------------------------------------
# extract the data sets that correspond to the meta_data

# Data set inventory:
# 1




# add the names of variables for data sets to data sets








#------------------------
# extract base services and export as a RDS/CSV
base_services <- extract_all_data(base_url = base_url, 
                                     data_sets = data_sets[1, ],
                                     org_units = org_units, 
                                     deb_period = paste0(start_year, '-', start_month, '-01'),
                                     end_period = paste0(end_year, '-', end_month, '-01'),
                                     userID = userID, 
                                     password = password,
                                     pace = 20,
                                     update_date = paste0(update_year, '-01-01'))

saveRDS(base_extraction, paste0(out_dir, '/base_services_', country, 
                                 '_', start_month, '_', start_year, '_', end_month, '_', end_year, '.rds'))

# write.csv(base_extraction, paste0(out_dir, '/base_extraction_', country,  '_', start_year, '_', end_year, '.csv'))

#-------------------------

#------------------------
# extract PNLS and export as a RDS/CSV
pnls <- extract_all_data(base_url = base_url, 
                                  data_sets = data_sets[17, ],
                                  org_units = org_units, 
                                  deb_period = paste0(start_year, '-', start_month, '-01'),
                                  end_period = paste0(end_year, '-', end_month, '-01'),
                                  userID = userID, 
                                  password = password,
                                  pace = 10,
                                  update_date = paste0(update_year, '-01-01'))

saveRDS(pnls, paste0(dir, 'pnls_', country, 
                     '_', start_month, '_', start_year, '_', end_month, '_', end_year, '.rds'))

# write.csv(pnls_extraction, paste0(out_dir, '/pnls_extraction_', country, 
#                                   '_', start_year, '_', end_year, '.csv'))
#------------------------

pnlt <- extract_all_data(base_url = base_url, 
                         data_sets = data_sets[18, ],
                         org_units = org_units, 
                         deb_period = paste0(start_year, '-', start_month, '-01'),
                         end_period = paste0(end_year, '-', end_month, '-01'),
                         userID = userID, 
                         password = password,
                         pace = 10,
                         update_date = paste0(update_year, '-01-01'))


saveRDS(pnlt, paste0(dir, 'pnlt_', country, 
                     '_', start_month, '_', start_year, '_', end_month, '_', end_year, '.rds'))



pnlp <- extract_all_data(base_url = base_url, 
                         data_sets = data_sets[15, ],
                         org_units = org_units, 
                         deb_period = paste0(start_year, '-', start_month, '-01'),
                         end_period = paste0(end_year, '-', end_month, '-01'),
                         userID = userID, 
                         password = password,
                         pace = 10,
                         update_date = paste0(update_year, '-01-01'))


saveRDS(pnlp, paste0(dir, 'pnlp_', country, 
                     '_', start_month, '_', start_year, '_', end_month, '_', end_year, '.rds'))










#------------------------
# extract PNLP sentinel sites (2 data sets) and export as a CSV
pnlp_extraction <<- extract_all_data(base_url = base_url, 
                                     data_sets = data_sets[15,],
                                     org_units = org_units, 
                                     deb_period = paste0(start_year, '-01-01'),
                                     end_period = paste0(end_year, '-01-01'),
                                     userID = userID, 
                                     password = password,
                                     pace = 10,
                                     update_date = '2018-01-01')

saveRDS(pnlp_extraction, paste0(dir, '/pnlp_extraction_', country, 
                                     '_', start_year, '_', end_year, '.rds'))



#-------------------------
pnlt_extraction <- extract_all_data(base_url = base_url, 
                                         data_sets = data_sets[18,],
                                         org_units = org_units, 
                                         deb_period = paste0(start_year, '-01-01'),
                                         end_period = paste0(end_year, '-04-01'),
                                         userID = userID, 
                                         password = password,
                                         pace = 10,
                                         update_date = '2017-01-01')

saveRDS(pnlt_extraction, paste0(out_dir, '/pnlt_extraction_', country, 
                                      '_', start_year, '_', end_year, '.rds'))

#---------------------------------------------
sigl <- extract_all_data(base_url = base_url, 
                                  data_sets = data_sets[3:4, ],
                                  org_units = org_units, 
                                  deb_period = paste0(start_year, '-', start_month, '-01'),
                                  end_period = paste0(end_year, '-', end_month, '-01'),
                                  userID = userID, 
                                  password = password,
                                  pace = 10,
                                  update_date = paste0(update_year, '-01-01'))

saveRDS(base_extraction, paste0(dir, '/sigl_', country, 
                                '_', start_month, '_', start_year, '_', end_month, '_', end_year, '.rds'))


#----------------------------------------------------


# to export a list of regularly reported data sets
data_sets_ids <- c("ktBWTI6yjTB", "iriO2vCt72x", "OeWrFwkFMvf", "s6yd0w2KXWa", 
                   "cKVdn82G240", "Fo5ux0Ja21i", "maDtHIFrSHx", "pePpSnKtAh3",
                   "pMbC0FJPkcm", "EbG2JnCIPKD", "ycHbewznGao", "mV0r6yDCZy3")

data_sets[datasets_ID %in% data_sets_ids]

reported_elements <- elements[datasets_ID %in% data_sets_ids]

write.csv(reported_elements, paste0(dir, 'elements_catalogue.csv'))


# --------------------
# merge in the meta data 
