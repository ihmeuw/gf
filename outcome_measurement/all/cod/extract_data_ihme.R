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

# sh /share/singularity-images/rstudio/shells/rstudio_qsub_script.sh -p 1247 -s 15 -P dhis_download

#----------------------------------
# Set the directory to download the data
if (Sys.info()[1] == 'Windows') {
  username <- "ccarelli"
  root <- "J:/"
} else {
  username <- Sys.getenv("USER")
  root <- "/home/j/"
}

#------------------------------------
# CHANGE THESE - set the start year, end year, and output directory

# select the start year and end year for the download
start_year <- '2015'
end_year <- '2018'
start_month <- '01'
end_month <- '05'

# change the update year to before the data begins
update_year <- '2015'

# define main directory
dir <- paste0(root, 'Project/Evaluation/GF/outcome_measurement/cod/dhis/')

#------------------------------------------------

#----------------------------------
# source the necessary functions to download the data 
source(paste0(dir, 'dhis_extracting_functions1.R')) # change to locate code 

#----------------------------------
# set the country, base_url, username, and password 
country <- 'drc'
base_url <- 'https://www.snisrdc.com'
userID <- 'Bethany_Huntley'
password <- 'Snisrdcongo1'

#------------------------------------------------
# import the necessary meta data for the download
# you will need organizational units and data sets 

data_sets <- readRDS(paste0(dir, "meta_data/data_sets.rds")) 
data_sets <- data.table(data_sets)
org_units <- readRDS(paste0(dir, "meta_data/org_units_list.rds")) 

#-----------------------------------------------

#------------------------------
# if org_units is a list and not a a data frame, convert to a data frame
# it should be saved as a data frame - sometimes it malfunctions

# org_units <- data.frame(org_unit_ID = org_units[[1]][1],
#                  org_unit_name = org_units[[1]][2],
#                  org_units_url = org_units[[1]][3])


#----
# to export a list of regularly reported data sets
data_sets_ids <- c("ktBWTI6yjTB", "iriO2vCt72x", "OeWrFwkFMvf", "s6yd0w2KXWa", 
              "cKVdn82G240", "Fo5ux0Ja21i", "maDtHIFrSHx", "pePpSnKtAh3",
              "pMbC0FJPkcm", "EbG2JnCIPKD", "ycHbewznGao", "mV0r6yDCZy3")

data_sets[datasets_ID %in% data_sets_ids]

reported_elements <- elements[datasets_ID %in% data_sets_ids]

write.csv(reported_elements, paste0(dir, 'elements_catalogue.csv'))


#------------------------------
# import the meta data for the merge (after the download)

# import the necessary meta data for the download
data_sets<- readRDS(paste0(dir, 'meta_data/data_sets.rds')) 
org_units <- readRDS(paste0(dir, 'meta_data/org_units_list.rds')) 

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






#------------------------
# extract base services and export as a RDS/CSV
base_services <- extract_all_data(base_url = base_url, 
                                     data_sets = data_sets[1, ],
                                     org_units = org_units, 
                                     deb_period = paste0(start_year, '-', start_month, '-01'),
                                     end_period = paste0(end_year, '-', end_month, '-01'),
                                     userID = userID, 
                                     password = password,
                                     pace = 10,
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

saveRDS(pnls, paste0(out_dir, '/pnls_', country, 
                     '_', start_month, '_', start_year, '_', end_month, '_', end_year, '.rds'))

# write.csv(pnls_extraction, paste0(out_dir, '/pnls_extraction_', country, 
#                                   '_', start_year, '_', end_year, '.csv'))
#------------------------


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
                                     update_date = '2016-01-01')

write.csv(pnlp_cs_extraction, paste0(out_dir, '/pnlp_cs_extraction_', country, 
                                     '_', start_year, '_', end_year, '.csv'))



#-------------------------
pnlp_hgr_extraction <<- extract_all_data(base_url = base_url, 
                                         data_sets = data_sets[16,],
                                         org_units = org_units, 
                                         deb_period = paste0(start_year, '-01-01'),
                                         end_period = paste0(end_year, '-04-01'),
                                         userID = userID, 
                                         password = password,
                                         pace = 10,
                                         update_date = '2016-01-01')

write.csv(pnlp_hgr_extraction, paste0(out_dir, '/pnlp_hgr_extraction_', country, 
                                      '_', start_year, '_', end_year, '.csv'))



# to export a list of regularly reported data sets
data_sets_ids <- c("ktBWTI6yjTB", "iriO2vCt72x", "OeWrFwkFMvf", "s6yd0w2KXWa", 
                   "cKVdn82G240", "Fo5ux0Ja21i", "maDtHIFrSHx", "pePpSnKtAh3",
                   "pMbC0FJPkcm", "EbG2JnCIPKD", "ycHbewznGao", "mV0r6yDCZy3")

data_sets[datasets_ID %in% data_sets_ids]

reported_elements <- elements[datasets_ID %in% data_sets_ids]

write.csv(reported_elements, paste0(dir, 'elements_catalogue.csv'))


# --------------------
# merge in the meta data 
