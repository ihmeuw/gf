# Extract DHIS2 Data - MPM Session Download

# Download a subset of the "Base Services" data set from DHIS2 
# Data will be merged with the meta data to create complete data sets
# Updated: June 4, 2018
#-------------------------------------------

#-------------------------------------------
# INSTRUCTIONS: Steps before starting the download:
# 1. Go to Basecamp and download the meta data:
# https://3.basecamp.com/3769859/buckets/4025874/vaults/1113679205

# 2. Move the meta_data into the folder you plan to use for the download.
# For example, "C:\Users\your_username\Documents\dhis_download"

# 3. Copy the file pathway for this folder. The easiest way to do this
# is by holding down the "shift" key while you right-click on the folder.
# Select 'copy as path' from the dropdown menu.
# Then paste the pathway inside "" next to 'dir'

# Remember: in R slashes are always ///

# 4. Set the start_month, start_year, end_month, and end_year
# RECOMMENDED: the data sets are large! Try one month at a time.

#------------------------------------------------
# load the packages you need to run the data - you don't need to change anything here
# if the package is not available on your computer, type:
# install.packages("package_name")

library(RCurl)
library(XML)
library(dplyr)
library(data.table)
library(plyr)
library(ggplot2)

#----------------------------------
# INPUTS TO CHANGE! 

# Set the output directory, or the directory you want to download to
dir <- 'C:/Users/ccarelli/dhis_download'

# Set the start date and end date, by choosing the month/year the download starts and ends
# These inputs should be two-digit string variables ('')
start_month <- '01'
start_year <- '2017'

end_year <- '2017'
end_month <- '03'

# You are now ready to download the data! Click CTRL+SHIFT+ENTER to run it all
# You don't need to change anything after this point :)
#-------------------------------------------


#-------------------------------------------
# Automatically download DHIS2 data 

# ----------------------
# Download and install the dhisextractr package

# You only need to install this the first time you download on a computer 
# Click "No" if R asks you to restart
install.packages("dhisextractr", lib.loc=paste0(dir, '/dhisextractr'))

# load dhisextractr
library(dhisextractr, lib.loc=dir)

# to check if the package uploaded correctly (help file will appear):
# ?extract_all_data


#----------------------------------
# source the functions from the other sets

source(paste0(dir, '/dhis_extracting_functions.R'))

#------------------------------------------------
# set the country, base_url, username, and password 

country <- 'drc'
base_url <- 'https://www.snisrdc.com'
userID <- 'Bethany_Huntley'
password <- 'Snisrdcongo1'


#------------------------------------------------

# import the necessary meta data for the download
data_sets<- readRDS(paste0(dir, '/data_sets.rds')) 
org_units <- readRDS(paste0(dir, '/org_units_list.rds')) 

#-----------------------------------------------

#------------------------------
# if org_units is a list and not a a data frame, convert to a data frame
# ignore this unless you are trying to fix a problem

# org_units <- data.frame(org_unit_ID = org_units[[1]][1],
#                  org_unit_name = org_units[[1]][2],
#                  org_units_url = org_units[[1]][3])

#------------------------------

#-----------------------------------------------
# extract the data sets that correspond to the meta_data

#------------------------
# extract the base services data set and export as a CSV

base_services1 <- extract_all_data(base_url = base_url, 
                                     data_sets = data_sets[1,],
                                     org_units = org_units[1:1000, ], 
                                     deb_period = paste0(start_year, '-', start_month, '-01'),
                                     end_period = paste0(end_year, '-', end_month, '-01'),
                                     userID = userID, 
                                     password = password,
                                     pace = 10,
                                     update_date = '2016-01-01')

# export a csv of the data
# write.csv(base_services, paste0(dir, '/merged_data/base_services_before_merge', country, '_',
#                                   start_month, '_', start_year, '_', end_month, '_', end_year, '.csv'))

#-----------------------------------------------------------------------
# Merge the base services extraction with the meta_data 

# make base services into a data table for easy coding
pnls <- data.table(pnlp_services1)

#------------------------
# upload the metadata files needed for the merge

# org_units is already uploaded - change into a data table
org_units <- data.table(org_units)

# upload the data elements
data_elements <- readRDS(paste0(dir, '/updated_data_elements.rds')) 
data_elements <- data.table(data_elements)
data_elements[ , element_name:=displayName]
data_elements[ , displayName:=NULL]

# upload the categories for the data elements
data_elements_categories <- readRDS(paste0(dir, '/data_elements_categories.rds')) 
data_elements_categories <- data.table(data_elements_categories)
data_elements_categories <- data_elements_categories[ ,.(category=ID, category_name=displayName)]

# upload the information about the organisational units - coordinates, opening date, etc.
org_units_description <- readRDS(paste0(dir, '/org_units_description.rds'))
org_units_description <- data.table(org_units_description)
org_units_description <- org_units_description[ ,.(org_unit_ID = id, coordinates, opening_date, parent_id)]

#------------------------
# merge in the names of the objects in the data set

# merge on org_unit_ID to get names of organisational units
base_services <- merge(base_services, org_units, by='org_unit_ID', all.x=TRUE)
base_services[ , length(unique(org_unit_ID))]

# merge on data element ID to get data sets and data sets name
base_services <- merge(base_services, data_elements, by='data_element_ID', all.x=TRUE)

# merge on category ID to get the age categories for the data elements
base_services <- merge(base_services, data_elements_categories, by='category', all.x=TRUE)
base_services[ , unique(category_name)]

base_services <- merge(base_services, org_units_description, by='org_unit_ID', all.x=TRUE)


#------------------------
# Look at it 

View(base_services) 
 

#------------------------
# export the final data as an RDS file
saveRDS(base_services, file=paste0(paste0(dir, '/merged_data/base_services', '_',  country, '_',
                                               start_month, '_', start_year, '_', end_month, '_', end_year, '.rds')))

# Export the final data set as a .csv 
# write.csv(base_services, paste0(dir, '/merged_data/base_services', country, '_',
#                                  start_month, '_', start_year, '_', end_month, '_', end_year, '.csv'))

#------------------------
# for fun, make a graph 

severe_malaria <- base_services[element_name=='A 1.4 Paludisme grave']
severe_malaria <- severe_malaria[  , .(cases = sum(as.numeric(value))), by=.(period, category_name, element_name)]

severe_malaria$category_name <- factor(severe_malaria$category_name, 
                         levels=c(">5 ans", "<5 ans"), 
                         labels=c("Over 5", "Under 5"))

name <- unique(severe_malaria$element_name)
colors <- c('#de2d26', '#3182bd' )
  
ggplot(severe_malaria, aes(x=period, y=cases, color=category_name, group=category_name)) + 
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_color_manual(values=colors) +
  labs (title = paste(name, '-', 'severe malaria cases in 1,000 health facilities'),
        x='Period', y="Number of severe malaria cases",
        caption='Source: DHIS2', color="Age Category") 



