# ----------------------------------------------
# David, Phillips, Caitlin O'Brien-Carelli
#
# 4/11/2018
# To extract a list of districts and facilities from Uganda Viral Load Dashboard: https://vldash.cphluganda.org/
# ----------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(tibble)
library(dplyr)
library(tm)

# --------------------


# ----------------------------------------------
# Files and directories

# data directory

# output file
dir = "J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/facilities"
# ----------------------------------------------


# ----------------------------------------------
# Load/prep data
# facilities and districts are stored in separate urls
            
  # store url
  url = 'https://vldash.cphluganda.org/other_data'
              
  # load
  data = fromJSON(url)
  
  
  # original function to extract the facility ids from the list
  #facilities_full <- data.table(rbindlist(lapply(1:length(data$facilities), function(x) data$facilities[[x]]))) 
 
 # create a data frame with only the facilities data 
 facilities_1 <- data$facilities
 
 # create a function that selects the facility names and ids
  select_names <- function(i) {
   y <- unlist(facilities_1[i])
   y <- y[c(2,3,4,5,7)]
   names(y) <- c("facility_id", "facility_name", "dhis2_name", "hub_id", "district_id")
   return(y)
  }
  
  # use lapply to run the select_names function and bind into a data table
  facilities <- lapply(1:length(data$facilities), function(x) select_names(x))
  facilities <- do.call('rbind', facilities)
  facilities <- data.table(facilities)
  
  #destring id #s
  facilities[,facility_id:=as.numeric(facility_id)]
  facilities[,hub_id:=as.numeric(hub_id)]
  facilities[,district_id:=as.numeric(district_id)]
  
# --------------------

# add district names and ids
districts_1 <- data$districts
  
  
  select_dist <- function(i) {
    y <- unlist(districts_1[i])
    y <- y[c(2:3)]
    names(y) <- c("district_id", "district_name")
    return(y)
  }
  
  # use lapply to run the select_names function and bind into a data table
  districts <- lapply(1:length(data$districts), function(x) select_dist(x))
  districts <- do.call('rbind', districts)
  districts <- data.table(districts)
  
  districts[, district_id:=as.numeric(district_id)]
  
  # drop 'District' from the district names
  districts[, district_name:=gsub('District','', district_name)]
  districts[, district_name:=gsub(' ','', district_name)]
  districts[, district_name]
  
# -------------------------------------
  # merge districts and facilities
  
  dist_facilities <- merge (facilities, districts, "district_id", all.x=TRUE)


# # -------------------------------------
# # 147 facilities are not associated with a district id
# # find the districts in the 2012 Uganda Health Facility Inventory
#   
# # determine the district ids for 147 facilities using the 2012 facility inventory  
#   facil_1 <- dist_facilities[is.na(district_id), .(unit=facility_name, facility_id=facility_id)]
#   facil_1[, unit:=tolower(unit)]
#   facil_1$unit <- sapply(strsplit(facil_1$unit, " "), '[', 1 )
#   facil_1 <- facil_1[!(is.na(facil_1$unit))]
# 
# 
# 
#   # import the inventory of health facilities
#   inventory <- read.csv("C:/Users/ccarelli/Desktop/inventory_correct.csv")
#   inventory <- data.table(district=inventory$district, unit=inventory$health_unit, level=inventory$level)
# 
#   inventory[ ,district:=as.character(district)]
#   inventory[ ,unit:=as.character(unit)]
#   inventory[ ,level:=as.character(level)]
#   
#   inventory[ ,district:=tolower(district)]
#   inventory[ ,unit:=tolower(unit)]
#   inventory[ ,level:=tolower(level)]
#   
#   
#    facil_1[(facil_1$unit %in% inventory$unit), unit]
#    
#    
#    #Remove facilities with missing names
#    missing_facilities <- join(facil_1, inventory, 
#                               by = "unit", type="left", match = "first")
#    
#   
  
 
 #------------------------------------------------------------
 
  
  # save full facilities and district data to merge into downloads from Uganda VL
  saveRDS(dist_facilities, file=paste0(dir,"/facilities.rds"))

   
# ----------------------------------------------
  
  
  
  
  