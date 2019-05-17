# --------------------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Explore supply chain data for PNLS, mainly on HIV test kits. 
# DATE: Last updated March 2019
# --------------------------------------------------------------------

#---------------------------------------------------------------------
# TO DO LIST 
#  Add available-usable stock to title where it refers to it in slides, 
# Split sex-stratified variables out into their own PDF. 
# For two indicators in test-kits PDFs, generate both 'total counts' and 'mean facility-days' maps
#     make these labels really clear! *DONE
# Generate a map that shows changes in stock-out days in a district over time, facet-wrapped by month, for only 2018 data. 
#   Take this same map and subset to the facility-level. *DONE
# 

# Not urgent - check online PNLS dashboard and make sure new first-line treatment exists. 
#---------------------------------------------------------------------

#Observations about this dataset: 
# There is no data for December 2017. 
# If stock category is NA, then it is sex-stratified. 
#We seem to just have data gaps for the same 3 districts in 2017? 

#Set up R, and read in data. 
#Read in data set, and source the setup. 
rm(list=ls())

# -----------------------------------------------
# Set up R
#------------------------------------------------
library(data.table)
library(raster)
library(ggplot2)
library(tibble)
library(dplyr)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(maptools)

setwd("C:/Users/elineb/Documents/gf")

j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j,  'Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/') #Home directory

#-------------------------------------------------
# Read in data 
# ------------------------------------------------
source('./core/standardizeDPSNames.R')

rawDT = readRDS(paste0(dir, 'pnls_sets/pnls_drug_2017_01_01_2018_12_01.rds'))
dt = copy(rawDT) #This is a big file, so it's nice to copy it here so rerunning code is fast. 

#Read in the shapefile
shapefile = shapefile("J:/Project/Evaluation/GF/mapping/cod/gadm36_COD_shp/gadm36_COD_1.shp")
shapefile@data$NAME_1 = standardizeDPSNames(shapefile@data$NAME_1)

# use the fortify function to convert from spatialpolygonsdataframe to data.frame
coord = data.table(fortify(shapefile)) 
coord[, id:=as.numeric(id)]
coord_ann = rbind(coord, coord)
coord_ann[, year:=rep(2017:2018, each=nrow(coord))] #What years do you have data for? 

#Make a coordinate map for the months you have available in the data. 
dates_avail = unique(dt[, .(date)][order(date)])
coord_months = data.table()
for (i in dates_avail){
  print(i)
  temp = coord
  temp[, date:=i]
  coord_months = rbind(coord_months, temp)
}

#Make it possible to merge the data with the shape file
shape_names = data.table(id = seq(0, 25, by=1), NAME_1=shapefile@data$NAME_1) #This matches the data when you fortify the shapefile below

dt[, NAME_1:=standardizeDPSNames(dps)]
dt[!NAME_1%in%shapefile@data$NAME_1] #Check that merge will work correctly - this data table should have 0 rows. 

dt = merge(dt, shape_names, by='NAME_1', all.x = TRUE)

#Make a year variable - be careful with the gaps in the data. 
dt[, year:=year(date)]

#--------------------------------------------------------------
#Clean the data 
#--------------------------------------------------------------
dt = dt[, -c('subpop', 'maternity', 'case')]

dt[stock_category=='Nbr de jours RS', stock_category:='number_of_days_stocked_out']
dt[stock_category=="Stock disponible utilisable", stock_category:='available_usable_stock']
dt[stock_category=='Stock Initial', stock_category:='initial_stock']
dt[stock_category=="Entrée", stock_category:='input']
dt[stock_category=='Sortie', stock_category:='output']

#Check to make sure there aren't impossible reporting periods for stock-out days per month, and drop these values
date_frame = data.table(month = seq(1, 12, by=1), expected_days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
dt[, month:=month(date)]
dt = merge(dt, date_frame, all.x = TRUE, by = 'month')

#unique(dt[value >31 & stock_category == "number_of_days_stocked_out", .(value)]) #Caitlin do we want to clean any of these? 
dt[value>expected_days & stock_category == "number_of_days_stocked_out", impossible_so_days:=TRUE] #Create a boolean value to flag which NAs you've created. 
dt[value>expected_days & stock_category == "number_of_days_stocked_out", value:=NA] #Replace impossible days stocked out with NA

# Generate a variable 'any_stock_out' where the denominator is all stock_category 'number of days stocked out' 
#   (Including the 'impossible' NAs for now), and the numerator = 1 if value != 0 & value != NA. 
#   (for that given month, they had at least one day stocked out.)
#   Numerator = 0 if value == 0. Numerator == NA if value == NA. 
dt[stock_category == 'number_of_days_stocked_out', any_stock_out:=0]
dt[!is.na(any_stock_out) & value>0 & !is.na(value), any_stock_out:=1]
dt[is.na(value) & !is.na(any_stock_out), any_stock_out:=NA]

#Create a variable to delineate first-line and second-line regimens

#From treatment regimen PDF in DRC - 
# Initiation du TAR (*) :
#   -Régime de traitement :
#   TDF + 3TC + EFV
# -Alternatives :
#   TDF + 3TC + NVP
# AZT + 3TC + EFV
# AZT + 3TC + NVP
dt[element_id == "jJuipTLZK4o", regimen:=1] # TDF/3TC/EFV(300/300/600 mg) - 30 ces
dt[element_id =="aozs6mB8T8n", regimen:=2] #AZT+3TC+NVP
dt[element_id =="W7sym5eCc44", regimen:=2]  #"AZT/3TC/NVP(300/150/200 mg) - 60 ces" 
dt[element_id == "pzMcLYBCPYG", regimen:=2] #AZT/3TC/NVP 60/30/50 mg ces disp - 60 ces
dt[element_id == "ANTg88cSB09", regimen:=2] #AZT+3TC+EFV

unique(dt[is.na(regimen), .(regimen, element)][order(regimen)])
#Are there any other second-line regimens we can pull here? 

#Save a cleaned data set here so you can run quantile regression 
saveRDS(dt, paste0(dir, "prepped/pnls_drug.rds"))

# 
# 
# 
# #---------------------------
# # drop out the 12 facilities that never reported 
# 
# missing = full_data[ , .(check=all(is.na(arvs)), check_t=all(is.na(test_kits))), by=facility]
# missing = missing[check==TRUE & check_t==TRUE]
# full_data = full_data[!facility %in% missing$facility]
# 
# #------------------------------------
# # merge in the regions
# regions = fread(paste0(j, "/Project/Evaluation/GF/mapping/uga/uga_geographies_map.csv"))
# regions = regions[ ,.(region = region10_name, district = dist112_name)]
# regions = regions[!duplicated(district)]
# full_data = merge(full_data, regions, by='district', all.x=T)
# 
# # check that every district has a region associated with it
# full_data[is.na(region)]
# 
# #-------------------------------------
# # save the output
# 
# # get the minimum and maximum year and add to the file name for export
# min_year = full_data[ , min(year)]
# max_year = full_data[ , max(year)]
# 
# # save as a data table
# saveRDS(full_data, paste0(OutDir, 'arv_stockouts_', min_year, '_', max_year, '.rds'))
# 
# #----------------------------
# 
# 
