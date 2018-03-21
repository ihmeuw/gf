
# ----------------------------------------------
# Irena Chen
#
# 3/14/2018

### This code is to make the actual maps of DRC's subnationals using shape files and resource tracking data: 

# ----------------------------------------------
# raster package is most of what you need
rm(list=ls())
library(rgeos)
library(raster)
library(ggplot2)
library(maptools)
library(dplyr)
library(tools)
library(data.table)
library(lubridate)
library(grDevices)
library(RColorBrewer)
library(readxl)
library(reshape)
library(scales)


# -------------------
##if you want to use the geospatial team's shapefiles: 
setwd("J:/WORK/11_geospatial/06_original shapefiles/GAUL_admin/admin2/g2015_2014_2")
shapeData = shapefile('g2015_2014_2.shp')

##or use ours to get the DRC provinces: 
setwd("J:/Project/Evaluation/GF/mapping/cod/")
shapeData = shapefile('cod_admbnda_adm1_rgc_20170711.shp')

# these have plot methods
plot(shapeData)

# use the fortify function to convert from spatialpolygonsdataframe to data.frame
# use IDs instead of names
shapeData@data

##lat and long coordinates for the provinces
coordinates = data.table(fortify(shapeData, region='ADM1_PCODE'))

##names/codes of provinces (will need to join this to the coords and join to the RT data)
names = data.table(shapeData@data)

##here's the provinces with the codes:
drc_provs <- names[, c("ADM1_REF", "ADM1_PCODE"), with=FALSE]
drc_provs <- unique(drc_provs) #use this dataframe to match the RT provinces w/ the codes

##LOAD THE RT DATA: 

totalCod <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/cod/prepped/all_fpm_budgets.csv",
                                fileEncoding="latin1"))


cod_data <- totalCod[grepl("DPS", totalCod$recipient),]







