
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

setwd("J:/WORK/11_geospatial/06_original shapefiles/GAUL_admin/admin2/g2015_2014_2"

shapeData = shapefile('g2015_2014_2.shp')







