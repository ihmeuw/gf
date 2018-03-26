# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 3/26/2018
# First map of Uganda by suppression ratio by district
# ----------------------------------------------
# Set up R
# raster package is most of what you need

rm(list=ls())
library(data.table)
library(rgeos)
library(raster)
library(ggplot2)
library(rgdal)


# set input/output directory
# ----------------------------------------------
dir <- 'J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard'

# upload the data with month, year, sex
uganda_vl <- readRDS(paste0(dir, "/sex_data.rds"))

# add suppression ratios for ease of mapping
ratio <- uganda_vl[, .(suppression_ratio=(sum(suppressed)/sum(valid_results))), by=.(month, year, sex)]
uganda_vl <- merge(uganda_vl, ratio, by=c('month','year', 'sex'))

# drop out the current month
uganda_vl <- uganda_vl[!(month==3 & year==2018)]


# change directory
setwd('J:/Project/Evaluation/GF/mapping/uga/')

# load the shapefile
shapeData = shapefile('uga_dist112_map.shp')

# check that shapeData is a spatialpolygonsdataframe
class(shapeData)

# these have plot methods
plot(shapeData)

# simplify the shape data (could create little gaps, maybe don't do this)
shapeData2 = gSimplify(shapeData, tol=0.1, topologyPreserve=TRUE)


# ----------------------------------------------
# import the district data 


# upload the data with month, year, sex
uganda_vl <- readRDS('J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/sex_data.rds')

# view the data set and variable names
class(uganda_vl) # check that it is a data table
str(uganda_vl)
names(uganda_vl)


# -----------















# ----------------------------------------------
# merge the files

# use the fortify function to convert from spatialpolygonsdataframe to data.frame
coordinates = data.table(fortify(shapeData2, region='ID_2')) # use IDs instead of names

# merge on municipality names
names = data.table(shapeData@data)
coordinates = merge(coordinates, names, by.x='id', by.y='ID_2')


# merge on the data (all.x=TRUE so the shapefile data doesn't disappear)




# draw the polygons using ggplot2
ggplot(coordinates, aes(x=long, y=lat, group=group, fill=as.numeric(id))) + 
  geom_polygon() + 
  geom_path() + 
  theme_void()


