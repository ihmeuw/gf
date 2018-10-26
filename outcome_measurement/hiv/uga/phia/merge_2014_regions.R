# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 10/11/2018
# Fix the regions 
# ----------------------------------------------
# Set up R

rm(list=ls())
library(data.table)
library(rgeos)
library(raster)
library(ggplot2)
library(rgdal)
library(tibble)
library(dplyr)
library(RColorBrewer)
library(maptools)
library(plyr)
library(ggrepel)

# --------------------
# detect if on windows or on the cluster 

root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# ----------------------------------------------
# Files and directories

# set input/output directory
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard')

# upload the data with month, year, sex
uvl <- readRDS(paste0(dir, "/prepped_data/sex_data.rds"))

# view the data set and variable names
str(uvl)

# there should be 112 districts to match the shape file
length(unique(uvl$district_name))

# ----------------------------------------------
#upload the shape file

# set working directory
setwd('J:/Project/Evaluation/GF/mapping/uga/')

# load the shapefile
shapeData <- shapefile('uga_dist112_map.shp')

# check that shapeData is a spatialpolygonsdataframe
class(shapeData)

# plot the shape file in the base package
plot(shapeData)

# simplify the shape data (could create little gaps, maybe don't do this)
gSimplify(shapeData, tol=0.5, topologyPreserve=TRUE)
plot(shapeData)

# ----------------------------------------------
# merge the data with the shape file

# identify the variable that contains district names and codes
shapeData@data %>% as_tibble()
unique(shapeData@data$dist112_na)
length(unique(shapeData@data$dist112_na)) # 112 districts

# check for unmatched district names in the shape file
unique(uvl$district_name)[!(unique(uvl$district_name)) %in% unique(shapeData@data$dist112_na)]

# create a data table that contains only district names and ids from the shape file
shape_names <- data.table(district_name=shapeData@data$dist112_na, id=shapeData@data$dist112)
str(shape_names)

#-----------------------------------------------
regions = data.table(read.csv(paste0(root, "/Project/Evaluation/GF/mapping/uga/uga_geographies_map.csv")))
regions = regions[ ,.(dist112_name, region10_name)]
regions = regions[region10_name=='Eastern']

shape_names[district_name %in% regions$dist112_name, color:=TRUE]
shape_names[!district_name %in% regions$dist112_name, color:=FALSE]


# use the fortify function to convert from spatialpolygonsdataframe to data.frame
coordinates <- data.table(fortify(shapeData, region='dist112')) 
coordinates[, id:=as.numeric(id)]

coordinates = merge(coordinates, shape_names, by='id')


# identify centroids
names = data.table(coordinates(shapeData))
names[, name := shapeData@data$dist112_n]
setnames(names, c('x','y','name'))

# create a map with the eastern region highlighted
ggplot(coordinates, aes(x=long, y=lat, group=group, fill=color)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  theme_void() + 
  geom_label_repel(data = names, aes(label = name, x = x, y = y, group = name), inherit.aes=FALSE)


# subset to the eastern region
east = coordinates[color==TRUE, unique(district_name)]
names = names[name %in% east]

# highlight only the mideast
mid_east = c('Pallisa', 'Kibuku', 'Butaleja', 'Tororo', 'Manafwa', 'Mbale', 'Bududa',
             'Budaka', 'Sironko', 'Bulambuli', 'Kapchorwa', 'Bukwo', 'Kween', 'Busia')


coordinates2 = coordinates[color==TRUE]
coordinates2[district_name %in% mid_east, color:=TRUE]
coordinates2[!district_name %in% mid_east, color:=FALSE]

ggplot(coordinates2, aes(x=long, y=lat, group=group, fill=color)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  theme_void() + 
  geom_label_repel(data = names, aes(label = name, x = x, y = y, group = name), inherit.aes=FALSE)


# view the mideast region
coordinates3 = coordinates
coordinates3[district_name %in% mid_east, color:=TRUE]
coordinates3[!district_name %in% mid_east, color:=FALSE]

ggplot(coordinates3, aes(x=long, y=lat, group=group, fill=color)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  theme_void() 

#------------------------------------------------------
# region map


regions = data.table(read.csv(paste0(root, "/Project/Evaluation/GF/mapping/uga/uga_geographies_map.csv")))

regions = regions[ ,.(region10_alt, district_name=dist112_name)]

# drop the duplicate row
regions[duplicated(district_name)]
regions[district_name=='Ntungamo']
regions = regions[order(district_name)]

regions = regions[-95]

coordinates = merge(coordinates, regions, by='district_name')

ggplot(coordinates, aes(x=long, y=lat, group=group, fill=region10_alt)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  theme_void() 


id = regions$region10_alt


# ensure order is correct
regions = regions[match(shapeData@data$dist112_na, regions$district_name)]
id = regions$region10_alt
shape2 = unionSpatialPolygons(shapeData, id)

# use the fortify function to convert from spatialpolygonsdataframe to data.frame
coordinates_reg <- data.table(fortify(shape2)) 


coordinates[, id:=as.numeric(id)]

coordinates = merge(coordinates, shape_names, by='id')

ggplot(coordinates, aes(x=long, y=lat, group=group, fill )) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  theme_void() 


