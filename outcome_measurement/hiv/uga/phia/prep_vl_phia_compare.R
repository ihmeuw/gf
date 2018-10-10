# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# Transform the VL data to match the PHIA data 
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
setwd(paste0(root, '/Project/Evaluation/GF/mapping/uga/'))

# load the shapefile
shapeData <- shapefile('uga_region10_map.shp')

# check that shapeData is a spatialpolygonsdataframe
class(shapeData)

# plot the shape file in the base package
plot(shapeData)

# simplify the shape data (could create little gaps, maybe don't do this)
gSimplify(shapeData, tol=0.5, topologyPreserve=TRUE)
plot(shapeData)

# ----------------------------------------------
# collapse the data into regions

# import the ten regions that are included in phia
regions = data.table(read.csv(paste0(root, "/Project/Evaluation/GF/mapping/uga/uga_geographies_map.csv")))
regions = regions[ ,.(region10_name, district_name=dist112_name)]

# subset to the relevant dates
vl = uvl[date>= '2016-08-01' & date <='2017-03-01',.(suppressed=sum(suppressed), valid_results=sum(valid_results)), by=district_name]

# check which districts are in which data set
vl$district_name %in% regions$district_name
regions$district_name  %in% vl$district_name

# there is a repeat row in regions
regions = regions[!duplicated(regions)]

# merge the names of regions and their associated districts with the vl data 
vl = merge(vl, regions, by='district_name')

# collapse on region
vl = vl[ ,.(suppressed=sum(suppressed), valid_results=sum(valid_results)), by=region10_name]
setnames(vl, 'region10_name', 'region')

# calculate the suppression ratio
vl[ ,ratio:=100*(suppressed/valid_results), by=region]

# round to the same number of digits as in phia
vl[ , ratio:=round(ratio, 1)]

#------------------------------------------
# merge the data with the shape file to create a comparison map 

# identify the variable that contains district names and codes
shapeData@data %>% as_tibble()
shape_names = data.table(region=unique(shapeData@data$region10_n), id=unique(shapeData@data$id))

# convert spaces to underscore in region names
shape_names$region = gsub('\\s', '_', shape_names$region)

# merge the names to get id
vl = merge(shape_names, vl, by='region')

#---------------------------
# prep coordinates and merge

# use the fortify function to convert from spatialpolygonsdataframe to data.frame
coordinates <- data.table(fortify(shapeData, region='region10'))
coordinates[ ,id:=as.numeric(id)]

coordinates_vl = merge(coordinates, vl, by='id')

# identify centroids
names = data.table(coordinates(shapeData))
names[, name2 := shapeData@data$region10_n]
names[, ratio := vl$ratio]
names[ ,name:=paste0(name2, ': ', ratio, '%')]
names[ ,c('name2', 'ratio'):=NULL]

# change the names of the variables to x and y
setnames(names, c('x','y','name'))

# ---------------
# graph the same time period as the phia graph in uganda vl 

# store colors
ratio_colors = brewer.pal(6, 'BuGn')

# set legend breaks
breaks = c(77, 80, 83, 85, 88)

# export the comparison map as a pdf
pdf(paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/phia_2016/vl_comparison_map.pdf'), height=6, width=9)
# map of regions 
ggplot(coordinates_vl, aes(x=long, y=lat, group=group, fill=ratio)) + 
  geom_polygon() + 
  geom_path(size=0.01, color="#636363") + 
  scale_fill_gradientn(colors=ratio_colors, breaks=breaks) + 
  theme_void() +
  coord_fixed() +
  labs(fill='% virally suppressed') +
  geom_label_repel(data = names, aes(label = name, x = x, y = y, group = name), inherit.aes=FALSE)
  
dev.off()

#---------------------------------------------------------------
# export a data set for the relevant time period

saveRDS(vl, paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/phia_2016/vl_region_data.rds'))

#---------------------------------------------------------------










  