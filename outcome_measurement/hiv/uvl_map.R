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
library(tibble)
library(dplyr)
library(RColorBrewer)


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

# view the data set and variable names
class(uganda_vl) # check that it is a data table
str(uganda_vl)

# ------------------------------

# change directory
setwd('J:/Project/Evaluation/GF/mapping/uga/')

# load the shapefile
shapeData = shapefile('uga_dist112_map.shp')

# check that shapeData is a spatialpolygonsdataframe
class(shapeData)

# plot the shape file in the base package
plot(shapeData)

# simplify the shape data (could create little gaps, maybe don't do this)
#shapeData2 = gSimplify(shapeData, tol=0.1, topologyPreserve=TRUE)


# ----------------------------------------------


# ----------------------------------------------
# merge the files


# -----------------
# make sure the names of the districts are the same 

# identify the variable that contains district names and codes
shapeData@data %>% as_tibble()

shapeData@data$dist112 %>% as_tibble()
shapeData@data$dist112_na %>% as_tibble()

# create a data table that contains only district names and ids from the shape file
shape_names <- data.table(dist_name=shapeData@data$dist112_na, dist_id=shapeData@data$dist112)
str(shape_names)

# create a data table that contains district names, time, and suppression ratios
ratio_table <- uganda_vl[ , .(suppression_ratio=100*(sum(suppressed)/sum(valid_results)), 
                             district_name=as.character(district_name)) , by=.(district_name)]

ratio_table[, dist_name:=gsub('District','', district_name)]
ratio_table[, dist_name:=gsub(' ','', dist_name)]

# Change Luwero=Luweero and Sembabule=Ssembabule
ratio_table[dist_name=="Luwero", dist_name:="Luweero"]
ratio_table[dist_name=="Sembabule", dist_name:="Ssembabule"]

# check for unmatched values
ratio <- ratio_table[,unique(dist_name)]
shape <- shape_names[, unique(dist_name)]
ratio <- sort(ratio)
shape <- sort(shape)

shape[!shape %in% ratio]
ratio[!ratio %in% shape]

# remake ratio_table with only the variables you want
ratio_table <- ratio_table[,.(dist_name, suppression_ratio)]

#merge 
ratio_table <- merge(shape_names, ratio_table, by="dist_name")
ratio_table <- ratio_table[,.(dist_name, id=dist_id, suppression_ratio)]

# -----------------

#coordinates <- data.frame(id=rownames(shapeData@data), dist_id=shapeData@data$dist112) %>% 
 # left_join (ratio_table, by="dist_id")


# use the fortify function to convert from spatialpolygonsdataframe to data.frame
coordinates <- data.table(fortify(shapeData, region='dist112')) 
coordinates[, id:=as.numeric(id)]

# coordinates by year for faceting
coordinates_year <- rbind(coordinates, coordinates, coordinates, coordinates, coordinates)
coordinates_year[, year:=rep(2014:2018, each=nrow(coordinates))]

# merge on municipality names
coordinates <- merge(coordinates, ratio_table, by="id", all.x=TRUE)
coordinates_year <- merge(coordinates_year, ratio_table[,c('','')], by=c('id', 'year'), all.x=TRUE)

# store colors
mapcolors <- brewer.pal(8, 'Spectral')

# draw the polygons using ggplot2
ggplot(coordinates, aes(x=long, y=lat, group=group, fill=as.numeric(suppression_ratio))) + 
  geom_polygon() + 
  geom_path() + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=mapcolors) + 
  theme_void()






# use the fortify function to convert from spatialpolygonsdataframe to data.frame
coordinates = data.table(fortify(shapeData2, region='ID_2')) # use IDs instead of names

# merge on municipality names
names = data.table(shapeData@data)
coordinates = merge(coordinates, names, by.x='dist_id', by.y='dist_id')





# merge on the data (all.x=TRUE so the shapefile data doesn't disappear)




