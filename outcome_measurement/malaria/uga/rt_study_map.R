# Create maps of HMIS data 

# 21/8/2018
#-----------------------

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

# ----------------------------------------------
# upload the shape file

# set the working directory to your local drive
setwd("C:/Users/ccarelli/Documents/uga/")


# load the shapefile
# you will need additional files in the folder to support the .shp file
shapeData <- shapefile('uga_dist112_map.shp')

# check that shapeData is a spatialpolygonsdataframe
# this should print out spatialpolygonsdataframe
class(shapeData)

# plot the shape file in the base package
plot(shapeData)

# simplify the shape data (could create little gaps, maybe don't do this)
gSimplify(shapeData, tol=0.5, topologyPreserve=TRUE)
plot(shapeData)


#--------------------------------------------
# determine which districts are included in the shape file

# print a list of all of the districts in the map 
unique(shapeData@data$dist112_na)

# confirm that 112 districts are included in the map
length(unique(shapeData@data$dist112_na))

# create a data table that contains only district names and ids from the shape file
# you can match based on name, but it's better to match on id 
# matching on id ensures you don't miss any districts when your merge data with the shape file
shape_names <- data.table(district_name=shapeData@data$dist112_na, id=shapeData@data$dist112)

# view the list of district names and ids
str(shape_names)

#-------------------------------------------
# before making a map, clean and check for outliers in your data
# you should only merge prepped, cleaned data with shape files


shape_names[ , rt:='No']
shape_names[district_name=='Jinja' | district_name=='Sheema' | district_name=='Bushenyi' | district_name=='Luweero' | district_name=='Oyam' | district_name=='Gulu', rt:='Yes']


rt_colors <- c('#d9d9d9', '#feb24c')

# MERGE THEM

coordinates <- data.table(fortify(shapeData, region='dist112')) 
coordinates[, id:=as.numeric(id)]
coordinates <- merge(coordinates, shape_names, by="id", all.x=TRUE)

ggplot(coordinates, aes(x=long, y=lat, group=group, fill=factor(rt))) + 
  geom_polygon() + 
  geom_path(size=0.01, color="#636363") + 
  theme_void() + 
  labs(title="Resource Tracking Study Districts",
       fill="RT District") +
  scale_fill_manual(values=rt_colors) +
  theme(plot.title=element_text(vjust=-4), plot.subtitle=element_text(vjust=-4), 
        plot.caption=element_text(vjust=6)) + coord_fixed()



#-------------------------------------------------

shape_names[ ,mock:=runif(112, min=10, max=50)]
shape_names[ ,mock:=round(mock,digits=0)]



