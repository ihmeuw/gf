# -------------------------------------------------------
# David Phillips
# 
# 12/28/2018
# Example of map-making at HZ level with SNIS
# -------------------------------------------------------


# -------------------------------------------------------
# Set up R
rm(list=ls()) # clear all data in memory

# load packages
library(raster) # the raster package is great for all types of map-making functions
library(rgeos) # this package has additional functions for working with maps (like gSimplify)
library(ggplot2) # ggplot for making the graphics
library(RColorBrewer) # this package has nice colors, might need to install.packages('RColorBrewer')

# set directory 
dir <- 'C:/local/Data for Results Chain Maps/'
setwd(dir)
# -------------------------------------------------------


# ---------------------------------------------------------------------
# Import vaccine data 

# load data 
data <- readRDS('snis_base_data.rds')

# aggregate to health zone level by year and indicator
byVars <- c('former_dps_name','health_zone','year','element_eng')
data <- data[, sum(value, na.rm=TRUE), by=byVars]

# rename
setnames(data, 'V1', 'value')

# keep only 2017 since 2018 is incomplete
data <- data[year==2017]
# ---------------------------------------------------------------------


# --------------------------------------
# Subset data 

# subset to just one variable
elements <- c('A 1.4 Presumed malaria treated', 'A 1.4 Confirmed simple malaria treated')
subset1 <- data[element_eng %in% elements]

# add together both elements
byVars <- c('former_dps_name','health_zone','year')
subset1 <- subset1[, sum(value, na.rm=TRUE), by=byVars]
setnames(subset1, 'V1', 'value')

# make other subsets that you need
# --------------------------------------


# --------------------------------------------------------------------------
# Import shapefile 

# import shapefile (this one is stored as an rdata file type)
shapedata <- readRDS('hz_shapefile.rds')

# make a simplified version of the map so it goes faster
simpleshapedata <- gSimplify(shapedata, tol=.1)

# convert the shapefile to a data frame
shapedata_df <- fortify(simpleshapedata, region='index')

# convert index to character in the shapefile so the merge (below) works 
shapedata@data$index <- as.character(shapedata@data$index)

# merge the names of the health zones to the fortified data frame
shapedata_df <- merge(shapedata_df, shapedata@data, by.x='id', by.y='index')
# --------------------------------------------------------------------------


# --------------------------------------------------------------------------------------
# Merge the datasets 

# merge the data to the shape data
plotdata <- merge(shapedata_df, subset1, by.x=c('Name', 'PROVNAME'), 
	by.y=c('health_zone', 'former_dps_name'), all.x=TRUE)
	
# make sure the order is still preserved
plotdata <- plotdata[order(plotdata$order),]
# --------------------------------------------------------------------------------------


# ---------------------------------------------------------------------
# Make a basic map 

# get nicer colors for map
mapColors = brewer.pal(10, 'RdYlBu')

# most basic map
ggplot(plotdata, aes(x=long, y=lat, group=group, fill=value)) + # generates empty grid
	geom_polygon() + # this fills in the areas with colors
	geom_path(color='grey65', size=.05) + # this draws the borders
	scale_fill_gradientn(paste('Legend Title'), colours=mapColors, na.value='white') + # apply nicer colors
	theme_void() # this removes the axes
	
# if you need a log scale, do this instead of line 70:
# scale_fill_gradientn(paste('Legend Title'), colours=mapColors, na.value='white', trans='log', breaks=c(100, 1000, 10000, 100000))
# ---------------------------------------------------------------------
