# -------------------------------------------------------------------
# Author: Francisco Rios Casas, adapted from code by Ellen Squires
# Date: July 27, 2019
# Description: sample code for making maps using Senegal data
# ---------------------------------------------------------------------

# -------------------------------------------------------
# set-up
library(ggplot2)
library(raster)
library(data.table)

# set directory 
dir <- "C:/Users/frc2/Documents/gf/mapping/sen"
setwd(dir)
# -------------------------------------------------------


# ------------------------------------------------
# import Senegal TB data
tbdata <- readRDS("C:/Users/frc2/Documents/data/tb/prepped_data/sen_tb_indicators_2.RDS")

# import region code names
senegal_regions <- fread('region_map_code_book.csv')

# specify data to plot
# 2016 
# Cas TB Toutes Formes Confondues (TFC)	Cases TB: all forms detected
# tb_tfc
# by region

DT1 = tbdata[annee=="2016", .(tb_tfc=sum(tb_tfc, na.rm=T)), by=c('region')]

# merge region codes to tb-data
DT1 <- merge(DT1, senegal_regions, by.x="region", by.y="tb_region")

#---------------------------------------------------
# import shapefile

# load shapefile for regions
mapdata = shapefile('shapefiles/gadm36_SEN_1.shp')

# check if names match
unique(DT1$GID_1) %in% unique(mapdata@data$GID_1)

# use the ggplot "fortify" function to convert the spatialpolygonsdataframe to a normal data frame
mapdata <- fortify(mapdata, region='GID_1')

# ---------------------------------------------------------------------
# merge map data with data that will be plotted
plotdata <- merge(mapdata, DT1, by.x="id", by.y="GID_1", all.x=TRUE)

# ---------------------------------------------------------------------
# Make a basic map 
plotdata = plotdata[order(plotdata$order),] # this to fix "polygon tearing"

# most basic map
ggplot(plotdata, aes(x=long, y=lat, group=group)) + # generates empty grid
  geom_polygon(aes(fill=tb_tfc)) # draws polygons
# ---------------------------------------------------------------------

# ----------------------------------------------------------------------------
# Make a better map 

# settings - color 
mapColors = colorRampPalette(c('#FFEEDC', '#FAA61A', '#5BA7B1', '#2D358E', '#1C1D48'))
mapColors = mapColors(10)

# settings - make the scale 
scale_limits <- c(0, 6000)
scale_breaks <- seq(0, 6000, by=1000)
scale_labels <- c("0", "1000", "2000", "3000", "4000", "5000", "6000")

# map coverage 
ggplot(data=plotdata, aes(x=long, y=lat, group=group)) + # generates empty grid, sets default dataset for plotting
  geom_polygon(aes(fill=tb_tfc)) + # overlays map on top of grid
  theme_minimal(base_size=16)  +  # sets grid/background to white and sets base font size
  geom_path(color="grey95", size=.05) + # sets color for district borders
  coord_fixed(ratio=1) + # keep proper aspect ratio
  scale_x_continuous("", breaks = NULL) + scale_y_continuous("", breaks = NULL) + #Label axes & remove grid lines
  ggtitle("Senegal, TB Toutes Formes Confondues (TFC), 2016") + #Map title
  scale_fill_gradientn(paste("Cases"), colours=mapColors, na.value = "white", limits=scale_limits, breaks=scale_breaks, labels=scale_labels) # defines the color scale / legend 
# ----------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# Make multiple maps

# reload the dataset
tbdata <- readRDS("C:/Users/frc2/Documents/data/tb/prepped_data/sen_tb_indicators_2.RDS")

# go back and subset to two or more years
DT1 <- tbdata[annee %in% c(2016, 2018), .(tb_tfc=sum(tb_tfc, na.rm=T)), by=c('region', 'annee')]

# merge the data again. this creates duplicates on purpose!
mapdata = shapefile('shapefiles/gadm36_SEN_1.shp')
mapdata <- fortify(mapdata, region='GID_1')
DT1 <- merge(DT1, senegal_regions, by.x="region", by.y="tb_region") # merge region codes to tb-data
plotdata <- merge(mapdata, DT1, by.x="id", by.y="GID_1")
# plotdata = plotdata[order(plotdata$order),] # use this to fix "polygon tearing"

# now make side by side maps using ggplot
ggplot(data=plotdata, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=tb_tfc)) + 
  facet_wrap(~annee) # the facet_wrap option for ggplot makes graphs side-by-side. you just enter a formula (~) and the variable you want to use to repeat the graphs

# more formatting on maps

# settings - color 
mapColors = colorRampPalette(c('#FFEEDC', '#FAA61A', '#5BA7B1', '#2D358E', '#1C1D48'))
mapColors = mapColors(10)

# settings - make the scale 
scale_limits <- c(0, 6000)
scale_breaks <- seq(0, 6000, by=1000)
scale_labels <- c("0", "1000", "2000", "3000", "4000", "5000", "6000")

# map coverage 
ggplot(data=plotdata, aes(x=long, y=lat, group=group)) + # generates empty grid, sets default dataset for plotting
  geom_polygon(aes(fill=tb_tfc)) + # overlays map on top of grid
  facet_wrap(~annee) + 
  theme_minimal(base_size=16)  +  # sets grid/background to white and sets base font size
  geom_path(color="grey95", size=.05) + # sets color for district borders
  coord_fixed(ratio=1) + # keep proper aspect ratio
  scale_x_continuous("", breaks = NULL) + scale_y_continuous("", breaks = NULL) + #Label axes & remove grid lines
  ggtitle("Senegal, TB Toutes Formes Confondues (TFC)") + #Map title
  scale_fill_gradientn(paste("Cases"), colours=mapColors, na.value = "white", limits=scale_limits, breaks=scale_breaks, labels=scale_labels) # defines the color scale / legend 

# ----------------------------------------------------------------------------

