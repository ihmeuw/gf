# ----------------------------------------------
# Making vector maps in R
#
# 10/8/2018
# Create a map of the VL Dashboard
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

# --------------------
# Files and directories

# set the input/output directory
dir <- ('C:/Users/ccarelli/Documents/uga_shape')


# ----------------------------------------------
# Shape file - the map
# upload the shape file

# set the working directory (where R is operating) to the input/output directory
setwd(dir)

# load the shapefile
# the other files, like .shx, most be in the uga_shape folder for the .shp file to load
shapeData <- shapefile('uga_dist112_map.shp')

# check that shapeData is a spatialpolygonsdataframe
# a spatialpolygons data frame is a type of mapping file
class(shapeData)

# plot the shape file to make sure it worked
# you should see uganda on the right
plot(shapeData)

# now, take the 'data' in the map and make it into a data set
# use the fortify function to convert from spatialpolygonsdataframe to data.frame
coordinates <- data.table(fortify(shapeData, region='dist112')) 
coordinates[, id:=as.numeric(id)] 

#---------------------------------------------------
# data to merge with shape file
# now that you have shape data for a map, you need a data set

# upload the viral load dashboard data 
# paste0 pastes the test stored in 'dir' with the following text with no spaces
vl <- readRDS(paste0(dir, "/vl_data.rds"))

# view the data set and variable names
str(vl)

# print out the data set
vl

#---------------------------
# reshape the data set - sum up the totals by year

# these data are great! but we want annual, not monthly maps
# the data are divided by month
# so let's change them to annual data 

# creat a year variable
# dates work like numbers in R - if you say 'less than' a date, it will take all dates before that
vl[date < '2017-01-01', year:=2016]
vl[date < '2018-01-01' & date > '2016-12-01', year:=2017] # make sure you get the right years!
vl[date > '2017-12-01', year:=2018]

# pro tip: you can also do with this grep:
# vl[grep(date, pattern='2018'), year:=2018]

# check that all three years are there
vl[ ,unique(year)]

# sum up the variables by distrist!
# be sure to include both year and district, since we want an annual total for each district
vl <- vl[ ,.(suppressed=sum(suppressed), valid_results=sum(valid_results)), by=.(year, district_name)]

# there should be 112 districts to match the shape file
length(unique(vl$district_name)) # yayy! there are 112

# pop quiz: how many rows should there be?

#---------------------------
# let's calculate a ratio, just for fun 

# calculate the viral suppression ratio:
# number suppressed/valid test results
vl[ , ratio:=100*(suppressed/valid_results)]

#---------------------------
# merge the data with the shape file
# hardest part: check if the names of the districts you have are in the shape file 

# identify the variable that contains district names and codes
# as_tibble() works like view - you can see the top of the data set 
# identify the variable with the names of the districts - in this case, dist112.na
as_tibble(shapeData@data) 

# shape files are strange - the names are usually stored in shapeData@data
unique(shapeData@data$dist112_na)

# check for unmatched district names in the shape file
# this code prints unmatched districts
unique(vl$district_name)[!(unique(vl$district_name)) %in% unique(shapeData@data$dist112_na)]

# create a data table that contains only district names and ids from the shape file
shape_names <- data.table(district_name=shapeData@data$dist112_na, id=shapeData@data$dist112)
str(shape_names)

# merge the names and ids from the shape file with your data 
# now VL has the 'ids' of the districts to merge the data with the shape file 
vl <- merge(vl, shape_names, by='district_name', all=T)


#----------------------------
# merge the data and the shape file 

# let's try an easy map first - just 2018
test = vl[year==2018]

# merge it with coordinates - the data table that contains the info in the map
test_map <- merge(coordinates, test, by='id', all=T)

# print it out - it has the map coordinates AND the data!
test_map

# MAP it! 
# set the first argument of ggplot() to your newly merged data set - test_map
# set 'fill' to the variable we want - in this case, the ratio
ggplot(test_map, aes(x=long, y=lat, group=group, fill=ratio)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  theme_void() 

#------------------------------
# Pop quiz - change the map to a different variable
# I also added code that makes it pretty

# you can create color palettes that you like
pretty_colors <- brewer.pal(9, 'Purples')

# which variable do you want to map?
# ggplot(???, aes(x=long, y=lat, group=group, fill=???)) + 
#   coord_fixed() +
#   geom_polygon() + 
#   geom_path(size=0.01) + 
#   theme_void() +
#   scale_fill_gradientn(colors=pretty_colors) +
#   labs(title="Number of valid test results by district, Uganda", 
#        caption="Source: Uganda Viral Load Dashboard", 
#        fill="Valid results") 


#-------------------------------------------------------------------
# Harder maps: facet wrapping by year

# this is the same, but you need to merge the map data with your data once for every year

# first, repeat the map data once for each year:
# in this case, we need it to repeat three times for three years
coordinates_year <- rbind(coordinates, coordinates, coordinates)

# for every row, add a year so we can merge by year
# this uses the repeat function: basically it repeats year over and over
coordinates_year[, year:=rep(2016:2018, each=nrow(coordinates))]

# now take the original data and merge it with coordinates_year
# this time, we have multiple years - so we need to make sure we have map data for each year
vl_map <- merge(coordinates_year, vl, by=c('id', 'year'), all=T)

# look at vl_map - did it work?
vl_map

# choose some nice colors
ratio_colors <- brewer.pal(8, 'Spectral')

# map it!!
# same code as before, but use facet_wrap() by year
# let's map the viral suppression ratio
ggplot(vl_map, aes(x=long, y=lat, group=group, fill=ratio)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01) +
  theme_void() +
  facet_wrap(~year) +
  scale_fill_gradientn(colors=ratio_colors) +
  labs(title="Viral suppression ratio by district, Uganda",
       caption="Source: Uganda Viral Load Dashboard",
       fill="% with virally suppressed")

# AWESOME! Now try it with your data
#------------------------------
