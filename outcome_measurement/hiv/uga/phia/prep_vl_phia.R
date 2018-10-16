# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# Transform the VL data to match the PHIA data regions
# creates corresponding maps and exports prepped code

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
setwd(paste0(root, '/Project/Evaluation/GF/mapping/uga/'))

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
# assign the relevant regions to the districts and facilities 

# import the ten regions that are included in phia
regions = fread(paste0(root, "/Project/Evaluation/GF/mapping/uga/uga_geographies_map.csv"))
regions = unique(regions[ ,.(region=region10_alt, district_name=dist112_name)])

# subset to the relevant dates
vl = uvl[date>= '2016-08-01' & date <='2017-03-01']

#------------------------------------------------
# export a facility level data set 

# merge the names of regions and their associated districts with the vl data 
vl = merge(vl, regions, by='district_name')

# export a facility level data set 
vl = vl[ ,.(suppressed=sum(suppressed), valid_results=sum(valid_results)), by=.(facility_name, district_name, region)]

# export the file to analyze - use the names in the shape file 
#saveRDS(vl, paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/phia_2016/prepped/vl_data.rds'))

#------------------------------------------
# aggregate districts into regions
vl = vl[ ,.(suppressed=sum(suppressed), valid_results=sum(valid_results)), by=region]

# calculate the suppression ratio
vl[ , ratio:=round(100*(suppressed/valid_results), 1), by=region]

# put the regions in the same order as the shape file
regions = regions[match(shapeData@data$dist112_n, regions$district_name)]
id = regions$region

# create coordinates for the old and new plots
shapeDataNew = unionSpatialPolygons(shapeData, id)

#fortify 
coordinates_new = fortify(shapeDataNew)

# merge the data with the coordinates
setnames(vl, 'region', 'id')
coordinates_new = merge(coordinates_new, vl, by='id')
coordinates_new = data.table(coordinates_new)

# identify centroids and label them
names = data.table(coordinates(shapeDataNew))
setnames(names, c('long', 'lat'))
name = unique(coordinates_new$id)
names = cbind(names, name)

# merge in the ratios to create complete labels
vl_new = vl[ ,.(name=id, ratio)]
names = merge(vl_new, names, by='name')

# fix the names to match the phia graphic
names$name = gsub(names$name, pattern='_', replacement='-')
names[grep('^Central', name), name:=(gsub(name, pattern='-', replacement=' '))]
names[grep('^West', name), name:=(gsub(name, pattern='-', replacement=' '))]

# create complete labels
names[ ,name:=paste0(name, ': ', ratio, '%')]
names[ ,ratio:=NULL]

# ---------------
# graph the same time period as the phia graph in uganda vl 

# store colors
ratio_colors = brewer.pal(6, 'BuGn')

# set legend breaks
breaks = c(80, 83, 86, 89)

# export the comparison map as a pdf
# pdf(paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/phia_2016/vl_comparison_map.pdf'), height=6, width=9)

# map of regions 
ggplot(coordinates_new, aes(x=long, y=lat, group=group, fill=ratio)) + 
  geom_polygon() + 
  scale_fill_gradientn(colors=ratio_colors, breaks=breaks) + 
  theme_void() +
  coord_fixed() +
  labs(fill='VLS') +
  geom_label_repel(data = names, aes(label = name, x = long, y = lat, group = name), inherit.aes=FALSE, size=5)
  
# dev.off()

#---------------------------------------------------------------

#---------------------------------------------------------------
# map 2011 or 2016 aids indicator survey estimates 
# these data are cleaned in the analysis function 

# select the survey or projected estimates and import the data 
ais = readRDS('J:/Project/Evaluation/GF/outcome_measurement/uga/phia_2016/prepped/ais_data.rds')
setnames(ais, 'region', 'id')

# round estimates and format as percentages
ais[ , art_coverage:=round(100*art_coverage, 1)]
ais[ , art_coverage_2011:=round(100*art_coverage_2011, 1)]

# create a set of art coverage colors
art_colors = brewer.pal(8, 'Spectral')

#--------------------------
# create regional labels 

# identify centroids and label them
ais_names = data.table(coordinates(shapeDataNew))
setnames(ais_names, c('long', 'lat'))
ais_names[ , id:=unique(coordinates_new$id)]

# merge in the ratios for complete labels
ais_names = merge(ais_names, ais, by='id')

# replace labels with hyphens to match phia graphics
ais_names$id = gsub(ais_names$id, pattern='_', replacement='-')
ais_names[grep('^Central', id), id:=(gsub(id, pattern='-', replacement=' '))]
ais_names[grep('^West', id), id:=(gsub(id, pattern='-', replacement=' '))]

# create labels with region names and art coverage ratios
ais_names[ ,tag:=paste0(id, ': ', art_coverage, '%' )]
ais_names[ ,tag_2011:=paste0(id, ': ', art_coverage_2011, '%' )]

# shape long
ais_names_2011 = ais_names[ ,.(id, long, lat, variable=tag_2011, year=2011)]
ais_names_2016 = ais_names[ ,.(id, long, lat, variable=tag, year=2016)]
ais_names = rbind(ais_names_2011, ais_names_2016)

#----------------------------
# repeat to facet wrap
coordinates_ais = merge(coordinates_new, ais, by='id')

# separate by years and shape long to facet wrap
art_2016 = coordinates_ais[ ,.(id, long, lat, order, hole, piece, group, variable=art_coverage)]
art_2016[ , year:=2016]
art_2011 = coordinates_ais[ ,.(id, long, lat, order, hole, piece, group, variable=art_coverage_2011)]
art_2011[ , year:=2011]
coordinates_ais = rbind(art_2011, art_2016)


#print out a comparative map of original and projected estimates
# pdf(paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/phia_2016/art_coverage_comparison_maps.pdf'), height=6, width=12)

# map of regions 
ggplot(coordinates_ais, aes(x=long, y=lat, group=group, fill=variable)) + 
  geom_polygon() + 
  scale_fill_gradientn(colors=art_colors) + 
  theme_void() +
  coord_fixed() +
  facet_wrap(~year) +
  labs(fill='ART Coverage(%)') +
  geom_label_repel(data = ais_names, aes(label = variable, x = long, y = lat, group = variable), inherit.aes=FALSE)


# dev.off()

#---------------------------------------------------------------




  