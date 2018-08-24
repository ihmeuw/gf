# ---------------------------------------------------
# David Phillips
# 
# 1/15/2018
# Code that makes one percent change map
# This is intended to be adapted to make other pct change maps
# ---------------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2) 
library(RColorBrewer) 
library(raster) 
# --------------------


# ----------------------------------------------------------------
# Store file and folder names

# root directory. CHANGE THIS TO THE LOCATION OF YOUR DATA
dir = 'C:/Users/davidp6/Google Drive/Work/IHME Work/GF/Workshops/uga_jan2018/SAE Exploration/raw files'

# change working directory to the root directory
setwd(dir)

# store raster file names
deathFile = 'children_malaria_deaths_mean_2015.tif'
deathRateFile = 'mortality_rate_per_10k_children_2015.tif'
incFile = '2015_all_ages_incidence_count_mean.tif'
prevFile = 'MODEL43.2015.PR.rmean.stable.tif'

# store shapefile names
shapeFileUGA = 'uga_dist112_map.shp'
shapeFileCOD = 'COD_adm3.shp'

# shapefile of lakes
shapeFileLakes = 'J:/WORK/11_geospatial/06_original shapefiles/GLWD_lakes/glwd_1.shp'

# output file name
outFile = '../../SAE Results Production/deaths_per_case_maps.pdf'
# ----------------------------------------------------------------
	

# ----------------------------------------------------
# Load prep/data

# load each file
deathRaster = raster(deathFile)
deathRateRaster = raster(deathRateFile)
incRaster = raster(incFile)
prevRaster = raster(prevFile)

# load shapeFile
shapeData = shapefile(shapeFileCOD)

# load the ground cover data
lakes = shapefile(shapeFileLakes)

# mask the bodies of water
deathRaster = mask(deathRaster, lakes, inverse=TRUE)
deathRateRaster = mask(deathRateRaster, lakes, inverse=TRUE)
incRaster = mask(incRaster, lakes, inverse=TRUE)
prevRaster = mask(prevRaster, lakes, inverse=TRUE)

# clip each raster to the country (i.e delete data from outside Uganda)
deaths = crop(deathRaster, extent(shapeData))
deaths = mask(deaths, shapeData)
deathRates = crop(deathRateRaster, extent(shapeData))
deathRates = mask(deathRates, shapeData)
inc = crop(incRaster, extent(shapeData))
inc = mask(inc, shapeData)
prev = crop(prevRaster, extent(shapeData))
prev = mask(prev, shapeData)

# project the deaths raster to the incidence raster
deathRates = projectRaster(deathRates, inc)
deaths = projectRaster(deaths, inc)
prev = projectRaster(prev, inc)

# format the rasters as data.tables
deathRates = data.table(as.data.frame(deathRates, xy=TRUE))
deaths = data.table(as.data.frame(deaths, xy=TRUE))
inc = data.table(as.data.frame(inc, xy=TRUE))
prev = data.table(as.data.frame(prev, xy=TRUE))
setnames(deathRates, c('x','y','deathRates'))
setnames(deaths, c('x','y','deaths'))
setnames(inc, c('x','y','inc'))
setnames(prev, c('x','y','prev'))

# check if every pixel is identical between the two files
# IF ANY OF THESE ARE TRUE THE CODE WILL BREAK AND WE WILL NEED TO DO SOME WORK TO FIX IT
if(any(!deaths$x %in% inc$x)) stop('There\'s a value of x in deaths that isn\'t in incidence')
if(any(!inc$x %in% deaths$x)) stop('There\'s a value of x in incidence that isn\'t in deaths')
if(any(!prev$x %in% deaths$x)) stop('There\'s a value of x in prev that isn\'t in deaths')
if(any(!deaths$y %in% inc$y)) stop('There\'s a value of y in deaths that isn\'t in incidence')
if(any(!inc$y %in% deaths$y)) stop('There\'s a value of y in incidence that isn\'t in deaths')
if(any(!prev$y %in% deaths$y)) stop('There\'s a value of y in prev that isn\'t in deaths')

# merge the two years together
data = merge(deaths, inc, by=c('x','y'))
data = merge(data, deathRates, by=c('x','y'))
data = merge(data, prev, by=c('x','y'))

# make sure there are no negatives (child death rates somehow have them)
data[deathRates<0, deathRates:=NA]
data[deaths<0, deaths:=NA]
data[inc<0, inc:=NA]
data[prev<0, prev:=NA]

# double check that the merge worked perfectly
dim(deathRates)
dim(deaths)
dim(inc)
dim(prev)
dim(data)
# ----------------------------------------------------


# -------------------------------------------------------------------
# Compute ratio

# compute "case fatality"
data[, ratio_inc:=(deaths/inc)*1000]
data[, ratio_prev:=((deathRates/prev)/10000)*1000] # first divide by 10,000 because the rates are per 10,000 and the prevalence is per population
data[!is.finite(ratio_prev), ratio_prev:=NA]

# truncate the scales to avoid outlier pixels (probably from neighboring countries)
quants = quantile(data$ratio_inc, c(.01, .99), na.rm=TRUE)
data[ratio_inc<quants[1], ratio_inc:=quants[1]]
data[ratio_inc>quants[2], ratio_inc:=quants[2]]
quants = quantile(data$ratio_prev, c(.01, .99), na.rm=TRUE)
data[ratio_prev<quants[1], ratio_prev:=quants[1]]
data[ratio_prev>quants[2], ratio_prev:=quants[2]]

# reshape long for easier mapping
data = melt(data, id.vars=c('x','y'))
# -------------------------------------------------------------------


# ----------------------------------------------------------------
# Set up to graph

# set up colors
colors = rev(brewer.pal(10, 'RdYlBu'))

# label indicators
data[variable=='deaths', label:='Child Deaths']
data[variable=='inc', label:='New Cases']
data[variable=='ratio_inc', label:='Child Deaths per Case']
data[variable=='ratio_prev', label:='Child Deaths per Prevalent Case (in 10,000\'s)']
# ----------------------------------------------------------------


# ----------------------------------------------------------------
# Map each raster

# map deaths per incident case
p1 = ggplot() + 
	geom_tile(data=data[variable=='ratio_inc'], aes(y=y, x=x, fill=value)) + 
	geom_path(data=shapeData, color='grey65', size=.05, 
		aes(y=lat, x=long, group=group), inherit.aes=FALSE) + 
	scale_fill_gradientn(colors=colors, na.value='white') + 
	coord_fixed(ratio=1) + 
	scale_x_continuous('', breaks = NULL) + 
	scale_y_continuous('', breaks = NULL) + 
	labs(title='Child Deaths per 1,000 Incident Cases', fill='') + 
	theme_minimal(base_size=16) + 
	theme(plot.title=element_text(hjust=.5)) 

# map deaths per prevalent case
p2 = ggplot() + 
	geom_tile(data=data[variable=='ratio_prev'], aes(y=y, x=x, fill=value)) + 
	geom_path(data=shapeData, color='grey65', size=.05, 
		aes(y=lat, x=long, group=group), inherit.aes=FALSE) + 
	scale_fill_gradientn(colors=colors, na.value='white') + 
	coord_fixed(ratio=1) + 
	scale_x_continuous('', breaks = NULL) + 
	scale_y_continuous('', breaks = NULL) + 
	labs(title='Child Deaths per 1,000 Prevalent Cases', fill='') + 
	theme_minimal(base_size=16) + 
	theme(plot.title=element_text(hjust=.5)) 
# ----------------------------------------------------------------


# -------------------------------
# Save the maps
pdf(outFile, height=6, width=10)
p1
p2
dev.off()
# -------------------------------
