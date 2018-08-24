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
itnFile2015 = 'mortality_rate_per_10k_children_2015.tif'
itnFile2000 = 'mortality_rate_per_10k_children_2000.tif'

# store shapefile names
shapeFileUGA = 'uga_dist112_map.shp'
shapeFileCOD = 'COD_adm3.shp'

# shapefile of lakes
shapeFileLakes = 'J:/WORK/11_geospatial/06_original shapefiles/GLWD_lakes/glwd_1.shp'

# output file name
outFile = '../../SAE Results Production/death_rate_pct_change_maps_2000_2015.pdf'
# ----------------------------------------------------------------


# ----------------------------------------------------
# Load prep/data

# load each file
itnRaster2015 = raster(itnFile2015)
itnRaster2000 = raster(itnFile2000)

# load shapeFile
shapeData = shapefile(shapeFileCOD)

# load the ground cover data
lakes = shapefile(shapeFileLakes)

# mask the bodies of water
itnRaster2015 = mask(itnRaster2015, lakes, inverse=TRUE)
itnRaster2000 = mask(itnRaster2000, lakes, inverse=TRUE)

# clip each raster to the country (i.e delete data from outside Uganda)
itn2015 = crop(itnRaster2015, extent(shapeData))
itn2015 = mask(itn2015, shapeData)
itn2000 = crop(itnRaster2000, extent(shapeData))
itn2000 = mask(itn2000, shapeData)

# format the rasters as data.tables
itn2015 = data.table(as.data.frame(itn2015, xy=TRUE))
itn2000 = data.table(as.data.frame(itn2000, xy=TRUE))
setnames(itn2015, c('x','y','itn2015'))
setnames(itn2000, c('x','y','itn2000'))

# check if every pixel is identical between the two files
# IF ANY OF THESE ARE TRUE THE CODE WILL BREAK AND WE WILL NEED TO DO SOME WORK TO FIX IT
if(any(!itn2015$x %in% itn2000$x)) stop('There\'s a value of x in 2015 that isn\'t in 2000')
if(any(!itn2000$x %in% itn2015$x)) stop('There\'s a value of x in 2000 that isn\'t in 2015')
if(any(!itn2015$y %in% itn2000$y)) stop('There\'s a value of y in 2015 that isn\'t in 2000')
if(any(!itn2000$y %in% itn2015$y)) stop('There\'s a value of y in 2000 that isn\'t in 2015')

# merge the two years together
itn = merge(itn2015, itn2000, by=c('x','y'))

# make sure there are no negatives (child death rates somehow have them)
itn[itn2015<0, itn2015:=NA]
itn[itn2000<0, itn2000:=NA]

# double check that the merge worked perfectly
dim(itn2015)
dim(itn2000)
dim(itn)
# ----------------------------------------------------


# -------------------------------------------------------------------
# Compute percent change

# regular percent change would be 2015/2000, 
# but we want "annualized" percent change: log(itn2015/itn2000)/(2015-2000)
itn[, pct_change:=log(itn2015/itn2000)/(2015-2000)]

# truncate the scales to avoid outlier pixels (probably from neighboring countries)
quants = quantile(itn$pct_change, c(.01, .99), na.rm=TRUE)
itn[pct_change<quants[1], pct_change:=quants[1]]
itn[pct_change>quants[2], pct_change:=quants[2]]

# reshape long for easier mapping
itn = melt(itn, id.vars=c('x','y'))
# -------------------------------------------------------------------


# ----------------------------------------------------------------
# Set up to graph

# set up colors
colors = rev(brewer.pal(10, 'RdYlBu'))

# label indicators
itn[variable=='itn2015', label:='2015']
itn[variable=='itn2000', label:='2000']
itn[variable=='pct_change', label:='% Change']
# ----------------------------------------------------------------


# ----------------------------------------------------------------
# Map each raster

# store both years side-by-side
p1 = ggplot() + 
	geom_tile(data=itn[label=='2015'], aes(y=y, x=x, fill=value*100)) + 
	geom_path(data=shapeData, color='grey65', size=.05, 
		aes(y=lat, x=long, group=group), inherit.aes=FALSE) + 
	scale_fill_gradientn(colors=colors, na.value='white') + 
	coord_fixed(ratio=1) + 
	# facet_wrap(~label, ncol=3) + 
	scale_x_continuous('', breaks = NULL) + 
	scale_y_continuous('', breaks = NULL) + 
	labs(title='Child Deaths per 10,000', fill='') + 
	theme_minimal(base_size=16) + 
	theme(plot.title=element_text(hjust=.5)) 

# store a percent change map
p2 = ggplot() + 
	geom_tile(data=itn[label=='% Change'], aes(y=y, x=x, fill=value*100)) + 
	geom_path(data=shapeData, color='grey65', size=.05, 
		aes(y=lat, x=long, group=group), inherit.aes=FALSE) + 
	scale_fill_gradientn(colors=colors, na.value='white') + 
	coord_fixed(ratio=1) + 
	scale_x_continuous('', breaks = NULL) + 
	scale_y_continuous('', breaks = NULL) + 
	labs(title='Percent Change', fill='') + 
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
