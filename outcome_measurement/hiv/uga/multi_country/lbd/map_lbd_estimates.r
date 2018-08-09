# -----------------------------------------------------
# David Phillips
#
# 5/15/2017
# Make simple maps of HIV prevalence for PCE countries
# -----------------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(raster)
library(RColorBrewer)
library(ggplot2)
library(grid)
library(gridExtra)
# --------------------


# ----------------------------------------------------------------------------------------
# Files and directories

# data directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
outDir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/multi_country/lbd/')

# input files
inDir = '/share/geospatial/mbg/hiv/hiv_test/output/2018_05_07_10_11_14/'
inFile = paste0(inDir, 'hiv_test_mean_raked_raster.tif')

# shapefiles
shapeFileUGA = paste0(outDir, '../../../mapping/uga/uga_dist112_map.shp')
shapeFileCOD = paste0(outDir, '../../../mapping/cod/COD_adm3.shp')

# shapefile of lakes
shapeFileLakes = paste0(j, '/WORK/11_geospatial/06_original shapefiles/GLWD_lakes/glwd_1.shp')

# output file
graphFile = paste0(outDir, 'HIV_Prevalence_2018_05_07_10_11_14.pdf')
# ----------------------------------------------------------------------------------------


# ---------------------------------------------------------
# Load/prep data

# load shapefiles
mapUGA = shapefile(shapeFileUGA)
mapCOD = shapefile(shapeFileCOD)

# load raster data
rasterData = raster(inFile)

# load the ground cover data
lakes = shapefile(shapeFileLakes)

# mask the bodies of water
rasterData = mask(rasterData, lakes, inverse=TRUE)

# crop to the two countries
rasterDataUGA = crop(rasterData, extent(mapUGA))
rasterDataUGA = mask(rasterDataUGA, mapUGA)		
rasterDataCOD = crop(rasterData, extent(mapCOD))
rasterDataCOD = mask(rasterDataCOD, mapCOD)		

# convert to data tables
dataUGA = data.table(as.data.frame(rasterDataUGA, xy=TRUE))
dataCOD = data.table(as.data.frame(rasterDataCOD, xy=TRUE))
shapeDataUGA = data.table(fortify(mapUGA))
shapeDataCOD = data.table(fortify(mapCOD))

# rename
setnames(dataUGA, c('x','y','prev'))
setnames(dataCOD, c('x','y','prev'))
# ---------------------------------------------------------


# ---------------------------------------------------------
# Crop to the 99.9th percentile to avoid border effects in DRC

dataCOD[prev>quantile(prev, .999, na.rm=TRUE), prev:=NA]

# ---------------------------------------------------------


# ----------------------------------------------------------
# Set up to graph

# colors
cols1 = brewer.pal(6, 'PuOr')
border = 'grey65'

# legend limits so both countries are on same scale
lims = range(c(dataUGA$prev, dataCOD$prev), na.rm=TRUE)*100
# ----------------------------------------------------------


# -------------------------------------------------------------------------------
# Graph

# store maps
ugaprev = ggplot(dataUGA, aes(y=y, x=x, fill=prev*100)) + 
	geom_tile() + 
	geom_path(data=shapeDataUGA, aes(x=long, y=lat, group=group)
		, color=border, size=.05, inherit.aes=FALSE) + 
	scale_fill_gradientn('PLHIV %', colors=cols1, 
		na.value='white') + 
	coord_fixed(ratio=1) + 
	scale_x_continuous('', breaks = NULL) + 
	scale_y_continuous('', breaks = NULL) + 
	labs(title='Uganda') + 
	theme_minimal(base_size=16) + 
	theme(plot.title=element_text(hjust=.5)) 
	
codprev = ggplot(dataCOD, aes(y=y, x=x, fill=prev*100)) + 
	geom_tile() + 
	geom_path(data=shapeDataCOD, aes(x=long, y=lat, group=group)
		, color=border, size=.05, inherit.aes=FALSE) + 
	scale_fill_gradientn('PLHIV %', colors=cols1, 
		na.value='white') + 
	coord_fixed(ratio=1) + 
	scale_x_continuous('', breaks = NULL) + 
	scale_y_continuous('', breaks = NULL) + 
	labs(title='DRC') + 
	theme_minimal(base_size=16) + 
	theme(plot.title=element_text(hjust=.5), plot.margin=unit(rep(-1,4), 'cm')) 

# put maps together
p1 = arrangeGrob(codprev, ugaprev, ncol=2)	
# -------------------------------------------------------------------------------


# --------------------------------
# Save graphs
pdf(graphFile, height=6, width=9)
grid.newpage()
grid.draw(p1)
dev.off()
# --------------------------------
