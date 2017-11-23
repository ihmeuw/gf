# --------------------------------------------------------
# David Phillips
#
# 11/17/2017
# Make simple maps of malaria indicators for PCE countries
# --------------------------------------------------------

# note: geom_tile has a bug when combined with facet_wrap
# https://github.com/tidyverse/ggplot2/issues/849

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(raster)
library(rgdal)
library(rgeos)
library(RColorBrewer)
library(ggplot2)
library(grid)
library(gridExtra)
# --------------------


# ----------------------------------------------
# Files and directories

# data directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/multi_country/map/')

# input files
actDir = paste0(dir, 'Africa Cube Public Data/ACT_use_rasters/rasters/rasters/')
itnDir = paste0(dir, 'Africa Cube Public Data/ITN_use_year_average_adj_rasters/rasters/')
irsDir = paste0(dir, 'Africa Cube Public Data/IRS_use_rasters/rasters/')
antmalDir = paste0(dir, 'NEJM Rasters/spatially_disaggregated_antimalarials/')
actFiles = paste0(artDir, list.files(actDir)[grepl('tif', list.files(artDir))])
itnFiles = paste0(itnDir, list.files(itnDir)[grepl('tif', list.files(itnDir))])
irsFiles = paste0(irsDir, list.files(irsDir)[grepl('tif', list.files(irsDir))])
antmalFiles = paste0(antmalDir, list.files(antmalDir)[!grepl('ovr', list.files(antmalDir))])
antmalFiles = antmalFiles[!grepl('xml', antmalFiles)]

# shapefiles
shapeFileUGA = paste0(dir, '../../../mapping/uga/uga_dist112_map.shp')
shapeFileCOD = paste0(dir, '../../../mapping/cod/COD_adm3.shp')

# output files
graphFile = paste0(dir, '/visualizations/graphs.pdf')
# ----------------------------------------------


# ----------------------------------------------------------
# Load/prep data

# load data
rasterData = raster(itnFiles[grepl('2015', itnFiles)])

# load shapefile
mapUGA = shapefile(shapeFileUGA)
mapCOD = shapefile(shapeFileCOD)

# simplify shapefiles for speed
mapUGA = gSimplify(mapUGA, tol=0.01, topologyPreserve=TRUE)
mapCOD = gSimplify(mapCOD, tol=0.01, topologyPreserve=TRUE)

# clip to Uganda/DRC
rasterDataUGA = crop(rasterData, extent(mapUGA))
rasterDataUGA = mask(rasterDataUGA, mapUGA)
rasterDataCOD = crop(rasterData, extent(mapCOD))
rasterDataCOD = mask(rasterDataCOD, mapCOD)

# format polygons as data.table
shapeDataUGA = data.table(fortify(mapUGA))
shapeDataCOD = data.table(fortify(mapCOD))

# format rasters as data.table
dataUGA = data.table(as.data.frame(rasterDataUGA, xy=TRUE))
dataCOD = data.table(as.data.frame(rasterDataCOD, xy=TRUE))
setnames(dataUGA, c('x','y','itn2014','country'))
setnames(dataCOD, c('x','y','itn2014','country'))
# ----------------------------------------------------------


# ----------------------------------------------------------------------
# Set up to graph

# colors
cols = brewer.pal(6, 'Spectral')
border = 'grey65'

# legend limits
min = floor(min(c(dataUGA$itn2014, dataCOD$itn2014), na.rm=TRUE)*100)
max = ceiling(max(c(dataUGA$itn2014, dataCOD$itn2014), na.rm=TRUE)*100)
# ----------------------------------------------------------------------


# ----------------------------------------------
# Graph

# store maps separately because geom_tile bug with facetting
uga = ggplot(dataUGA, aes(y=y, x=x, fill=itn2014*100)) + 
	geom_tile() + 
	geom_path(data=shapeDataUGA, aes(x=long, y=lat, group=group)
		, color=border, size=.05, inherit.aes=FALSE) + 
	scale_fill_gradientn('% ITN\nUsage', colors=cols, 
		na.value='white', limits=c(min,max)) + 
	coord_fixed(ratio=1) + 
	scale_x_continuous('', breaks = NULL) + 
	scale_y_continuous('', breaks = NULL) + 
	labs(title='Uganda') + 
	theme_minimal(base_size=16) + 
	theme(plot.title=element_text(hjust=.5)) 
	
cod = ggplot(dataCOD, aes(y=y, x=x, fill=itn2014*100)) + 
	geom_tile() + 
	geom_path(data=shapeDataCOD, aes(x=long, y=lat, group=group)
		, color=border, size=.05, inherit.aes=FALSE) + 
	scale_fill_gradientn('% ITN\nUsage', colors=cols, 
		na.value='white', limits=c(min,max)) + 
	coord_fixed(ratio=1) + 
	scale_x_continuous('', breaks = NULL) + 
	scale_y_continuous('', breaks = NULL) + 
	labs(title='DRC') + 
	theme_minimal(base_size=16) + 
	theme(plot.title=element_text(hjust=.5)) 
	
# put maps together
p = arrangeGrob(cod, uga, ncol=2)
# ----------------------------------------------


# --------------------------------
# Save graphs
# pdf(graphFile, height=6, width=9)
grid.newpage()
grid.draw(p)
# dev.off()
# --------------------------------
