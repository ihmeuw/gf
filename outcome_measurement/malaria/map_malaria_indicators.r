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
# Parameters and settings

# year
year = 2014

# indicators
inds = c('itn','antmal','prev')
# ----------------------------------------------


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
prevDir = paste0(dir, '/Africa Cube Public Data/Prevalence_annual_means_rasters/rasters/')
actFiles = paste0(actDir, list.files(actDir)[grepl('tif', list.files(actDir))])
itnFiles = paste0(itnDir, list.files(itnDir)[grepl('tif', list.files(itnDir))])
irsFiles = paste0(irsDir, list.files(irsDir)[grepl('tif', list.files(irsDir))])
antmalFiles = paste0(antmalDir, list.files(antmalDir)[!grepl('ovr', list.files(antmalDir))])
antmalFiles = antmalFiles[!grepl('xml', antmalFiles)]
prevFiles = paste0(prevDir, list.files(prevDir)[grepl('tif', list.files(prevDir))])

# shapefiles
shapeFileUGA = paste0(dir, '../../../mapping/uga/uga_dist112_map.shp')
shapeFileCOD = paste0(dir, '../../../mapping/cod/COD_adm3.shp')

# output files
graphFile = paste0(dir, '/visualizations/graphs.pdf')
# ----------------------------------------------


# ----------------------------------------------------------
# Load/prep data

# load data
if ('itn' %in% inds) rasterDataitn = raster(itnFiles[grepl(year, itnFiles)])
if ('act' %in% inds) rasterDataact = raster(actFiles[grepl(year, itnFiles)])
if ('irs' %in% inds) rasterDatairs = raster(irsFiles[grepl(year, irsFiles)])
if ('antmal' %in% inds) rasterDataantmal = raster(antmalFiles[grepl(year, antmalFiles)])
if ('prev' %in% inds) rasterDataprev = raster(prevFiles[grepl(year, prevFiles)])

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

# legend limits so both countries are on same scale
min = floor(min(c(dataUGAitn$value, dataCODitn$value), na.rm=TRUE)*100)
max = ceiling(max(c(dataUGAitn$value, dataCODitn$value), na.rm=TRUE)*100)
# ----------------------------------------------------------------------


# ----------------------------------------------
# Map comparing countries

# store maps separately because geom_tile bug with facetting
uga = ggplot(dataUGAitn, aes(y=y, x=x, fill=value*100)) + 
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
	
cod = ggplot(dataCODitn, aes(y=y, x=x, fill=value*100)) + 
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
