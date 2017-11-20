# --------------------------------------------------------
# David Phillips
#
# 11/17/2017
# Make simple maps of malaria indicators for PCE countries
# --------------------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(raster)
library(RColorBrewer)
library(ggplot2)
# --------------------


# ----------------------------------------------
# Files and directories

# data directory
dir = 'J:/Project/Evaluation/GF/outcome_measurement/multi_country/map/'

# input files
artDir = paste0(dir, 'Africa Cube Public Data/ACT_use_rasters/rasters/rasters/')
itnDir = paste0(dir, 'Africa Cube Public Data/ITN_use_year_average_adj_rasters/rasters/')
irsDir = paste0(dir, 'Africa Cube Public Data/IRS_use_rasters/rasters/')
antmalDir = paste0(dir, 'NEJM Rasters/spatially_disaggregated_antimalarials/')
artFiles = paste0(artDir, list.files(artDir)[grepl('tif', list.files(artDir))])
itnFiles = paste0(itnDir, list.files(itnDir)[grepl('tif', list.files(itnDir))])
irsFiles = paste0(irsDir, list.files(irsDir)[grepl('tif', list.files(irsDir))])
antmalFiles = paste0(antmalDir, list.files(antmalDir)[grepl('tif', list.files(antmalDir))])

# output files
graphFile = paste0(dir, '/visualizations/graphs.pdf')
# ----------------------------------------------


# ----------------------------------------------
# Load/prep data

# load
data = data.table(as.data.frame(raster((itnFiles[15])), xy=TRUE))

# rename
setnames(data, c('x','y','itn'))

# label year
data[, year:=2000+15]

# clip to Uganda/DRC
data = data[y<=5.4 & y>= -13.5 & x>=12.4 & x<=35.04]
# ----------------------------------------------


# ----------------------------------------------
# Graph

# colors
cols = brewer.pal(6, 'Spectral')

# store graph
p = ggplot(data, aes(y=y, x=x, color=itn)) + 
	geom_tile() + 
	scale_color_gradientn('Z', colors=cols) + 
	theme_minimal()
# ----------------------------------------------


# --------------------------------
# Save graphs
pdf(graphFile, height=6, width=9)
p
dev.off()
# --------------------------------
