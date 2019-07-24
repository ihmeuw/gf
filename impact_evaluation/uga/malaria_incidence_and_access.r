# ------------------------------------------------------------------------------
# David Phillips
# 
# 7/24/2019
# Analysis of changes in malaria incidence compared to the MAP access covariate
# ------------------------------------------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(gridExtra)
library(viridis)
library(raster)
library(rgeos)
# --------------------


# --------------------------------------------------------------------------------
# Files and directories

# location of covariate file
accessFile = 'J:/WORK/11_geospatial/01_covariates/00_MBG_STANDARD/access2/mean/synoptic/access2_mean_synoptic.tif'

# location of incidence files
incFile2010 = 'J:/Project/Evaluation/GF/outcome_measurement/multi_country/map/2019 Publication/clippedLayers/country_profiles_data/2019_Global_Pf_Incidence__2010788d475c-71e3-4a9a-8e5d-8eefb2fa40f6.tiff'
incFile2017 = 'J:/Project/Evaluation/GF/outcome_measurement/multi_country/map/2019 Publication/clippedLayers/country_profiles_data/2019_Global_Pf_Incidence__2017948351e4-b955-4caa-821a-d2ffbffad5b2.tiff'

# location of Uganda shapefile
shapeFile = 'J:/Project/Evaluation/GF/mapping/uga/uga_dist112_map.shp'

# output file
outFile = 'J:/Project/Evaluation/GF/impact_evaluation/uga/visualizations/incidence_vs_access.pdf'
# --------------------------------------------------------------------------------


# -------------------------------
# Load data

# load access data
access = raster(accessFile)

# load incidence data
inc2010 = raster(incFile2010)
inc2017 = raster(incFile2017)

# load shapefile
shapeData = shapefile(shapeFile)
# -------------------------------


# ------------------------------------------------------------------------------
# Prep data

# crop rasters to just Uganda
access = crop(access, extent(shapeData))
access = mask(access, shapeData)
inc2010 = crop(inc2010, extent(shapeData))
inc2010 = mask(inc2010, shapeData)
inc2017 = crop(inc2017, extent(shapeData))
inc2017 = mask(inc2017, shapeData)

# project rasters to ensure they have identical pixels to 2017
access = projectRaster(access, inc2017)
inc2010 = projectRaster(inc2010, inc2017)

# convert rasters to data.tables and merge together
access_dt = data.table(as.data.frame(access, xy=TRUE))
inc2010_dt = data.table(as.data.frame(inc2010, xy=TRUE))
inc2017_dt = data.table(as.data.frame(inc2017, xy=TRUE))
data = merge(inc2017_dt, inc2010_dt, by=c('x','y'))
data = merge(data, access_df, by=c('x','y'))

# clean up 
setnames(data, c('x','y','inc2010','inc2017','access'))
data[is.na(access) | is.na(inc2010) | is.na(inc2017), access:=NA]
data[is.na(access) | is.na(inc2010) | is.na(inc2017), inc2010:=NA]
data[is.na(access) | is.na(inc2010) | is.na(inc2017), inc2017:=NA]

# compute change in incidence rate
data[, change:=inc2017-inc2010]
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Graph

# map change
p1=ggplot(data, aes(y=y, x=x, fill=change)) + 
	geom_raster() + 
	scale_fill_viridis(na.value='white') + 
	coord_fixed() + 
	labs(title='Change in Malaria Incidence Rate, 2010-2017', fill='Incidence Rate in 2017\nMinus Incidence Rate in 2010') + 
	theme_void()

# map access
p2=ggplot(data, aes(y=y, x=x, fill=access)) + 
	geom_raster() + 
	scale_fill_viridis(na.value='white') + 
	coord_fixed() + 
	labs(title='Travel Time to Nearest Town', fill='Travel Time to\nNearest Town (minutes)') + 
	theme_void()

# scatter change vs access
p3=ggplot(data, aes(y=change, x=access)) + 
	geom_point() + 
	labs(title='Change in Malaria Incidence Rate Compared to Travel Time', 
		y='Change in Malaria Incidence Rate 2010-2017', x='Travel Time to Nearest Town (minutes)',
		caption='Unit of analysis: 5x5 km pixels in Uganda') + 
	theme_bw()
# ------------------------------------------------------------------------------


# --------------------------------
# Save
pdf(outFile, height=5.5, width=8)
p1
p2
p3
dev.off()
# --------------------------------
