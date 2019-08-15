# -----------------------------------------------------
# Raster maps for the Uganda Annual Country Report
# Caitlin O'Brien-Carelli, David Phillips, Audrey Batzel
# 1/3/2019

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
library(rgdal)
# --------------------

# determine the most recent version of the raster map
# run_date = fread(paste0('/ihme/code/geospatial/jdv6/lbd_hiv/5_publications/africa_hiv_prev/run_dates.txt'))
# run_date = run_date[indicator == 'hiv_test' & group == 'final_results', run_date]

# most recent run date as of publication
run_date = "2018_11_08_18_37_25"

# ----------------------------------------------------------------------------------------
# Files and directories

# data directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
outDir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/multi_country/lbd/')
export_dir_uga = paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/lbd_prev')
export_dir_code = paste0(j, '/Project/Evaluation/GF/outcome_measurement/')

# input files
timestamp = run_date
inDir = paste0('/share/geospatial/mbg/hiv/hiv_test/output/', timestamp,'/')
inFile = paste0(inDir, 'hiv_test_mean_raked_raster.tif')

# shapefiles
shapeFileCOD = (paste0(outDir, '../../../mapping/cod/COD_adm3.shp'))

# alternate who shape file
# mapCOD = shapefile(paste0(j, '/Project/Evaluation/GF/mapping/cod/health_zones_who/health2.shp'))

# shapefile of lakes
shapeFileLakes = paste0(j, '/WORK/11_geospatial/06_original shapefiles/GLWD_lakes/glwd_1.shp')

# set the year you want to map
y = 2004

# specify band to get a specific year of data
# band 1=2000, band 17=2016.... so for 2015 band=16 and for 2010 band=11
band_to_year = data.table(band=c(1:19), year=c(2000:2018))
band = band_to_year[year==y, band]

# output file
graphFile = paste0(outDir, 'HIV_Prevalence_', timestamp, '_', y, '_new.pdf')

# ----------------------------------------------------------------------------------------
# COD map

# load shapefiles
mapCOD = shapefile(shapeFileCOD)

# load raster data
rasterData1 = raster(inFile, band=band)

# load the ground cover data
lakes = shapefile(shapeFileLakes)

# crop and mask to drc
rasterDataCOD1 = crop(rasterData1, extent(mapCOD))
rasterDataCOD1 = mask(rasterDataCOD1, mapCOD, buffer=)

# mask the bodies of water
rasterDataCOD1 = mask(rasterDataCOD1, lakes, inverse=TRUE)

# convert to data tables
dataCOD1 = data.table(as.data.frame(rasterDataCOD1, xy=TRUE))

# import shape file
shapeDataCOD = data.table(fortify(mapCOD))

# rename
setnames(dataCOD1, c('x','y','prev'))

# colors
cols1 = brewer.pal(9, 'Reds')
border = 'grey65'
breaks = c(1, 3, 5, 7)

# legend limits so both countries are on same scale
lims = range(dataCOD1$prev, na.rm=TRUE)*100

# store maps
ggplot(dataCOD1, aes(y=y, x=x, fill=prev*100)) + 
  geom_tile() + 
  geom_path(data=shapeDataCOD, aes(x=long, y=lat, group=group),
            color=border, size=.05, inherit.aes=FALSE) +
  scale_fill_gradientn('HIV Prevalence (%)', colors=cols1,
                       na.value='white', breaks=breaks) +
  coord_fixed(ratio=1) + 
  scale_x_continuous('', breaks = NULL) + 
  scale_y_continuous('', breaks = NULL) + 
  theme_minimal(base_size=16) + 
  theme(plot.title=element_text(hjust=.5)) 

# --------------------------------
