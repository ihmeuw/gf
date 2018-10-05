# Audrey Batzel
# 8-14-18 & 9/28/18
#
# Code taken from David Phillips (here: ./core/aggregate_populations.r and ./vfm/prep_distributions.r) 
# to aggregate world pop estimates to DPS and HZ level population estimates in DRC
# 
# -----------------------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(raster)
library(rgeos)
library(parallel)
library(RColorBrewer)
library(ggplot2)

# change working directory to the root of the repo
# change to H drive for the Cluster?
setwd('C:/local/gf/')
# --------------------


# -----------------------------------------------------------
# DIRECTORIES

# root directory
# change depending on cluster/not
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
main_dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/')

# Worldpop directory
pop_dir = paste0(j, '/WORK/11_geospatial/01_covariates/00_MBG_STANDARD/worldpop/total/1y/')

# Shapefile directory
hz_shape_dir = paste0(main_dir, 'drc_shapefiles/health2/')
dps_shape_dir = paste0(main_dir, 'drc_shapefiles/gadm36_COD_shp/')

# output directory
out_dir = paste0(main_dir, 'worldpop_data/')

# FILES

# shapefiles
shapeFileDPS = paste0(dps_shape_dir, 'gadm36_COD_1.shp')
shapeFileHZ = paste0(hz_shape_dir, 'health2.shp')

# shapefile of lakes
shapeFileLakes = paste0(j, '/WORK/11_geospatial/06_original shapefiles/GLWD_lakes/glwd_1.shp')

# # worldpop DRC population raster
# popFile15 = paste0(dir, 'worldpop_data/COD15adjv5/COD15adjv5.tif')
# popFile10 = paste0(dir, 'worldpop_data/COD10adjv5/COD10adjv5.tif')

# population files
popFiles = paste0(pop_dir, list.files(pop_dir, 'tif'))
popFiles = popFiles[!grepl('.ovr|.aux|.xml', popFiles)]

# choose which pop file to use
year = 2017
popFile = popFiles[grep(year, popFiles)]

# functions
source('./core/standardizeDPSNames.r')

# output file
pop_estimates_2017 <- '2017_worldpop_DRC_DPS.csv'
# -----------------------------------------------------------


# -----------------------------
# Load data

# load the population raster data
popData = raster(popFile)

# load the shapefiles
map = shapefile(shapeFileDPS)
lakes = shapefile(shapeFileLakes)
# crop lakes shapefile to the extent of DRC map
lakes = crop(lakes, extent(map))
# -----------------------------


# ---------------------------------------------------------------------------
# prep shapefiles
# simplify shapefile for speed
mapDatatmp = map@data
map = gSimplify(map, tol=0.01, topologyPreserve=TRUE)
map = as(map, 'SpatialPolygonsDataFrame')
map@data = mapDatatmp

# clip to current country
popData = crop(popData, extent(map))
popData = mask(popData, map)		

# mask the bodies of water
popData = mask(popData, lakes, inverse=TRUE)

# extract pixels by DPS (in parallel for speed)
# extractedData = sapply(extract(rasterData, map), sum)
extractedData = unlist(mclapply(map@data$NAME_1, function(x) { 
  currentDPS = crop(popData, extent(map[map@data$NAME_1==x,]))
  currentDPS = mask(currentDPS, map[map@data$NAME_1==x,])	
  sum(getValues(currentDPS), na.rm=TRUE)
}, mc.cores=ifelse(Sys.info()[1]=='Windows',1,36)))

# sum over provinces
currentPOPdata = data.table(dps=map@data$NAME_1, 
                            population=extractedData)

write.csv(currentPOPdata, paste0(out_dir, pop_estimates_2017))
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Old version of this code:
# Compute population by province

# use the shapefile to group together the pixels inside each province 
# this makes a list with one element for each province and
# a data.frame that contains the pixels inside each element
# with one column for each year, plus population
popExtract15 = extract(popRaster15, shapeData)
popExtract10 = extract(popRaster10, shapeData)

# sum over provinces
pop15 = data.table(province=shapeData@data$NAME_1, pop=sapply(popExtract15, sum, na.rm=T))
pop10 = data.table(province=shapeData@data$NAME_1, pop=sapply(popExtract10, sum, na.rm=T))
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Save population estimates
write.csv(pop15, paste0(dir, "worldpop_data/2015_pop_estimates_by_dps.csv"))
write.csv(pop10, paste0(dir, "worldpop_data/2010_pop_estimates_by_dps.csv"))
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Aggregated version
popRaster15_agg <- aggregate(popRaster15, fact=8, fun=sum)
popRaster10_agg <- aggregate(popRaster10, fact=8, fun=sum)

popExtract15_agg = extract(popRaster15_agg, shapeData)
popExtract10_agg = extract(popRaster10_agg, shapeData)

pop15 = data.table(province=shapeData@data$NAME_1, pop=sapply(popExtract15_agg, sum, na.rm=T))
pop10 = data.table(province=shapeData@data$NAME_1, pop=sapply(popExtract10_agg, sum, na.rm=T))

write.csv(pop15, paste0(dir, "worldpop_data/2015_pop_estimates_by_dps(aggregated raster).csv"))
write.csv(pop10, paste0(dir, "worldpop_data/2010_pop_estimates_by_dps(aggregated raster).csv"))




