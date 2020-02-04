# ---------------------------
# Set up R
rm(list=ls()) # clear memory
library(raster)
library(data.table) # for faster/easier data analysis
library(ggplot2) # for making any kind of graph
library(RColorBrewer) # for nicer colors
# ---------------------------


# -----------------------------------------------------------
# Store file and folder names

# root directory. CHANGE THIS TO THE LOCATION OF YOUR DATA
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/')

# change working directory to the root directory
setwd(dir)

# store file name of DRC population raster
popFile15 = paste0(dir, 'worldpop_data/COD15adjv5/COD15adjv5.tif')
popFile10 = paste0(dir, 'worldpop_data/COD10adjv5/COD10adjv5.tif')

# store file name of "shapefile" which defines the provinces
shapeFile =  paste0(dir, 'drc_shapefiles/gadm36_COD_shp/gadm36_COD_1.shp')
# -----------------------------------------------------------


# -----------------------------
# Load data

# load the population data
popRaster15 = raster(popFile15)
popRaster10 = raster(popFile10)

# load the shapefile
shapeData = shapefile(shapeFile)
# -----------------------------


# ---------------------------------------------------------------------------
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
