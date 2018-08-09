

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
dir = 'J:/Project/Evaluation/GF/covariates/cod/'

# change working directory to the root directory
setwd(dir)

# store file name of DRC population raster
popFile = 'COD15adjv5.tif'
popFile = "J:/Project/Evaluation/GF/outcome_measurement/multi_country/map/NEJM Rasters/malaria_death_summary_rasters/mean/00_all_malaria_deaths_mean_2000.tif"

# store file name of "shapefile" which defines the provinces
shapeFile = '../../mapping/cod/gadm36_COD_shp/gadm36_COD_1.shp'
# -----------------------------------------------------------


# -----------------------------
# Load data

# load the population data
popRaster = raster(popFile)

# load the shapefile
shapeData = shapefile(shapeFile)
# -----------------------------


# ---------------------------------------------------------------------------
# Compute population by province

# use the shapefile to group together the pixels inside each province 
# this makes a list with one element for each province and
# a data.frame that contains the pixels inside each element
# with one column for each year, plus population
popExtract = extract(popRaster, shapeData)

# sum over provinces
pop = data.table(province=shapeData@data$NAME_1, pop=sapply(popExtract, sum))
# ---------------------------------------------------------------------------
