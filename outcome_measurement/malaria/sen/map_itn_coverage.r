# ---------------------------------------------------------------------------
# David Phillips
# 
# 7/8/2019
# Make a basic raster map from Malaria Atlas Project estimates
# ---------------------------------------------------------------------------


# ---------------------------
# Set up R
rm(list=ls()) # clear memory
library(data.table) # for faster/easier data analysis
library(ggplot2) # for making any kind of graph
library(viridis) # for nicer colors
library(raster) # for the function that loads shapefiles
# ---------------------------


# -----------------------------------------------------------
# Store file and folder names

# store file name for MAP estimates CHANGE TO THE LOCATION OF YOUR DATA
rasterFile = 'J:/Project/Evaluation/GF/outcome_measurement/multi_country/map/Pre-2019 Publication/Africa Cube Public Data/ITN_use_year_average_adj_rasters/rasters/2015.ITN.use.yearavg.adj.stable.tif'

# store file name for shapefile CHANGE TO THE LOCATION OF YOUR SHAPEFILE
shapeFile = 'J:/Project/Evaluation/GF/mapping/sen/gadm36_SEN_2.shp'

# output file name CHANGE TO THE LOCATION OF YOUR OUTPUT FILES
outFile = 'J:/Project/Evaluation/GF/outcome_measurement/sen/visualizations/raster_itn_map.pdf'
# -----------------------------------------------------------


# -------------------------------------------------------------------------------
# Load data and explore what's in it

# load the raster data from MAP
rasterData = raster(rasterFile)

# load the shape data
# note: this won't work without the .shx and .dbf files that are also in this folder!
shapeData = shapefile(shapeFile)

# clip the raster data to the outline of Senegal
rasterData = crop(rasterData, extent(shapeData))
rasterData = mask(rasterData, shapeData)		

# format the raster data as a data.frame
data = as.data.frame(rasterData, xy=TRUE)
names(data) = c('x','y','itn_coverage')

# format the shape data as a data.frame using fortify
fortifiedShapeData = fortify(shapeData)
# -------------------------------------------------------------------------------


# ----------------------------------------------------------------
# Use the ggplot2 package to make a map with most vector and raster data
	
# make a basic raster map
ggplot(data, aes(y=y,x=x,fill=itn_coverage)) + 
	geom_raster()
	
# now make a raster map and put the vector map 'on top' of it
ggplot() + 
	geom_raster(data=data, aes(y=y,x=x,fill=itn_coverage)) + # you can put the data and aes inside the geom if you want to use two different datasets
	geom_path(data=fortifiedShapeData, aes(y=lat, x=long, group=group))

# now improve the map the same way as before
ggplot() + 
	geom_raster(data=data, aes(y=y,x=x,fill=itn_coverage)) + 
	geom_path(data=fortifiedShapeData, aes(y=lat, x=long, group=group)) + 
	scale_fill_viridis(na.value='white') + 
	theme_void() + 
	labs(title='ITN Coverage in Senegal, 2015', fill='ITN Coverage')
# ----------------------------------------------------------------
