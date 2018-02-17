
# raster package is most of what you need
library(rgeos)
library(raster)
library(ggplot2)
library(maptools)
library(dplyr)
library(tools)
library(data.table)
library(lubridate)
library(grDevices)
library(RColorBrewer)
library(readxl)
library(reshape)
library(scales)

# change directory
setwd('J:/Project/Evaluation/GF/mapping/gtm/')

# load the shapefile
shapeData = shapefile('GTM_munis_only.shp')

## load the sicoin data:

sicoin_data <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/prepped_sicoin_data.csv"
                                   ,fileEncoding="latin1"))

sicoin_data$year <- year(sicoin_data$start_date)

##do a sum of sicoin data to get data by muni, year, disease, and source
byVars = names(sicoin_data)[names(sicoin_data)%in%c('source', 'disease', 'loc_id', 'loc_name')]
sicoin_data = sicoin_data[, list(budget=sum(budget), disbursement=sum(disbursement), expenditure=sum(expenditure)), by=byVars]


malData <- sicoin_data[disease=="malaria" & source=="ghe"]

# shapeData is a spatialpolygonsdataframe
class(shapeData)

# these have plot methods
plot(shapeData)

# simplify the shape data (could create little gaps, maybe don't do this)
shapeData2 = gSimplify(shapeData, tol=0.01, topologyPreserve=TRUE)

# use the fortify function to convert from spatialpolygonsdataframe to data.frame
# use IDs instead of names
coordinates = data.table(fortify(shapeData, region='Codigo'))
coordinates$id <- as.numeric(coordinates$id)

# merge on municipality names
names = data.table(shapeData@data)
coord_and_names = merge(coordinates, names, by.x='id', by.y='Codigo', allow.cartesian=TRUE)


# merge on the data (all.x=TRUE so the shapefile data doesn't disappear)
sicoin_data$id <- as.numeric(lapply(sicoin_data$loc_id, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE)))
graphData <- merge(coord_and_names, sicoin_data,by='id', all.x=TRUE, allow.cartesian=TRUE)




# draw the polygons using ggplot2
gtm_plot <- (ggplot(graphData, aes(x=long, y=lat, group=group, fill=as.numeric(budget/1000000))) + 
	geom_path() + 
	geom_polygon() +
	theme_void()) +
  labs(fill='Total Budget (USD Millions)') 
  
	
	
