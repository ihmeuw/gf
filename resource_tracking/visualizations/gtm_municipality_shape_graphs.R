
# ----------------------------------------------
# Irena Chen
#
# 2/22/2018

### This code is to make the actual maps of Guatemala's municipalities using shape files and SICOIN data: 

# ----------------------------------------------


# raster package is most of what you need
rm(list=ls())
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


# ----------------------------------------------
# change directory
setwd('J:/Project/Evaluation/GF/mapping/gtm/')

# load the shapefile
shapeData = shapefile('GTM_munis_only.shp')

## load the admin1 shape file: 

adminData = shapefile('archive/GTM_adm1.shp')


## load the sicoin data:

sicoin_data <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/prepped_sicoin_data.csv"
                                   ,fileEncoding="latin1"))
sicoin_data$year <- year(sicoin_data$start_date)


# the sicoin loc_id has leading zeroes, which the shape files don't have
## this gets rid of it: 

sicoin_data$id <- as.numeric(lapply(sicoin_data$loc_id, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE)))



##let's do HIV first since we have that at the yearly level (may have to work around w/ Malaria data)

hivData <- sicoin_data[disease=="hiv"]

## something that might be cool is to get the absorption ratio of disb/budget by year, source, disease, etc. 
byVars = names(hivData)[names(hivData)%in%c('source', 'loc_id', 'loc_name', 'year')]
hivData  = hivData [, list(budget=sum(budget), disbursement=sum(disbursement), expenditure=sum(expenditure)), 
                          by=byVars]

##do a sum of sicoin data to get data by muni, year, disease, and source

hivData <- hivData[with(hivData, order(source, loc_name, year)), ]
hivData[, cumulative_budget:=cumsum(budget), by=c('source', 'loc_name', 'year')]
hivData[, cumulative_disbursement:=cumsum(disbursement),by=c('source', 'loc_name', 'year')]
hivData[, absorption:=cumulative_disbursement/cumulative_budget]

# ----------------------------------------------

## split by source (GF vs GHE)
gheData <- hivData[source=="ghe"]
gfData <- hivData[source=="gf"]


# ----------------------------------------------
## do a histogram of GHE budget values: 
hist(gheData$budget/1000000)
boxplot(gheData$budget)


hist(gheData$budget/1000000, breaks=10, main="Breaks=10")

## get quantiles of the budget/disb data:
quantile(gheData$absorption, na.rm=TRUE)


##let's make buckets of the budget variable: 
library(Hmisc)

# ----------------------------------------------
## Malaria data: 

malData <- sicoin_data[disease=="malaria"]

malData <- malData[source=="gf"]








  # ----------------------------------------------
## get the important info from the shape data files - lat/long coordinates, muni codes, etc.

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
admin_dataset = merge(admin_coords, admin_names, by.x = 'id', by.y='ID_1', allow.cartesian=TRUE)


# ----------------------------------------------
##get shape data ready to do yearly graphs: 
gheData <- gheData[with(gheData, order(year)), ]

# draw the polygons using ggplot2

## I put the merge function inside the graphing function/loop - longer time to process, but it frees up memory in R: 
colors <- c('#b2e2e2', '#66c2a4', '#238b45', '#006d2c')
names(colors) <- levels(gheData$binned_budget)

gtm_plots <- list()
i = 1
for (k in unique(gheData$year)){
  shapedata <- copy(coord_and_names)
  subdata <- gheData[year==k]
  shapedata$year <- k
  # merge on the data (all.x=TRUE so the shapefile data doesn't disappear)
  graphdata <- merge(shapedata, subdata,by=c('year','id'), all.x=TRUE, allow.cartesian=TRUE)
  plot <- (ggplot() + geom_polygon(data=graphdata, aes(x=long, y=lat, group=group, fill=binned_budget)) + 
     coord_equal() +
    geom_path() +
      geom_map(map=admin_dataset, data=admin_dataset, 
               aes(map_id=id,group=group), color="#FF0000", alpha=0.8) + 
    # geom_polygon(data=admin_dataset, aes(x=long, y=lat, group=group), color="red", alpha=0) + 
  	theme_void() +
  	 scale_fill_manual(na.value = "grey50", 
  	                    values = colors) + 
    labs(title=paste(k, "GHE HIV Resources"), fill='Budget ($$ USD)'))
  gtm_plots[[i]] <- plot
  i=i+1
}

pdf("H:/rt_data/gtm_ghe_hiv_by_muni.pdf", height=6, width=9)
invisible(lapply(gtm_plots, print))
dev.off()

	
	
