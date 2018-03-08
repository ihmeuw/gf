
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

## load the admin1 shape with the projection: 
adminData = shapefile('gtm_region.shp')


## load the sicoin data:

sicoin_data <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/prepped_sicoin_data.csv"
                                   ,fileEncoding="latin1"))
sicoin_data$year <- year(sicoin_data$start_date)


# the sicoin loc_id has leading zeroes, which the shape files don't have
## this gets rid of it: 

sicoin_data$id <- as.numeric(lapply(sicoin_data$loc_id, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE)))



##I'm doing HIV in this example, but this works for Malaria and TB as well: 

hivData <- sicoin_data[disease=="hiv"]

## something that might be cool is to get the absorption ratio of disb/budget by year, source, disease, etc. 
byVars = names(hivData)[names(hivData)%in%c('source', 'loc_id', 'loc_name', 'year', 'id')]
hivData  = hivData [, list(budget=sum(na.omit(budget)), disbursement=sum(na.omit(disbursement))), 
                          by=byVars]
##get absorption: 
hivData[, absorption:=disbursement/budget]

# ----------------------------------------------
## if you want the cumulative budgets and disbursements, uncomment: 

# hivData <- hivData[with(hivData, order(source, loc_name, year)), ]
# hivData[, cumulative_budget:=cumsum(budget), by=c('source', 'loc_name', 'year', 'id')]
# hivData[, cumulative_disbursement:=cumsum(disbursement),by=c('source', 'loc_name', 'year', 'id')]


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
admin_coords <- data.table(fortify(adminData, region='ID_1'))
coordinates$id <- as.numeric(coordinates$id)

# merge on municipality names
names = data.table(shapeData@data)
admin_names <- data.table(adminData@data)
coord_and_names = merge(coordinates, names, by.x='id', by.y='Codigo', allow.cartesian=TRUE)
admin_dataset = merge(admin_coords, admin_names, by.x = 'id', by.y='ID_1', allow.cartesian=TRUE)


# ----------------------------------------------
##get shape data ready to do yearly graphs: 
gheData <- hivData[source=="ghe"]
gheData <-gheData[with(gheData, order(year, loc_name)), ]
gfData <- hivData[source=="gf"]
gfData<- gfData[with(gfData, order(year, loc_name)), ]

##budget and disbursement: 

colScale <-  scale_fill_gradient2(low='#ffe1e6', mid='#ef8307', high='#12ed8e',
                                  na.value = "grey70",space = "Lab", midpoint = 1.6, ## play around with this to get the gradient 
                                  # that you want, depending on data values 
                                  breaks=c(0, 0.5 ,1, 1.5, 2, 2.5), limits=c(0,3))


##absorption: 
colScale <-  scale_fill_gradient2(low='#ffe1e6', mid='#00FFff', high='#0606aa',
                                  na.value = "grey70",space = "Lab", midpoint = 0.5, ## play around with this to get the gradient 
                                  # that you want, depending on data values 
                                  breaks=c(0, 0.2, 0.4,0.6, 0.8), limits=c(0,1))

# ----------------------------------------------
### if you want:  Get names and id numbers corresponding to administrative areas to plot as labels: 
# gtm_region_centroids <- data.frame(long = coordinates(adminData)[, 1], 
# gtm_region_centroids[, 'ID_1'] <- adminData@data[,'ID_1']
# gtm_region_centroids[, 'NAME_1'] <-adminData@data[,'NAME_1']
# gtm_region_centroids$NAME_1[18] <- "Totonicapán"
# gtm_region_centroids$NAME_1[22] <- "Sololá"
# gtm_region_centroids$NAME_1[21] <- "Suchitepéquez"
# gtm_region_centroids$NAME_1[3] <- "Sacatepéquez"
# gtm_region_centroids$NAME_1[1] <- "Quiché"
# gtm_region_centroids$NAME_1[7] <- "Petén"


# ----------------------------------------------

# draw the polygons using ggplot2
## I put the shape files inside the for loop to save on memory in R - probably slows down the processing time a little 
gtm_plots <- list()
i = 1
for (k in unique(gheData$year)){
  shapedata <- copy(coord_and_names)
  subdata <- gheData[year==k]
  shapedata$year <- k ## add "year" to shapefile in order to merge datasets 
  # merge on the data (all.x=TRUE so the shapefile data doesn't disappear)
  graphdata  <- merge(shapedata, subdata,by=c('year','id'), all.x=TRUE, allow.cartesian=TRUE)
  plot <- (ggplot() + geom_polygon(data=graphdata, aes(x=long, y=lat, group=group, fill=budget/1000000)) + 
             coord_equal() + ##so the two shapefiles have the same proportions 
             geom_path() +
             geom_map(map=admin_dataset, data=admin_dataset,
                      aes(map_id=id,group=group), size=1, color="#4b2e83", alpha=0) + 
             # geom_polygon(data=admin_dataset, aes(x=long, y=lat, group=group), color="red", alpha=0) + 
             colScale +
             theme_void() +  
             ## uncomment if you want the department names: 
             # geom_label_repel(data = gtm_region_centroids, aes(label = NAME_1, x = long, y = lat, group = NAME_1), 
             #                  size = 3, fontface = 'bold', color = 'black',
             #                  box.padding = 0.35, point.padding = 0.3,
             #                  segment.color = 'grey50', nudge_x = 0.7, nudge_y = 4.5) + 
             labs(title=paste(k, "GHE Budgets"), fill='USD (millions)'))
  gtm_plots[[i]] <- plot
  i=i+1
}


pdf("J:/Project/Evaluation/GF/resource_tracking/gtm/visualizations/municipality_viz/ghe_hiv_absorption.pdf", height=6, width=9)
invisible(lapply(gtm_plots, print))
dev.off()

# ----------------------------------------------
##THIS IS TO CLEAN THE MALARIA DATA: 

malData <- sicoin_data[disease=="malaria"]

## function to get the YE budget numbers from the monthly data: 
get_december_budget <- function(budget, start_date){
  x <- budget
  if(month(start_date)==12){
    x <- x
  } else {
    x <- 0
  }
  return(x)
}


malData$new_budget <- as.numeric(mapply(get_december_budget, malData$budget, malData$start_date))

## sum up the data by source, loc_id and year (ignore monthly data for now)
byVars <- names(malData)[names(malData)%in%c('source', 'loc_id', 'loc_name', 'year', 'id')]
malData  <- malData [, list(budget=sum(new_budget), disbursement=sum(disbursement), expenditure=sum(expenditure)), 
                     by=byVars]

## how to define absorption:
malData$absorption <- malData$disbursement/malData$budget
malData <- malData[with(malData, order(source, year, loc_name)), ]

##split by GF vs GHE 
gfData <- malData[source=="gf"]
gheData <- malData[source=="ghe"]

## now use the above code to get the graphs 




