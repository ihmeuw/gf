
# ----------------------------------------------
# Irena Chen
#
# 2/22/2018

### This code is to make the actual maps of Guatemala's municipalities using shape files and SICOIN data: 

# ----------------------------------------------
###### Set up R / install packages  ###### 
# ----------------------------------------------
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
library(ggrepel)


# ----------------------------------------------
# Shape files: 
# ----------------------------------------------
mapping_dir <- 'J:/Project/Evaluation/GF/mapping/gtm/'

# load the shapefile
shapeData = shapefile(paste0(mapping_dir,'GTM_munis_only.shp'))

## load the admin1 shape with the projection: 
adminData = shapefile(paste0(mapping_dir,'gtm_region.shp'))


# ----------------------------------------------
## get the important info from the shape data files - lat/long coordinates, muni codes, etc.
# ----------------------------------------------

# use the fortify function to convert from spatialpolygonsdataframe to data.frame

# use IDs instead of names
coordinates = data.table(fortify(shapeData, region='Codigo'))
coordinates$id <- as.numeric(coordinates$id)
names = data.table(shapeData@data)
# merge on municipality names
coord_and_names = merge(coordinates, names, by.x='id', by.y='Codigo', allow.cartesian=TRUE)


##do the same at the department level
admin_coords <- data.table(fortify(adminData, region='ID_1'))
admin_names <- data.table(adminData@data)
admin_dataset = merge(admin_coords, admin_names, by.x = 'id', by.y='ID_1', allow.cartesian=TRUE)

# ----------------------------------------------
## load the sicoin data:
# ----------------------------------------------
sicoin_data <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/prepped_sicoin_data.csv"
                                   ,fileEncoding="latin1"))
sicoin_data$start_date <- as.Date(sicoin_data$start_date,"%Y-%m-%d")
sicoin_data$year <- year(sicoin_data$start_date)

# the sicoin loc_id has leading zeroes, which the shape files don't have
## this gets rid of it: 
sicoin_data$id <- as.numeric(lapply(sicoin_data$adm2, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE)))

# ----------------------------------------------
##I'm using malaria in this example, but the other diseases work fine
# ----------------------------------------------
malData <- sicoin_data[disease=="malaria"&financing_source=="ghe"]

## aggregate by the variables that you want: 
byVars = names(malData)[names(malData)%in%c('loc_name','module','year', 'id')]
malData  = malData[, list(budget=sum(na.omit(budget)), disbursement=sum(na.omit(disbursement))), 
                          by=byVars]
##get absorption: 
malData[, absorption:=disbursement/budget]

# ----------------------------------------------
## if you want the cumulative budgets and disbursements, uncomment: 
# ----------------------------------------------
# malData<- malData[with(malData, order(source, loc_name, year)), ]
# malData[, cumulative_budget:=cumsum(budget), by=c('source', 'loc_name', 'year', 'id')]
# malData[, cumulative_disbursement:=cumsum(disbursement),by=c('source', 'loc_name', 'year', 'id')]

# ----------------------------------------------
## set up the color scale gradient for the maps
# ----------------------------------------------
colScale <-  scale_fill_gradient2(low='#66047c', mid='#edc393', high='#68e86a',
                                  na.value = "grey70",space = "Lab",
                                  midpoint = 1.6, ## play around with this to get the gradient 
                                  # that you want, depending on data values 
                                  breaks=c(0, 0.5 ,1, 1.5, 2, 2.5), limits=c(0,3))

# ----------------------------------------------
### if you want:  Get names and id numbers corresponding to the departments to plot as labels: 
# ----------------------------------------------
gtm_region_centroids <- data.frame(long = coordinates(adminData)[, 1],lat = coordinates(adminData)[, 2])
gtm_region_centroids[, 'ID_1'] <- adminData@data[,'ID_1'] 
gtm_region_centroids[, 'NAME_1'] <-adminData@data[,'NAME_1']
gtm_region_centroids$NAME_1[18] <- "Totonicapán"
gtm_region_centroids$NAME_1[22] <- "Sololá"
gtm_region_centroids$NAME_1[21] <- "Suchitepéquez"
gtm_region_centroids$NAME_1[3] <- "Sacatepéquez"
gtm_region_centroids$NAME_1[1] <- "Quiché"
gtm_region_centroids$NAME_1[7] <- "Petén"

# ----------------------------------------------
## create the actual maps 
# ----------------------------------------------
## I put the shape files inside the for loop to save on memory in R 
## if you want to run this on the cluster, you can probably change this - might speed up the processing time 
gtm_plots <- list()
i = 1
for (k in unique(gheData$year)){
  shapedata <- copy(coord_and_names)
  subdata <- gheData[year==k]
  shapedata$year <- k ## add "year" to shapefile in order to merge datasets 
  # merge on the data (all.x=TRUE so the shapefile data doesn't disappear)
  graphdata  <- merge(shapedata, subdata,by=c('year','id'), all.x=TRUE, allow.cartesian=TRUE)
  plot <- (ggplot() + geom_polygon(data=graphdata, aes(x=long, y=lat, group=group, fill=budget/1000000)) + 
             coord_equal() + ##so that your map doesn't look weirdly squished or flat 
             geom_path() +
             geom_map(map=admin_dataset, data=admin_dataset,
                      aes(map_id=id,group=group), size=1, color="#4b2e83", alpha=0) + 
             # geom_polygon(data=admin_dataset, aes(x=long, y=lat, group=group), color="red", alpha=0) + 
             colScale +
             theme_void() +  
             ## comment if you don't want the department names: 
             geom_label_repel(data = gtm_region_centroids, aes(label = NAME_1, x = long, y = lat, group = NAME_1), 
                    size = 3, fontface = 'bold', color = 'black',
                    box.padding = 0.35, point.padding = 0.3,
                    segment.color = 'grey50', nudge_x = 0.7, nudge_y = 4.5) + 
             labs(title=paste(k, "GHE Budgets"), fill='USD (millions)'))
  gtm_plots[[i]] <- plot
  i=i+1
}
# ----------------------------------------------
## export the list of graphs as a PDF
# ----------------------------------------------
pdf("malaria_by_year.pdf", height=6, width=9)
invisible(lapply(gtm_plots, print))
dev.off()




