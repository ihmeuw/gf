
# ----------------------------------------------
# Set up R
rm(list=ls())
library(data.table)
library(reshape2)
library(stringr)
library(readxl)
library(rlang)
library(zoo)
library(lubridate)
library(raster)
library(rgeos)
library(data.table)
library(ggplot2)
library(maptools)
library(scales)
library(ggrepel)

# ---------------------------------------------
## set up the directories/J Drive
# ----------------------------------------------
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j') # have to declare the J Drive differently on the cluster 
# raster_dir = paste0(j, '/Project/Evaluation/GF/covariates/gtm/')
shape_dir <- paste0(j, '/Project/Evaluation/GF/mapping/gtm/')
case_dir <-  paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/MALARIA/case_notifications/EPIVIGILA - Semana 52 2017 csv/')
export_dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/visualizations/')

# ----------------------------------------------
## read the case notifications data and subset for malaria
# ----------------------------------------------
# case notification dataset with facility id 
case_notifications <- data.table(read.csv(paste0(case_dir, "Epivigila a Dic 2017.csv")))

# dataset that has the municipality codes attached to each facility id 

muni_facility_data <-  data.table(read.csv(paste0(case_dir, "DataConfig-Establecimientos.csv")))
##subset by the disease type that we want 
# the DataConfig - PatoSNVS csv has the specific malaria pathologies that these codes represent
malData <- case_notifications[Id_patologia%in%c(27, 28, 29)] 

# ----------------------------------------------
## set up shape data 
# ----------------------------------------------

## load the municipality level shape data
shapeData = shapefile(paste0(shape_dir,'GTM_munis_only.shp'))
coordinates = data.table(fortify(shapeData, region='Codigo'))
coordinates$id <- as.numeric(coordinates$id)
coord_and_names = merge(coordinates, names, by.x='id', by.y='Codigo', allow.cartesian=TRUE)


## load the department level shape data 
adminData = shapefile(paste0(shape_dir,'gtm_region.shp'))
names = data.table(shapeData@data)
admin_coords <- data.table(fortify(adminData, region='ID_1'))
admin_names <- data.table(adminData@data)
admin_dataset = merge(admin_coords, admin_names, by.x = 'id', by.y='ID_1', allow.cartesian=TRUE)

# ----------------------------------------------
### optional: Plot the names of the departments on the map: 
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
## shape the case notification data so that we can plot it
# ----------------------------------------------
setnames(malData, c("year", "week", "facility_id", "disease_id", "age_group", "cases", "male_id", "female_id"))

## grab the facility and municipality ids from the facility dataset
muni_facility_data <- muni_facility_data[,c("Padre", "ID_ESTABLECIMIENTO"), with=FALSE]
setnames(muni_facility_data, c("loc_id", "facility_id"))

## join on facility id to get the municipality code: 

mal_muni_data <- merge(muni_facility_data, malData, by="facility_id")

# right now, we don't care about the other variables (just year and location )
byVars = names(mal_muni_data)[names(mal_muni_data)%in%c('year', 'loc_id')]
malaria_dataset= mal_muni_data[, list(cases=sum(na.omit(cases))), by=byVars]


# loc_id has leading zeroes, which the shape files don't have
## this gets rid of it: 
malaria_dataset$id <- as.numeric(lapply(malaria_dataset$loc_id, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE)))

# order by year so that when we plot, it plots every year in order 
malaria_dataset <- malaria_dataset[with(malaria_dataset, order(year)), ]


malaria_dataset$case_binned <- cut(malaria_dataset$cases/1000,
                             breaks= c(seq(0,1, by=0.1),2:4, Inf),right = FALSE)
colors <- c('#066d28' ,
             '#028924',
             '#03a315',
             '#3bb564',
             '#1de59f',
             '#37E1E6',
             '#85e636',
             '#ff7f00',
             '#cab2d6',
             '#9e82ba', 
             '#f9a7be',
             '#f78080',
             '#af445b', 
             '#ff447c'
             
)
names(colors) <- levels(malaria_dataset$case_binned )


colScale <-  scale_fill_gradient2(low='#028924', mid='#f9f98b', high='#66047c',
                                  na.value = "grey70",space = "Lab", midpoint = 300, ## play around with this to get the gradient 
                                  # that you want, depending on data values 
                                  breaks=c(0,100, 200, 300, 400,500, 600,700), limits=c(0,700))

# ----------------------------------------------
## make GIS maps of the case notification data 
# ----------------------------------------------
## I put the shape files inside the for loop to save on memory in R - probably slows down the processing time a little 
# if you run this on the cluster, feel free to rewrite this 
gtm_plots <- list()
i = 1
for (k in unique(malaria_dataset$year)){
  shapedata <- copy(coord_and_names)
  subdata <- malaria_dataset[year==k]
  shapedata$year <- k ## add "year" to shapefile in order to merge datasets 
  # merge on the data (all.x=TRUE so the shapefile data doesn't disappear)
  graphdata  <- merge(shapedata, subdata,by=c('year','id'), all.x=TRUE, allow.cartesian=TRUE)
  plot <- (ggplot() + geom_polygon(data=graphdata, aes(x=long, y=lat, group=group, fill=case_binned)) + 
             coord_equal() + ##so the two shapefiles have the same proportions 
             geom_path() +
             geom_map(map=admin_dataset, data=admin_dataset,
                      aes(map_id=id,group=group), size=1, color="#4b2e83", alpha=0) + 
             geom_polygon(data=admin_dataset, aes(x=long, y=lat, group=group), color="#0d069e", alpha=0) + 
             theme_void() +  
             scale_fill_manual(name="Cases (in thousands)", values=colors,na.value="grey50", drop=F) + 
             ## uncomment if you want the department names: 
             geom_label_repel(data = gtm_region_centroids, aes(label = NAME_1, x = long, y = lat, group = NAME_1), 
                          size = 3, fontface = 'bold', color = 'black',
                      box.padding = 0.35, point.padding = 0.3,
                      segment.color = 'grey50', nudge_x = 0.7, nudge_y = 4.5) + 
             labs(title=paste(k, "Malaria Case Notifications"), fill='Number of Cases'))
  gtm_plots[[i]] <- plot
  i=i+1
}


# ----------------------------------------------
## export the list of maps as a PDF 
# ----------------------------------------------

pdf(paste0(export_dir, "subnational_maps/malaria_case_notifications.pdf"), height=6, width=9)
invisible(lapply(gtm_plots, print))
dev.off()




