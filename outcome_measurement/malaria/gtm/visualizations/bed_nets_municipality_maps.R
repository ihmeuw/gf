# ----------------------------------------------
# Irena Chen 
# 4/5/18
# Integrate the GTM malaria supply chain data with shapefiles to make GIS visualizations  
# ----------------------------------------------
###### Set up R / install packages  ###### 
# ----------------------------------------------
library(readxl)
library(rgeos)
library(lubridate)
library(raster)
library(ggplot2)
library(maptools)
library(tools)
library(data.table)
library(lubridate)
library(grDevices)
library(RColorBrewer)
library(reshape)
library(scales)
library(ggrepel)
library(dplyr)
library(zoo)
library(stringr)
# ----------------------------------------------
###### Set global variables  ###### 
# ----------------------------------------------
mapping_dir <- 'J:/Project/Evaluation/GF/mapping/gtm/'
local_dir <- "J:/Project/Evaluation/GF/outcome_measurement/gtm/"
# ----------------------------------------------
######## Load the municipality level shapefile ########
# ----------------------------------------------
# load the shapefile
shapeData = shapefile(paste0(mapping_dir,'GTM_munis_only.shp'))
coordinates = data.table(fortify(shapeData, region='Codigo'))
coordinates$id <- as.numeric(coordinates$id)
names = data.table(shapeData@data)
coord_and_names = merge(coordinates, names, by.x='id', by.y='Codigo', allow.cartesian=TRUE)
## load the admin1 shape with the projection - we will need this to add department names to the map: 
adminData = shapefile(paste0(mapping_dir,'gtm_region.shp'))
admin_coords <- data.table(fortify(adminData, region='ID_1'))
admin_names <- data.table(adminData@data)
admin_dataset = merge(admin_coords, admin_names, by.x = 'id', by.y='ID_1', allow.cartesian=TRUE)

# ----------------------------------------------
### if you want:  Get names and id numbers corresponding to administrative areas to plot as labels: 
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
######## Load the bed net data ########
# ----------------------------------------------

bnData <- data.table(read.csv(paste0(local_dir, "prepped_data/", "bednet_prepped_data.csv")))
bnData$start_date <- as.Date(bnData$start_date,"%Y-%m-%d")


## something that might be cool is to get the absorption ratio of disb/budget by year, source, disease, etc. 
bnData$year <- year(bnData$start_date)
byVars = names(bnData)[names(bnData)%in%c('adm1','adm2','year')]
bnData  =  bnData[, list(num_persons=sum(na.omit(num_persons)), household_beds=sum(na.omit(household_beds)),
                  bed_nets=sum(na.omit( bed_nets))),by=byVars]



##luckily, the "adm2" code matches the "id" in the municipality shape file, so we can join on that 
setnames(bnData, "adm2", "id")

##add some variables of interest: 
bnData[, bn_over_beds:=bed_nets/household_beds]
bnData[, bn_over_people:=bed_nets/num_persons]

# ----------------------------------------------
######## Load the FPM data ########
# ----------------------------------------------

sicoin_data <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/prepped_sicoin_data.csv"
                                   ,fileEncoding="latin1"))



gtmBudgets <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/prepped_fpm_pudr.csv", 
                                  fileEncoding = "latin1"))
gtmBudgets$start_date <- as.Date(gtmBudgets$start_date,"%Y-%m-%d")
gtmBudgets <- gtmBudgets[!(data_source=="pudr"&year>2015)]

fpm_malaria <- gtmBudgets[(disease=="malaria"&year%in%c(2014, 2015, 2016, 2017))]
fpm_malaria <- fpm_malaria[gf_module=="Vector control"]


# ----------------------------------------------
# For time series - create an "end_date" to plot nicely without gaps 
# ----------------------------------------------
total_dataset[, end_date:=start_date+period-1]

tmp = copy(total_dataset)
tmp$start_date = NULL
setnames(tmp, 'end_date', 'start_date')
total_dataset$end_date = NULL
total_dataset = rbind(total_dataset, tmp)


# ----------------------------------------------
######## Load the antimalarial drug data  ########
# ----------------------------------------------

amData <- data.table(read.csv(paste0(local_dir, "prepped_data/", "antimalarial_prepped_data.csv")))

am_delivered <- amData[grepl("_deliveredToUser", amData$antimalarial_input)]
# ----------------------------------------------
######## Geo-maps of the data ########
# ----------------------------------------------

##color scale for the mapping values 
colScale <-  scale_fill_gradient2(low='#0606aa', mid='#00FFff', high='#ffa3b2',
                                  na.value = "grey70",space = "Lab", midpoint = 60, ## play around with this to get the gradient 
                                  # that you want, depending on data values 
                                  breaks=c(0, 20, 40, 60, 80), limits=c(0,100))

## color scale for %s (between 0 and 1) - same thing as above basically
colScale <-  scale_fill_gradient2(low='#0606aa', mid='#00FFff', high='#ffa3b2',
                                  na.value = "grey70",space = "Lab", midpoint = 0.5, ## play around with this to get the gradient 
                                  # that you want, depending on data values 
                                  breaks=c(0, 0.2, 0.4,0.6, 0.8), limits=c(0,1))


##loop that saves each plot in a list: 
gtm_plots <- list()
i = 1
for (k in unique(bnData$year)){
  shapedata <- copy(coord_and_names)
  subdata <- bnData[year==k]
  shapedata$year <- k ## add "year" to shapefile in order to merge datasets 
  # merge on the data (all.x=TRUE so the shapefile data doesn't disappear)
  graphdata  <- merge(shapedata, subdata,by=c('year','id'), all.x=TRUE, allow.cartesian=TRUE)
  plot <- (ggplot() + geom_polygon(data=graphdata, aes(x=long, y=lat, group=group, fill=bed_nets/1000)) + 
             coord_equal() + ##so the two shapefiles have the same proportions 
             geom_path() +
             geom_map(map=admin_dataset, data=admin_dataset,
                      aes(map_id=id,group=group), size=1, color="#4b2e83", alpha=0) + 
             geom_polygon(data=admin_dataset, aes(x=long, y=lat, group=group), color="#4e0589", alpha=0) + 
             colScale +
             theme_void() +  
             geom_label_repel(data = gtm_region_centroids, aes(label = NAME_1, x = long, y = lat, group = NAME_1), 
                             size = 3, fontface = 'bold', color = 'black',
                             box.padding = 0.35, point.padding = 0.3,
                           segment.color = 'grey50', nudge_x = 0.7, nudge_y = 4.5) + 
             labs(title=paste(k, "Distributed Bed Nets"), fill='# of Bed Nets (in thousands)'))
  gtm_plots[[i]] <- plot
  i=i+1
}



##export graphs as PDF 
pdf(paste0(local_dir, "visualizations/", "FILE NAME.pdf"), height=6, width=9)
invisible(lapply(gtm_plots, print))
dev.off()


# ----------------------------------------------
######## Line graphs of the bed net data ########
# ----------------------------------------------
 ggplot(total_dataset, aes(x = start_date, y= value/100000)) + 
    geom_line(aes(color=variable), size=0.75) +
    ggtitle("Guatemala GF Budget and Bed Net Distribution") +
    labs(x = "Start Date", y = "USD (100k)") +
    theme_bw()

