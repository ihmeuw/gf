
# ----------------------------------------------
# Irena Chen 
# 6/20/2018
# Start visualizations of the VR data 
## RUN THIS ON THE CLUSTER SINCE SOME FILES ARE VERY BIG 
# ----------------------------------------------
###### Set up R / install packages  ###### 
# ----------------------------------------------

rm(list=ls())
library(raster)
library(rgeos)
library(data.table)
library(ggplot2)
library(maptools)
# ----------------------------------------------
##set up J Drive 
# ----------------------------------------------
if (Sys.info()[1] == 'Windows') {
  username <- "irenac2"
  root <- "J:/"
} else {
  username <- Sys.getenv("USER")
  root <- "/home/j/"
}
# ----------------------------------------------
##set set up the directories to read/export files: 
# ----------------------------------------------
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
raster_dir = paste0(j, '/Project/Evaluation/GF/covariates/gtm/')
shape_dir <- paste0(j, '/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/')
vr_dir <-  paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/prepped_data/')
export_dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/visualizations/')
merge_dir <- paste0(j, "/WORK/11_geospatial/11_vr/vr_data_inputs/muni_merges/GTM/")

## ----------------------------------------------
## read files 
# ----------------------------------------------
popData = raster(paste0(raster_dir, "/worldpop/Guatemala 100m Population/GTM_pph_v2b_2015.tif"))
vrData <- data.table(fread(paste0(vr_dir, "vr_2009_2016.csv")))
vectorData = shapefile(paste0(shape_dir, "vr_gaul_gtm.shp"))
mergeData <- data.table(fread(paste0(merge_dir, "GTM_muni_merges_2009_2016.csv")))

# ----------------------------------------------
## set up shape data 
# ----------------------------------------------
coordinates = data.table(fortify(vectorData, region='GAUL_CODE'))
names = data.table(vectorData@data)
coord_and_names = merge(coordinates, names, by.x='id', by.y='GAUL_CODE', allow.cartesian=TRUE)



# ----------------------------------------------
## subset TB deaths from the overall VR data 
# ----------------------------------------------
tb_death_codes <- c(297,
                    954,
                    934,
                    946,
                    947
)
vrTb <- vrData[cause_id%in%tb_death_codes]


setnames(vrTb, "deaths", "tb_deaths")
##most recent year is 2015 for raster, so subset the VR data to 2015 as well: 
tb_map_dataset <- vrTb[year_id==2015]

# right now, we don't care about the other variables (just year and location )
byVars = names(vrTb)[names(vrTb)%in%c('year_id', 'location_id')]
tb_map_dataset= vrTb[, list(tb_deaths=sum(na.omit(tb_deaths))), by=byVars]

## total deaths 
byVars = names(vrData)[names(vrData)%in%c('year_id', 'location_id')]
annualVr= vrData[, list(deaths=sum(na.omit(deaths))), by=byVars]

##merge the two datasets back together 
tb_map_dataset <- merge(tb_map_dataset, annualVr, by=c("location_id", "year_id"))

##calculate TB mortality over total mortality
tb_map_dataset[,tb_rate:=tb_deaths/deaths]


# ----------------------------------------------
## connect the VR dataset to the shapefile dataset  
# ----------------------------------------------

##dataset that joins the shape file to the VR data: 
tb_map_merge <- merge(tb_map_dataset, mergeData, by.x="location_id", by.y="adm2_gbd_id", all.x=TRUE)

merge_coords <- merge(mergeData, coord_and_names, by.x="uid", by.y="id", allow.cartesian=TRUE)
#rename the loc id so that we can join it to the VR data later: 
setnames(merge_coords, "adm2_gbd_id", "location_id")

##### I don't even think we needed this but including it in case we ever want to use population ### 
# aggRaster <- extract(popData, vectorData)
# aggRaster = data.table(do.call('rbind',lapply(aggRaster, sum, na.rm=TRUE)))
# aggRaster[, GAUL_CODE:=vectorData@data[,'GAUL_CODE']]


# ----------------------------------------------
## municipality graphs by year 
# ----------------------------------------------
gtm_plots <- list()
i=1
for(k in unique(tb_map_dataset$year_id)){
  shapedata <- copy(merge_coords)
  graphData <- tb_map_dataset[year_id==k]
  shapedata$year_id <- k
  graphdata <- merge(graphData, shapedata, by=c("year_id", "location_id"),
                     all.y=TRUE, allow.cartesian=TRUE) ##all.y so that shape data doesn't get dropped
  graphdata[,tb_rate:=deaths/V1]
  plot <- (ggplot() + geom_polygon(data=graphdata, aes(x=long, y=lat, group=group, fill=tb_rate)) + 
           coord_equal() + ##so the two shapefiles have the same proportions 
           geom_path() +
           # geom_map(map=admin_dataset, data=admin_dataset,
           #          aes(map_id=id,group=group), size=1, color="#4b2e83", alpha=0) + 
           # geom_polygon(data=admin_dataset, aes(x=long, y=lat, group=group), color="#4e0589", alpha=0) + 
            scale_fill_gradient2(low='#9aeaea', mid='#216fff', high='#0606aa',
                                  na.value = "grey70",space = "Lab", midpoint = 10, ## play around with this to get the gradient 
                                  # that you want, depending on data values 
                                  breaks=c(0, 10, 20, 30, 40, 50), limits=c(0,50)) + 
           theme_void() +  
           # geom_label_repel(data = gtm_region_centroids, aes(label = NAME_1, x = long, y = lat, group = NAME_1), 
           #                  size = 3, fontface = 'bold', color = 'black',
           #                  box.padding = 0.35, point.padding = 0.3,
           #                  segment.color = 'grey50', nudge_x = 0.7, nudge_y = 4.5) + 
           labs(title=paste0(k, " Guatemala TB Mortality by Municipality"), fill='TB Deaths/Total Deaths'))
gtm_plots[[i]] <- plot
i=i+1
}

##export graphs as PDF 
pdf(paste0(export_dir, "tb_mortality_by_muni.pdf"), height=6, width=9)
invisible(lapply(gtm_plots, print))
dev.off()








