
# ----------------------------------------------
# Audrey Batzel
# 6/26/2018
# Map visualizations of PNLP outputs data
#
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
  username <- "abatzel"
  root <- "J:/"
} else {
  username <- Sys.getenv("USER")
  root <- "/home/j/"
}
# ----------------------------------------------
##set set up the directories to read/export files: 
# ----------------------------------------------
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j/')
shape_dir <- paste0(j, "Project/Evaluation/GF/outcome_measurement/cod/drc_shapefiles/gadm36_COD_shp/")
#shape_dir2 <- paste0(j, "Project/Evaluation/GF/outcome_measurement/cod/")
#shape_dir3 <- paste0(j, "Project/Evaluation/GF/outcome_measurement/cod/shapefiles/")
export_dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/visualizations/')
dir_data <- paste0(j, "/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/")

drcShape <- shapefile(paste0(shape_dir, "gadm36_COD_1.shp"))
#drcShape2 <- shapefile(paste0(shape_dir, "gadm36_COD_2.shp"))
#drcShape0 <- shapefile(paste0(shape_dir, "gadm36_COD_0.shp"))

#drcShape_test <- shapefile(paste0(shape_dir3, "healthsites.shp"))

# these don't save properly so you may have to re-copy and paste the name in the shapefile
coordinates = as.data.table(fortify(drcShape, region='NAME_1'))
coordinates$id <- gsub("Bas-Uélé", "Bas-Uele", coordinates$id)
coordinates$id <- gsub("�???quateur", "Equateur", coordinates$id)
coordinates$id <- gsub("Haut-Uélé", "Haut-Uele", coordinates$id)
coordinates$id <- gsub("Kasaï", "Kasai", coordinates$id)
coordinates$id <- gsub("Kasaï-Central", "Kasai Central", coordinates$id)
coordinates$id <- gsub("Kasaï-Oriental", "Kasai Oriental", coordinates$id)
coordinates$id <- gsub("Maï-Ndombe", "Mai-Ndombe", coordinates$id)

coordinates$id <- tolower(coordinates$id)

coordinates$id <- gsub("nord-kivu", "nord kivu", coordinates$id)
coordinates$id <- gsub("sud-kivu", "sud kivu", coordinates$id)
coordinates$id <- gsub("kasai-central", "kasai central", coordinates$id)
coordinates$id <- gsub("kasai-oriental", "kasai oriental", coordinates$id)
coordinates$id <- gsub("nord-ubangi", "nord ubangi", coordinates$id)
coordinates$id <- gsub("sud-ubangi", "sud ubangi", coordinates$id)
coordinates$id <- gsub("kongo-central", "kongo central", coordinates$id)

# names = data.table(drcShape@data)
# names$zs_id <- as.character(names$zs_id)
# coord_and_names = merge(coordinates, names, by.x='id', by.y='zs_id', allow.cartesian=TRUE)

# ----------------------------------------------  

# input files
fullData <- "imputedData_forGraphing_run2.rds"

# read in data
dt <- readRDS( paste0(dir_data, fullData) ) 

# ----------------------------------------------  
id_vars <- c("dps", "health_zone", "date", "year")

dt[, year:= year(date)]
dt$dps <- gsub("bas congo", "kongo central", dt$dps)

subpop <- NA
indicators <- c("stockOutASAQ", "stockOutartLum")
subpop <- c("14yrsAndOlder", "6to13yrs", "1to5yrs", "2to11mos") #, "used", "1st", "2nd", "3rd")
y=2017
makeMap(indicators, subpop, 2017)


i <- 1
drc_plots <- list()
makeMap <- function(indicators, subpop, y) {
  if( is.na(subpop)){
    dtSubset <- dt[year==y & indicator %in% indicators,]
  } else {
    dtSubset <- dt[year==y & indicator %in% indicators & subpopulation %in% subpop,]
  }
  dtSubset <- dtSubset[is.na(subpopulation) | subpopulation %in%  c("14yrsAndOlder", "6to13yrs", "1to5yrs", "2to11mos"),]
  dtMap <- dtSubset[, .(totalStockouts = sum(mean, na.rm=T)), by=c("dps", "year")]
  
  dtSubsetFac <- dt[year==y & indicator== "healthFacilities" & subpopulation== "total",]
  dtMapFac <- dtSubsetFac[, .(avgFac = mean(mean)), by=.(dps, year, health_zone)]
  dtMapFac <- dtMapFac[, .(totFac = sum(avgFac)), by=.(dps, year)]
  
  dtMap <- merge(dtMap, dtMapFac, all=T, by=c("dps","year"))
  dtMap <- dtMap[, total := totalStockouts/totFac]
  
  # dtMapWide <- dcast(dtMap, dps + year ~ indicator, value.var="total")
  # dtMapWide[, percentUsed := ((ASAQused/ASAQreceived)*100)]
  
  graphData <- merge(coordinates, dtMap, by.x='id', by.y='dps', all=TRUE, allow.cartesian=TRUE)
  
  # if (mean(dtMap$total)>1000000){
  #   graphData <- graphData[, totalTransformed := total/1000000]
  #   units = 1000000
  # } else 
    if (mean(dtMap$total)>100000){
    graphData <- graphData[, totalTransformed := total/100000]
    units = 100000
  } else if (mean(dtMap$total)>10000){
    graphData <- graphData[, totalTransformed := total/10000]
    units = 10000
  } else if (mean(dtMap$total)>1000) {
    graphData <- graphData[, totalTransformed := total/1000]
    units = 1000
  } else {
    graphData <- graphData[, totalTransformed := total]
    units = NA
  }
  
  max = max(graphData$totalTransformed)
  min = min(graphData[id != "0", totalTransformed])
  mid = (max/2)
  
  plot <- (ggplot() + geom_polygon(data=graphData, aes(x=long, y=lat, group=group, fill=totalTransformed)) + 
             coord_equal() + ##so the two shapefiles have the same proportions 
             geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
             # geom_map(map=admin_dataset, data=admin_dataset,
             #          aes(map_id=id,group=group), size=1, color="#4b2e83", alpha=0) + 
             # geom_polygon(data=admin_dataset, aes(x=long, y=lat, group=group), color="#4e0589", alpha=0) + 
             scale_fill_gradient2(low='#fee0d2', mid='#fb6a4a', high='#99000d',             # blues - (low='#9aeaea', mid='#216fff', high='#0606aa',
                                  na.value = "grey70",space = "Lab", midpoint = mid, ## play around with this to get the gradient 
                                  # that you want, depending on data values 
                                  breaks=round(seq(0, (max*0.90), by=((max*0.90)/4))), limits=c(0,max)) + 
            #theme_void()) +  labs(title= paste0("DRC: ", indicators, " ", subpop, " by Province, ", y), fill=paste0('Doses (in ', as.integer(units),"s)"))
            theme_void()+
            theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=10), plot.caption=element_text(size=12))) +  
            labs(title= paste0("Stock-outs of ACTs at Health Facilities ", y), fill=paste0('Days per facility')) +
            labs(caption="Source: Programme National de Lutte contre le Paludisme (PNLP)")
  
  print(plot)

           #   # geom_label_repel(data = gtm_region_centroids, aes(label = NAME_1, x = long, y = lat, group = NAME_1), 
           #   #                  size = 3, fontface = 'bold', color = 'black',
           #   #                  box.padding = 0.35, point.padding = 0.3,
           #   #                  segment.color = 'grey50', nudge_x = 0.7, nudge_y = 4.5) + 
           #   


  drc_plots[[i]] <- plot
  i <- i+1
}
#-------------------------------------------



##export graphs as PDF 
pdf(paste0(export_dir, "asaq_by_healthzone.pdf"), height=8, width=8)
invisible(lapply(drc_plots, print))
dev.off()



  