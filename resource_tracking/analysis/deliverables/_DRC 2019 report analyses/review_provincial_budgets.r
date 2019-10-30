# Explore DRC provincial-level budgets 
# Emily Linebarger, 10/29/2019 

rm(list=ls()) 
library(data.table)
library(raster)
library(ggplot2)
library(ggrepel)
library(knitr)
library(tibble)
library(dplyr)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(rmarkdown)
library(maptools)
repo_root = "C:/Users/elineb/Documents/gf/" #Set to the root of your repository 
setwd(repo_root)
source('./core/standardizeDPSNames.R')

dt = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/COD/prepped_data/drc_provincial_budgets.RDS")

save_loc = "J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_DRC 2019 annual report/"

#Data quality error - there are a few files that I can't pin down the location for.
#Drop these out of the data for now. 
dt = dt[!is.na(province)]

#Collapse to just the columns you need. 
dt = dt[, .(budget=sum(budget, na.rm=T)), by=c('gf_module', 'gf_intervention', 'activity_description', 'disease', 'primary_recipient', 
                                               'implementer', 'province', 'start_date')]
dt[, NAME_1:=standardizeDPSNames(province)]

#Read in shapefile, and merge onto data
shapefile = shapefile("J:/Project/Evaluation/GF/mapping/cod/gadm36_COD_shp/gadm36_COD_1.shp")
shapefile@data$NAME_1 = standardizeDPSNames(shapefile@data$NAME_1)

shape_names = data.table(id = seq(0, 25, by=1), NAME_1=shapefile@data$NAME_1) #This matches the data when you fortify the shapefile below
dt = merge(dt, shape_names, by='NAME_1', all.x=T)

#Fortify data for making a map data set later.  
coord = data.table(fortify(shapefile)) 
coord[, id:=as.numeric(id)]

# -----------------------------------------------
# PLOT THE DATA 
# -----------------------------------------------
total_budget = dt[, .(budget=sum(budget, na.rm=T)), by=c('province', 'id')]
total_budget = merge(total_budget, coord, by='id', allow.cartesian=T, all.y=T)

p1 = ggplot(total_budget, aes(x=long, y=lat, group=group, fill=budget)) + 
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01) +
  scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) +
  theme_void() +
  labs(title="Total budget, by province", fill="Budget") +
  theme(plot.title=element_text(vjust=-1))

#Look at a few specific modules 
by_mod = dt[, .(budget=sum(budget, na.rm=T)), by=c('gf_module', 'province', 'id')]
by_mod = merge(by_mod, coord, by='id', allow.cartesian=T, all.y=T)
by_mod = by_mod[!is.na(gf_module)]

pdf(paste0(save_loc, "by_module_maps.pdf"), height=8, width=12) 

for (m in unique(by_mod$gf_module)){
  plot_data = by_mod[gf_module==m]
  additional_provinces = coord[!id%in%plot_data$id]
  additional_provinces[id%in%dt$id, budget:=0]
  plot_data = rbind(plot_data, additional_provinces, fill=T) #Make sure you have all of the boundaries, even if a given province didn't have that module. 
  p = ggplot(plot_data, aes(x=long, y=lat, group=group, fill=budget)) + 
    coord_fixed() +
    geom_polygon() +
    geom_path(size=0.01) +
    scale_fill_gradientn(colors=(brewer.pal(9, 'Greens'))) +
    theme_void() +
    labs(title=paste0("Budget for ", m, " by province"), fill="Budget") +
    theme(plot.title=element_text(vjust=-1))
print(p) 
} 
dev.off() 

