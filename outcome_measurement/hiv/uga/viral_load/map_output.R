# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 4/10/2019
# Rbind the UVL data sets together
# Run dist_facilities_uvl.R to download facility and district names
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(ggplot2)
library(stringr) 
library(plyr)
library(RColorBrewer)
library(raster)
library(maptools)
library(gplots)
library(corrplot)
library(raster)
library(maptools)
library(ggrepel)
# --------------------

# -----------------------------------------------
# detect if operating on windows or on the cluster 

j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

inDir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/age_analyses/')
outDir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/age_analyses/')

# -----------------------------------------------

dt = readRDS(paste0(inDir, 'model_output.rds'))

# ----------------------------------------------------------------------
# Files and directories

# set working directory
setwd(paste0(j, '/Project/Evaluation/GF/mapping/uga/'))

# uganda shapefile
DistMap = shapefile('uga_dist112_map.shp')

#------------------------------
# create a regional shape file 

# import the ten regions that are included in phia
regions = fread(paste0(j, "/Project/Evaluation/GF/mapping/uga/uga_geographies_map.csv"))
regions = unique(regions[ ,.(region=region10_alt, district_name=dist112_name)])

# put the regions in the same order as the shape file
regions = regions[match(DistMap@data$dist112_n, regions$district_name)]
id = regions$region

# create coordinates for the old and new plots
RegMap  = unionSpatialPolygons(DistMap, id)

#------------------------------
# fortify the maps 
reg_coord = fortify(RegMap)

#-------------------------------
# test code

dt
plot(RegMap)

dt[ ,c('lower', 'upper'):=NULL]

dt[ ,ratio:=round(100*ratio, 1)]

#-------------------------------
# subset to the regional estimates and ratios by sex, age, region

dt = dt[age_cat=='15 - 19' | age_cat=='20 - 24']
pred = dt[year==2018 ,.(predictions = unique(predictions)), by=.(id = region, sex, age_cat)]

pred15 = pred[age_cat=='15 - 19']
ratio_15 = dt[age_cat=='15 - 19' & year==2018, .(ratio=round(100*sum(suppressed/sum(valid_results)), 1)), by=.(sex, id=region)]

pred15 = merge(pred15, ratio_15, by=c('id', 'sex'))


# map of 15 - 19 year olds
  
  # double the coordinates
  reg_coord_sex = data.table(rbind(reg_coord, reg_coord))
  reg_coord_sex[ ,sex:=rep(c('Female', 'Male'), each=nrow(reg_coord))]

  #------------------------------
  # merge to map and melt data
  mapDataReg = merge(pred15, reg_coord_sex, by=c('id', 'sex'), all.x=TRUE)
  mapDataReg = data.table(melt(mapDataReg, id.vars=c('long','lat', 'id','group','order','hole','piece', 'age_cat', 'sex')))
  
  # --------------------------------------------------------------------
  # create labels for the regional maps
  
  # identify centroids and label them
  names = data.table(coordinates(RegMap))
  setnames(names, c('long', 'lat'))
  names[ , id:=unique(reg_coord$id)]
  
  # merge in the ratios for complete labels
  names = merge(names, pred15, by='id', all.y=T)
  
  # replace labels with hyphens to match phia graphics
  names$id = gsub(names$id, pattern='_', replacement='-')
  names[grep('^Central', id), id:=(gsub(id, pattern='-', replacement=' '))]
  names[grep('^West', id), id:=(gsub(id, pattern='-', replacement=' '))]
  
  # create the final labels
  names[ , predictions:=round(predictions, 1)]
  names[ , label_pred:=paste0(id, ': ', predictions, '%')]
  names[ , label_ratio:=paste0(id, ': ', ratio , '%')]
    
  # finalize labels
  labels_pred = names[ , .(long, lat, sex, label_pred)]
  labels_ratio = names[ , .(long, lat, sex, label_ratio)]
  # --------------------------------------
  
  p1 = ggplot(mapDataReg[variable=='predictions'], aes(x=long, y=lat, group=group, fill=value)) + 
    geom_polygon() + 
    scale_fill_gradientn('VS %', colours=brewer.pal(9, 'Reds')) + 
    coord_fixed(ratio=1) + 
    scale_x_continuous('', breaks = NULL) + 
    scale_y_continuous('', breaks = NULL) + 
    facet_wrap(~sex) + 
    labs(title='2018 predicted viral suppression, females and males 20 - 24', 
         caption='Source: Uganda Viral Load Dashboard') + 
    theme_minimal(base_size=20) + 
    theme(plot.caption=element_text(size=14)) +
    geom_label_repel(data = labels_pred, aes(label = label_pred, x = long, y = lat, group = label_pred), inherit.aes=FALSE, size=4) 
  
  p2 = ggplot(mapDataReg[variable=='ratio'], aes(x=long, y=lat, group=group, fill=value)) + 
    geom_polygon() + 
    scale_fill_gradientn('VS %', colours=brewer.pal(9, 'Blues')) + 
    coord_fixed(ratio=1) + 
    scale_x_continuous('', breaks = NULL) + 
    scale_y_continuous('', breaks = NULL) + 
    facet_wrap(~sex) + 
    labs(title='2018 viral suppression, females and males 20 - 24', 
         caption='Source: Uganda Viral Load Dashboard') + 
    theme_minimal(base_size=20) + 
    theme(plot.caption=element_text(size=14)) +
    geom_label_repel(data = labels_ratio, aes(label = label_ratio, x = long, y = lat, group = label_ratio), inherit.aes=FALSE, size=4) 
  
  bar = names[ ,.(id, sex, ratio, predictions)]
  bar = melt(bar, id.vars=c('id', 'sex'))
  bar[variable=='predictions', variable:='Predicted suppression']
  bar[variable=='ratio', variable:='Original ratio']
  
  # samples received by sex, age, year - all years
  ggplot(bar, aes(x=id, y=value, fill=sex)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw() +
    facet_wrap(~variable) +
    labs(title='Percent virally suppressed, ages 15 - 19, 2018',
         y='Viral suppression ratio', x='Region', fill='Sex') +
    coord_cartesian(ylim=c(50, 100)) +
    theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90))
  
  
  
  
  
  p2 = ggplot(mapDataReg[year==2017 & variable=='vl_suppression'], aes(x=long, y=lat, group=group, fill=value)) + 
    geom_polygon() + 
    facet_wrap(~sex) + 
    scale_fill_gradientn('VS %', colours=brewer.pal(9, 'Blues')) + 
    coord_fixed(ratio=1) + 
    scale_x_continuous('', breaks = NULL) + 
    scale_y_continuous('', breaks = NULL) + 
    labs(title='2017 viral suppression, females and males 15 - 24', 
         caption='Source: Uganda Viral Load Dashboard') + 
    theme_minimal(base_size=20) + 
    theme(plot.caption=element_text(size=14)) +
    geom_label_repel(data = labels_2017, aes(label = label, x = long, y = lat, group = label), inherit.aes=FALSE, size=4) 
  
  
  pdf(paste0(outDir, 'age_maps.pdf'), width=12, height =9)
  p1
  p2
  dev.off()