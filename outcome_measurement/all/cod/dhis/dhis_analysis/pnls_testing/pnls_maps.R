# Create maps of the PNLS testing data at the DPS level
# Caitlin O'Brien-Carelli
# 8/17/2019
# ----------------------------------------------

# --------------------
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr) 
library(maptools)
library(raster)
library(RColorBrewer)
library(gridExtra)
library(grid)
library(ggrepel)
# --------------------

# --------------------
# set working directories

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
# dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')
# # read in the data 
# dt = readRDS(paste0(dir, 'prepped/pnls_final/pnls_vct_final.rds'))

# local directory
dir = "C:/Users/ccarelli/Documents/pnls_data/"

# read in the data locally
dt = readRDS(paste0(dir, "pnls_vct_final_labels.rds"))

# subset the data table to only a single year
year = 2018

dt = dt[year(date)==year]

# start with all provinces, then maps for just gf health zones

#---------------------------------------------------
# import the shape file

# set working directory
setwd('C:/Users/ccarelli/Documents/drc_shape_files/')

# dps level shapefile
map = shapefile('gadm36_COD_1.shp')
coord = fortify(map, region='NAME_1')
# names = map@data$NAME_1
coord = data.table(coord)

coord[id=="Bas-Uélé", id:="Bas Uele"]
coord[id=="�???quateur" , id:="Equateur"]
coord[id=="Haut-Lomami" , id:="Haut Lomami"]
coord[id=="Haut-Uélé" , id:="Haut Uele"]
coord[id=="Kasaï", id:="Kasai"]
coord[id=="Kasaï-Central", id:="Kasai Central"]
coord[id=="Maï-Ndombe", id:="Maindombe"]
coord[id=="Nord-Kivu", id:="Nord Kivu"]
coord[id=="Nord-Ubangi", id:="Nord Ubangi"]
coord[id=="Sud-Kivu", id:="Sud Kivu"]
coord[id=="Sud-Ubangi", id:="Sud Ubangi"]
coord[id=="Kasaï-Oriental", id:="Kasai Oriental"]
coord[grep("ateur", id), id:="Equateur"]
coord[id=="Kongo-Central", id:="Kongo Central"]
coord[id=='Haut-Katanga', id:='Haut Katanga']

names = coord[ ,unique(id)]
dps = dt[ ,unique(dps)]
dps[!(dps %in% names)]
#------------------------------------------------------------------
# HIV Testing Visualizations

# export locally as a pdf
# pdf(paste0(dir, '/pnls_dps_maps.pdf'), width=14, height=9)

#----------------------
# COLOR SCHEMES

quad_colors = c('#542788','#66bd63', '#b2182b', '#4575b4')
sex_colors = c('#31a354', '#b2182b', '#4575b4')
tri_colors = c('#a50026', '#fdae61', '#abd9e9')
test_colors = c('#a50026', '#fdae61',  '#4575b4')
colors12 = c(brewer.pal(11, 'Spectral'), '#a6d96a')
bi= c("#fdae61", "#8073ac")
op = c('#f1a340', '#998ec3')
#----------------------

# subset only to the most important variables
# make DPS maps of each 

key_vars = c('Tested and received the results','HIV+')

# check you have the correct variables
dt[variable %in% key_vars, unique(variable), by=subpop]

# subset
dt = dt[variable %in% key_vars]

#---------------------------------------------------
# percent of tests on key populations, gf dps

# all tests, all hiv+, and test positivity
dps = dt[funder=='The Global Fund',.(value=sum(value)), by=.(variable, dps, subpop)]
dps = dps[variable=='Tested and received the results']

# sun to key versus general population
dps[subpop=='Clients', key:='general']
dps[subpop!='Clients', key:='key_pop']
dps = dps[ ,.(value=sum(value)), by=.(dps, key)]

# calculate the ratio of key populations tested
dps = dcast(dps, dps~key)
dps[ ,total:=key_pop+general]
dps[ ,ratio:=round(100*(key_pop/total), 1)]
setnames(dps, 'dps', 'id')

# replace missing field in equateur with 0
dps[id=='Equateur', ratio:=0]
dps[id=='Equateur', total:=0]

# merge with coordinates
coord_test = merge(dps, coord, by='id', all=T)

#---------------------------------------------------
# create labels 

# identify centroids and label them
names = data.table(coordinates(map))
setnames(names, c('long', 'lat'))
names[ , id:=unique(coord$id)]

dps[ , label:=paste0(id, ": ", ratio, "%")]
labels = dps[ ,.(id, label)]
names = merge(names, labels, by='id', all=T)

#---------------------------------------------------
# output the map

pdf(paste0(dir, 'outputs/tests_on_key_pops_map.pdf'), width=12, height=9)

ggplot(coord_test, aes(x=long, y=lat, group=group, fill=total)) + 
  geom_polygon() + 
  scale_fill_gradientn('Tests', colours=brewer.pal(9, 'YlGn')) + 
  coord_fixed(ratio=1) + 
  scale_x_continuous('', breaks = NULL) + 
  scale_y_continuous('', breaks = NULL) + 
  labs(title='Total HIV tests performed, 2018',
       subtitle='Global Fund-supported DPS') + 
  theme_minimal(base_size=16) + 
  theme(plot.caption=element_text(size=18))


ggplot(coord_test, aes(x=long, y=lat, group=group, fill=ratio)) + 
  geom_polygon() + 
  scale_fill_gradientn('Percent (%)', colours=brewer.pal(9, 'BrBG')) + 
  coord_fixed(ratio=1) + 
  scale_x_continuous('', breaks = NULL) + 
  scale_y_continuous('', breaks = NULL) + 
  labs(title='Percent of HIV tests performed on key populations, 2018',
       subtitle='Global Fund-supported DPS') + 
  theme_minimal(base_size=16) + 
  theme(plot.caption=element_text(size=18))+
geom_label_repel(data = names, aes(label = label, x = long, y = lat, group = id), inherit.aes=FALSE, size=5)

dev.off()

#---------------------------------------------------
# test positivity

# all tests, all hiv+, and test positivity
pos = dt[funder=='The Global Fund',.(value=sum(value)), by=.(variable, dps)]
pos[variable=='HIV+', variable:='hiv']
pos[variable=='Tested and received the results', variable:='tested']

# calculate the ratio of key populations tested
pos = dcast(pos, dps~variable)
pos[ ,ratio:=round(100*(hiv/tested), 1)]
setnames(pos, 'dps', 'id')

# merge with coordinates
coord_pos = merge(pos, coord, by='id', all=T)

#---------------------------------------------------
# create labels 

# identify centroids and label them
names2 = data.table(coordinates(map))
setnames(names2, c('long', 'lat'))
names2[ , id:=unique(coord$id)]

pos[ , label:=paste0(id, ": ", ratio, "%")]
labels = pos[ ,.(id, label)]
names2 = merge(names2, labels, by='id', all=T)

pdf(paste0(dir, 'outputs/test_positivity_map.pdf'), width=12, height=9)

ggplot(coord_pos, aes(x=long, y=lat, group=group, fill=ratio)) + 
  geom_polygon() + 
  scale_fill_gradientn('Percent (%)', colours=brewer.pal(9, 'Blues')) + 
  coord_fixed(ratio=1) + 
  scale_x_continuous('', breaks = NULL) + 
  scale_y_continuous('', breaks = NULL) + 
  labs(title='Percent of HIV tests that were positive, 2018',
       subtitle='Global Fund-supported DPS') + 
  theme_minimal(base_size=16) + 
  theme(plot.caption=element_text(size=18))+
  geom_label_repel(data = names2, aes(label = label, x = long, y = lat, group = id), inherit.aes=FALSE, size=5)

dev.off()

