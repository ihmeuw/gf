# --------------------------------------------------------
# David Phillips
#
# 11/17/2017
# Make simple maps of malaria indicators for PCE countries
# The working directory should be the root of this repo
# --------------------------------------------------------

# note: geom_tile has a bug when combined with facet_wrap
# https://github.com/tidyverse/ggplot2/issues/849

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(raster)
library(rgdal)
library(rgeos)
library(RColorBrewer)
library(ggplot2)
library(grid)
library(gridExtra)
# --------------------


# ----------------------------------------------
# Parameters and settings

# iso3s
iso3s = c('COD','UGA')

# year
years = 2015

# indicators
inds = c('itn','antmal','prev')

# whether to crop to the inner 99th percentile for viz purposes
crop = TRUE
# ----------------------------------------------


# ----------------------------------------------
# Files and directories

# data directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/multi_country/map/')

# output files
graphFile = paste0(dir, '/visualizations/descriptive_maps.pdf')

# load prep function
source('./outcome_measurement/malaria/load_map_data.r')
# ----------------------------------------------


# ------------------------------------------------------
# Load/prep data
out = loadMapData(iso3s, years, inds, crop)
data = out$data
mapCOD = out$maps$COD
mapUGA = out$maps$UGA
shapeData = out$shapeData
# ------------------------------------------------------


# ----------------------------------------------------------------------
# Set up to graph

# colors
cols1 = rev(brewer.pal(6, 'RdYlBu'))
cols2 = brewer.pal(6, 'RdYlGn')
border = 'grey65'

# legend limits so both countries are on same scale
limsprev = range(data$prev, na.rm=TRUE)*100
limsitn = range(data$itn, na.rm=TRUE)*100
limsantmal = range(data$antmal, na.rm=TRUE)*100
# ----------------------------------------------------------------------


# ----------------------------------------------
# Maps comparing countries

# store maps separately because geom_tile bug with facetting

# prevalence (separate legends for visibility, modify margins of drc to make it slightly larger)
ugaprev = ggplot(data[iso3=='UGA'], aes(y=y, x=x, fill=prev*100)) + 
	geom_tile() + 
	geom_path(data=shapeData[iso3=='UGA'], aes(x=long, y=lat, group=group)
		, color=border, size=.05, inherit.aes=FALSE) + 
	scale_fill_gradientn('PfPR', colors=cols1, 
		na.value='white') + 
	coord_fixed(ratio=1) + 
	scale_x_continuous('', breaks = NULL) + 
	scale_y_continuous('', breaks = NULL) + 
	labs(title='Uganda') + 
	theme_minimal(base_size=16) + 
	theme(plot.title=element_text(hjust=.5)) 
	
codprev = ggplot(data[iso3=='COD'], aes(y=y, x=x, fill=prev*100)) + 
	geom_tile() + 
	geom_path(data=shapeData[iso3=='COD'], aes(x=long, y=lat, group=group)
		, color=border, size=.05, inherit.aes=FALSE) + 
	scale_fill_gradientn('PfPR', colors=cols1, 
		na.value='white') + 
	coord_fixed(ratio=1) + 
	scale_x_continuous('', breaks = NULL) + 
	scale_y_continuous('', breaks = NULL) + 
	labs(title='DRC') + 
	theme_minimal(base_size=16) + 
	theme(plot.title=element_text(hjust=.5), plot.margin=unit(rep(-1,4), 'cm')) 

# ITN
ugaitn = ggplot(data[iso3=='UGA'], aes(y=y, x=x, fill=itn*100)) + 
	geom_tile() + 
	geom_path(data=shapeData[iso3=='UGA'], aes(x=long, y=lat, group=group)
		, color=border, size=.05, inherit.aes=FALSE) + 
	scale_fill_gradientn('% ITN\nUsage', colors=cols2, 
		na.value='white', limits=limsitn) + 
	coord_fixed(ratio=1) + 
	scale_x_continuous('', breaks = NULL) + 
	scale_y_continuous('', breaks = NULL) + 
	labs(title='Uganda') + 
	theme_minimal(base_size=16) + 
	theme(plot.title=element_text(hjust=.5)) 
	
coditn = ggplot(data[iso3=='COD'], aes(y=y, x=x, fill=itn*100)) + 
	geom_tile() + 
	geom_path(data=shapeData[iso3=='COD'], aes(x=long, y=lat, group=group)
		, color=border, size=.05, inherit.aes=FALSE) + 
	scale_fill_gradientn('% ITN\nUsage', colors=cols2, 
		na.value='white', limits=limsitn) + 
	coord_fixed(ratio=1) + 
	scale_x_continuous('', breaks = NULL) + 
	scale_y_continuous('', breaks = NULL) + 
	labs(title='DRC') + 
	theme_minimal(base_size=16) + 
	theme(plot.title=element_text(hjust=.5), legend.position='none') 

# antimalarials
ugaantmal = ggplot(data[iso3=='UGA'], aes(y=y, x=x, fill=antmal*100)) + 
	geom_tile() + 
	geom_path(data=shapeData[iso3=='UGA'], aes(x=long, y=lat, group=group)
		, color=border, size=.05, inherit.aes=FALSE) + 
	scale_fill_gradientn('ACT %', colors=cols2, 
		na.value='white') + 
	coord_fixed(ratio=1) + 
	scale_x_continuous('', breaks = NULL) + 
	scale_y_continuous('', breaks = NULL) + 
	labs(title='Uganda') + 
	theme_minimal(base_size=16) + 
	theme(plot.title=element_text(hjust=.5)) 
	
codantmal = ggplot(data[iso3=='COD'], aes(y=y, x=x, fill=antmal*100)) + 
	geom_tile() + 
	geom_path(data=shapeData[iso3=='COD'], aes(x=long, y=lat, group=group)
		, color=border, size=.05, inherit.aes=FALSE) + 
	scale_fill_gradientn('ACT %', colors=cols2, 
		na.value='white') + 
	coord_fixed(ratio=1) + 
	scale_x_continuous('', breaks = NULL) + 
	scale_y_continuous('', breaks = NULL) + 
	labs(title='DRC') + 
	theme_minimal(base_size=16) + 
	theme(plot.title=element_text(hjust=.5), plot.margin=unit(rep(-1,4), 'cm')) 
	
# put maps together
p1 = arrangeGrob(codprev, ugaprev, ncol=2)
p2 = arrangeGrob(coditn, ugaitn, ncol=2)
p3 = arrangeGrob(codantmal, ugaantmal, ncol=2)
# ----------------------------------------------


# --------------------------------
# Save graphs
pdf(graphFile, height=6, width=9)
grid.newpage()
grid.draw(p1)
grid.newpage()
grid.draw(p2)
grid.newpage()
grid.draw(p3)
dev.off()
# --------------------------------
