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

# year
year = 2014

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
# ----------------------------------------------


# ------------------------------------------------------
# Load/prep data
source('./outcome_measurement/malaria/load_map_data.r')
# ------------------------------------------------------


# ----------------------------------------------------------------------
# Set up to graph

# colors
cols = brewer.pal(6, 'Spectral')
border = 'grey65'

# legend limits so both countries are on same scale
min = floor(min(c(dataUGAitn$value, dataCODitn$value), na.rm=TRUE)*100)
max = ceiling(max(c(dataUGAitn$value, dataCODitn$value), na.rm=TRUE)*100)
# ----------------------------------------------------------------------


# ----------------------------------------------
# Map comparing countries

# store maps separately because geom_tile bug with facetting
uga = ggplot(dataUGAitn, aes(y=y, x=x, fill=value*100)) + 
	geom_tile() + 
	geom_path(data=shapeDataUGA, aes(x=long, y=lat, group=group)
		, color=border, size=.05, inherit.aes=FALSE) + 
	scale_fill_gradientn('% ITN\nUsage', colors=cols, 
		na.value='white', limits=c(min,max)) + 
	coord_fixed(ratio=1) + 
	scale_x_continuous('', breaks = NULL) + 
	scale_y_continuous('', breaks = NULL) + 
	labs(title='Uganda') + 
	theme_minimal(base_size=16) + 
	theme(plot.title=element_text(hjust=.5)) 
	
cod = ggplot(dataCODitn, aes(y=y, x=x, fill=value*100)) + 
	geom_tile() + 
	geom_path(data=shapeDataCOD, aes(x=long, y=lat, group=group)
		, color=border, size=.05, inherit.aes=FALSE) + 
	scale_fill_gradientn('% ITN\nUsage', colors=cols, 
		na.value='white', limits=c(min,max)) + 
	coord_fixed(ratio=1) + 
	scale_x_continuous('', breaks = NULL) + 
	scale_y_continuous('', breaks = NULL) + 
	labs(title='DRC') + 
	theme_minimal(base_size=16) + 
	theme(plot.title=element_text(hjust=.5), legend.position='none') 
	
# put maps together
p1 = arrangeGrob(cod, uga, ncol=2)
# ----------------------------------------------


# ----------------------------------------------
# Map comparing intervention coverage

# store maps separately because geom_tile bug with facetting
plots = list()
for(i in inds) {
		# assign title
		if (i=='itn') title = 'ITN Usage'
		if (i=='act') title = 'ACT Coverage'
		if (i=='antmal') title = 'Antimalarial Coverage'
		if (i=='irs') title = 'IRS Coverage'
		if (i=='prev') title = 'Prevalence (PfPR)'
		
		# store map
		plots[[i]] = ggplot(get(paste0('dataCOD',i)), aes(y=y, x=x, fill=value*100)) + 
			geom_tile() + 
			geom_path(data=shapeDataCOD, aes(x=long, y=lat, group=group)
				, color=border, size=.05, inherit.aes=FALSE) + 
			scale_fill_gradientn('%', colors=cols, na.value='white') + 
			coord_fixed(ratio=1) + 
			scale_x_continuous('', breaks = NULL) + 
			scale_y_continuous('', breaks = NULL) + 
			labs(title=title) + 
			theme_minimal(base_size=16) + 
			theme(plot.title=element_text(hjust=.5)) 
}
# ----------------------------------------------


# --------------------------------
# Save graphs
pdf(graphFile, height=6, width=9)
grid.newpage()
grid.draw(p1)
plots[[1]]
plots[[2]]
plots[[3]]
dev.off()
# --------------------------------
