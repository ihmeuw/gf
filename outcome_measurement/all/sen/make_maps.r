# ---------------------------------------------------
# David Phillips
# 
# 5/13/2019
# Code that makes maps from LBD estimates for Senegal
# ---------------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2) 
library(RColorBrewer) 
library(raster) 
library(grid)
library(gridExtra)
library(rgeos)
# --------------------


# ----------------------------------------------------------------
# Store file and folder names

# root directory CHANGE THIS TO THE LOCATION OF YOUR BASECAMP DATA
dir = 'C:/Users/davidp6/Google Drive/Work/IHME Work/GF/Workshops/sen_may2019/Data/LBD'

# change working directory to the root directory
setwd(dir)

# store raster file names
hivFile = 'Raw HIV Files/IHME_AFRICA_HIV_2000_2017_HIV_PREV_MEAN_2017_Y2019M03D15.TIF'
pfprFile = 'Raw Malaria Files/MODEL43.2015.PR.rmean.stable.tif'
antimalarialFile = 'Raw Malaria Files/2015.ACT.tif'
itnFile = 'Raw Malaria Files/2015.ITN.use.yearavg.adj.stable.tif'
untreatedFile = 'Raw Malaria Files/2015_15-plus_untreated_incidence_count_mean.tif'
incidenceFile = 'Raw Malaria Files/2015_all_ages_incidence_rate_mean.tif'
mortalityFile = 'Raw Malaria Files/children_malaria_deaths_mean_2015.tif'

# store shapefile names
shapeFile = 'SEN_adm/SEN_adm2.shp'

# list of indicators (ACT, IRS, and untreated are national level only)
inds = c('hiv','pfpr','antimalarial','itn','incidence','mortality')

# output file name
outFile = 'LBD_Maps.pdf'
# ----------------------------------------------------------------


# ----------------------------------------------------
# Load prep/data

# load each 2015 file and store them in a list
rasters = lapply(inds, function(ind) {
	return(raster(get(paste0(ind, 'File'))))
})

# load Senegal shapeFile
shapeData = shapefile(shapeFile)

# clip each raster to Senegal
rasters = lapply(seq(length(rasters)), function(i) {
	tmp = crop(rasters[[i]], extent(shapeData))
	mask(tmp, shapeData)
})

# simplify the shapefile so it's faster
mapDatatmp = shapeData@data
shapeData = gSimplify(shapeData, tol=0.01, topologyPreserve=TRUE)
shapeData = as(shapeData, 'SpatialPolygonsDataFrame')
shapeData@data = mapDatatmp
# ----------------------------------------------------


# --------------------------------------------------------------
# Format as data.tables and append all rasters together

# format as data.table and append together by indicator
rasterDts = lapply(seq(length(rasters)), function(i) {
	tmp = data.table(as.data.frame(rasters[[i]], xy=TRUE))
	tmp[, country:='Senegal']
	setnames(tmp, c('x','y',inds[i],'country'))
})

# multiply percentages by 100
rasterDts[[2]][, pfpr:=pfpr*100]
rasterDts[[3]][, antimalarial:=antimalarial*100]
rasterDts[[4]][, itn:=itn*100]

# format shapedata as data.table too
map = data.table(fortify(shapeData))
map[,country:='Senegal']
# --------------------------------------------------------------


# --------------------------------------------------------------
# Truncate the scales to avoid outlier pixels (probably from neighboring countries)
inds = c('hiv','pfpr','antimalarial','itn','incidence','mortality')

# pfpr
q99 = quantile(rasterDts[[which(inds=='pfpr')]]$pfpr, .99, na.rm=TRUE)
rasterDts[[which(inds=='pfpr')]][pfpr>q99, pfpr:=q99]

# antimalarial
q99 = quantile(rasterDts[[which(inds=='antimalarial')]]$antimalarial, .99, na.rm=TRUE)
rasterDts[[which(inds=='antimalarial')]][antimalarial>q99, antimalarial:=q99]

# incidence
q99 = quantile(rasterDts[[which(inds=='incidence')]]$incidence, .99, na.rm=TRUE)
rasterDts[[which(inds=='incidence')]][incidence>q99, incidence:=q99]
# --------------------------------------------------------------


# ----------------------------------------------------------------
# Set up to graph

# set up colors
colors = brewer.pal(10, 'Spectral')

# set up labels and legends
labs = c('HIV Prevalence','PfPR','Antimalarial','ITN','Incidence','Child Mortality')
legs = c('(%)', ' (%)',' Coverage',' Coverage',' (rate)',' (rate)')

# identify which indicators should have reversed colors
revs = c(0,0,1,1,1,1,0,0,0)
		
# set up margins
# mSenegal = c(-3,-2,-3,-3)
# ----------------------------------------------------------------


# ----------------------------------------------------------------
# Map each raster

# set up a list to store plots
plots = list()

# loop over indicators	
for(i in seq(length(inds))) { 
	
	# store labels
	lab = labs[i]
	leg = legs[i]
	
	# reverse colors if necessary
	tmpColors = ifelse(revs[i]==1, rev(colors), colors)

	# store plot
	plots[[i]] = ggplot() + 
		geom_tile(data=rasterDts[[i]], 
			aes_string(y='y', x='x', fill=inds[i])) + 
		geom_path(data=map, color='grey65', size=.05, 
			aes(y=lat, x=long, group=group), inherit.aes=FALSE) + 
		scale_fill_gradientn(colors=colors, na.value='white') + 
		coord_fixed(ratio=1) + 
		scale_x_continuous('', breaks = NULL) + 
		scale_y_continuous('', breaks = NULL) + 
		labs(title=paste(lab, leg), fill='') + 
		theme_minimal(base_size=16) + 
		theme(plot.title=element_text(hjust=.5)) 
}
# ----------------------------------------------------------------


# -------------------------------
# Save
pdf(outFile, height=6, width=10)
for(i in seq(length(inds))) {
	grid.draw(plots[[i]])
}
dev.off()
# -------------------------------
