# ---------------------------------------------------
# David Phillips
#
# 1/22/2017
# Look up numbers for 2018 country/synthesis reports
# ---------------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(raster)
# --------------------


# -------------------------------------------------------------------------
# Files and directories

# data directory (temporary location)
dir = 'J:/Project/Evaluation/GF/outcome_measurement/multi_country/map/'

# input file
pfprFile = paste0(dir, 'Africa Cube Public Data/Prevalence_annual_means_rasters/rasters/MODEL43.2015.PR.rmean.stable.tif')
actFile = paste0(dir, '/NEJM Rasters/spatially_disaggregated_antimalarials/2015.ACT.tif')
itnFile = paste0(dir, 'Africa Cube Public Data/ITN_use_year_average_adj_rasters/rasters/2015.ITN.use.yearavg.adj.stable.tif')

# list of indicators (ACT, IRS, and untreated are national level only)
inds = c('pfpr','act','itn')
years = c(2000, 2005, 2010, 2015)

# store shapefile names
shapeFileUGA = paste0(dir, '../../../mapping/uga/uga_region10_map.shp')
shapeFileCOD = paste0(dir, '../../../mapping/cod/COD_adm0.shp')
# -------------------------------------------------------------------------


# --------------------------------------------------------------------------------
# Load/prep data

# load rasters
rasters = list()
i=1
for(ind in inds) { 
	for(y in years) { 
		inFile = paste0(ind, 'File')
		inFile = gsub('2015', y, get(inFile))
		rasters[[i]] = raster(inFile)
		i = i + 1
	}
}

# load shapefiles
shapeDatauga = shapefile(shapeFileUGA)
shapeDatacod = shapefile(shapeFileCOD)

# clip each raster to the country and format as data.table
clips = list()
i=1
j=1
for(ind in inds) { 
	for(yea in years) { 
		for(iso in c('cod', 'uga')) { 
			clips[[j]] = crop(rasters[[i]], extent(get(paste0('shapeData', iso))))
			clips[[j]] = mask(clips[[j]], get(paste0('shapeData', iso)))
			clips[[j]] = data.table(as.data.frame(clips[[j]], xy=TRUE))
			setnames(clips[[j]], c('x','y','value'))
			clips[[j]][, indicator:=ind]
			clips[[j]][, year:=yea]
			clips[[j]][, iso3:=iso]
			j = j + 1
		}
		i= i + 1
	}
}

# append all data
data = do.call('rbind', clips)
# --------------------------------------------------------------------------------


# --------------------------------------------------------------------
# Display numbers

# compute spread by country-year-indicator
ranges = data[, quantile(value, .95, na.rm=TRUE)-quantile(value, .05, na.rm=TRUE), by=c('iso3','indicator','year')]
quantiles = data[, list('p05'=quantile(value, .05, na.rm=TRUE)), by=c('iso3','indicator','year')]
quantiles$p95 = data[, quantile(value, .95, na.rm=TRUE), by=c('iso3','indicator','year')]$V1
quantiles
dcast(ranges, indicator+iso3~year)
# --------------------------------------------------------------------
