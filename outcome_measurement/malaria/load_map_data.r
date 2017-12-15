# --------------------------------------------------------
# David Phillips
#
# 11/27/2017
# Script to load data into memory
# Originally intended to be called by map_malaria_indicators.r and map_unmet_need.r
# Inputs:
# iso3s - a character vector specifying which country(s) to clip data to (currently only handles COD and UGA)
# years - a numeric vector specifying which year to analyze
# inds - a character vector specifying which indicators to analyze
# crop - a logical vector of length 1 specifying whether to crop values to inner 95th percentile (for viz purposes)
# Outputs:
# out - a list containing 3 objects:
# 		1. data - a data.table with rows for country-pixel-years and columns for each ind
# 		2. maps - list of spatialPolygonsDataFrames, one per iso3
# 		3. shapeData - a data.table containing all objects in `map`, fortified and rbinded together
# 		4. baserasters - a list of RasterLayers, one per country containing the coordinate grid everything is projected to
# --------------------------------------------------------


# ------------------------------------------------------
# Start function
loadMapData = function(iso3s, years, inds, crop=FALSE) { 
# ------------------------------------------------------

	# ------------------------
	# Load necessary packages
	library(data.table)
	library(raster)
	library(rgdal)
	library(rgeos)
	# ------------------------
	
	
	# ---------------------------------------------------------------------------------------
	# Files and directories

	# data directory
	j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
	dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/multi_country/map/')

	# input files
	actDir = paste0(dir, 'Africa Cube Public Data/ACT_use_rasters/rasters/rasters/')
	itnDir = paste0(dir, 'Africa Cube Public Data/ITN_use_year_average_adj_rasters/rasters/')
	irsDir = paste0(dir, 'Africa Cube Public Data/IRS_use_rasters/rasters/')
	antmalDir = paste0(dir, 'NEJM Rasters/spatially_disaggregated_antimalarials/')
	popDir = paste0(dir, 'NEJM Rasters/unmasked_population_for_incidence/')
	prevDir = paste0(dir, '/Africa Cube Public Data/Prevalence_annual_means_rasters/rasters/')
	actFiles = paste0(actDir, list.files(actDir)[grepl('tif', list.files(actDir))])
	itnFiles = paste0(itnDir, list.files(itnDir)[grepl('tif', list.files(itnDir))])
	irsFiles = paste0(irsDir, list.files(irsDir)[grepl('tif', list.files(irsDir))])
	antmalFiles = paste0(antmalDir, list.files(antmalDir)[!grepl('ovr', list.files(antmalDir))])
	antmalFiles = antmalFiles[!grepl('xml', antmalFiles)]
	popFiles = paste0(popDir, list.files(popDir)[!grepl('ovr', list.files(popDir))])
	popFiles = popFiles[!grepl('xml', popFiles)]
	prevFiles = paste0(prevDir, list.files(prevDir)[grepl('tif', list.files(prevDir))])

	# shapefiles
	shapeFileUGA = paste0(dir, '../../../mapping/uga/uga_dist112_map.shp')
	shapeFileCOD = paste0(dir, '../../../mapping/cod/COD_adm3.shp')
	# ---------------------------------------------------------------------------------------
	
	
	# ------------------------------------------------------------------------------
	# Load/prep shapefile data
	
	# start list of maps
	maps = list()
	
	# loop over iso3s
	i=1
	for(iso3 in iso3s) { 
		
		# load shapefile
		map = shapefile(get(paste0('shapeFile', iso3)))
		
		# simplify shapefiles for speed
		if (iso3=='UGA') region = 'dist112'
		if (iso3!='UGA') region = 'ID_3'
		mapDatatmp = map@data
		map = gSimplify(map, tol=0.01, topologyPreserve=TRUE)
		map = as(map, 'SpatialPolygonsDataFrame')
		map@data = mapDatatmp
		mapDatatmp[[region]] = as.numeric(mapDatatmp[[region]])
		
		# format polygons as data.table
		shapeDatatmp = data.table(fortify(map, region=region))
		shapeDatatmp[,id:=as.numeric(id)]
		shapeDatatmp = merge(shapeDatatmp, mapDatatmp, by.x='id', by.y=region, all.x=TRUE)
		
		# append and store output
		shapeDatatmp[, iso3:=iso3]
		if (i==1) shapeData = shapeDatatmp 
		if (i>1) shapeData = rbind(shapeData, shapeDatatmp, fill=TRUE)
		maps[[iso3]] = map
	}
	# ------------------------------------------------------------------------------
	
	
	# ----------------------------------------------------------
	# Load/prep raster data
	
	# set up list for base rasters
	baseRasters = list()
	
	# loop over years
	y=1
	for(year in years) {
		
		# load all-country data
		if ('itn' %in% inds) rasterDataitn = raster(itnFiles[grepl(year, itnFiles)])
		if ('act' %in% inds) rasterDataact = raster(actFiles[grepl(year, itnFiles)])
		if ('irs' %in% inds) rasterDatairs = raster(irsFiles[grepl(year, irsFiles)])
		if ('antmal' %in% inds) rasterDataantmal = raster(antmalFiles[grepl(year, antmalFiles)])
		if ('pop' %in% inds) rasterDatapop = raster(popFiles[grepl(year, popFiles) & grepl('05-15', popFiles)])
		if ('pop_total' %in% inds) rasterDatapop_total = raster(popFiles[grepl(year, popFiles) & grepl('total', popFiles)])
		if ('prev' %in% inds) rasterDataprev = raster(prevFiles[grepl(year, prevFiles)])
		
		# loop over iso3s
		i=1
		for(iso3 in iso3s) { 
			
			# clip each raster to current country and project to the same grid as the first ind
			j = 1
			for(ind in inds) {
				
				# clip to current country
				rasterData = get(paste0('rasterData',inds[j]))
				rasterData = crop(rasterData, extent(maps[[iso3]]))
				rasterData = mask(rasterData, maps[[iso3]])		
				
				# store base raster
				if (j==1 & y==1) baseRasters[[iso3]] = rasterData

				# project the current indicator to the first one
				if (j>1 | y>1) rasterData = projectRaster(rasterData, baseRasters[[iso3]])
				
				# format raster as data.table
				tmpData = data.table(as.data.frame(rasterData, xy=TRUE))
				setnames(tmpData, c('x','y',ind))
				
				# crop to 99th percentile so extreme points don't skew map scales (if specified)
				if (ind!='pop' & crop==TRUE) {
					q1 = quantile(tmpData[[ind]], .005, na.rm=TRUE)
					q99 = quantile(tmpData[[ind]], .995, na.rm=TRUE)
					if (q1>0) tmpData[get(ind)<q1, (ind):=q1]
					tmpData[get(ind)>q99, (ind):=q99]
				}
								
				# merge (projectRaster should enable perfect merging)
				if (j==1) countryData = tmpData
				if (j>1) countryData = merge(countryData, tmpData, by=c('x','y'),all=TRUE) 
				j=j+1
			}
			
			# add country data to overall data
			countryData[, iso3:=iso3]
			if (i==1) yearData = countryData
			if (i>1) yearData = rbind(yearData, countryData)
			i=i+1
		}
		
		# add year
		yearData[, year:=year]
		if (y==1) data = yearData
		if (y>1) data = rbind(data, yearData)
		y=y+1
	}
	# ----------------------------------------------------------
	
	
	# -----------------------
	# Return output
	out = list('data'=data,
		'maps'=maps,
		'shapeData'=shapeData, 
		'baseRasters'=baseRasters
	)
	return(out)
	# -----------------------

# ------------
# end function
}
# ------------
