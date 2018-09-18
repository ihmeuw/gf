# --------------------------------------------------
# David Phillips
#
# 9/11/2018
# Compare resource allocation (commodities) to need 
# The current working directory should be the root of this repo
# --------------------------------------------------


# TO DO
# identify age groups in incidence rasters and do an age-specific merge
# use PNLP-to-shapefile HZ names to connect (for now just using PNLP incidence)

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(raster)
library(rgeos)
library(parallel)
library(RColorBrewer)
library(ggplot2)
# --------------------


# ----------------------------------------------
# Parameters and settings

# whether or not to prep MAP data
prepMAP = FALSE
# ----------------------------------------------


# --------------------------------------------------------------------------------------------
# Files and directories

# switch for cluster
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')

# PNLP data directory
dataDir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/')

# MAP directory
mapDir = paste0(j, '/WORK/11_geospatial/01_covariates/00_MBG_STANDARD/map_pf_incidence/mean/1y/')

# output directory
outDir = paste0(j, '/Project/Evaluation/GF/vfm/visualizations')

# shapefile
shapeFile = paste0(j, '/Project/Evaluation/GF/mapping/cod/health_zones_who/health2.shp')

# shapefile of lakes
shapeFileLakes = paste0(j, '/WORK/11_geospatial/06_original shapefiles/GLWD_lakes/glwd_1.shp')

# PNLP input file
inFile = paste0(dataDir, 'post_imputation/imputedData_run2_long_corrected.rds')

# map files
mapFiles = paste0(mapDir, list.files(mapDir, 'tif'))
mapFiles = mapFiles[!grepl('.ovr|.aux|.xml', mapFiles)]

# output files
outFile = paste0(dataDir, '../pnlp_map_', tolower(analysisLevel), '_year_level.rds')

# functions
source('./core/standardizeDRCNames.r')
# --------------------------------------------------------------------------------------------


# --------------------------------------------------------------------------
# Load/prep PNLP data

# load
data = readRDS(inFile)

# subset observations
keepVars = c('ASAQreceived_14yrsAndOlder', 'ASAQreceived_1to5yrs', 
	'ASAQreceived_2to11mos', 'ASAQreceived_6to13yrs', 
	'ArtLum_received', 'ITN_received', 
	'RDT_received', 'newCasesMalariaMild_5andOlder', 
	'newCasesMalariaMild_pregnantWomen', 'newCasesMalariaMild_under5', 
	'newCasesMalariaSevere_5andOlder', 'newCasesMalariaSevere_pregnantWomen', 
	'newCasesMalariaSevere_under5')
data = data[variable %in% keepVars]

# identify year
data[, year:=year(date)]
			
# collapse to year level 
# because we aren't sure Amelia is imputing individual months correctly
idVars = c('province','dps','health_zone','subpopulation','year')
data = data[, .(mean=mean(imp_value), upper=quantile(imp_value,.975), 
				lower=quantile(imp_value,.025)), by=c(idVars, 'indicator')]

# reshape wide
valueVars = c('mean','lower','upper')
formula = as.formula(paste(paste(idVars, collapse='+'),'~indicator'))
PNLPData = dcast.data.table(data, formula, value.var=valueVars)
# --------------------------------------------------------------------------


# ------------------------------------------------------------
# Load/prep MAP data (commented out until we can connect the shapefile to PNLP)
if(prepMAP) {
	# load shapefile
	map = shapefile(shapeFile)

	# simplify shapefile for speed
	mapDatatmp = map@data
	map = gSimplify(map, tol=0.01, topologyPreserve=TRUE)
	map = as(map, 'SpatialPolygonsDataFrame')
	map@data = mapDatatmp

	# load the ground cover data
	lakes = shapefile(shapeFileLakes)
	lakes = crop(lakes, extent(map))

	# loop over years, crop to DRC, mask water and aggregate to HZ-level
	i=1
	for(f in mapFiles) { 
		
		# skip unnecessary years
		year = gsub('.*1y_', '', f)
		year = as.numeric(gsub('_.*', '', year))
		print(year)
		if (!year %in% unique(wideData$year)) next
		
		# load raster data
		rasterData = stack(f)

		# clip to current country
		rasterData = crop(rasterData, extent(map))
		rasterData = mask(rasterData, map)		

		# mask the bodies of water
		rasterData = mask(rasterData, lakes, inverse=TRUE)
		
		# extract pixels by HZ (in parallel for speed)
		# hzExtract = sapply(extract(rasterData, map), sum)
		hzExtract = unlist(mclapply(map@data$Name, function(x) { 
			currentHZ = crop(rasterData, extent(map[map@data$Name==x,]))
			currentHZ = mask(currentHZ, map[map@data$Name==x,])	
			sum(getValues(currentHZ), na.rm=TRUE)
		}, mc.cores=ifelse(Sys.info()[1]=='Windows',1,36)))
		
		# sum over provinces
		currentHzData = data.table(health_zone=map@data$Name, 
									pf_prevalence=hzExtract)
		
		# add year and append
		currentHzData[, year:=year]
		if (i==1) hzData = currentHzData
		if (i>1) hzData = rbind(hzData, currentHzData)
		i=i+1
	}
}
# ------------------------------------------------------------


# -----------------------------------------------
# Merge PNLP and MAP data

# standardize admin names
PNLPData[, dps:=standardizeDRCNames(dps, level=tolower(analysisLevel))]
MAPData[, dps:=standardizeDRCNames(dps, level=tolower(analysisLevel))]

# merge
if (prepMAP & analysisLevel=='DPS') analysisData = merge(PNLPData, MAPData, by=c('year','dps')
if (prepMAP & analysisLevel=='HZ') analysisData = merge(PNLPData, MAPData, by=c('year','health_zone')
if (!prepMAP) analysisData = PNLPData

# save
saveRDS(analysisData, outFile)
# -----------------------------------------------
