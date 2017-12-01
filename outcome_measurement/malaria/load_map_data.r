# --------------------------------------------------------
# David Phillips
#
# 11/27/2017
# Script to load data into memory
# Originally intended to be called by map_malaria_indicators.r and map_unmet_need.r
# Two "inputs" should be in memory when this is called:
# iso3s - a character vector specifying which country(s) to clip data to (currently only handles COD and UGA)
# year - a numeric vector of length 1 specifying which year to analyze
# inds - a character vector specifying which indicators to analyze
# --------------------------------------------------------

# to do:
# this should eventually be turned into a more general-purpose function
# clean up handling of input iso3s
# add functionality for multiple years
# handle clipping better so data can be projected to any raster

# ----------------------------------------------
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

# output files
graphFile = paste0(dir, '/visualizations/graphs.pdf')
# ----------------------------------------------


# ----------------------------------------------------------
# Load/prep data

# load data
if ('itn' %in% inds) rasterDataitn = raster(itnFiles[grepl(year, itnFiles)])
if ('act' %in% inds) rasterDataact = raster(actFiles[grepl(year, itnFiles)])
if ('irs' %in% inds) rasterDatairs = raster(irsFiles[grepl(year, irsFiles)])
if ('antmal' %in% inds) rasterDataantmal = raster(antmalFiles[grepl(year, antmalFiles)])
if ('pop' %in% inds) rasterDatapop = raster(popFiles[grepl(year, popFiles) & grepl('05-15', popFiles)])
if ('pop_total' %in% inds) rasterDatapop_total = raster(popFiles[grepl(year, popFiles) & grepl('total', popFiles)])
if ('prev' %in% inds) rasterDataprev = raster(prevFiles[grepl(year, prevFiles)])

# load shapefile
if ('UGA' %in% iso3s) mapUGA = shapefile(shapeFileUGA)
if ('COD' %in% iso3s) mapCOD = shapefile(shapeFileCOD)

# simplify shapefiles for speed
if ('UGA' %in% iso3s) mapUGADatatmp = mapUGA@data
if ('COD' %in% iso3s) mapCODDatatmp = mapCOD@data
if ('UGA' %in% iso3s) mapUGA = gSimplify(mapUGA, tol=0.01, topologyPreserve=TRUE)
if ('COD' %in% iso3s) mapCOD = gSimplify(mapCOD, tol=0.01, topologyPreserve=TRUE)
if ('UGA' %in% iso3s) mapUGA = as(mapUGA, 'SpatialPolygonsDataFrame')
if ('UGA' %in% iso3s) mapUGA@data = mapUGADatatmp
if ('COD' %in% iso3s) mapCOD = as(mapCOD, 'SpatialPolygonsDataFrame')
if ('COD' %in% iso3s) mapCOD@data = mapCODDatatmp

# format polygons as data.table
if ('UGA' %in% iso3s) shapeDataUGA = data.table(fortify(mapUGA, region='dist112'))
if ('UGA' %in% iso3s) shapeDataUGA[,id:=as.numeric(id)]
if ('COD' %in% iso3s) shapeDataCOD = data.table(fortify(mapCOD, region='ID_3'))
if ('UGA' %in% iso3s) shapeDataUGA = merge(shapeDataUGA, mapUGADatatmp, by.x='id', by.y='dist112', all.x=TRUE)
if ('COD' %in% iso3s) shapeDataCOD = merge(shapeDataCOD, mapCODDatatmp, by.x='id', by.y='ID_3', all.x=TRUE)

# format rasters
for(i in inds) {
	
	rasterData = get(paste0('rasterData',i))

	# clip to Uganda/DRC
	if ('UGA' %in% iso3s) rasterDataUGA = crop(rasterData, extent(mapUGA))
	if ('UGA' %in% iso3s) rasterDataUGA = mask(rasterDataUGA, mapUGA)
	if ('COD' %in% iso3s) rasterDataCOD = crop(rasterData, extent(mapCOD))
	if ('COD' %in% iso3s) rasterDataCOD = mask(rasterDataCOD, mapCOD)

	# format rasters as data.table
	if ('UGA' %in% iso3s) dataUGA = data.table(as.data.frame(rasterDataUGA, xy=TRUE))
	if ('COD' %in% iso3s) dataCOD = data.table(as.data.frame(rasterDataCOD, xy=TRUE))
	if ('UGA' %in% iso3s) setnames(dataUGA, c('x','y','value'))
	if ('COD' %in% iso3s) setnames(dataCOD, c('x','y','value'))
	
	# crop to 99th percentile so extreme points don't skew map scales
	if (i!='pop' & crop==TRUE) {
		if ('UGA' %in% iso3s) { 
			q1 = quantile(dataUGA$value, .005, na.rm=TRUE)
			q99 = quantile(dataUGA$value, .995, na.rm=TRUE)
			if (q1>0) dataUGA[value<q1, value:=q1]
			dataUGA[value>q99, value:=q99]
		}
		if ('COD' %in% iso3s) { 
			q1 = quantile(dataCOD$value, .005, na.rm=TRUE)
			q99 = quantile(dataCOD$value, .995, na.rm=TRUE)
			if (q1>0) dataCOD[value<q1, value:=q1]
			dataCOD[value>q99, value:=q99]
		}
	}
	
	# assign
	if ('UGA' %in% iso3s) assign(paste0('dataUGA',i), dataUGA) 
	if ('COD' %in% iso3s) assign(paste0('dataCOD',i), dataCOD) 
}
# ----------------------------------------------------------
