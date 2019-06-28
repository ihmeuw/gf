# Look up prevalence in and out of Global Fund provinces according to MAP


# ----------------------------------------------
# Set up R
source('./impact_evaluation/drc/set_up_r.r')
dpsShapeFile = paste0(j, '/Project/Evaluation/GF/mapping/cod/gadm36_COD_shp/gadm36_COD_1.shp')
# ----------------------------------------------


# ------------------------------------------------------------------
# Load/prep MAP estimates

# load DRC shapefile
map = shapefile(dpsShapeFile)

# define appropriate year range
year = 2017
allFiles = c(mapPrevalenceFiles, popFiles)
allFiles = allFiles[grepl(year, allFiles)]

# display for user
print('Aggregating rasters...')

# load rasters
f = mapPrevalenceFiles[length(mapPrevalenceFiles)]

# load raster
rasterData = raster(f)

# multiply by population
# load population
popFile = popFiles[grepl(year, popFiles)]
popData = raster(popFile)
# crop to drc for speed
rasterData = crop(rasterData, extent(map))
popData = crop(popData, extent(map))
# project to match population
rasterData = projectRaster(rasterData, popData)
# multiply
rasterData = rasterData * popData

# aggregate prevalence to admin1 level
provExtractprev = unlist(mclapply(unique(map@data$NAME_1), function(x) {
	currentProv = crop(rasterData, extent(map[map@data$NAME_1==x,]))
	currentProv = raster::mask(currentProv, map[map@data$NAME_1==x,])    
	sum(getValues(currentProv), na.rm=TRUE)
}, mc.cores=ifelse(Sys.info()[1]=='Windows',1,10)))

# aggregate population to admin1 level
provExtractpop = unlist(mclapply(unique(map@data$NAME_1), function(x) {
	currentProv = crop(popData, extent(map[map@data$NAME_1==x,]))
	currentProv = raster::mask(currentProv, map[map@data$NAME_1==x,])    
	sum(getValues(currentProv), na.rm=TRUE)
}, mc.cores=ifelse(Sys.info()[1]=='Windows',1,10)))

# assemble into a data.table
data = data.table(admin1_id=seq(length(provExtract)), 
	province=map@data$NAME_1, file=basename(f), prevalence=provExtractprev, 
	population=provExtractpop)
# ------------------------------------------------------------------


# ------------------------------------------------------------------
# Label and aggregate to GF vs PMI

# list provinces
gfProvs = c('Bas-Uélé', 'Équateur', 'Haut-Uélé', 'Ituri', 'Kinshasa', 'Kongo-Central', 'Kwango', 
	'Kwilu', 'Maï-Ndombe', 'Maniema', 'Mongala', 'Nord-Kivu', 'Nord-Ubangi', 'Sud-Ubangi', 
	'Tshopo', 'Tshuapa')

pmiProvs = c('Haut-Katanga', 'Lualaba', 'Haut-Lomami', 'Tanganyika', 'Sud-Kivu', 'Lomami', 
	'Kasaï-Central', 'Kasaï-Oriental', 'Sankuru')

dfidProvs = 'Kasaï'

# label
data[province %in% gfProvs, funder:='Global Fund']
data[province %in% pmiProvs, funder:='PMI']
data[province %in% dfidProvs, funder:='DFID']

# aggregate
agg = data[, lapply(.SD, sum), by=funder, .SDcols=c('prevalence','population')]

# recompute prevalence as a proportion
agg[, pcv:=prevalence/population]
# ------------------------------------------------------------------
