# -----------------------------------------------------

# -----------------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(raster)
library(RColorBrewer)
library(ggplot2)
library(grid)
library(gridExtra)
library(rgdal)
# --------------------
# determine the most recent version of the raster map
run_date = fread(paste0('/ihme/code/geospatial/jdv6/lbd_hiv/5_publications/africa_hiv_prev/run_dates.txt'))
run_date = run_date[indicator == 'hiv_test' & group == 'final_results', run_date]

# ----------------------------------------------------------------------------------------
# Files and directories

# data directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
outDir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/multi_country/lbd/')

# input files
timestamp = run_date
inDir = paste0('/share/geospatial/mbg/hiv/hiv_test/output/', timestamp,'/')
inFile = paste0(inDir, 'hiv_test_mean_raked_raster.tif')

# shapefiles
shapeFileUGA = paste0(outDir, '../../../mapping/uga/uga_dist112_map.shp')
shapeFileCOD = paste0(outDir, '../../../mapping/cod/COD_adm3.shp')

# shapefile of lakes
shapeFileLakes = paste0(j, '/WORK/11_geospatial/06_original shapefiles/GLWD_lakes/glwd_1.shp')

# specify band to get a specific year of data
# band 1=2000, band 17=2016.... so for 2015 band=16 and for 2010 band=11
y = 2001

band_to_year = data.table(band= c(1:18), year= c(2000:2018))
band = band_to_year[year==y, band]

# output file
graphFile = paste0(outDir, 'HIV_Prevalence_', timestamp, '_', y, '_new.pdf')
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------
# Load/prep data

# load shapefiles
mapUGA = shapefile(shapeFileUGA)
mapCOD = shapefile(shapeFileCOD)

# load raster data
rasterData = raster(inFile, band=band)

#--------------# use this in order to get a rate of change between the two years; comment out if not using #------------------
graphFile = paste0(outDir, 'HIV_Prevalence_', timestamp, '_', "percent_change_2010to2015", '.pdf')

# load the ground cover data
lakes = shapefile(shapeFileLakes)

# mask the bodies of water
rasterData = mask(rasterData, lakes, inverse=TRUE)

# crop to the two countries
rasterDataUGA = crop(rasterData, extent(mapUGA))
rasterDataUGA = mask(rasterDataUGA, mapUGA)        
rasterDataCOD = crop(rasterData, extent(mapCOD))
rasterDataCOD = mask(rasterDataCOD, mapCOD)        

# convert to data tables
dataUGA = data.table(as.data.frame(rasterDataUGA, xy=TRUE))
dataCOD = data.table(as.data.frame(rasterDataCOD, xy=TRUE))
shapeDataUGA = data.table(fortify(mapUGA))
shapeDataCOD = data.table(fortify(mapCOD))

# rename
setnames(dataUGA, c('x','y','prev'))
setnames(dataCOD, c('x','y','prev'))
# ----------------------------------------------------------------

# ----------------------------------------------------------
# Set up to graph

# colors
cols1 = rev(brewer.pal(6, 'RdYlBu'))
border = 'grey65'
breaks = c(4, 8, 12)
  
# legend limits so both countries are on same scale
lims = range(c(dataUGA$prev, dataCOD$prev), na.rm=TRUE)*100
# ----------------------------------------------------------


# -------------------------------------------------------------------------------
# Graph

# store maps
 ggplot(dataUGA, aes(y=y, x=x, fill=prev*100)) + 
	geom_tile() + 
	geom_path(data=shapeDataUGA, aes(x=long, y=lat, group=group)
		, color=border, size=.05, inherit.aes=FALSE) + 
	scale_fill_gradientn('PLHIV %', colors=cols1, 
		na.value='white') + 
	coord_fixed(ratio=1) + 
	scale_x_continuous('', breaks = NULL) + 
	scale_y_continuous('', breaks = NULL) + 
	labs(title='Uganda') + 
	theme_minimal(base_size=16) + 
	theme(plot.title=element_text(hjust=.5)) 


#-------------
# add projection
tifFile = paste0(j, '/Project/Evaluation/GF/outcome_measurement/multi_country/lbd/projection/uga_hiv_2018.tif')
uga_tif = raster(tifFile)

uga_tif = crop(uga_tif, mapUGA)   
uga = data.table(as.data.frame(uga_tif, xy=TRUE))

ggplot(uga, aes(y=y, x=x, fill=uga_hiv_2018*100)) + 
  geom_tile() + 
  geom_path(data=shapeDataUGA, aes(x=long, y=lat, group=group)
            , color=border, size=.05, inherit.aes=FALSE) + 
  scale_fill_gradientn('PLHIV %', colors=cols1, 
                       na.value='white') + 
  coord_fixed(ratio=1) + 
  scale_x_continuous('', breaks = NULL) + 
  scale_y_continuous('', breaks = NULL) + 
  labs(title='Uganda') + 
  theme_minimal(base_size=16) + 
  theme(plot.title=element_text(hjust=.5)) 

#-------------


	
codprev = ggplot(dataCOD, aes(y=y, x=x, fill=prev*100)) + 
	geom_tile() + 
	geom_path(data=shapeDataCOD, aes(x=long, y=lat, group=group)
		, color=border, size=.05, inherit.aes=FALSE) + 
	scale_fill_gradientn('PLHIV %', colors=cols1, 
		na.value='white') + 
	coord_fixed(ratio=1) + 
	scale_x_continuous('', breaks = NULL) + 
	scale_y_continuous('', breaks = NULL) + 
	labs(title='DRC') + 
	theme_minimal(base_size=16) + 
	theme(plot.title=element_text(hjust=.5), plot.margin=unit(rep(-1,4), 'cm')) 

# put maps together
p1 = arrangeGrob(codprev, ugaprev, ncol=2, top = toString(y))	

#----------------# for percent change graph:#------------------------------------------
# p1 = arrangeGrob(codprev, ugaprev, ncol=2, top = "Percent Change 2010 to 2015")	
# -------------------------------------------------------------------------------


# --------------------------------
# Save graphs
pdf(graphFile, height=6, width=9)
grid.newpage()
grid.draw(p1)
dev.off()
# --------------------------------
