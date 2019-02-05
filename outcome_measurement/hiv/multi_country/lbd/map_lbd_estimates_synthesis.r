# -----------------------------------------------------
# Raster maps for the Uganda Annual Country Report
# Caitlin O'Brien-Carelli, David Phillips, Audrey Batzel
# 1/3/2019

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
# run_date = fread(paste0('/ihme/code/geospatial/jdv6/lbd_hiv/5_publications/africa_hiv_prev/run_dates.txt'))
# run_date = run_date[indicator == 'hiv_test' & group == 'final_results', run_date]

# most recent run date as of publication
run_date = "2018_11_08_18_37_25"

# ----------------------------------------------------------------------------------------
# Files and directories

# data directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
outDir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/multi_country/lbd/')
export_dir_uga = paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/lbd_prev')
export_dir_code = paste0(j, '/Project/Evaluation/GF/outcome_measurement/')

# input files
timestamp = run_date
inDir = paste0('/share/geospatial/mbg/hiv/hiv_test/output/', timestamp,'/')
inFile = paste0(inDir, 'hiv_test_mean_raked_raster.tif')

# shapefiles
shapeFileUGA = paste0(outDir, '../../../mapping/uga/uga_dist112_map.shp')
shapeFileCOD = paste0(outDir, '../../../mapping/cod/COD_adm3.shp')

# shapefile of lakes
shapeFileLakes = paste0(j, '/WORK/11_geospatial/06_original shapefiles/GLWD_lakes/glwd_1.shp')

# set the year you want to map
y = 2015
  
# specify band to get a specific year of data
# band 1=2000, band 17=2016.... so for 2015 band=16 and for 2010 band=11
band_to_year = data.table(band=c(1:19), year=c(2000:2018))
band = band_to_year[year==y, band]

# output file
graphFile = paste0(outDir, 'HIV_Prevalence_', timestamp, '_', y, '_new.pdf')

# ----------------------------------------------------------------------------------------
# COD map
# load shapefiles
mapCOD = shapefile(shapeFileCOD)

# load raster data
rasterData1 = raster(inFile, band=band)

# load the ground cover data
lakes = shapefile(shapeFileLakes)

# crop to the two countries
rasterDataCOD1 = crop(rasterData1, extent(mapCOD))

# mask the bodies of water
# rasterDataCOD1 = mask(rasterDataCOD1, lakes, inverse=TRUE)

# convert to data tables
dataCOD1 = data.table(as.data.frame(rasterDataCOD1, xy=TRUE))

# import shape file
shapeDataCOD = data.table(fortify(mapCOD))

# rename
setnames(dataCOD1, c('x','y','prev'))

# colors
cols1 = rev(brewer.pal(6, 'RdYlBu'))
border = 'grey65'
breaks = c(5, 10, 15)

# legend limits so both countries are on same scale
lims = range(dataCOD1$prev, na.rm=TRUE)*100

# store maps
ggplot(dataCOD1, aes(y=y, x=x, fill=prev*100)) + 
  geom_tile() + 
  geom_path(data=shapeDataCOD, aes(x=long, y=lat, group=group),
            color=border, size=.05, inherit.aes=FALSE) + 
  scale_fill_gradientn('HIV Prevalence (%)', colors=cols1, 
               na.value='white', breaks=breaks) + 
  coord_fixed(ratio=1) + 
  scale_x_continuous('', breaks = NULL) + 
  scale_y_continuous('', breaks = NULL) + 
  theme_minimal(base_size=16) + 
  theme(plot.title=element_text(hjust=.5)) 



# ---------------------------------------------------------

y1 = 2014
y2 = 2017
# Load/prep data

# load shapefiles
mapUGA = shapefile(shapeFileUGA)
mapCOD = shapefile(shapeFileCOD)

# load raster data
rasterData1 = raster(inFile, band=band)
rasterData2 = raster(inFile, band=band1)
rasterData3 = raster(inFile, band=band2)

#-----------------------
# load the ground cover data
lakes = shapefile(shapeFileLakes)

# mask the bodies of water
rasterData1 = mask(rasterData1, lakes, inverse=TRUE)
rasterData2 = mask(rasterData2, lakes, inverse=TRUE)
rasterData3 = mask(rasterData3, lakes, inverse=TRUE)

# crop to the two countries
rasterDataUGA1 = crop(rasterData1, extent(mapUGA))
rasterDataUGA1 = mask(rasterDataUGA1, mapUGA)    

rasterDataUGA2 = crop(rasterData2, extent(mapUGA))
rasterDataUGA2 = mask(rasterDataUGA2, mapUGA)    

rasterDataUGA3 = crop(rasterData3, extent(mapUGA))
rasterDataUGA3 = mask(rasterDataUGA3, mapUGA)    

# convert to data tables
dataUGA1 = data.table(as.data.frame(rasterDataUGA1, xy=TRUE))
dataUGA2 = data.table(as.data.frame(rasterDataUGA2, xy=TRUE))
dataUGA3 = data.table(as.data.frame(rasterDataUGA3, xy=TRUE))

# add years to facet wrap
dataUGA1[ ,year:=2000]
dataUGA2[ ,year:=2014]
dataUGA3[ ,year:=2017]

# rbind the bands together
dataUGA = rbind(dataUGA1, dataUGA2, dataUGA3)

# import shape file
shapeDataUGA = data.table(fortify(mapUGA))

# rename
setnames(dataUGA, c('x','y','prev', 'year'))

# colors
cols1 = rev(brewer.pal(6, 'RdYlBu'))
border = 'grey65'
breaks = c(5, 10, 15)

# legend limits so both countries are on same scale
lims = range(dataUGA$prev, na.rm=TRUE)*100

# store maps
ggplot(dataUGA1, aes(y=y, x=x, fill=prev*100)) + 
  geom_tile() + 
  geom_path(data=shapeDataUGA, aes(x=long, y=lat, group=group),
            color=border, size=.05, inherit.aes=FALSE) + 
  scale_fill_gradientn('HIV Prevalence (%)', colors=cols1, 
                       na.value='white', breaks=breaks) + 
  coord_fixed(ratio=1) + 
  scale_x_continuous('', breaks = NULL) + 
  scale_y_continuous('', breaks = NULL) + 
  theme_minimal(base_size=16) + 
  facet_wrap(~year) +
  theme(plot.title=element_text(hjust=.5),
        strip.text.x = element_text(size=20)) 


# ----------------------------------------------------------
# Set up to graph

# colors
cols1 = rev(brewer.pal(6, 'RdYlBu'))
border = 'grey65'
breaks = c(5, 10, 15)

# legend limits so both countries are on same scale
lims = range(dataUGA$prev, na.rm=TRUE)*100

# store maps
ggplot(dataUGA, aes(y=y, x=x, fill=prev*100)) + 
  geom_tile() + 
  geom_path(data=shapeDataUGA, aes(x=long, y=lat, group=group),
            color=border, size=.05, inherit.aes=FALSE) + 
  scale_fill_gradientn('HIV Prevalence (%)', colors=cols1, 
                       na.value='white', breaks=breaks) + 
  coord_fixed(ratio=1) + 
  scale_x_continuous('', breaks = NULL) + 
  scale_y_continuous('', breaks = NULL) + 
  theme_minimal(base_size=16) + 
  facet_wrap(~year) +
  theme(plot.title=element_text(hjust=.5),
        strip.text.x = element_text(size=20)) 


# ----------------------------------------------------------
# Set up to graph

# colors
cols1 = rev(brewer.pal(6, 'RdYlBu'))
border = 'grey65'
breaks = c(5, 10, 15)
  
# legend limits so both countries are on same scale
lims = range(dataUGA$prev, na.rm=TRUE)*100
# ----------------------------------------------------------

# -------------------------------------------------------------------------------
# Graph

pdf(paste0(export_dir, '/raster_facet.pdf'), height=7, width=12)

# store maps
 ggplot(dataUGA, aes(y=y, x=x, fill=prev*100)) + 
	geom_tile() + 
	geom_path(data=shapeDataUGA, aes(x=long, y=lat, group=group),
	       color=border, size=.05, inherit.aes=FALSE) + 
	scale_fill_gradientn('HIV Prevalence (%)', colors=cols1, 
		na.value='white', breaks=breaks) + 
	coord_fixed(ratio=1) + 
	scale_x_continuous('', breaks = NULL) + 
	scale_y_continuous('', breaks = NULL) + 
	theme_minimal(base_size=16) + 
  facet_wrap(~year) +
	theme(plot.title=element_text(hjust=.5),
	      strip.text.x = element_text(size=20)) 

dev.off()

#------------------------------------
# projection map

tifFile = paste0(j, '/Project/Evaluation/GF/outcome_measurement/multi_country/lbd/projection/uga_hiv_2018.tif')
uga_tif = raster(tifFile)

# aggregate tif file
uga_tif = crop(uga_tif, mapUGA)   
uga = data.table(as.data.frame(uga_tif, xy=TRUE))
setnames(uga, c('x', 'y', 'prev'))
uga[ , year:=2018]

# create a subset
uga2 = dataUGA[year==2017]

# bind them together
uga = rbind(uga, uga2)

# set the legend breaks
breaks2 = c(4, 8, 12)

pdf(paste0(export_dir, '/raster_projection.pdf'), height=7, width=12)

# map of 2017 and 2018
ggplot(uga, aes(y=y, x=x, fill=prev*100)) + 
  geom_tile() + 
  geom_path(data=shapeDataUGA, aes(x=long, y=lat, group=group)
            , color=border, size=.05, inherit.aes=FALSE) + 
  scale_fill_gradientn('HIV Prevalence (%)', colors=cols1, 
                       na.value='white', breaks=breaks2) + 
  coord_fixed(ratio=1) + 
  scale_x_continuous('', breaks = NULL) + 
  scale_y_continuous('', breaks = NULL) + 
  facet_wrap(~year) +
  theme_minimal(base_size=16) + 
  theme(strip.text.x = element_text(size=20)) 

dev.off()



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
