# -----------------------------------------------------
# David Phillips
#
# 5/15/2017
# Make simple maps of HIV prevalence for PCE countries
# 8/3/2018 
# Modified by Audrey Batzel; Use different bands of the raster data to make maps of HIV prevalence in different years
# Modified by Jen Ross Sept 2018; Project forward HIV prevalence rasters using annual differences.

#Still to do (as of Sept 2018) is to fix the DRC 2020 raster, which has some negative pixel values for HIV prevalence.
#Also should experiment with windows of different year lengths to inform the projection (currently 2010 - 2017

# ----------------------------------------------------



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


# ----------------------------------------------------------------------------------------
# Files and directories

# data directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
outDir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/multi_country/lbd/projection/')

# input files
timestamp = '2018_08_19_11_20_44' #change this to use a different run of the lbd hiv model
inDir = paste0('/share/geospatial/mbg/hiv/hiv_test/output/', timestamp,'/')
inFile = paste0(inDir, 'hiv_test_mean_raked_raster.tif')
#Does this timestamp have all of the values until 2017? or is this an earlier model?

# shapefiles
mapUGA = shapefile(paste0(j, '/Project/Evaluation/GF/mapping/uga/uga_dist112_map.shp'))
mapCOD = shapefile(paste0(j, '/Project/Evaluation/GF/mapping/cod/health_zones_who/health2.shp'))

# shapefile of lakes
shapeFileLakes = shapefile(paste0(j, '/WORK/11_geospatial/06_original shapefiles/GLWD_lakes/glwd_1.shp'))

# ---------------------------------------------------------
# Load/prep data
# load raster data
rasterBrick = brick(inFile) #Looks like this may be a more computationally efficient way of working with the raster files than stack.
plot(rasterBrick) #Why does this only do 16 bands?

#--------------# use this in order to get a rate of change between the two years; comment out if not using #------------------
graphFile = paste0(outDir, 'HIV_Prevalence_', timestamp, '_', "percent_change_2010to2017", '.pdf')
rasterEnd = raster(inFile, band= 17)
rasterStart = raster(inFile, band= 11)
no_years = 7

# mask the bodies of water
rasterEnd = mask(rasterEnd, shapeFileLakes, inverse=TRUE)

#Calculate annual difference by subtracting end prevalence from start prevalence and dividing by # years
ann_diff = (rasterEnd - rasterStart)/no_years
plot(ann_diff, main = 'Annualized difference in HIV prevalence, 2010 - 2017')

#Project forward by adding the annual difference to the last year
proj_2018 = rasterEnd+ann_diff
plot(proj_2018, main = "HIV prevalence in 2018 projected from changes 2010 - 2017")
writeRaster(proj_2018, file=paste0(outDir, "HIV_2018"), format="GTiff", overwrite = TRUE)

#Probably more efficient to just store these projections for our two countries than for the whole continent
# crop to the two countries.
rasterEndUGA = crop(rasterEnd, extent(mapUGA))
rasterEndUGA = mask(rasterEnd, mapUGA)		
rasterEndCOD = crop(rasterEnd, extent(mapCOD))
rasterEndCOD = mask(rasterEnd, mapCOD)		

ann_diffUGA = crop(ann_diff, extent(mapUGA))
ann_diffUGA = mask(ann_diff, mapUGA)
ann_diffCOD = crop(ann_diff, extent(mapCOD))
ann_diffCOD = mask(ann_diff, mapCOD)

#Project forward Uganda rasters
uga_hiv_2018 = rasterEndUGA + ann_diffUGA
uga_hiv_2019 = rasterEndUGA + 2*ann_diffUGA
uga_hiv_2020 = rasterEndUGA + 3*ann_diffUGA
#Can check that these are actually different with cellStats(rasterName, 'range')

#Save Uganda raster projections
writeRaster(uga_hiv_2018, file=paste0(outDir, "uga_hiv_2018"), format="GTiff", overwrite = TRUE)
writeRaster(uga_hiv_2019, file=paste0(outDir, "uga_hiv_2019"), format="GTiff", overwrite = TRUE)
writeRaster(uga_hiv_2020, file=paste0(outDir, "uga_hiv_2020"), format="GTiff", overwrite = TRUE)

#Project forward DRC rasters
cod_hiv_2018 = rasterEndCOD + ann_diffCOD
cod_hiv_2019 = rasterEndCOD + 2*ann_diffCOD
cod_hiv_2020 = rasterEndCOD + 3*ann_diffCOD

#Save DRC raster projections
writeRaster(cod_hiv_2018, file=paste0(outDir, "cod_hiv_2018"), format="GTiff", overwrite = TRUE)
writeRaster(cod_hiv_2019, file=paste0(outDir, "cod_hiv_2019"), format="GTiff", overwrite = TRUE)
writeRaster(cod_hiv_2020, file=paste0(outDir, "cod_hiv_2020"), format="GTiff", overwrite = TRUE)
#Note that the lowest end of the range for DRC 2020 has some pixels with negative prevalence. This needs to be fixed.


#This is Audrey's old code for working in data.table. I have not updated the filenames yet to work with code above--------------------------------------------

# convert to data tables
dataUGA = data.table(as.data.frame(rasterDataUGA, xy=TRUE))
dataCOD = data.table(as.data.frame(rasterDataCOD, xy=TRUE))
shapeDataUGA = data.table(fortify(mapUGA))
shapeDataCOD = data.table(fortify(mapCOD))

# rename
setnames(dataUGA, c('x','y','prev'))
setnames(dataCOD, c('x','y','prev'))
# ---------------------------------------------------------


# ---------------------------------------------------------
# Crop to the 99.9th percentile to avoid border effects in DRC

dataCOD[prev>quantile(prev, .999, na.rm=TRUE), prev:=NA]

# ---------------------------------------------------------


# ----------------------------------------------------------
# Set up to graph

# colors
cols1 = brewer.pal(6, 'PuOr')
border = 'grey65'

# legend limits so both countries are on same scale
lims = range(c(dataUGA$prev, dataCOD$prev), na.rm=TRUE)*100
# ----------------------------------------------------------


# -------------------------------------------------------------------------------
# Graph

# store maps
ugaprev = ggplot(dataUGA, aes(y=y, x=x, fill=prev*100)) + 
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

