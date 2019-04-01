# -----------------------------------------------------
# Raster maps PDFs for deliverables
# Caitlin O'Brien-Carelli, David Phillips, Audrey Batzel
# 3/29/2019

# must be run on the cluster
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

# country input - cod or uga 
country = 'cod'

# ----------------------------------------------------------------------------------------
# Files and directories

# data directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
outDir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/multi_country/lbd/')
export_dir_uga = paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/lbd_prev')
export_dir_cod = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/lbd_prev')

# input files
timestamp = run_date
inDir = paste0('/share/geospatial/mbg/hiv/hiv_test/output/', timestamp,'/')
inFile = paste0(inDir, 'hiv_test_mean_raked_raster.tif')

# output files
graphFile = paste0(outDir, 'HIV_Prevalence_', country, '_', timestamp, '_all_years.pdf')

# shapefiles
shapeFileUGA = paste0(outDir, '../../../mapping/uga/uga_dist112_map.shp')
shapeFileCOD = paste0(outDir, '../../../mapping/cod/COD_adm3.shp')

# shapefile of lakes
shapeFileLakes = paste0(j, '/WORK/11_geospatial/06_original shapefiles/GLWD_lakes/glwd_1.shp')

# ----------------------------------------------------------------------------------------
# set up the list for multiple plots
list_of_plots = NULL
i = 1

# create a list of years and raster bands
years = data.table(band=c(1:18), year=c(2000:2017))

# -------------------------
# load shapefiles
if (country == 'cod') { map = shapefile(shapeFileCOD) }
if (country == 'uga') { map = shapefile(shapeFileUGA) }

# load the ground cover data
lakes = shapefile(shapeFileLakes)

# -------------------------
# create a list of maps 

for (r in years$year) {
  
  # set the year you want to map as the band in the raster
  band = years[year==r, band]
  
  # load raster data
  rasterData1 = raster(inFile, band=band)
  
  # crop to the country borders
  rasterData1 = crop(rasterData1, extent(map))
  
  # mask to the country borders
  rasterData1 = mask(rasterData1, map)  
  
  # mask the bodies of water
  rasterData1 = mask(rasterData1, lakes, inverse=TRUE)
  
  # convert to data tables
  data1 = data.table(as.data.frame(rasterData1, xy=TRUE))
  
  # import shape file
  shapeData = data.table(fortify(map))
  
  # rename
  setnames(data1, c('x','y','prev'))
  
  # colors
  cols1 = rev(brewer.pal(6, 'RdYlBu'))
  reds = brewer.pal(9, 'Reds')
  border = 'grey65'
  
  # colors differ for uganda and drc
  if (country == 'cod') {colors = reds}
  if (country == 'uga') {colors = cols1}
  
  # legend limits so both countries are on same scale
  lims = range(data1$prev, na.rm=TRUE)*100
  
  # add year to distinguish the data sets
  data1$year = r
  
  # set name to year
  name = data1[ ,unique(year)]
  
  # create the plots
  list_of_plots[[i]] = ggplot(data1, aes(y=y, x=x, fill=prev*100)) +
    geom_tile() +
    geom_path(data=shapeData, aes(x=long, y=lat, group=group),
              color=border, size=.05, inherit.aes=FALSE) +
    scale_fill_gradientn('HIV Prevalence (%)', colors=colors,
                         na.value='white') +
    coord_fixed(ratio=1) +
    scale_x_continuous('', breaks = NULL) +
    scale_y_continuous('', breaks = NULL) +
    theme_minimal(base_size=16) +
    labs(title = name) +
    theme(plot.title=element_text(hjust=.5))
  
  # run the next plot
  i = i + 1
}

# -------------------------
# print a pdf of the maps for every year
pdf(graphFile, width=12, height=9)

for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 

dev.off()
# -------------------------

