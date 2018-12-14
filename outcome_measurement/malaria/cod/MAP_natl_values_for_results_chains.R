# Aggregate MAP estimates to national estimate

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

# --------------------------------------------------
# Files and directories
dir_lbd = "J:/WORK/11_geospatial/01_covariates/00_MBG_STANDARD/"
shpfile_COD = "J:/Project/Evaluation/GF/mapping/cod/COD_adm3.shp"
  
dir_inc = paste0(dir_lbd, "map_pf_incidence/mean/1y/")
dir_itn = paste0(dir_lbd, "mapitncov/mean/1y/")
dir_act = paste0(dir_lbd, "map_antimalarial/mean/1y/")
dir_worldpop = paste0(dir_lbd, "worldpop/total/1y/")

act_file = "map_antimalarial_mean_1y_"
itn_file = "mapitncov_mean_1y_"
inc_file = "map_pf_incidence_mean_1y_" 
wp_file = "worldpop_total_1y_"

file_ending = "_00_00.tif"
# --------------------------------------------------

# --------------------------------------------------
# load shapefile for DRC
shp_COD = shapefile(shpfile_COD)
# --------------------------------------------------

# --------------------------------------------------
# create a data table to add to in the loop 
results <- data.table(year = (2000:2016), act_coverage_under5 = 0, acts_total_under5 = 0, 
                      itn_coverage = 0, itns_total = 0, 
                      incidence = 0, prevalence = 0)

# loop through years
for (y in 2000:2016){
  y <- as.character(y)
  
  # read in raster for ACT coverage (units: percentage of cases of fever treated with ACTs - under 5) for given year
  inFile = paste0(dir_act, act_file, y, file_ending)
  rasterData_ACT = raster(inFile)
  # read in raster for incidence for given year
  inFile2 = paste0(dir_inc, inc_file, y, file_ending)
  rasterData_INC = raster(inFile2)

  # clip to DRC
  rasterData_ACT = crop(rasterData_ACT, extent(shp_COD))
  rasterData_ACT = mask(rasterData_ACT, shp_COD)		
  
  rasterData_INC = crop(rasterData_INC, extent(shp_COD))
  rasterData_INC = mask(rasterData_INC, shp_COD)	
  
  # project inc raster to act one 
  rasterData_INC = projectRaster(rasterData_INC, rasterData_ACT)
  
  # convert rasters to data.tables
  dt_act = data.table(as.data.frame(rasterData_ACT, xy=TRUE))
  setnames(dt_act, c('x','y',"acts_percent"))
  dt_inc = data.table(as.data.frame(rasterData_INC, xy=TRUE))
  setnames(dt_inc, c('x','y',"incidence"))
  
  # merge (projectRaster should enable perfect merging)
  dt <- merge(dt_act, dt_inc)
  
  # multiply ACT raster by incidence raster to get a raster of counts for those treated with ACTs
  dt[,acts_counts:= acts_percent * incidence]
  
  # aggregate counts to national level 
  dt_natl <- dt[, .(natl_acts_count=sum(acts_counts, na.rm=TRUE), natl_incidence=sum(incidence, na.rm=TRUE))]
  # then divide to get national % coverage with acts
  percent_coverage = dt_natl$natl_acts_count / dt_natl$natl_incidence * 100 
  
  # add this to results 
  results[ year ==y, act_coverage_under_5 := percent_coverage]
  results[ year ==y, acts_total := dt_natl$natl_acts_count]
  results[ year ==y, incidence := dt_natl$natl_incidence]
  
}













