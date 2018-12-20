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
dir_prev =paste0(dir_lbd, "map_pf_prevalence/mean/1y/")
dir_itn = paste0(dir_lbd, "mapitncov/mean/1y/")
dir_act = paste0(dir_lbd, "map_antimalarial/mean/1y/")
dir_worldpop = paste0(dir_lbd, "worldpop/total/1y/")

act_file = "map_antimalarial_mean_1y_"
itn_file = "mapitncov_mean_1y_"
inc_file = "map_pf_incidence_mean_1y_" 
wp_file = "worldpop_total_1y_"
prev_file = "map_pf_prevalence_mean_1y_"

file_ending = "_00_00.tif"

out_dir = "J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/MAP_natl_data/"
outFile = "MAP_natl_data_2010to2017.rds"
# --------------------------------------------------

# --------------------------------------------------
# load shapefile for DRC
shp_COD = shapefile(shpfile_COD)
# --------------------------------------------------

# --------------------------------------------------
# create a data table to add to in the loop 
# --------------------------------------------------
results <- data.table(year = (2000:2017), act_coverage_under5 = 0, acts_total_under5 = 0, 
                      itn_coverage = 0, itn_total = 0, 
                      incidence_per100k = 0, prevalence = 0,
                      population = 0)
# --------------------------------------------------

# --------------------------------------------------
# loop through years and get data for results table
# --------------------------------------------------
for (y in 2000:2017){
  y <- as.character(y)
  
  if (y == "2017"){  # only incidence, population, and prevalence are avail for 2017
    # read in raster for incidence for given year
    inFile2 = paste0(dir_inc, inc_file, y, file_ending)
    rasterData_INC = raster(inFile2)
    # read in raster for worldpop for given year
    inFile_wp = paste0(dir_worldpop, wp_file, y, file_ending)
    rasterData_WP = raster(inFile_wp)
    # read in raster for prevalence for given year
    inFile_prev = paste0(dir_prev, prev_file, y, file_ending)
    rasterData_PREV = raster(inFile_prev)
  } else {
    # read in raster for ACT coverage (units: percentage of cases of fever treated with ACTs - under 5) for given year
    inFile = paste0(dir_act, act_file, y, file_ending)
    rasterData_ACT = raster(inFile)
    # read in raster for ITN coverage for given year
    inFile_itn = paste0(dir_itn, itn_file, y, file_ending)
    rasterData_ITN = raster(inFile_itn)
    # read in raster for incidence for given year
    inFile2 = paste0(dir_inc, inc_file, y, file_ending)
    rasterData_INC = raster(inFile2)
    # read in raster for worldpop for given year
    inFile_wp = paste0(dir_worldpop, wp_file, y, file_ending)
    rasterData_WP = raster(inFile_wp)
    # read in raster for prevalence for given year
    inFile_prev = paste0(dir_prev, prev_file, y, file_ending)
    rasterData_PREV = raster(inFile_prev)
  }
  
  # clip to DRC
  rasterData_ACT = crop(rasterData_ACT, extent(shp_COD))
  rasterData_ACT = mask(rasterData_ACT, shp_COD)		
  
  rasterData_INC = crop(rasterData_INC, extent(shp_COD))
  rasterData_INC = mask(rasterData_INC, shp_COD)
  
  rasterData_WP = crop(rasterData_WP, extent(shp_COD))
  rasterData_WP = mask(rasterData_WP, shp_COD)
  
  rasterData_ITN = crop(rasterData_ITN, extent(shp_COD))
  rasterData_ITN = mask(rasterData_ITN, shp_COD)
  
  rasterData_PREV = crop(rasterData_PREV, extent(shp_COD))
  rasterData_PREV = mask(rasterData_PREV, shp_COD)
  
  # project inc raster to act one 
  rasterData_INC = projectRaster(rasterData_INC, rasterData_ACT)
  rasterData_WP = projectRaster(rasterData_WP, rasterData_ACT)
  rasterData_ITN = projectRaster(rasterData_ITN, rasterData_ACT)
  rasterData_PREV = projectRaster(rasterData_PREV, rasterData_ACT)
  
  # convert rasters to data.tables
  dt_act = data.table(as.data.frame(rasterData_ACT, xy=TRUE))
  setnames(dt_act, c('x','y',"acts_percent"))
  dt_inc = data.table(as.data.frame(rasterData_INC, xy=TRUE))
  setnames(dt_inc, c('x','y',"incidence"))
  dt_wp = data.table(as.data.frame(rasterData_WP, xy=TRUE))
  setnames(dt_wp, c('x','y',"population"))
  dt_itn = data.table(as.data.frame(rasterData_ITN, xy=TRUE))
  setnames(dt_itn, c('x','y',"itn_percent"))
  dt_prev = data.table(as.data.frame(rasterData_PREV, xy=TRUE))
  setnames(dt_prev, c('x','y',"prevalence"))
  
  # merge (projectRaster should enable perfect merging)
  dt <- merge(dt_act, dt_inc)
  dt <- merge(dt, dt_wp)
  dt <- merge(dt, dt_itn)
  dt <- merge(dt, dt_prev)
  
#####################  
  # calculations with rasters    ## NOTE: need to convert percents/rates to counts before aggregating so they are scaled appropriately, then aggregate and re-divide
    # multiply INC raster by population raster to get a raster of incidence counts (# of cases)
    dt[, num_cases:= incidence * population]
    # multiply ACT raster by incidence raster to get a raster of counts for those treated with ACTs
    dt[, acts_count:= acts_percent * num_cases]
    
    # multiple ITN raster by population to get a count of ITNs
    dt[, itn_count := itn_percent * population]
    
    dt[, prevalence:= prevalence * population]

  # aggregate counts to national level 
  dt_natl <- dt[, .(natl_acts_count= sum(acts_count, na.rm=TRUE), 
                    natl_num_cases= sum(num_cases, na.rm=TRUE),
                    natl_itn_count= sum(itn_count, na.rm=TRUE),
                    natl_population= sum(population, na.rm=TRUE),
                    natl_prevalence= sum(prevalence, na.rm=TRUE))]
  # then divide to get national % coverage with acts and coverage of itns
  percent_coverage_ACT = (dt_natl$natl_acts_count / dt_natl$natl_num_cases) * 100 
  percent_coverage_ITN = (dt_natl$natl_itn_count / dt_natl$natl_population) * 100 
  
#####################    
  # add this to results
  
  if (y == "2017"){ 
    results[ year == y, incidence_per100k := ((dt_natl$natl_num_cases/dt_natl$natl_population) * 100000)]
    results[ year == y, prevalence := dt_natl$natl_prevalence]
    
    results[ year == y, population := dt_natl$natl_population]
  } else {
    results[ year == y, act_coverage_under5 := percent_coverage_ACT]
    results[ year == y, acts_total_under5 := dt_natl$natl_acts_count]
    results[ year == y, itn_coverage := percent_coverage_ITN]
    results[ year == y, itn_total := dt_natl$natl_itn_count]
    
    results[ year == y, incidence_per100k := ((dt_natl$natl_num_cases/dt_natl$natl_population) * 100000)]
    results[ year == y, prevalence := dt_natl$natl_prevalence]
    
    results[ year == y, population := dt_natl$natl_population]
  }
}

# save data
saveRDS(results, paste0(out_dir, outFile))
# --------------------------------------------------

# --------------------------------------------------
# graph results
# --------------------------------------------------

results_long <- melt.data.table(results, id.vars= "year", variable.name = "indicator")

g <- ggplot(results_long[ indicator %in% c("act_coverage_under5", "itn_coverage") & value != 0], aes(x=year, y= value, color = indicator)) + 
  theme_bw() + geom_point(size = 2 ) + geom_line(size = 1) +
  ggtitle(paste0("ITN coverage and ACT coverage (percent of under 5 cases of fever treated with ACTs)")) +
  ylab("Percent coverage") + xlab("Year") + labs(caption = "Source: Malaria Atlas Project") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_blank(), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14)) +
  facet_wrap( ~indicator, scales = "free_y") +
  scale_y_continuous( label= scales :: comma, limits= c(0, NA) ) + guides(color=FALSE)
print(g)

g <- ggplot(results_long[ indicator %in% c("incidence_per100k", "prevalence")], aes(x=year,  y= value, color = indicator)) + 
  theme_bw() + geom_point() + geom_line() +
  ggtitle(paste0("Incidence and prevalence of Plasmodium falciparum over time in DRC")) +
  ylab(" ? ") + xlab("Year") + labs(caption = "Source: Malaria Atlas Project") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_blank(), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14)) +
  scale_y_continuous( label= scales :: comma, limits= c(0, NA) ) +
  facet_wrap( ~indicator, scales = "free_y") + guides(color=FALSE)
print(g)

# --------------------------------------------------





