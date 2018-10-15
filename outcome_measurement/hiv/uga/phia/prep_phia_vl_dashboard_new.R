# ------------------------------------------------------------------------------------------------
# David Phillips
#
# 10/10/2018
# Function that preps data from PHIA and the Uganda National Viral Load Dashboard
# Intended to be called by compare_phia_to_vl_dashboard.r
# Inputs:
# dir - directory where data files exist
# level - what level should the returned data be? Options: 'region', 'district', 'facility'
# annual - (logical) whether VLD data should be annual or in the time frame matching PHIA
# Outputs:
# data - a data.table with both datasets merged together, collapsed (or not) to the specified level
# ------------------------------------------------------------------------------------------------


# ----------------------------------------------------------
# Start function
prepVL = function(dir=dir, level='region', annual=FALSE) { 
  # ------------------------------------------------------
  
  
  # -------------------------------------------------------------------------------
  # Handle inputs
  if (is.null(dir)) stop('Provide data directory')
  if (class(dir)!='character') stop('dir must be character')
  opts = c('region','district','facility')
  if (!level %in% opts) stop(paste('level must be in', paste(opts, collapse=' ')))
  # -------------------------------------------------------------------------------
  
  # -------------------------------------------------------------------------------------------
  # Files and directories
  
  # input files phia and vl data 
  inFilePHIA = paste0(dir, 'vl_suppression_by_region.csv')
  inFileVLD = paste0(dir, 'vl_region_data.rds')
  inFileVLDdist =  paste0(dir, 'vl_dist_data.rds')
  
  # input files - ais and 
  inFileAIS = 'J:/DATA/MACRO_AIS/UGA/2011/UGA_AIS6_2011_IND_Y2012M10D11.DTA'
  inDirART = 'J:/WORK/04_epi/01_database/02_data/hiv/spectrum/summary/170617_hotsauce_high'
  inFileART = paste0(inDirART, '/locations/UGA_spectrum_prep.csv')
  
  # district/region maps
  distMapFile = 'J:/Project/Evaluation/GF/mapping/uga/uga_geographies_map.csv'
  distAltMapFile = 'J:/Project/Evaluation/GF/mapping/uga/uga_alternate_dist_names.csv'
  regAltMapFile = 'J:/Project/Evaluation/GF/mapping/uga/uga_alternate_region_names.csv'
  
# -------------------------------------------------------------------------------------------
  # Load/prep PHIA and vl data sets
  
  # load phia totals
  phia = fread(inFilePHIA)

  # load vl dashboard
  vl = readRDS(inFileVLD)
  setnames(vl, 'id', 'region')
  
  # load the first level vl 
  vld = readRDS(inFileVLDdist)
  setnames(vld, 'id', 'region')
  
  # convert viral load regions 
  regAltMap = fread(regAltMapFile)
  
  # check that the regions are the same
  if (all(vl$region %in% phia$region)!=TRUE) print("Some regions don't match!")

# -------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------
  # Load/prep AIS dataset and ART estimates
  
  # load the 2011 aids indicator survey
  ais = data.table(read.dta13(inFileAIS))
  
  # collapse to estimate art coverage at the region level
  national = ais[, list('art_coverage'=mean(s535=='yes', na.rm=TRUE))]
  ais = ais[, list('art_coverage'=mean(s535=='yes', na.rm=TRUE)), by='v024']
  
  # convert the names of the regions to be the same as the vl and phia data
  ais[ , v024:=as.character(v024)]
  ais[ , v024:=capitalize(ais$v024)]
  
  # convert names to match phia and vl estimates
  ais[grep('^Central', v024), v024:=(gsub(v024, pattern='-', replacement=' '))]
  
  # convert the names
  ais[v024=="East central", v024:="East-Central"]
  ais[v024=="Mid western", v024:="Mid-West"]
  ais[v024=="Mid eastern", v024:="Mid-East"]
  ais[v024=="West nile", v024:="West Nile"]
  ais[v024=="North east", v024:="North-East"]
  ais[v024== "South western", v024:="South-West"]
  ais[v024=="Mid northern", v024:="Mid-North"]
  
  # alter the variable names to match 2016 art estimates 
  setnames(ais, 'v024', 'region')

  # normalize around current ART estimates from GBD
  art = fread(inFileART)
  art = art[measure=='ART' & metric=='Rate' & year_id==2016 & sex_id==3 & age_group_id==22]
  ais[ , art_coverage_2011:=art_coverage]
  ais[, art_coverage:=art_coverage*(art$mean/national$art_coverage)]

  # export the regional ais art coverage estimates for mapping
  # ais_export = ais[ ,.(region, art_coverage=round(100*art_coverage, 1), art_coverage_2011=round(100*art_coverage_2011, 1))]
  # saveRDS(ais_export, paste0(dir, 'ais_estimates.rds'))
  
  # ------------------------------------------------------------------------------------
  
  # -------------------------------------------------------------------------------------------
  # Merge datasets and format for analysis
  
  # merge
  facLevelData = merge(phia, vld, by='region')
  
  # clean up variable names
  setnames(facLevelData, c('VLS prevalence (%)', '95% CI'), c('phia_vls', 'phia_vls_ci'))
  
  # split confidence intervals
  facLevelData[, c('phia_vls_lower', 'phia_vls_upper'):=tstrsplit(phia_vls_ci, '-', fixed=TRUE)]
  facLevelData[, phia_vls_lower:=as.numeric(phia_vls_lower)]
  facLevelData[, phia_vls_upper:=as.numeric(phia_vls_upper)]
  
  # handle level input
  if (level=='region') byVars = c('region')
  if (level=='district') byVars = c('region','dist_name','dist112')
  if (level=='facility') byVars = c('region','dist_name','dist112','Hub','Facility')
  
  # include time if annual is specified
  if (annual) byVars = c(byVars, 'year')
  
  # collapse to specified level
  data = facLevelData[ , .(phia_vls=mean(phia_vls), 
                             phia_vls_lower=mean(phia_vls_lower), 
                             phia_vls_upper=mean(phia_vls_upper), 
                             samples=sum(valid_results), 
                             vl_suppressed=sum(suppressed)), 
                        by=byVars]
  
  # recompute suppression from the dashboard data
  data[, vld_suppression:=100*(vl_suppressed/samples)]
  
  # fix region names in ais 
  ais[ , region:=gsub(region, pattern='\\s', replacement='_')]
  ais[ , region:=gsub(region, pattern='-', replacement='_')]

  # bring in coverage estimates
  data = merge(data, ais, 'region')
  
  # compute vld suppression adjusted for coverage
  data[, vld_suppression_adj:=vld_suppression*art_coverage]
  # -------------------------------------------------------------------------------------------
  
  
  # --------------
  # End function
  return(data)
}
# ------------------




