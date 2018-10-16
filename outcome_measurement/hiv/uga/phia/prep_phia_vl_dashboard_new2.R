# ------------------------------------------------------------------------------------------------
# David Phillips, Caitlin O'Brien-Carelli
#
# 10/16/2018
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
  inFilePHIA = paste0(dir, 'prepped/vl_suppression_by_region.csv')
  inFileVLD = paste0(dir, 'prepped/vl_data.rds')

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
  
  # convert viral load regions 
  regAltMap = fread(regAltMapFile)
  
  # check that the regions are the same
  if (all(unique(vl$region) %in% phia$region)!=TRUE) print("One or more regions are mismatched!")

# -------------------------------------------------------------------------------------------
  # Load/prep AIS dataset and ART estimates
  
  # load the 2011 aids indicator survey
  ais = data.table(read.dta13(inFileAIS))
  
  # collapse to estimate art coverage at the region level
  national = ais[, list('art_coverage'=mean(s535=='yes', na.rm=TRUE))]
  ais = ais[, list('art_coverage'=mean(s535=='yes', na.rm=TRUE)), by='v024']
  
  # convert the names of the regions to be the same as the vl and phia data
  ais[ , v024:=as.character(v024)]
  ais[ , v024:=capitalize(ais$v024)]
  
  # normalize around current ART estimates
  art = fread(inFileART)
  art = art[measure=='ART' & metric=='Rate' & year_id==2016 & sex_id==3 & age_group_id==22]
  ais[ , art_coverage_2011:=art_coverage]
  ais[, art_coverage:=art_coverage*(art$mean/national$art_coverage)]

  
  # change the ais regions to match the vl dashboard
  # these regions are the same as phia, but the spelling differs
  setnames(ais, 'v024', 'region')
  
  ais[ , first:=capitalize((unlist(lapply(strsplit(ais$region, "\\s"), "[", 1))))]
  ais[ , second:=capitalize((unlist(lapply(strsplit(ais$region, "\\s"), "[", 2))))]
  ais[region!='Kampala' , region:=paste0(first, '_', second)]
  ais[ ,c('first', 'second'):=NULL]
  ais[region=='Mid_Western', region:='Mid_West']
  ais[region=='Mid_Eastern', region:='Mid_East']
  ais[region=='South_Western', region:='South_West']
  ais[region=='Mid_Northern', region:='Mid_North']
  ais[region=='Kampala', region:='Kampala']
  
  # export a file to map
  saveRDS(ais, paste0(dir, 'prepped/ais_data.rds'))
    
  # ------------------------------------------------------------------------------------
  
  # -------------------------------------------------------------------------------------------
  # Merge datasets and format for analysis
  
  # merge
  facLevelData = merge(phia, vl, by='region')
  
  # clean up variable names
  setnames(facLevelData, c('VLS prevalence (%)', '95% CI'), c('phia_vls', 'phia_vls_ci'))
  
  # split confidence intervals
  facLevelData[, c('phia_vls_lower', 'phia_vls_upper'):=tstrsplit(phia_vls_ci, '-', fixed=TRUE)]
  facLevelData[, phia_vls_lower:=as.numeric(phia_vls_lower)]
  facLevelData[, phia_vls_upper:=as.numeric(phia_vls_upper)]
  
  # handle level input
  if (level=='region') byVars = c('region')
  if (level=='district') byVars = c('district_name', 'region')
  if (level=='facility') byVars = c('facility_name', 'district_name', 'region')
  
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




