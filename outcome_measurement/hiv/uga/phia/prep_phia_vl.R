# ----------------------------
# David Phillips, Caitlin O'Brien-Carelli
#
# 10/22/2018
# Function that preps data from PHIA and the Uganda National Viral Load Dashboard
#------------------------------
# Inputs:
# dir - directory where data files exist
# level - what level should the returned data be? Options: 'region', 'district', 'facility'
# annual - (logical) whether VLD data should be annual or in the time frame matching PHIA
# Outputs:
# data - a data.table with both datasets merged together, collapsed to the specified level
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
  
  # check that the regions are the same in phia and vl data 
  if (all(unique(vl$region) %in% phia$region)!=TRUE) print("One or more regions are mismatched!")

# -------------------------------------------------------------------------------------------
  # Load/prep AIS dataset and ART estimates
  
  # load the 2011 aids indicator survey
  ais = data.table(read.dta13(inFileAIS))
  
  # variables in ais:
  # s535: are you taking arvs?
  # v005 = women's individual sample weight (six decimals)
  # v021: primary sampling unit
  # v022: sample strata for sampling errors (identical to v023: stratification used in sample design)
  # v024: region
  
  # subset to on arvs, sample weights, region
  ais = ais[ ,.(s535, v005, v021, v022, v024)]
  
  # subset to only PLHIV
  ais = ais[!is.na(s535)]

  # divide the sample weights by one million
  # v005 = women's individual sample weight (six decimals)
  ais[ , v005:=(v005/1000000)]
  
  # ----------------------------------------------
  # apply survey weights
  
  # create a vector of the sample weights
  weight = ais$v005
  
  des = svydesign(id=ais$v021, strata=ais$v022, weights=weight, data=ais )
  
  # add survey weights 
  result = data.table(svyby(~s535, ~v024, des, svymean))
  
  # collapse to estimate art coverage at the region level
  ais = result[ ,.(region=as.character(v024), art_coverage_2011=s535yes)]
  
  # convert the names of the regions to be the same as the vl and phia data
  ais[ , first:=capitalize((unlist(lapply(strsplit(ais$region, "\\s"), "[", 1))))]
  ais[ , second:=capitalize((unlist(lapply(strsplit(ais$region, "\\s"), "[", 2))))]
  ais[!is.na(second), region:=paste(first, second)]
  ais[region=='kampala', region:=capitalize(region)]
  ais[region=='East Central' | region=='North East' | region=='Central 1' | region=='Central 2' | region=='West Nile', region:=gsub('\\s', '_', region)]  
  ais[region=='Mid Eastern', region:='Mid_East']
  ais[region=='Mid Northern', region:='Mid_North']
  ais[region=='South Western', region:='South_West']
  ais[region=='Mid Western', region:='Mid_West']
  ais[ ,c('first', 'second'):=NULL]
  
  # normalize around the phia 2016 estimate of art coverage
  art = 0.725*0.904
  national_2011 = mean(ais$art_coverage_2011)
  ais[, art_coverage:=art_coverage_2011*(art/national_2011)]

  # normalize around 206 GBD ART national estimates 
  art_gbd = fread(inFileART)
  art_gbd = art_gbd[measure=='ART' & metric=='Rate' & year_id==2016 & sex_id==3 & age_group_id==22]
  ais[, art_coverage_gbd:=art_coverage_2011*(art_gbd$mean/national_2011)]

  # export a file to map
  saveRDS(ais, paste0(dir, 'prepped/ais_data.rds'))
  ais[ , art_coverage_gbd:=NULL]
    
# ------------------------------------------------------------------------------------
# Merge phia, vl, and ais datasets and format for analysis
  
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
  if (level=='district') byVars = c('district', 'region')
  if (level=='facility') byVars = c('facility', 'district', 'region')
  
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
  
  # if using gbd estimates
  #data[ , vld_suppression_adj_gbd:=vld_suppression*art_coverage_gbd]
  # -------------------------------------------------------------------------------------------
  
  
# --------------
# End function
  return(data)
  print(paste('The data is at the', level, 'level.'))
  
}
# ------------------




