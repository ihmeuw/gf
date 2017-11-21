# ------------------------------------------------------------------------------------------------
# David Phillips
#
# 10/31/2017
# Function that preps data from PHIA and the Uganda National Viral Load Dashboard
# Intended to be called by compare_phia_to_vl_dashboard.r
# Inputs:
# dir - directory where data files exist
# level - what level should the returned data be? Options: 'region', 'district', 'facility'
# Outputs:
# data - a data.table with both datasets merged together, collapsed (or not) to the specified level
# ------------------------------------------------------------------------------------------------


# --------------------------------------------
# Start function
prepVL = function(dir=NULL, level='region') { 
	# ----------------------------------------


	# -------------------------------------------------------------------------------
	# Handle inputs
	if (is.null(dir)) stop('Provide data directory')
	if (class(dir)!='character') stop('dir must be character')
	opts = c('region','district','facility')
	if (!level %in% opts) stop(paste('level must be in', paste(opts, collapse=' ')))
	# -------------------------------------------------------------------------------
	
	
	# -------------------------------------------------------------------------------------------
	# Files and directories

	# input files
	inFilePHIA = paste0(dir, 'phia_2016/vl_suppression_by_region.csv')
	inFileVLD = paste0(dir, 'vl_dashboard/facilities_suppression_201710311708_aug16_mar17.csv')
	inFileAIS = 'J:/DATA/MACRO_AIS/UGA/2011/UGA_AIS6_2011_IND_Y2012M10D11.DTA'
	inFileART = 'J:/WORK/04_epi/01_database/02_data/hiv/spectrum/summary/170617_hotsauce_high/locations/UGA_spectrum_prep.csv'
	
	# district/region maps
	distMapFile = paste0(dir, '../../mapping/uga/uga_geographies_map.csv')
	distAltMapFile = paste0(dir, '../../mapping/uga/uga_alternate_dist_names.csv')
	regAltMapFile = paste0(dir, '../../mapping/uga/uga_alternate_region_names.csv')
	# -------------------------------------------------------------------------------------------


	# -------------------------------------------------------------------------------------------
	# Load/prep PHIA dataset

	# load
	phiaData = fread(inFilePHIA)

	# map phia to standard regions
	regAltMap = fread(regAltMapFile)
	phiaData = merge(phiaData, regAltMap, by.x='Region', by.y='region10_alt_name', all.x=TRUE)
	phiaData[is.na(region10_name), region10_name:=Region]
	phiaData$Region = NULL
	# -------------------------------------------------------------------------------------------


	# -------------------------------------------------------------------------------------------
	# Load/prep VLD dataset

	# load
	vldData = fread(inFileVLD)

	# correct non-standard district names
	distAltMap = fread(distAltMapFile)
	vldData[, District:=gsub(' District', '', District)]
	vldData = merge(vldData, distAltMap, by.x='District', by.y='dist_alt_name', all.x=TRUE)
	vldData[is.na(dist_name), dist_name:=District]

	# test for matching district names
	distMap = fread(distMapFile)
	t1 = unique(vldData$dist_name)[!unique(vldData$dist_name) %in% unique(distMap$dist112_name)]
	t2 = unique(distMap$dist112_name)[!unique(distMap$dist112_name) %in% unique(vldData$dist_name)]
	if (length(t1)>0) stop('Warning! There are some districts in the VLD data that aren\'t in the standard 112 list!')
	if (length(t2)>0) stop('Warning! There are some districts the standard 112 list that aren\'t in in the VLD data!')

	# map vld data to standard regions
	distMap = distMap[, c('region10_name', 'region10', 'dist112_name', 'dist112'), with=FALSE]
	vldData = merge(vldData, distMap, by.x='dist_name', by.y='dist112_name', all.x=TRUE)
	vldData[, dist112:=as.character(dist112)]
	# -------------------------------------------------------------------------------------------


	# ------------------------------------------------------------------------------------
	# Load/prep AIS dataset and ART estimates

	# load
	aisData = data.table(read.dta13(inFileAIS))

	# collapse to estimate art coverage at the region level
	national = aisData[, list('art_coverage'=mean(s535=='yes', na.rm=TRUE))]
	aisData = aisData[, list('art_coverage'=mean(s535=='yes', na.rm=TRUE)), by='v024']

	# map to standard regions
	aisData[,v024:=toTitleCase(as.character(v024))]
	aisData[,v024:=gsub(' ', '_', v024)]
	aisData = merge(aisData, regAltMap, by.x='v024', by.y='region10_alt_name', all.x=TRUE)
	aisData[is.na(region10_name), region10_name:=v024]
	aisData$v024 = NULL
	
	# normalize around current ART estimates
	artData = fread(inFileART)
	artData = artData[measure=='ART' & metric=='Rate' & year_id==2016 & sex_id==3 & age_group_id==22]
	aisData[, art_coverage:=art_coverage*(artData$mean/national$art_coverage)]
	# ------------------------------------------------------------------------------------


	# -------------------------------------------------------------------------------------------
	# Merge datasets and format for analysis

	# merge
	facLevelData = merge(phiaData, vldData, by='region10_name')

	# clean up variable names
	setnames(facLevelData, c('VLS Prevalence (%)', '95% CI', 'Valid Results', 'Suppressed Results'), c('phia_vls', 'phia_vls_ci', 'samples', 'vl_suppressed_samples'))

	# split confidence intervals
	facLevelData[, c('phia_vls_lower', 'phia_vls_upper'):=tstrsplit(phia_vls_ci, '-', fixed=TRUE)]
	facLevelData[, phia_vls_lower:=as.numeric(phia_vls_lower)]
	facLevelData[, phia_vls_upper:=as.numeric(phia_vls_upper)]

	# handle level input
	if (level=='region') byVars = c('region10_name','region10')
	if (level=='district') byVars = c('region10_name','dist_name','dist112')
	if (level=='facility') byVars = c('region10_name','dist_name','dist112','Hub','Facility')
	
	# collapse to specified level
	data = facLevelData[, list(phia_vls=mean(phia_vls), 
						phia_vls_lower=mean(phia_vls_lower), 
						phia_vls_upper=mean(phia_vls_upper), 
						samples=sum(samples), 
						vl_suppressed_samples=sum(vl_suppressed_samples)), 
						by=byVars]
						
	# recompute suppression from the dashboard data
	data[, vld_suppression:=vl_suppressed_samples/samples*100]

	# bring in coverage estimates
	data = merge(data, aisData, 'region10_name')

	# compute vld suppression adjusted for coverage
	data[, vld_suppression_adj:=vld_suppression*art_coverage]
	# -------------------------------------------------------------------------------------------


	# --------------
	# End function
	return(data)
}
# ------------------

