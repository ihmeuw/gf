# ------------------------------------------------------------------------------------------------
# David Phillips
#
# 10/31/2017
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
prepVL = function(dir=NULL, level='region', annual=FALSE) { 
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

	# input files
	inFilePHIA = paste0(dir, 'phia_2016/vl_suppression_by_region.csv')
	inFileVLD = paste0(dir, 'vl_dashboard/facilities_suppression_201710311708_aug16_mar17.csv')
	inFileVLD15 = paste0(dir, 'vl_dashboard/facilities_suppression_201711211146_jan15_dec15.csv')
	inFileVLD16 = paste0(dir, 'vl_dashboard/facilities_suppression_201711211213_jan16_dec16.csv')
	inFileVLD17 = paste0(dir, 'vl_dashboard/facilities_suppression_201711211214_jan17_nov17.csv')
	inFileAIS = 'J:/DATA/MACRO_AIS/UGA/2011/UGA_AIS6_2011_IND_Y2012M10D11.DTA'
	inDirART = 'J:/WORK/04_epi/01_database/02_data/hiv/spectrum/summary/170617_hotsauce_high'
	inFileART = paste0(inDirART, '/locations/UGA_spectrum_prep.csv')
	
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

	# load data that matches PHIA time frame
	if (!annual) vldData = fread(inFileVLD)

	# load annual data if specified
	if (annual) {
		i=1
		for(y in c(15,16,17)) {
			tmp = fread(get(paste0('inFileVLD',y)))
			tmp[, year:=2000+y]
			if (i==1) vldData = tmp
			if (i>1) vldData = rbind(vldData, tmp)
			i=i+1
		}
	}
	
	# correct non-standard district names
	distAltMap = fread(distAltMapFile)
	vldData[, District:=gsub(' District', '', District)]
	vldData = merge(vldData, distAltMap, by.x='District', by.y='dist_alt_name', all.x=TRUE)
	vldData[is.na(dist_name), dist_name:=District]
	vldData = vldData[District!='District Left Blank']

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
	
	# include time if annual is specified
	if (annual) byVars = c(byVars, 'year')
	
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

