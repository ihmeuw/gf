# ----------------------------------------------
# David Phillips
# 
# 3/10/2019
# Prep outputs, outcomes and impact indicators for "second half" dose response model
# Intended to be run by 1_master_file.r
# WARNING: the line `aggregate to admin2 level` uses 16 cores on the cluster
# ----------------------------------------------


# to do
# use incidence as denominator for ACT coverage
# add malaria deaths, SSC variables, SP administered, ANC visits and suspectedMalaria from DHIS
# fix handling of duplicate HZ names in shapefile, pnlp and snis

# ----------------------------------------------
# Set up R
source('./impact_evaluation/_common/set_up_r.r')
source('./core/standardizeHZNames.R')
reprep_rasters=FALSE
# ----------------------------------------------


# ------------------------------------------------------------------
# Load/prep MAP estimates

# load DRC shapefile
map = shapefile(admin2ShapeFile)

# define appropriate year range
yearRange = seq(2000, 2019)

if(reprep_rasters==TRUE) {
	# display for user
	print('Aggregating rasters...')

	# load rasters
	allFiles = c(mapITNFiles, mapACTFiles, mapIncidenceFiles, 
				mapPrevalenceFiles, mapMortalityFiles, popFiles)
	percentages = c(mapITNFiles, mapACTFiles, mapIncidenceFiles, mapPrevalenceFiles)
	for(i in seq(length(allFiles))) {
		
		# look up file name
		f = allFiles[i]
		
		# skip years outside the appropriate range
		if (!grepl(paste0(yearRange, collapse='|'),f)) next 
		
		# load raster
		rasterData = raster(f)
		
		# multiply by population if necessary
		if (f %in% percentages) {
			# load population
			currentYear = yearRange[sapply(yearRange,function(x) grepl(x,f))]
			if (!any(grepl(currentYear, popFiles))) popFile = popFiles[length(popFiles)]
			if (any(grepl(currentYear, popFiles))) popFile = popFiles[grepl(currentYear, popFiles)]
			popData = raster(popFile)
			# crop to drc for speed
			rasterData = crop(rasterData, extent(map))
			popData = crop(popData, extent(map))
			# project to match population
			rasterData = projectRaster(rasterData, popData)
			# multiply
			rasterData = rasterData * popData
		}
		
		# aggregate to admin2 level
		hzExtract = unlist(mclapply(map@data$Name, function(x) {
			currentHZ = crop(rasterData, extent(map[map@data$Name==x,]))
			currentHZ = raster::mask(currentHZ, map[map@data$Name==x,])    
			sum(getValues(currentHZ), na.rm=TRUE)
		}, mc.cores=ifelse(Sys.info()[1]=='Windows',1,24)))
		
		# assemble into a data.table
		tmpData = data.table(admin2_id=seq(length(hzExtract)), 
			health_zone=map@data$Name, file=basename(f), value=hzExtract)
		
		# test
		if (nrow(tmpData)!=nrow(map@data)) { 
			stop(paste('Not all admin2s are present in: ', f))
		}
		
		# add to data table
		if (i==1) data = copy(tmpData)
		if (i>1) data = rbind(data, tmpData)
		
		# print progress
		pct_complete = floor(i/length(allFiles)*100)
		cat(paste0('\r', pct_complete, '% Complete'))
		flush.console() 
	}

	# add more variables
	for(y in yearRange) data[grepl(y, file), year:=y]
	data[grepl('antimalarial',file), indicator:='act_coverage']
	data[grepl('itncov',file), indicator:='itn_coverage']
	data[grepl('pf_incidence',file), indicator:='incidence']
	data[grepl('pf_prevalence',file), indicator:='prevalence']
	data[grepl('deaths',file), indicator:='mortality']
	data[grepl('infant',file), age:='infant']
	data[grepl('child',file), age:='child']
	data[grepl('adult',file), age:='adult']
	data[grepl('worldpop',file), indicator:='population']

	# aggregate age groups FIX ME!
	data = data[, .(value_lbd=sum(value)), by=c('health_zone','year','indicator')]

	# save here to avoid having to rerun this so much
	saveRDS(data, outputFile2c_estimates)
}
if (reprep_rasters==FALSE) data = readRDS(outputFile2c_estimates)
# ------------------------------------------------------------------


# ------------------------------------------------------------------------
# Load/prep program data

# load
pnlp = readRDS(pnlpHZFile) 
dt_base = readRDS(snisBaseFile)

# subset
inds = c('severeMalariaTreated','mildMalariaTreated',
	'ITN','RDT','SP','SSCACT',
	'ANC','SSCcasesCrossReferred','SSCcasesReferred','suspectedMalaria',
	'newCasesMalariaMild','newCasesMalariaSevere','malariaDeaths')
pnlp = pnlp[indicator %in% inds]
pnlp = pnlp[!which(indicator=='ITN' & subpopulation=='received')]
pnlp = pnlp[!which(indicator=='SP' & subpopulation%in%c('2nd','3rd','4th'))]
pnlp = pnlp[!which(indicator=='ANC' & subpopulation%in%c('2nd','3rd','4th'))]
pnlp = pnlp[!which(indicator=='RDT' & subpopulation%in%c('positive','received'))]
elems = c('A 1.4 Severe malaria treated', 'A 1.4 Confirmed simple malaria treated',
		'"A 1.5 Severe malaria treated - pregnant woman', 
		'A 1.5 Confirmed simple malaria treated - pregnant woman',
		'A 1.4 Severe malaria', 'A 1.4 Confirmed simple malaria',
		'A 1.5 Severe malaria - pregnant woman', 
		'A 1.5 Confirmed simple malaria - pregnant woman') 
dt_base = dt_base[element_eng %in% elems & date<'2018-07-01']

# aggreate age groups
byVars = c('dps','health_zone','date','indicator')
pnlp = pnlp[, .(value_pnlp=sum(mean)), by=byVars]
dt_base[grepl('Severe malaria treated', element_eng), 
	indicator:='severeMalariaTreated']
dt_base[grepl('simple malaria treated', element_eng), 
	indicator:='mildMalariaTreated']
dt_base[element_eng %in% c('A 1.4 Confirmed simple malaria', 
	'A 1.5 Confirmed simple malaria - pregnant woman'), 
	indicator:='newCasesMalariaMild']
dt_base[element_eng %in% c('A 1.4 Severe malaria', 
	'A 1.5 Severe malaria - pregnant woman'), 
	indicator:='newCasesMalariaSevere']
dt_base = dt_base[!is.na(indicator), .(value_snis=sum(value)), by=byVars]

# make year variables
pnlp[, year:=year(date)]
dt_base[, year:=year(date)]
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Append and reshape

# standardize health zones
data[, health_zone:=standardizeHZNames(health_zone)]
pnlp[, health_zone:=standardizeHZNames(health_zone)]
dt_base[, health_zone:=standardizeHZNames(health_zone)]

# collapse out DPS FIX ME!!!
byVars = c('health_zone','indicator','year')
data = data[, .(value_lbd=sum(value_lbd)), by=byVars]
byVars = c('health_zone','date','indicator','year')
pnlp = pnlp[, .(value_pnlp=sum(value_pnlp)), by=byVars]
dt_base = dt_base[, .(value_snis=sum(value_snis)), by=byVars]

# reshape wide FIX ME
data_wide = dcast(data, health_zone+year~indicator, value.var='value_lbd')
pnlp_wide = dcast(pnlp, health_zone+date+year~indicator, value.var='value_pnlp')
dt_base_wide = dcast(dt_base, health_zone+date+year~indicator, value.var='value_snis')

# combine data FIX ME! ADD DPS TO HANDLE DUPLICATE HZ NAMES
pnlp_wide = pnlp_wide[year<2018]
dt_base_wide = dt_base_wide[year>=2018]
program = rbind(pnlp_wide, dt_base_wide, fill=TRUE)
data_wide = data_wide[year>=min(program$year)]
data_wide = merge(data_wide, program, by=c('health_zone','year'), all.y=TRUE)

data_wide = data_wide[!is.na(health_zone)]
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Ensure that all indicators are present for each health zone

# population should never be zero
data_wide[population==0, population:=NA]

# drop health zones where population is entirely missing for a health zone 
# ... these HZs must not be available in the shapefile
data_wide[, all_missing:=all(is.na(population)), by='health_zone']
data_wide = data_wide[all_missing!=TRUE]
data_wide$all_missing = NULL

# one health zone has all zeroes for incidence. drop it too
data_wide[incidence==0, incidence:=NA]
data_wide[, all_missing:=all(is.na(incidence)), by='health_zone']
data_wide = data_wide[all_missing!=TRUE]
data_wide$all_missing = NULL
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Compute rates 

# program data
data_wide[, ACTs_CHWs_rate:=SSCACT/(SSCcasesCrossReferred+SSCcasesReferred)]
data_wide[, SP_rate:=SP/ANC]
data_wide[, RDT_rate:=RDT/suspectedMalaria]
data_wide[, ITN_rate:=ITN/population]
data_wide[, mildMalariaTreated_rate:=mildMalariaTreated/newCasesMalariaMild]
data_wide[, severeMalariaTreated_rate:=severeMalariaTreated/newCasesMalariaSevere]
data_wide[, malariaDeaths_rate:=malariaDeaths/population*100000]
data_wide[, newCasesMalariaMild_rate:=newCasesMalariaMild/population*100000]
data_wide[, newCasesMalariaSevere_rate:=newCasesMalariaSevere/population*100000]

# model estimates
data_wide[, act_coverage_rate:=act_coverage/incidence]
data_wide[, itn_coverage_rate:=itn_coverage/population]
data_wide[, incidence_rate:=(incidence/12)/population*100000]
data_wide[, prevalence_rate:=(prevalence/12)/population]
data_wide[, mortality_rate:=(mortality/12)/population*100000]
# ------------------------------------------------------------------------


# --------------------------------
# Save intermediate file
data_wide = data_wide[order(health_zone, date)]
saveRDS(data_wide, outputFile2c)
# --------------------------------
