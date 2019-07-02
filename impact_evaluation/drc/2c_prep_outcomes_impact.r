# ----------------------------------------------
# David Phillips
# 
# 3/10/2019
# Prep outputs, outcomes and impact indicators for 'second half' dose response model
# Intended to be run by 1_master_file.r
# WARNING: the line `aggregate to admin2 level` uses 16 cores on the cluster
# ----------------------------------------------


# to do
# use incidence as denominator for ACT coverage
# fix handling of duplicate HZ names in shapefile

# ----------------------------------------------
# Set up R
source('./impact_evaluation/drc/set_up_r.r')
source('./core/standardizeHZNames.R')
source('./core/standardizeDPSNames.R')
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
	data_est = data[, .(value_lbd=sum(value)), by=c('health_zone','year','indicator')]

	# save here to avoid having to rerun this so much
	saveRDS(data_est, outputFile2c_estimates)
}
if (reprep_rasters==FALSE) data_est = readRDS(outputFile2c_estimates)
# ------------------------------------------------------------------


# ------------------------------------------------------------------------
# Load/prep program data

# load data from outputs_activites and combined file to get new variables for outcomes_impact
data_2b = readRDS(outputFile2b)
pnlp_snis = readRDS(combined_data_file)
pnlp_snis[ element == "B 10.2 Cas de fièvre dans une zone à risque du paludisme", indicator:= "SSCfevers"]

# subset
inds = c('simpleConfMalariaTreated','newCasesMalariaSimpleConf', 'newCasesMalariaSevere',
	        'ANC','SSCfevers','suspectedMalaria', 'malariaDeaths', 'totalDeathsAllDiseases', 'SP')

pnlp_snis = pnlp_snis[indicator %in% inds & date <= '2019-03-01']
# check that all indicators are in the combined file
if(!all(inds %in% pnlp_snis$indicator)) stop('Some indicators have gone missing from combinedFile')

pnlp_snis[indicator=='simpleConfMalariaTreated', indicator:='mildMalariaTreated']
pnlp_snis[indicator=='newCasesMalariaSimpleConf', indicator:='newCasesMalariaMild']
pnlp_snis = pnlp_snis[!which(indicator=='ANC' & subpopulation%in%c('2nd','3rd','4th'))]
pnlp_snis = pnlp_snis[!which(indicator=='SP' & subpopulation%in%c('2nd','3rd','4th', 'available', 'consumed', 'lost', 'received'))]

# set aside under-5
subpops = c('1to5yrs','2to11mos','under5','completedUnder5','0to11mos')
under5 = pnlp_snis[subpopulation %in% subpops]
under5[, indicator:=paste0(indicator,'_under5')]

# check that data set is unique for date and indicator
if ( nrow(unique(pnlp_snis[,.(indicator, date)])) != nrow(unique(pnlp_snis[,.(indicator, data_set, date)])) ) stop("Cannot sum over data set - you have overlapping dates in different data sets!")

# aggreate out all remaining subpopulations (manually confirmed ok)
byVars = c('dps','health_zone','date','year', 'indicator', 'data_set')
pnlp_snis = pnlp_snis[, .(value_snis=sum(value)), by=byVars]
under5 = under5[, .(value_snis=sum(value)), by=byVars]

# add under-5s as new rows
pnlp_snis = rbind(pnlp_snis, under5)
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Append data together and reshape
pnlp_snis[, dps:=standardizeDPSNames(dps)]

# standardize health zones
data_est[, health_zone:=standardizeHZNames(health_zone)]
  
# health zone name changes to make other data sources consistent with PNLP (see PNLP documenation for more info)
data_est[health_zone == "kashobwe", health_zone := "kasenga" ]
data_est[health_zone == "alimbongo", health_zone := "lubero"]
data_est[health_zone == "kibirizi", health_zone := "rutshuru"]
data_est[health_zone == "mabalako", health_zone := "beni"]
data_est[health_zone == "kamango", health_zone := "mutwanga"]

# collapse out DPS FIX ME!!!
byVars = c('health_zone','indicator','year') 
data_est = data_est[, .(value_lbd=sum(value_lbd)), by=byVars]

# *******CHECK THIS********
# aggregate pnlp_snis data to quarterly level data..
pnlp_snis[, quarter := quarter(date)]
# make date format match data_2b
pnlp_snis[, quarter := as.character(quarter)]
pnlp_snis[ quarter == '1', quarter:= '.00']
pnlp_snis[ quarter == '2', quarter:= '.25']
pnlp_snis[ quarter == '3', quarter:= '.50']
pnlp_snis[ quarter == '4', quarter:= '.75']
pnlp_snis[, year := as.character(year)]
pnlp_snis[, date := paste0(year, quarter)]
pnlp_snis[, quarter := NULL]
pnlp_snis[, date := as.numeric(date)]

# date is now quarterly so can sum over this variable for date
pnlp_snis = pnlp_snis[, .(value_snis=sum(value_snis)), by=.(dps, health_zone, date, year, indicator)]

# change this so SP isn't duplicated when we combine pnlp_snis and data_2b... also
# we decided to use the sum of SP 1st, 2nd, 3rd, and 4th(where applicable) for outputs,
# but only SP_1st to get SP rate over ANC visits for outcomes
data_2b[indicator == "SP", indicator := "SP_all"]

#make data_2b match with pnlp_snis
setnames(data_2b, "value", "value_snis")
data_2b[, date := as.character(date)]
data_2b[, year := substr(date, 1, 4)]
data_2b[, date := as.numeric(date)]
data_2b[, year := as.numeric(year)]

pnlp_snis = rbindlist(list(pnlp_snis, data_2b), use.names = TRUE, fill = TRUE)
# *************************

# collapse out DPS (cont.)
pnlp_snis = pnlp_snis[, .(value_snis=sum(value_snis)), by=c(byVars, 'date')]

# reshape wide FIX ME
data_est_wide = dcast(data_est, health_zone+year~indicator, value.var='value_lbd')
pnlp_snis_wide = dcast(pnlp_snis, health_zone+date+year~indicator, value.var='value_snis')

# combine data FIX ME! ADD DPS TO HANDLE DUPLICATE HZ NAMES
data_est_wide = data_est_wide[year>=min(pnlp_snis_wide$year) & year<=max(pnlp_snis_wide$year)]
pnlp_snis_wide[ , year := as.numeric(year)]

# hz_shp = unique(data_est_wide$health_zone)
# hz_pnlp_snis = unique(pnlp_snis_wide$health_zone)
# hz_pnlp_snis[!hz_pnlp_snis %in% hz_shp]
# hz_shp[!hz_shp %in% hz_pnlp_snis]
# length(hz_pnlp_snis[hz_pnlp_snis %in% hz_shp])
# length(hz_shp[hz_shp %in% hz_pnlp_snis])

data_wide = merge(data_est_wide, pnlp_snis_wide, by=c('health_zone','year'), all.y=TRUE)

# data_wide = data_wide[!is.na(health_zone)]
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

# # one health zone has all zeroes for incidence. drop it too
# data_wide[incidence==0, incidence:=NA]
# data_wide[, all_missing:=all(is.na(incidence)), by='health_zone']
# data_wide = data_wide[all_missing!=TRUE]
# data_wide$all_missing = NULL
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Compute rates 
# set var names to be what's below:
setnames(data_wide, 'ACTs_SSC', 'SSCACT')
setnames(data_wide, 'RDTs_SSC', 'SSCRDT')
setnames(data_wide, 'ITN_consumed', 'ITN') # is this right? it would be ITNs used/distributed not ITNs recevied (at health facilities) 
setnames(data_wide, 'RDT_completed', 'RDT')

# program data
data_wide[, ACTs_CHWs_rate:=SSCACT/SSCfevers]
  # data_wide[, ACTs_CHWs_under5_rate:=SSCACT_under5/(SSCfevers)]
data_wide[, RDTs_CHWs_rate:=SSCRDT/SSCfevers]
data_wide[, SP_rate:=SP/ANC]
data_wide[, RDT_rate:=RDT/suspectedMalaria]
data_wide[, ITN_rate:=ITN/population]
data_wide[, mildMalariaTreated_rate:=mildMalariaTreated/newCasesMalariaMild]
data_wide[, mildMalariaTreated_under5_rate:=mildMalariaTreated_under5/newCasesMalariaMild_under5]
data_wide[, severeMalariaTreated_rate:=severeMalariaTreated/newCasesMalariaSevere]
data_wide[, severeMalariaTreated_under5_rate:=severeMalariaTreated_under5/newCasesMalariaSevere_under5]
data_wide[, malariaDeaths_rate:=malariaDeaths/population*100000]
data_wide[, malariaDeaths_under5_rate:=malariaDeaths_under5/population*100000]
data_wide[, newCasesMalariaMild_rate:=newCasesMalariaMild/population*100000]
data_wide[, newCasesMalariaMild_under5_rate:=newCasesMalariaMild_under5/population*100000]
data_wide[, newCasesMalariaSevere_rate:=newCasesMalariaSevere/population*100000]
data_wide[, newCasesMalariaSevere_under5_rate:=newCasesMalariaSevere_under5/population*100000]

# set whole-series infinite rates to all zero
rateVars = names(data_wide)[grepl('_rate',names(data_wide))]
for(v in rateVars) { 
	data_wide[, tmp:=sum(!is.finite(get(v))), , by='health_zone']
	data_wide[, N:=.N, , by='health_zone']
	data_wide[tmp==N, (v):=0]
}
data_wide$tmp = NULL
data_wide$N = NULL

# set individual infinite rates (denominator = zero) to NA... about 500 of these
rateVars = names(data_wide)[grepl('_rate',names(data_wide))]
for(v in rateVars) data_wide[!is.finite(get(v)), (v):=NA]

# model estimates
data_wide[, act_coverage_rate:=act_coverage/incidence]
data_wide[, itn_coverage_rate:=itn_coverage/population]
data_wide[, incidence_rate:=(incidence/12)/population*100000]
data_wide[, prevalence_rate:=(prevalence/12)/population]
data_wide[, mortality_rate:=(mortality/12)/population*100000]
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Identify HZs with iCCM

# all iccm data before 2015 was imputed
iccmVars = c('SSCACT','SSCfevers','ACTs_CHWs_rate')
iccmVars = c(iccmVars, paste0(iccmVars,'_under5'))
for(v in iccmVars) data_wide[year<2015, (v):=0]
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Test unique identifiers
n1 = nrow(data_wide)
n2 = nrow(data_wide[,c('health_zone','date'),with=FALSE])
if (n1!=n2) stop('health_zone and date do not uniquely identify rows!')
# ------------------------------------------------------------------------


# --------------------------------
# Save intermediate file
data_wide = data_wide[order(health_zone, date)]
saveRDS(data_wide, outputFile2c)
archive(outputFile2c)
# --------------------------------
