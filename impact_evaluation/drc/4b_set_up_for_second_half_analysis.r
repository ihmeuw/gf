# ------------------------------------------------
# David Phillips
# 
# 3/12/2019
# Final pre-processing for "second-half" impact evaluation model
# This is built for the pilot dataset
# The current working directory should be the root of this repo (set manually by user)
# ------------------------------------------------


source('./impact_evaluation/drc/set_up_r.r')

# -----------------------------------------------------------------
# Load/prep data

# load completeness estimates from outputFile4a (TEMPORARY)
load(outputFile4a)
complVars = names(data)[grepl('completeness',names(data))]
completeness = data[,c('orig_health_zone','date', complVars), with=FALSE]
setnames(completeness, 'orig_health_zone', 'health_zone')
completeness = completeness[, lapply(.SD, mean), by=c('health_zone','date')]

# load
data = readRDS(outputFile3b)

# replace data with corrected data
adjVars = names(data)[grepl('_adj',names(data))]
for(v in adjVars) data[, (gsub('_adj','',v)):=get(v)]
data = data[, -adjVars, with=FALSE]

# drop unnecessary variables
dropVars = c('act_coverage','incidence','prevalence','mortality','itn_coverage','year')
data = data[,-dropVars, with=FALSE]

# convert date to numeric
data[, date:=as.numeric(year(date)+((month(date)-1)/12))]

# make MI ratio
data[, case_fatality:=malariaDeaths/(newCasesMalariaMild+newCasesMalariaSevere)]
data[, case_fatality_under5:=malariaDeaths_under5/(newCasesMalariaMild_under5+newCasesMalariaSevere_under5)]

# bring in completeness (TEMPORARY)
data = merge(data, completeness, by=c('health_zone','date'), all.x=TRUE)
data = data[order(health_zone, date)]
# library(zoo)
# for(v in complVars) data[, (v):=na.locf(get(v)), by='health_zone']

# apply limits
data[ITN>1000, ITN:=NA]
data[SSCACT>50000, SSCACT:=NA]
data[SSCACT_under5>1000, SSCACT_under5:=NA]
data[!is.finite(SP_rate), SP_rate:=NA]
data[!is.finite(RDT_rate), RDT_rate:=NA]
data[ACTs_CHWs_rate>1000, ACTs_CHWs_rate:=NA]
data[ACTs_CHWs_under5_rate>500, ACTs_CHWs_under5_rate:=NA]
data[mildMalariaTreated_rate>2, mildMalariaTreated_rate:=NA]
data[mildMalariaTreated_under5_rate>100, mildMalariaTreated_under5_rate:=NA]
data[severeMalariaTreated_rate>2.5, severeMalariaTreated_rate:=NA]
data[severeMalariaTreated_under5_rate>200, severeMalariaTreated_under5_rate:=NA]
data[newCasesMalariaMild_rate>100000, newCasesMalariaMild_rate:=NA]
data[newCasesMalariaMild_under5_rate>100000, newCasesMalariaMild_under5_rate:=NA]
data[newCasesMalariaSevere_rate>100000, newCasesMalariaSevere_rate:=NA]
data[newCasesMalariaSevere_under5_rate>50000, newCasesMalariaSevere_under5_rate:=NA]
data[malariaDeaths_rate>500, malariaDeaths_rate:=NA]
data[malariaDeaths_under5_rate>2000, malariaDeaths_under5_rate:=NA]
data[case_fatality>1, case_fatality:=NA]
data[!is.finite(case_fatality_under5), case_fatality_under5:=NA]
# -----------------------------------------------------------------


# -----------------------------------------------------------------
# Ensure all variables have complete time series 

# extrapolate where necessary using GLM (better would be to use multiple imputation)
i=0
for(v in names(data)) {
	for(h in unique(data$health_zone)) { 
		i=i+1
		if (!any(is.na(data[health_zone==h][[v]]))) next
		if (!any(!is.na(data[health_zone==h][[v]]))) next
		form = as.formula(paste0(v,'~date'))
		lmFit = lm(form, data[health_zone==h])
		data[health_zone==h, tmp:=(predict(lmFit, newdata=data[health_zone==h]))]
		data[health_zone==h & is.na(get(v)), (v):=tmp]
		data[tmp<0, (v):=0]
		pct_complete = floor(i/(length(names(data))*length(unique(data$health_zone)))*100)
		cat(paste0('\r', pct_complete, '% Complete'))
		flush.console() 
	}
}
data$tmp = NULL

# na omit
data = na.omit(data)
# -----------------------------------------------------------------


# -----------------------------------------------------------------------
# Data transformations

# remake ITN_rate now that it can be cumulative
data = data[order(health_zone, date)]
data[,ITN_cumul:=cumsum(ITN_rate*population), by='health_zone']
data[, ITN_rate_cumul:=ITN_cumul/population]

# split before trasnformations
untransformed = copy(data)

# log-transform
logVars = c('ITN','RDT','SP','SSCACT','mildMalariaTreated','severeMalariaTreated',
	'SSCACT_under5','mildMalariaTreated_under5','severeMalariaTreated_under5', 
	'RDT_rate','SP_rate','ACTs_CHWs_rate','ACTs_CHWs_under5_rate',
	'ITN_rate','ITN_rate_cumul','case_fatality','case_fatality_under5',
	'newCasesMalariaMild_rate','newCasesMalariaSevere_rate','malariaDeaths_rate',
	'newCasesMalariaMild_under5_rate','newCasesMalariaSevere_under5_rate','malariaDeaths_under5_rate')
for(v in logVars) { 
	data[, (v):=log(get(v))]
	data[!is.finite(get(v)), (v):=quantile(data[is.finite(get(v))][[v]],.01,na.rm=T)]
}

# compute leads (after rescaling because it creates more NA's)
leadVars = c('newCasesMalariaMild_rate', 'newCasesMalariaSevere_rate', 
	'malariaDeaths_rate', 'case_fatality',
	'newCasesMalariaMild_under5_rate', 'newCasesMalariaSevere_under5_rate', 
	'malariaDeaths_under5_rate', 'case_fatality_under5')
for(v in leadVars) { 
	data[, (paste0('lead_',v)):=data.table::shift(get(v),type='lead'), by='health_zone']
	untransformed[, (paste0('lead_',v)):=data.table::shift(get(v),type='lead'), by='health_zone']
}
data = na.omit(data)
# -----------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
# Run final tests

# test unique identifiers
test = nrow(data)==nrow(unique(data[,c('health_zone','date'), with=F]))
if (test==FALSE) stop(paste('Something is wrong. date does not uniquely identify rows.'))

# test for collinearity
# ---------------------------------------------------------------------------------------


# -------------------------------------------------------
# Save file
print(paste('Saving:', outputFile4b)) 
save(list=c('data', 'untransformed'), file=outputFile4b)

# save a time-stamped version for reproducibility
archive(outputFile4b)
# -------------------------------------------------------
