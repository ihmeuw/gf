# ------------------------------------------------
# David Phillips
# 
# 3/12/2019
# Final pre-processing for "second-half" impact evaluation model
# This is built for the pilot dataset
# The current working directory should be the root of this repo (set manually by user)
# ------------------------------------------------


source('./impact_evaluation/_common/set_up_r.r')

# -----------------------------------------------------------------
# Load/prep data

# load
data = readRDS(outputFile2c)

# drop unnecessary variables
dropVars = c('act_coverage','incidence','prevalence','mortality','itn_coverage','year')
data = data[,-dropVars, with=FALSE]

# convert date to numeric
data[, date:=as.numeric(year(date)+((month(date)/12)-1))]
# -----------------------------------------------------------------


# -----------------------------------------------------------------------
# Data transformations and other fixes for Heywood cases

# apply limits
data[!is.finite(SP_rate), SP_rate:=NA]
data[!is.finite(RDT_rate), RDT_rate:=NA]
data[ACTs_CHWs_rate>1000, ACTs_CHWs_rate:=NA]
data[mildMalariaTreated_rate>2, mildMalariaTreated_rate:=NA]
data[severeMalariaTreated_rate>2.5, severeMalariaTreated_rate:=NA]
data[newCasesMalariaMild_rate>100000, newCasesMalariaMild_rate:=NA]
data[newCasesMalariaSevere_rate>100000, newCasesMalariaSevere_rate:=NA]
data[malariaDeaths_rate>1000, malariaDeaths_rate:=NA]

# last-minute prep that shouldn't be necessary after bugs are fixed
# extrapolate where necessary TEMPORARY
i=0
for(v in names(data)) {
	for(h in unique(data$health_zone)) { 
		i=i+1
		if (!any(is.na(data[health_zone==h][[v]]))) next
		if (!any(!is.na(data[health_zone==h][[v]]))) next
		form = as.formula(paste0(v,'~date'))
		lmFit = glm(form, data[health_zone==h], family='poisson')
		data[health_zone==h, tmp:=exp(predict(lmFit, newdata=data[health_zone==h]))]
		# print(ggplot(data[health_zone==h], aes_string(y=v,x='date')) + geom_point() + geom_line(aes(y=tmp)) + labs(title=v, subtitle=h))
		data[health_zone==h & is.na(get(v)), (v):=tmp]
		pct_complete = floor(i/(length(names(data))*length(unique(data$health_zone)))*100)
		cat(paste0('\r', pct_complete, '% Complete'))
		flush.console() 
	}
}
data$tmp = NULL

# na omit
data = na.omit(data)

# log-transform
logVars = c('RDT_rate','SP_rate','ACTs_CHWs_rate','ITN_rate',
	'newCasesMalariaMild_rate','newCasesMalariaSevere_rate','malariaDeaths_rate')
for(v in logVars) data[, (v):=log(get(v))]
for(v in logVars) data[!is.finite(get(v)), (v):=quantile(data[is.finite(get(v))][[v]],.01,na.rm=T)]

# rescale variables to have similar variance
# see Kline Principles and Practice of SEM (2011) page 67
scaling_factors = data.table(date=1)
for(v in names(data)) { 
	if (v %in% c('health_zone','date')) next
	s=1
	while(var(data[[v]]/s)>10) s=s*10
	while(var(data[[v]]/s)<1) s=s/10
	scaling_factors[,(v):=s]
}
scaling_factors = scaling_factors[rep(1,nrow(data))]
for(v in names(scaling_factors)) data[, (v):=get(v)/scaling_factors[[v]]]

# compute lags (after rescaling because it creates more NA's)
data[, lag_ITN_rate:=data.table::shift(ITN_rate), by='health_zone']
data[, lag_mildMalariaTreated_rate:=data.table::shift(mildMalariaTreated_rate), by='health_zone']
data[, lag_severeMalariaTreated_rate:=data.table::shift(severeMalariaTreated_rate), by='health_zone']
data = na.omit(data)

# health zone dummies
data[, health_zone:=gsub(' |-', '_', health_zone)]
hzs = unique(data$health_zone)
data[, (hzs):=lapply(hzs, function(x) health_zone==x)]
# -----------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
# Run final tests

# test unique identifiers
test = nrow(data)==nrow(unique(data[,c('health_zone','date'), with=F]))
if (test==FALSE) stop(paste('Something is wrong. date does not uniquely identify rows.'))

# test for collinearity

# test for variables with an order of magnitude different variance

# ---------------------------------------------------------------------------------------


# ---------------------------------------------------------
# Save file
save(list=c('data', 'scaling_factors'), file=outputFile5c)

# save a time-stamped version for reproducibility
date_time = gsub('-|:| ', '_', Sys.time())
outputFile5cArchive = gsub('prepped_data/', 'prepped_data/archive/', outputFile5c)
outputFile5cArchive = gsub('.rdata', paste0('_', date_time, '.rdata'), outputFile5cArchive)
file.copy(outputFile5c, outputFile5cArchive)
# ---------------------------------------------------------
