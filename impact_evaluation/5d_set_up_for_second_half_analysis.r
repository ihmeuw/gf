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
data = readRDS(outputFile3b)

# replace data with corrected data
adjVars = names(data)[grepl('_adj',names(data))]
for(v in adjVars) data[, (gsub('_adj','',v)):=get(v)]
data = data[, -adjVars, with=FALSE]

# drop unnecessary variables
dropVars = c('act_coverage','incidence','prevalence','mortality','itn_coverage','year')
data = data[,-dropVars, with=FALSE]

# convert date to numeric
data[, date:=as.numeric(year(date)+((month(date)/12)-1))]

# make MI ratio
data[, case_fatality:=malariaDeaths/(newCasesMalariaMild+newCasesMalariaSevere)]
# -----------------------------------------------------------------


# -----------------------------------------------------------------------
# Data transformations and other fixes for Heywood cases

# apply limits
data[ITN>1000, ITN:=NA]
data[SSCACT>50000, SSCACT:=NA]
data[!is.finite(SP_rate), SP_rate:=NA]
data[!is.finite(RDT_rate), RDT_rate:=NA]
data[ACTs_CHWs_rate>1000, ACTs_CHWs_rate:=NA]
data[mildMalariaTreated_rate>2, mildMalariaTreated_rate:=NA]
data[severeMalariaTreated_rate>2.5, severeMalariaTreated_rate:=NA]
data[newCasesMalariaMild_rate>100000, newCasesMalariaMild_rate:=NA]
data[newCasesMalariaSevere_rate>100000, newCasesMalariaSevere_rate:=NA]
data[malariaDeaths_rate>500, malariaDeaths_rate:=NA]
data[case_fatality>1, case_fatality:=NA]

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

# remake ITN_rate now that it can be cumulative
data = data[order(health_zone, date)]
data[,ITN_cumul:=cumsum(ITN_rate*population), by='health_zone']
data[, ITN_rate_cumul:=ITN_cumul/population]

# split before trasnformations
untransformed = copy(data)

# log-transform
logVars = c('ITN','RDT','SP','SSCACT','mildMalariaTreated','severeMalariaTreated',
	'RDT_rate','SP_rate','ACTs_CHWs_rate','ITN_rate','ITN_rate_cumul','case_fatality',
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

# compute leads (after rescaling because it creates more NA's)
leadVars = c('newCasesMalariaMild_rate', 'newCasesMalariaSevere_rate', 'malariaDeaths_rate', 'case_fatality')
for(v in leadVars) data[, (paste0('lead_',v)):=data.table::shift(get(v),type='lead'), by='health_zone']
for(v in leadVars) untransformed[, (paste0('lead_',v)):=data.table::shift(get(v),type='lead'), by='health_zone']
data = na.omit(data)
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
save(list=c('data', 'untransformed', 'scaling_factors'), file=outputFile5d)

# save a time-stamped version for reproducibility
archive(outputFile5d)
# ---------------------------------------------------------
