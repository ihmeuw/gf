# ------------------------------------------------
# David Phillips
# 
# 10/29/2019
# Final pre-processing for impact evaluation model (spanning both 'halves')
# This should make 4a and 4b irrelevant once confirmed that the model works
# The current working directory should be the root of this repo (set manually by user)
# Note: for some reason the regressions under 'extrapolate where necessary' are very slow on the cluster
# ------------------------------------------------

source('./impact_evaluation/drc/set_up_r.r')

# -----------------------------------------------------------------
# Load/prep data from step 3

# load
data = readRDS(outputFile3)

# bring in population estimates where possible if the model is a per-capita model
if(fileLabel=='_pc') { 
	pop = readRDS(outputFile2c)
	pop = pop[,c('health_zone','date','population'), with=F]
	data = merge(data,pop, by=c('health_zone','date'), all.x=TRUE)
}

# make unique health zone names for convenience
data[, orig_health_zone:=health_zone]
data[, health_zone:=paste0(health_zone, '_', dps)]
data$dps = NULL

# last-minute prep that should be done earlier in the process
	# combine the two ITN budget categories since FGH can't distinguish
	data[, other_dah_M1_1:=other_dah_M1_1+other_dah_M1_2]
	data$other_dah_M1_2 = NULL
	
	# combine M2 (all case management) with M2_1 (facility tx) for GF budgets (one summary budget from 2015-2017 has it)
	data[, exp_M2_1:=exp_M2_1+exp_M2]
	data$exp_M2 = NULL
	
	# set other_dah to NA (not 0) after 2016
	for(v in names(data)[grepl('other_dah',names(data))]) data[date>=2017 & get(v)==0, (v):=NA]
	
	# iccm didn't exist prior to 2014, wasn't reported until 2015, consider it zero
	data[date<2015, value_ACTs_SSC:=0]
	
	# completeness reported as a percentage not proportion
	complVars = names(data)[grepl('completeness',names(data))]
	for(v in complVars) data[get(v)>1, (v):=get(v)/100]

# subset dates now that cumulative variables are computed
data = data[date>=2010 & date<2018.75]

# use orig health zone for merge
data$health_zone = NULL
setnames(data, 'orig_health_zone', 'health_zone')
data = data[, lapply(.SD, mean), by=c('health_zone','date')]
# -----------------------------------------------------------------


# -----------------------------------------------------------------
# Load/prep data from step 3b

# load completeness estimates from outputFile4a (TEMPORARY)
# load(outputFile4a)
# complVars = names(data)[grepl('completeness',names(data))]
# completeness = data[,c('orig_health_zone','date', complVars), with=FALSE]
# setnames(completeness, 'orig_health_zone', 'health_zone')
# completeness = completeness[, lapply(.SD, mean), by=c('health_zone','date')]

# load
data2 = readRDS(outputFile3b)

# replace data2 with corrected data2
adjVars = names(data2)[grepl('_adj',names(data2))]
for(v in adjVars) data2[, (gsub('_adj','',v)):=get(v)]
data2 = data2[, -adjVars, with=FALSE]

# drop unnecessary variables
dropVars = c('act_coverage','incidence','prevalence','mortality','itn_coverage','year',
	'ITN', 'mildMalariaTreated', 'SSCACT', 'RDT', 'severeMalariaTreated', 'SP', 
	'malariaDeaths_under5', 'SSCfevers', 'malariaDeaths', 'malariaDeaths_under5', 
	'totalDeathsAllDiseases_under5', 'malariaDeaths_rate', 'malariaDeaths_under5_rate', 
	'newCasesMalariaMild_rate', 'newCasesMalariaMild_under5_rate', 
	'newCasesMalariaSevere_rate', 'newCasesMalariaSevere_under5_rate', 
	'incidence_rate', 'prevalence_rate', 'mortality_rate', 'SSCfevers_under5')
data2 = data2[,-dropVars, with=FALSE]

# convert date to numeric
if (class(data2$date)=='Date') data2[, date:=as.numeric(year(date)+((month(date)-1)/12))]

# apply limits
# data2[SSCACT_under5>1000, SSCACT_under5:=NA]
data2[!is.finite(SP_rate), SP_rate:=NA]
data2[!is.finite(RDT_rate), RDT_rate:=NA]
data2[ACTs_CHWs_rate>1000, ACTs_CHWs_rate:=NA]
# data2[ACTs_CHWs_under5_rate>500, ACTs_CHWs_under5_rate:=NA]
data2[mildMalariaTreated_rate>2, mildMalariaTreated_rate:=NA]
data2[mildMalariaTreated_under5_rate>100, mildMalariaTreated_under5_rate:=NA]
data2[severeMalariaTreated_rate>2.5, severeMalariaTreated_rate:=NA]
data2[severeMalariaTreated_under5_rate>200, severeMalariaTreated_under5_rate:=NA]
# -----------------------------------------------------------------


# -----------------------------------------------------------------
# Merge two halves together
data = merge(data2, data, by=c('health_zone','date'), all.x=TRUE)
data = data[order(health_zone, date)]
# -----------------------------------------------------------------


# -----------------------------------------------------------------
# Ensure all variables have complete time series 

# drop zero-variance variables
numVars = names(data)[!names(data)%in%c('orig_health_zone','health_zone','date')]
for(v in numVars) if (all(is.na(data[[v]]))) data[[v]] = NULL

# extrapolate where necessary using GLM (better would be to use multiple imputation)
i=1
for(v in numVars) {
	for(h in unique(data$health_zone)) { 
		i=i+1
		if (!any(is.na(data[health_zone==h][[v]]))) next
		if (!any(!is.na(data[health_zone==h][[v]]))) next
		form = as.formula(paste0(v,'~date'))
		if (any(data[health_zone==h][[v]]<0, na.rm=TRUE)) { f='gaussian'
		} else { f='poisson' } 
		lmFit = glm(form, data[health_zone==h], family=f)
		data[health_zone==h, tmp:=predict(lmFit, newdata=data[health_zone==h])]
		if (f=='poisson') data[health_zone==h, tmp:=exp(tmp)]
		lim = max(data[health_zone==h][[v]], na.rm=T)+sd(data[health_zone==h][[v]], na.rm=T)
		data[health_zone==h & tmp>lim, tmp:=lim]
		# ggplot(data[health_zone==h], aes_string(y=v, x='date')) + geom_point() + geom_point(aes(y=tmp),color='red')
		data[health_zone==h & is.na(get(v)), (v):=tmp]
		pct_complete = floor(i/(length(numVars)*length(unique(data$health_zone)))*100)
		cat(paste0('\r', pct_complete, '% Complete'))
		flush.console() 
	}
}
data$tmp = NULL

# na omit (for health zones that were entirely missing)
data = na.omit(data)
# -----------------------------------------------------------------


# -----------------------------------------------------------------------
# Data transformations

# make cumulative variables
cumulVars = names(data)[grepl('exp|other_dah|ghe|oop', names(data))]
cumulVars = c(cumulVars, 'value_ITN_received', 'value_RDT_received', 'value_ACT_received', 
	'value_ITN_consumed', 'value_ACTs_SSC', 'value_RDT_completed', 'value_SP', 
	'value_severeMalariaTreated', 'value_totalPatientsTreated', 'value_totalPatientsTreated_under5', 
	'value_ACT_received_under5', 'value_ACTs_SSC_under5','value_severeMalariaTreated_under5')
cumulVars = cumulVars[!cumulVars %in% c('value_ACTs_SSC_under5')] # drop until we can create this variable
for(v in cumulVars) { 
	nv = gsub('value_','',v) 
	data[, (paste0(nv,'_cumulative')):=cumsum(get(v)), by='health_zone']
}

# split before transformations
untransformed = copy(data)

# transform completeness variables using approximation of logit that allows 1's and 0's
# (Smithson et al 2006 Psychological methods 'A better lemon squeezer')
smithsonTransform = function(x) { 
	N=length( x[!is.na(x)] )
	logit(((x*(N-1))+0.5)/N)
}
for(v in complVars) { 
	data[get(v)>1, (v):=1]
	data[, (v):=smithsonTransform(get(v))]
}

# log-transform
logVars = c('RDT_rate','SP_rate','ACTs_CHWs_rate','ACTs_CHWs_under5_rate',
		'ITN_rate','ITN_rate_cumul')
logVars = logVars[!logVars %in% c('ACTs_CHWs_under5_rate','SSCACT_under5')]
for(v in logVars) { 
	data[, (v):=log(get(v))]
	data[!is.finite(get(v)), (v):=quantile(data[is.finite(get(v))][[v]],.01,na.rm=T)]
}

# compute lags
lagVars = names(data)[grepl('exp|other_dah|ghe|oop', names(data))]
for(v in lagVars) { 
	data[, (paste0('lag_',v)):=data.table::shift(get(v),type='lag',n=2), by='health_zone']
	untransformed[, (paste0('lag_',v)):=data.table::shift(get(v),type='lag',n=2), by='health_zone']
}
data = na.omit(data)

# per capita variables of everything in model 1
if(fileLabel=='_pc') { 
	pcVars = c('ITN_received_cumulative', 'RDT_received_cumulative', 
		'ACT_received_cumulative', 'ITN_consumed_cumulative', 
		'ACTs_SSC_cumulative', 'RDT_completed_cumulative', 
		'SP_cumulative', 'severeMalariaTreated_cumulative', 
		'totalPatientsTreated_cumulative', 'lag_exp_M1_1_cumulative', 
		'lag_exp_M1_2_cumulative', 'lag_exp_M2_1_cumulative', 
		'lag_other_dah_M2_cumulative', 'lag_exp_M2_3_cumulative', 
		'lag_exp_M3_1_cumulative', 'lag_other_dah_M1_1_cumulative', 
		'lag_ghe_cumulative', 'lag_other_dah_M2_3_cumulative', 
		'lag_exp_M2_6_cumulative')
	for(v in pcVars) { 
		data[, (paste0(v, '_pc')):=get(v)/population]
		untransformed[, (paste0(v, '_pc')):=get(v)/population]
	}
}
# -----------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
# Run final tests

# test unique identifiers
test = nrow(data)==nrow(unique(data[,c('health_zone','date'), with=F]))
if (test==FALSE) stop(paste('Something is wrong. date does not uniquely identify rows.'))

# test for missingness
test = nrow(data)==nrow(na.omit(data))
if(test==FALSE) stop('Something is wrong. There are missing values after GLM imputation')
# ---------------------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Save file
print(paste('Saving:', outputFile4)) 
save(list=c('data', 'untransformed'), file=outputFile4)

# save a time-stamped version for reproducibility
archive(outputFile4)
# -------------------------------------------------------------------------
