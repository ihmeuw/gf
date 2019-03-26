# ------------------------------------------------
# David Phillips
# 
# 1/18/2019
# Final pre-processing for impact evaluation model
# This is built for the pilot dataset
# The current working directory should be the root of this repo (set manually by user)
# ------------------------------------------------

# TO DO
# how to implement counterfactual before calculating cumulatives and standardizing variance???

source('./impact_evaluation/_common/set_up_r.r')

# -----------------------------------------------------------------
# Load/prep data

# load
data = readRDS(outputFile3)

# make unique health zone names for convenience
data[, health_zone:=paste0(health_zone, '_', dps)]
data$dps = NULL

# last-minute prep that shouldn't be necessary after bugs are fixed
	# combine the two ITN budget categories since FGH can't distinguish
	data[, other_dah_M1_1:=other_dah_M1_1+other_dah_M1_2]
	data$other_dah_M1_2 = NULL
	
	# combine M2 (all case management) with M2_1 (facility tx) for GF budgets (one summary budget from 2015-2017 has it)
	data[, exp_M2_1:=exp_M2_1+exp_M2]
	data$exp_M2 = NULL
	
	# set other_dah to NA (not 0) after 2016
	for(v in names(data)[grepl('other_dah',names(data))]) data[date>=2017 & get(v)==0, (v):=NA]
	
	# drop M2_3 from other_dah for now because it's identifcal to M2_1
	# data$other_dah_M2_3 = NULL
	
	# iccm didn't exist prior to 2014, wasn't reported until 2015, consider it zero
	data[date<2015, ACTs_SSC:=0]

# compute cumulative budgets
rtVars = names(data)
rtVars = rtVars[grepl('exp|other_dah', rtVars)]
for(v in rtVars) data[, (paste0(v,'_cumulative')):=cumsum(get(v)), by='health_zone']

# subset dates now that cumulative variables are computed
data = data[date>=2010 & date<2019]
# -----------------------------------------------------------------


# -----------------------------------------------------------------------
# Data transformations and other fixes for Heywood cases

# drop zero-variance variables
numVars = names(data)[names(data)!='health_zone']
for(v in numVars) { 
	if (sd(data[[v]],na.rm=T)==0) data[[v]] = NULL
}

# extrapolate where necessary TEMPORARY
i=1
for(v in numVars) {
	for(h in unique(data$health_zone)) { 
		i=i+1
		if (!any(is.na(data[health_zone==h][[v]]))) next
		if (!any(!is.na(data[health_zone==h][[v]]))) next
		form = as.formula(paste0(v,'~date'))
		lmFit = glm(form, data[health_zone==h], family='poisson')
		data[health_zone==h, tmp:=exp(predict(lmFit, newdata=data[health_zone==h]))]
		lim = max(data[health_zone==h][[v]], na.rm=T)+sd(data[health_zone==h][[v]], na.rm=T)
		data[health_zone==h & tmp>lim, tmp:=lim]
		# print(ggplot(data[health_zone==h], aes_string(y=v,x='date')) + geom_point() + geom_line(aes(y=tmp)) + labs(title=v))
		data[health_zone==h & is.na(get(v)), (v):=tmp]
		pct_complete = floor(i/(length(numVars)*length(unique(data$health_zone)))*100)
		cat(paste0('\r', pct_complete, '% Complete'))
		flush.console() 
	}
}
data$tmp = NULL

# now remake ghe_cumulative TEMPORARY
data[, ghe_cumulative:=cumsum(ghe), by='health_zone']
data[, oop_cumulative:=cumsum(oop), by='health_zone']
data[, ITN_received_cumulative:=cumsum(ITN_received), by='health_zone']
data[, RDT_received_cumulative:=cumsum(RDT_received), by='health_zone']
data[, ACT_received_cumulative:=cumsum(ACT_received), by='health_zone']
data[, ITN_consumed_cumulative:=cumsum(ITN_consumed), by='health_zone']
data[, ACTs_SSC_cumulative:=cumsum(ACTs_SSC), by='health_zone']
data[, RDT_completed_cumulative:=cumsum(RDT_completed), by='health_zone']
data[, SP_cumulative:=cumsum(SP), by='health_zone']
data[, severeMalariaTreated_cumulative:=cumsum(severeMalariaTreated), by='health_zone']
data[, totalPatientsTreated_cumulative:=cumsum(totalPatientsTreated), by='health_zone']

# na omit (for health zones that were entirely missing)
data = na.omit(data)

# drop completeness variables (for now)
for(v in names(data)[grepl('completeness', names(data))]) data[[v]]=NULL

# split before trasnformations
untransformed = copy(data)

# transform completeness variables
# for(v in names(data)[grepl('completeness', names(data))]) data[, (v):=logit(get(v))]

# # log-transform all variables (NEED CONFIRMATION AT HZ LEVEL)
logVars =names(data)[!names(data)%in%c('health_zone','date')]
logVars = c('ITN_consumed_cumulative','ACTs_SSC_cumulative','RDT_completed_cumulative','SP_cumulative','severeMalariaTreated_cumulative','totalPatientsTreated_cumulative')
for(v in logVars) data[, (v):=log(get(v))]
for(v in logVars) data[!is.finite(get(v)), (v):=quantile(data[is.finite(get(v))][[v]],.01,na.rm=T)]

# rescale variables to have similar variance
# see Kline Principles and Practice of SEM (2011) page 67
scaling_factors = data.table(date=1)
for(v in names(data)) { 
	if (v %in% c('health_zone','date')) next
	s=1
	while(var(data[[v]]/s)>1000) s=s*10
	while(var(data[[v]]/s)<100) s=s/10
	scaling_factors[,(v):=s]
}
scaling_factors = scaling_factors[rep(1,nrow(data))]
for(v in names(scaling_factors)) data[, (v):=get(v)/scaling_factors[[v]]]
# data[, lapply(.SD, var)]
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
save(list=c('data', 'untransformed', 'scaling_factors'), file=outputFile5a)

# save a time-stamped version for reproducibility
date_time = gsub('-|:| ', '_', Sys.time())
outputFile5aArchive = gsub('prepped_data/', 'prepped_data/archive/', outputFile5a)
outputFile5aArchive = gsub('.rdata', paste0('_', date_time, '.rdata'), outputFile5aArchive)
file.copy(outputFile5a, outputFile5aArchive)
# ---------------------------------------------------------
