# ------------------------------------------------
# David Phillips
# 
# 1/18/2019
# Final pre-processing for first half impact evaluation model
# The current working directory should be the root of this repo (set manually by user)
# Note: for some reason the regressions under "extrapolate where necessary" are very slow on the cluster...
# ------------------------------------------------

# -----------------------------------------------------------------
# Load/prep data

# load
data = readRDS(outputFile3)

# 	
# 	# set other_dah to NA (not 0) after 2016
# 	for(v in names(data)[grepl('other_dah',names(data))]) data[date>=2017 & get(v)==0, (v):=NA]
# 	
# 	# iccm didn't exist prior to 2014, wasn't reported until 2015, consider it zero
# 	data[date<2015, value_ACTs_SSC:=0]
# 	
# 	# completeness reported as a percentage not proportion
# 	complVars = names(data)[grepl('completeness',names(data))]
# 	for(v in complVars) data[get(v)>1, (v):=get(v)/100]
# 
# # subset dates now that cumulative variables are computed
# data = data[date>=2010 & date<2018.75]
# -----------------------------------------------------------------


# -----------------------------------------------------------------
# Ensure all variables have complete time series 

# drop zero-variance variables
numVars = names(data)[!names(data)%in%c('department','date')]
for(v in numVars) if (all(is.na(data[[v]]))) data[[v]] = NULL

# extrapolate where necessary using GLM (better would be to use multiple imputation)
i=1
for(v in numVars) {
	for(h in unique(data$department)) { 
		i=i+1
		if (!any(is.na(data[department==h][[v]]))) next
		if (!any(!is.na(data[department==h][[v]]))) next
		form = as.formula(paste0(v,'~date'))
		lmFit = glm(form, data[department==h], family='poisson')
		data[department==h, tmp:=exp(predict(lmFit, newdata=data[department==h]))]
		lim = max(data[department==h][[v]], na.rm=T)+sd(data[department==h][[v]], na.rm=T)
		data[department==h & tmp>lim, tmp:=lim]
		data[department==h & is.na(get(v)), (v):=tmp]
		pct_complete = floor(i/(length(numVars)*length(unique(data$department)))*100)
		cat(paste0('\r', pct_complete, '% Complete'))
		flush.console() 
	}
}
data$tmp = NULL

# na omit (for health zones that were entirely missing)
# data = na.omit(data)
# -----------------------------------------------------------------


# -----------------------------------------------------------------------
# Data transformations and other fixes for Heywood cases

# # make cumulative variables
# cumulVars = names(data)[grepl('exp|other_dah|ghe|oop', names(data))]
# cumulVars = c(cumulVars, 'value_ITN_received', 'value_RDT_received', 'value_ACT_received', 
# 	'value_ITN_consumed', 'value_ACTs_SSC', 'value_RDT_completed', 'value_SP', 
# 	'value_severeMalariaTreated', 'value_totalPatientsTreated', 'value_totalPatientsTreated_under5', 
# 	'value_ACT_received_under5', 'value_ACTs_SSC_under5','value_severeMalariaTreated_under5')
# for(v in cumulVars) { 
# 	nv = gsub('value_','',v) 
# 	data[, (paste0(nv,'_cumulative')):=cumsum(get(v)), by='health_zone']
# }
# 
# # split before transformations
untransformed = copy(data)
# 
# # transform completeness variables using approximation of logit that allows 1's and 0's
# # (Smithson et al 2006 Psychological methods "A better lemon squeezer")
# smithsonTransform = function(x) { 
# 	N=length( x[!is.na(x)] )
# 	prop_lsqueeze = logit(((x*(N-1))+0.5)/N)
# }
# for(v in complVars) { 
# 	data[get(v)>1, (v):=1]
# 	data[, (v):=smithsonTransform(get(v))]
# }
# 
# # log-transform some variables
# # logVars = c('ITN_consumed_cumulative','ACTs_SSC_cumulative', 'ACT_received_cumulative', 
# 	# 'RDT_completed_cumulative','SP_cumulative', 'ITN_received_cumulative', 
# 	# 'severeMalariaTreated_cumulative','totalPatientsTreated_cumulative')
# # for(v in logVars) { 
# 	# data[, (v):=log(get(v))]
# 	# data[!is.finite(get(v)), (v):=quantile(data[is.finite(get(v))][[v]],.01,na.rm=T)]
# # }
# 
# # compute lags
# lagVars = names(data)[grepl('exp|other_dah|ghe|oop', names(data))]
# for(v in lagVars) { 
# 	data[, (paste0('lag_',v)):=data.table::shift(get(v),type='lag',n=2), by='health_zone']
# 	untransformed[, (paste0('lag_',v)):=data.table::shift(get(v),type='lag',n=2), by='health_zone']
# }
# data = na.omit(data)
# # -----------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
# Run final tests

# test unique identifiers
test = nrow(data)==nrow(unique(data[,c('department','date'), with=F]))
if (test==FALSE) stop(paste('Something is wrong. date does not uniquely identify rows.'))

# test for collinearity

# ---------------------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Save file
print(paste('Saving:', outputFile4a)) 
save(list=c('data', 'untransformed'), file=outputFile4a)

# save a time-stamped version for reproducibility
archive(outputFile4a)
# -------------------------------------------------------------------------
