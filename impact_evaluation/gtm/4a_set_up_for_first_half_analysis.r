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
library(faraway) #For viewing collinearity, below. 

# load
data = readRDS(outputFile3)
modelVersion = 'gtm_tb_first_half2'
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
# # subset dates now that cumulative variables are computed (MOVE THIS AFTER CUMULATIVE SUMS EMILY)
# data = data[date>=2010 & date<2018.75]
# -----------------------------------------------------------------

#------------------------------------------------------------------
source(paste0('./impact_evaluation/gtm/models/', modelVersion, '.R'))

# reduce the data down to only necessary variables
parsedModel = lavParseModelString(model)
modelVars = unique(c(parsedModel$lhs, parsedModel$rhs))
modelVars = c(modelVars, 'department', 'date')
modelVars = gsub("_cumulative", "", modelVars)
reporting = data[, unique(modelVars), with=F]

#What variables are reporting for what years? 
report_long = melt(reporting, id.vars=c('department', 'date'))
report_long[!is.na(value), value:=1]
report_long = report_long[, .(total_by_dept=sum(value, na.rm=TRUE)), by=c('date', 'variable')]
totalVars=length(unique(report_long$variable))
report_long[total_by_dept!=0, var_by_year:=1]
report_long[total_by_dept==0, var_by_year:=0]
report_long[, vars_available_pct:=(sum(var_by_year)/totalVars)*100, by='date']
write.csv(unique(report_long[, .(date, vars_available_pct)]), "C:/Users/elineb/Desktop/variables_available_by_year.csv", row.names=F)

#Do the same check, but exclude 0's. 
report_long2 = melt(reporting, id.vars=c('department', 'date'))
report_long2[value==0, value:=NA]
report_long2[!is.na(value), value:=1]
report_long2 = report_long2[, .(total_by_dept=sum(value, na.rm=TRUE)), by=c('date', 'variable')]
totalVars=length(unique(report_long2$variable))
report_long2[total_by_dept!=0, var_by_year:=1]
report_long2[total_by_dept==0, var_by_year:=0]
report_long2[, vars_available_pct:=(sum(var_by_year)/totalVars)*100, by='date']
write.csv(unique(report_long2[, .(date, vars_available_pct)]), "C:/Users/elineb/Desktop/variables_available_by_year_excl_0.csv", row.names=F)


#------------------------------------------------------------------
# Check for linear dependence - added by EL 7/29/2019
source(paste0('./impact_evaluation/gtm/models/', modelVersion, '.R'))

# reduce the data down to only necessary variables
parsedModel = lavParseModelString(model)
modelVars = unique(c(parsedModel$lhs, parsedModel$rhs))
#We'll want to use the cumulative vars in the final model, but remove this for this test 
modelVars = gsub("_cumulative", "", modelVars)
data = data[, unique(modelVars), with=FALSE]

# -----------------------------------------------------------------
# Ensure all variables have complete time series 

# drop zero-variance variables
numVars = names(data)[!names(data)%in%c('department','date')]
for(v in numVars) if (all(is.na(data[[v]]))) data[[v]] = NULL

#EMILY - WE WANT TO ONLY IMPUTE VARIABLES THAT ARE COUNTS. 
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
cumulVars = names(data)
cumulVars = cumulVars[!grepl("total", cumulVars)]
cumulVars = cumulVars[!cumulVars%in%c('department', 'date', 'year', 'min')]
for(v in cumulVars) {
	nv = gsub('value_','',v)
	data[, (paste0(nv,'_cumulative')):=cumsum(get(v)), by='department']
}
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
