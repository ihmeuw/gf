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
data = readRDS(outputFile3b)

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
source(paste0('./impact_evaluation/gtm/models/', modelVersion1, '.R'))

# reduce the data down to only necessary variables
parsedModel = lavParseModelString(model)
modelVars = unique(c(parsedModel$lhs, parsedModel$rhs))
# modelVars = c(modelVars, 'department', 'date')
modelVars = gsub("_cumulative", "", modelVars)
modelVars = gsub("_log", "", modelVars)
# reporting = data[, unique(modelVars), with=F]
# 
# #What variables are reporting for what years? 
# report_long = melt(reporting, id.vars=c('department', 'date'))
# report_long[!is.na(value), value:=1]
# report_long = report_long[, .(total_by_dept=sum(value, na.rm=TRUE)), by=c('date', 'variable')]
# totalVars=length(unique(report_long$variable))
# report_long[total_by_dept!=0, var_by_year:=1]
# report_long[total_by_dept==0, var_by_year:=0]
# report_long[, vars_available_pct:=(sum(var_by_year)/totalVars)*100, by='date']
# write.csv(unique(report_long[, .(date, vars_available_pct)]), "C:/Users/elineb/Desktop/variables_available_by_year.csv", row.names=F)
# 
# #Do the same check, but exclude 0's. 
# report_long2 = melt(reporting, id.vars=c('department', 'date'))
# report_long2[value==0, value:=NA]
# report_long2[!is.na(value), value:=1]
# report_long2 = report_long2[, .(total_by_dept=sum(value, na.rm=TRUE)), by=c('date', 'variable')]
# totalVars=length(unique(report_long2$variable))
# report_long2[total_by_dept!=0, var_by_year:=1]
# report_long2[total_by_dept==0, var_by_year:=0]
# report_long2[, vars_available_pct:=(sum(var_by_year)/totalVars)*100, by='date']
# write.csv(unique(report_long2[, .(date, vars_available_pct)]), "C:/Users/elineb/Desktop/variables_available_by_year_excl_0.csv", row.names=F)
# 

#------------------------------------------------------------------
# Check for linear dependence - added by EL 7/29/2019
# source(paste0('./impact_evaluation/gtm/models/', modelVersion, '.R'))
# 
# # reduce the data down to only necessary variables
# parsedModel = lavParseModelString(model)
# modelVars = unique(c(parsedModel$lhs, parsedModel$rhs))
# #We'll want to use the cumulative vars in the final model, but remove this for this test 
# modelVars = gsub("_cumulative", "", modelVars)
# data = data[, unique(modelVars), with=FALSE]

# -----------------------------------------------------------------
# Ensure all variables have complete time series 

# drop zero-variance variables
# numVars = names(data)[!names(data)%in%c('department','date')]
#EMILY - WE WANT TO ONLY IMPUTE VARIABLES THAT ARE COUNTS. 
for(v in backCastVars) if (all(is.na(data[[v]]))) data[[v]] = NULL #backCastVars is set in the 'set_up_r' script. 

  #CURRENTLY NOT IMPUTING ADDITIONAL CASES DETECTED VIA ACF BECAUSE WE KNOW IT'S VERY DEPARTMENT SPECIFIC- ANY OTHERS? EL 8/12/19
names(data)[!names(data)%in%c(backCastVars, 'date', 'department')]
# extrapolate where necessary using GLM (better would be to use multiple imputation)
i=1
for(v in backCastVars) {
	for(h in unique(data$department)) { 
		i=i+1
		#First, check whether all values for this department and this variable are zero. 
		# if they are, don't backcast. 
		values = unique(data[department==h, as.vector(get(v))]) #Get a vector of the unique values of the variable.
		values[is.na(values)] = 0
		zero_compare = rep(0, length(values)) #Get an equal length vector of zeros.
		if (all(values==zero_compare)){
		  print(paste0(v, " is completely zero for department", h, " - making 0 for the entire time series in this department"))
		  data[department==h, (v):=0]
		} else {
  		#Backcast if it doesn't fall into this category. 
  		if (!any(is.na(data[department==h][[v]]))) next
  		if (!any(!is.na(data[department==h][[v]]))) next
  		form = as.formula(paste0(v,'~date'))
  		lmFit = glm(form, data[department==h], family='poisson')
  		data[department==h, tmp:=exp(predict(lmFit, newdata=data[department==h]))]
  		lim = max(data[department==h][[v]], na.rm=T)+sd(data[department==h][[v]], na.rm=T)
  		data[department==h & tmp>lim, tmp:=lim]
  		data[department==h & is.na(get(v)), (v):=tmp]
		} 
		pct_complete = floor(i/(length(backCastVars)*length(unique(data$department)))*100)
		cat(paste0('\r', pct_complete, '% Complete'))
		flush.console() 
	}
}
data$tmp = NULL

# na omit (for health zones that were entirely missing)
# data = na.omit(data)
# -----------------------------------------------------------------

#---------------------------------------------------------------
# Replace NAs with zeros after back-casting DP 8/16/19 
allVars = names(data)[!names(data)%in%c('date', 'department')]
for (v in allVars){
  data[is.na(get(v)), (v):=0]
}

#------------------------------------------------------------
# Drop variables that are not being used in model object before cumulative sum. "
modelVarSubset = modelVars[!modelVars%in%c("gf_mdrtb*gf_rssh", "gf_tbhiv*gf_rssh", "gf_tb*gf_rssh")] #Don't worry about the interaction terms. 
modelVarSubset = c(modelVarSubset, 'gf_rssh')
missingVars = modelVarSubset[!modelVarSubset%in%names(data)]
if (length(missingVars)!=0) print("There are missing model variables!")
data = data[, c(modelVarSubset, 'department', 'date'), with=F]



# -----------------------------------------------------------------------
# Data transformations and other fixes for Heywood cases

# # make cumulative variables - all NAs should be replaced with zeros in step above. 
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
# log-transform some variables
for(v in logVars) { #logVars created in 'set_up_r' script. 
  newName = paste0((v), "_log")
	data[, (newName):=log(get(v))]
	data[!is.finite(get(newName)), (newName):=quantile(data[is.finite(get(newName))][[newName]],.01,na.rm=T)]
}

# 
# # compute lags
# lagVars = names(data)[grepl('exp|other_dah|ghe|oop', names(data))]
# for(v in lagVars) { 
# 	data[, (paste0('lag_',v)):=data.table::shift(get(v),type='lag',n=2), by='health_zone']
# 	untransformed[, (paste0('lag_',v)):=data.table::shift(get(v),type='lag',n=2), by='health_zone']
# }
# data = na.omit(data)
# # -----------------------------------------------------------------------

#Only keep variables that will be used in the model. 
modelVars = unique(c(parsedModel$lhs, parsedModel$rhs))
modelVarSubset = modelVars[!modelVars%in%c("gf_mdrtb_cumulative:gf_rssh_cumulative", 
                                           "gf_tbhiv_cumulative:gf_rssh_cumulative", "gf_tb_cumulative:gf_rssh_cumulative")] #Don't worry about the interaction terms. 
modelVarSubset = c(modelVarSubset, 'gf_rssh_cumulative')
modelVarSubset = c(modelVarSubset, 'department', 'date')
data = data[, modelVarSubset, with=F]

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
