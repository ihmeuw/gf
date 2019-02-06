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

# last-minute prep that shouldn't be necessary after bugs are fixed
	# combine the two ITN budget categories since FGH can't distinguish
	data[, other_dah_M1_1:=other_dah_M1_1+other_dah_M1_2]
	data$other_dah_M1_2 = NULL
	
	# set other_dah to NA (not 0) after 2016
	for(v in names(data)[grepl('other_dah',names(data))]) data[date>=2017, (v):=NA]
	
	# drop M2_3 from other_dah for now because it's identifcal to M2_1
	# data$other_dah_M2_3 = NULL

# compute cumulative budgets
rtVars = names(data)
rtVars = rtVars[grepl('budget|other_dah', rtVars)]
for(v in rtVars) data[, (paste0(v,'_cumulative')):=cumsum(get(v))]

# subset dates now that cumulative variables are computed
data = data[date>=2010 & date<2019]
# -----------------------------------------------------------------


# -----------------------------------------------------------------------
# Hotfixes for Heywood cases

# drop zero-variance variables
for(v in names(data)) if (sd(data[[v]],na.rm=T)==0) data[[v]] = NULL

# extrapolate where necessary TEMPORARY
for(v in names(data)) {
	form = as.formula(paste0(v,'~date'))
	lmFit = glm(form, data, family='poisson')
	data[, tmp:=exp(predict(lmFit, newdata=data))]
	# print(ggplot(data, aes_string(y=v,x='date')) + geom_point() + geom_line(aes(y=tmp)) + labs(title=v))
	data[is.na(get(v)), (v):=tmp]
}
data$tmp = NULL

# drop completeness variables (for now)
for(v in names(data)[grepl('completeness', names(data))]) data[[v]]=NULL

# transform completeness variables
# for(v in names(data)[grepl('completeness', names(data))]) data[, (v):=logit(get(v))]

# na omit
data = na.omit(data)

# rescale variables to have similar variance
# see Kline Principles and Practice of SEM (2011) page 67
scaling_factors = data.table(date=1)
for(v in names(data)) { 
	if (v=='date') next
	s=1
	while(var(data[[v]]/s)>1000) s=s*10
	while(var(data[[v]]/s)<100) s=s/10
	scaling_factors[,(v):=s]
}
scaling_factors = scaling_factors[rep(1,nrow(data))]
data = data/scaling_factors
# data[, lapply(.SD, var)]
# -----------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
# Run final tests

# test unique identifiers
test = nrow(data)==nrow(unique(data[,'date', with=F]))
if (test==FALSE) stop(paste('Something is wrong. date does not uniquely identify rows.'))

# test for collinearity

# test for variables with an order of magnitude different variance

# ---------------------------------------------------------------------------------------


# ---------------------------------------------------------
# Save file
save(list=c('data', 'scaling_factors'), file=outputFile5a)
# ---------------------------------------------------------
