# ------------------------------------------------
# Francicso Rios, David Phillips
# 
# 8/19/2019
# Final pre-processing for first half impact evaluation model
# The current working directory should be the root of this repo (set manually by user)
# Note: for some reason the regressions under "extrapolate where necessary" are very slow on the cluster...
# ------------------------------------------------

source('./impact_evaluation/sen/set_up_r.r')

#-----------------------------------------------------------------
# Load/prep data

data = readRDS(outputFile3)

# bring in population estimates where possible if the model is a per-capita model
#if(fileLabel=='_pc') { 
#	pop = readRDS(outputFile2c)
#	pop = pop[,c('region','date','population'), with=F]
#	data = merge(data,pop, by=c('region','date'), all.x=TRUE)
#}

# subset dates now that cumulative variables are computed
data = data[date>=2010 & date<2018.75]
# -----------------------------------------------------------------


# -----------------------------------------------------------------
# Ensure all variables have complete time series 

# drop zero-variance variables
numVars = names(data)[!names(data)%in%c('region','date')]
for(v in numVars) if (all(is.na(data[[v]]))) data[[v]] = NULL

# extrapolate where necessary using GLM (better would be to use multiple imputation)
i=1
for(v in numVars) {
	for(h in unique(data$region)) { 
		i=i+1
		if (!any(is.na(data[region==h][[v]]))) next
		if (!any(!is.na(data[region==h][[v]]))) next
		form = as.formula(paste0(v,'~date'))
		lmFit = glm(form, data[region==h], family='poisson')
		data[region==h, tmp:=exp(predict(lmFit, newdata=data[region==h]))]
		lim = max(data[region==h][[v]], na.rm=T)+sd(data[region==h][[v]], na.rm=T)
		data[region==h & tmp>lim, tmp:=lim]
		# ggplot(data[region==h], aes_string(y=v, x='date')) + geom_point() + geom_point(aes(y=tmp),color='red')
		data[region==h & is.na(get(v)), (v):=tmp]
		pct_complete = floor(i/(length(numVars)*length(unique(data$region)))*100)
		cat(paste0('\r', pct_complete, '% Complete'))
		flush.console() 
	}
}
data$tmp = NULL

# na omit (for regions that were entirely missing)
data = na.omit(data)
# -----------------------------------------------------------------


# -----------------------------------------------------------------------
# Data transformations

# make cumulative variables
cumulVars = names(data)[grepl('exp_|other_dah|ghe|oop', names(data))]
cumulVars = c(cumulVars, 'tb_tfc', 'ntr_rhz', 'ntr_erhz', 'ntr_all', 
	'ntr_erhz', 'ntr_serhz', 'ntr_cpx', 'tot_confirme', 
	'com_cause', 'com_radio', 'com_enf_ref', 'com_mobsoc', 
	'com_nom_touss', 'com_enf_ref', 'tb_vih_arv', 'tot_genexpert',
	'tb_vih_arv', 'tpm_chimio_enf', 'tpm_chimio_pvvih', 'dx_count')

for(v in cumulVars) { 
	nv = gsub('value_','',v) 
	data[, (paste0(nv,'_cumulative')):=cumsum(get(v)), by='region']
}

# split before transformations
untransformed = copy(data)

# update the complVars vector to refer to any proportion variable
complVars = c('perf_lab', 
              'gueris_taux',
              'mdr_tx_rate')

# transform completeness variables using approximation of logit that allows 1's and 0's
# (Smithson et al 2006 Psychological methods "A better lemon squeezer")
 smithsonTransform = function(x) { 
 	N=length( x[!is.na(x)] )
 	logit(((x*(N-1))+0.5)/N)
 }
 for(v in complVars) { 
 	data[get(v)>1, (v):=1]
 	data[, (v):=smithsonTransform(get(v))]
 }

# log-transform some variables
# logVars = c('ITN_consumed_cumulative','ACTs_SSC_cumulative', 'ACT_received_cumulative', 
	# 'RDT_completed_cumulative','SP_cumulative', 'ITN_received_cumulative', 
	# 'severeMalariaTreated_cumulative','totalPatientsTreated_cumulative')
# for(v in logVars) { 
	# data[, (v):=log(get(v))]
	# data[!is.finite(get(v)), (v):=quantile(data[is.finite(get(v))][[v]],.01,na.rm=T)]
# }

# compute lags
lagVars = names(data)[grepl('exp_|other_dah|ghe|oop', names(data))]
for(v in lagVars) { 
	data[, (paste0('lag_',v)):=data.table::shift(get(v),type='lag',n=2), by='region']
	untransformed[, (paste0('lag_',v)):=data.table::shift(get(v),type='lag',n=2), by='region']
}
data = na.omit(data)

# per capita variables of everything in model 1
#if(fileLabel=='_pc') { 
#	pcVars = c("ITN_received_cumulative", "RDT_received_cumulative", 
#		"ACT_received_cumulative", "ITN_consumed_cumulative", 
#		"ACTs_SSC_cumulative", "RDT_completed_cumulative", 
#		"SP_cumulative", "severeMalariaTreated_cumulative", 
#		"totalPatientsTreated_cumulative", "lag_exp_M1_1_cumulative", 
#		"lag_exp_M1_2_cumulative", "lag_exp_M2_1_cumulative", 
#		"lag_other_dah_M2_cumulative", "lag_exp_M2_3_cumulative", 
#		"lag_exp_M3_1_cumulative", "lag_other_dah_M1_1_cumulative", 
#		"lag_ghe_cumulative", "lag_other_dah_M2_3_cumulative", 
#		"lag_exp_M2_6_cumulative")
#	for(v in pcVars) { 
#		data[, (paste0(v, '_pc')):=get(v)/population]
#		untransformed[, (paste0(v, '_pc')):=get(v)/population]
#	}
#}
# -----------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
# Run final tests

# test unique identifiers
test = nrow(data)==nrow(unique(data[,c('region','date'), with=F]))
if (test==FALSE) stop(paste('Something is wrong. date does not uniquely identify rows.'))

# test for missingness
test = nrow(data)==nrow(na.omit(data))
if(test==FALSE) stop('Something is wrong. There are missing values after GLM imputation')
# ---------------------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Save file
print(paste('Saving:', outputFile4a)) 
save(list=c('data', 'untransformed'), file=outputFile4a)

# save a time-stamped version for reproducibility
archive(outputFile4a)
# -------------------------------------------------------------------------
