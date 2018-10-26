# -------------------------------------------
# David Phillips
#
# 12/11/2017
# Analyze correlates of absorption
# -------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(boot)
library(readxl)
library(data.table)
library(stringr)
library(ggplot2)
library(GGally)
# ------------------


# -----------------------------------------------------------------
# Files and directories

# root directory for input/output
dir = 'J:/Project/Evaluation/GF/resource_tracking/multi_country/'

# input data
inFile = paste0(dir, 'mapping/prepped_gos_data.csv')

# place to store the regression output
regOutFile = paste0(dir, '../../vfm/outputs/absorption_correlates_model_fit.rdata')

# output graphs
outFile = paste0(dir, '../../vfm/visualizations/absorption_correlates_exploration.pdf')
# -----------------------------------------------------------------


# ----------------------------------------------------------------------
# Load/prep data

# load
allData = fread(inFile)

# identify quarters
allData[, quarter:=quarter(start_date)]

# collapse to module-quarter level
byVars = c('disease','country','grant_number','year','quarter','abbrev_module')
data = allData[, list('budget'=sum(budget,na.rm=TRUE), 
			'expenditure'=sum(expenditure,na.rm=TRUE)), by=byVars]

# compute absorption
data[, absorption:=expenditure/budget]

# define lemon squeeze function (store N to global environment for later use)
lemonSqueeze = function(x) { 
	N <<- length(x[!is.na(x)])
	return(logit(((x*(N-1))+0.5)/N))
}
reverseLemonSqueeze = function(x) { 
	N = length(x[!is.na(x)])
	return(((inv.logit(x)*N)-0.5)/(N-1))
}

# handle 1's and 0's so logit doesn't drop them
data = data[is.finite(absorption) & absorption>=0]
data[absorption>1, absorption:=1] 
data[, absorption:=lemonSqueeze(absorption)]
# data[absorption>=1, absorption:=max(data[absorption<1]$absorption)] 
# data[absorption<=0, absorption:=min(data[absorption>0]$absorption)] 
# data[, absorption:=logit(absorption)]
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
# Generate extra predictor variables

# year within grant and years from end of grant
data[, grant_year:=as.numeric(as.factor(year)), by='grant_number']
data[, years_from_end:=max(grant_year)-grant_year+1, by='grant_number']
data[, yearid:=as.numeric(as.factor(year)), by='grant_number']
data[, quarterid:=(((yearid-1)*4))+quarter]
data[, quarters_from_end:=max(quarterid)-quarterid+1, by='grant_number']

# total budget of the grant
data[, total_budget:=sum(budget,na.rm=TRUE), by='grant_number']

# number of modules within grant
data[, num_modules:=length(unique(abbrev_module)), by='grant_number']
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
# Univariate regressions

# all confounding variables to SDA
summary(lm(absorption~year, data=data))
summary(lm(absorption~quarterid, data=data))
summary(lm(absorption~quarters_from_end, data=data))
summary(lm(absorption~disease, data=data))
summary(lm(absorption~country, data=data))
summary(lm(absorption~log(total_budget), data=data))
summary(lm(absorption~num_modules, data=data))
summary(lm(absorption~abbrev_module, data=data))

# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
# Graph correlations

vars = c('absorption','year','quarters_from_end','disease','country','total_budget','num_modules','abbrev_module')
logs = c('quarterid','total_budget')
#ggpairs(data[,vars,with=FALSE], cardinality_threshold=23)

# graph univariates
uniPlots = list()
i=1
for(v in vars) { 
	if (class(data[[v]]) %in% c('factor','character')) next
	uniPlots[[i]] = ggplot(data, aes_string(x=v)) + 
		geom_histogram() + 
		theme_bw()
	i=i+1
	if (v %in% logs) { 
		data[, (paste0('log_',v)):=log(get(v))]
		uniPlots[[i]] = ggplot(data, aes_string(x=paste0('log_',v))) + 
			geom_histogram() + 
			theme_bw()
		i=i+1
	}
}


# graph bivariates
biPlots = list()
i=1
for(v in vars[vars!='absorption']) { 
	if (!class(data[[v]]) %in% c('factor','character')) { 
		biPlots[[i]] = ggplot(data, aes_string(y='absorption', x=v)) + 
			geom_point() + 
			geom_smooth(method='lm') + 
			theme_bw()
		i=i+1
	}
	if (!class(data[[v]]) %in% c('factor','character') & v %in% logs) { 
		biPlots[[i]] = ggplot(data, aes_string(y='absorption', x=paste0('log_',v))) + 
			geom_point() + 
			geom_smooth(method='lm') + 
			theme_bw()
		i=i+1
	}
	if (class(data[[v]]) %in% c('factor','character')) { 
		biPlots[[i]] = ggplot(data, aes_string(y='absorption', x=v)) + 
			geom_boxplot() + 
			geom_jitter(width=.1) + 
			labs(x='') + 
			theme_bw() + 
			theme(axis.text.x=element_text(angle=45, hjust=1))
		i=i+1
	}
}

# graph select multivariates
extraPlots=list()
i=1
extraPlots[[i]] = ggplot(data, aes(y=absorption, x=disease)) + 
	geom_boxplot() + 
	geom_jitter(width=.1) + 
	facet_wrap(~country) + 
	theme_bw()

i=1
extraPlots[[i]] = ggplot(data, aes(y=absorption, x=year)) + 
	geom_point() + 
	geom_smooth(method='lm') + 
	facet_wrap(~country) + 
	theme_bw()

i=1
extraPlots[[i]] = ggplot(data, aes(y=absorption, x=year)) + 
	geom_point() + 
	geom_smooth(method='lm') + 
	facet_wrap(~abbrev_module) + 
	theme_bw()
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
# Save
pdf(outFile, height=5.5, width=10)
for(p in seq(length(uniPlots))) print(uniPlots[[p]])
for(p in seq(length(biPlots))) print(biPlots[[p]])
for(p in seq(length(biPlots))) print(extraPlots[[p]])
dev.off()
# ----------------------------------------------------------------------


