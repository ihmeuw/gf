# ----------------------------------------------------
# David Phillips
# 
# 8/23/2019
# Measure drivers of mortality using program data
# Alternative to the same using GBD estimates
# ----------------------------------------------------


# ----------------
# Set up R
rm(list=ls())
library(data.table)
library(car) # for "Anova" not "anova"
library(GGally)
library(gridExtra)
library(boot)
library(RColorBrewer)
# ----------------


# --------------------------------------------------------------------------------------
# Files and directories

# root directory
dir = 'J:/Project/Evaluation/GF/impact_evaluation/'

# input file
inFile = paste0(dir, 'gtm/raw_data/impact_7.15.19.csv')
inFile_GBD = paste0(dir, 'mortality/prepped_data/tb_pce_data.rds')

# population data (worldpop)
popFile = 'J:/Project/Evaluation/GF/covariates/gtm/worldpop/Guatemala_Municipios_IGN2017_worldpop2010-2012-2015.csv'

# Use GBD estimates or GTM raw program data?
use_GBD = TRUE

# output file
if (use_GBD == TRUE){
  outFile = paste0(dir, 'gtm/visualizations/mortality_drivers/mortality_explained_variance_usingGBDestimates.pdf')
} else {
  outFile = paste0(dir, 'gtm/visualizations/mortality_drivers/mortality_explained_variance.pdf')
}
# --------------------------------------------------------------------------------------


# -----------------------------------------------------------------------------------------
# Load/prep populations

# load
populations = fread(popFile)

# subset columns
vars = c('COD_MUNI__','Poblacion2015','Poblacion2012','Poblacion2010')
populations = populations[, vars, with=FALSE]

# reshape long
populations = melt(populations, id.vars='COD_MUNI__', value.name='population', variable.name='year')
populations[, year:=as.numeric(gsub('Poblacion', '', year))]

# drop munis with no population
populations = populations[!COD_MUNI__ %in% c(0,2000)]

# expand years
years = data.table(expand.grid('COD_MUNI__'=unique(populations$COD_MUNI__), 'year'=seq(2009, 2017)))
populations = merge(populations, years, by=c('COD_MUNI__', 'year'), all=TRUE)

# interpolate years
lmFit = lm(log(population)~year*factor(COD_MUNI__), populations)
populations[, prediction:=exp(predict(lmFit, newdata=populations))]
populations[, population:=prediction]
# -----------------------------------------------------------------------------------------


# -----------------------------------------------------------------------------------------
# Load/prep data

# load impact inFile
data = fread(inFile)

# rename
data = data[, 1:5, with=FALSE]
setnames(data, c('date','department','municipality','case_notification_rate','mortality_rate'))

# collapse to department level
data = merge(data, populations, by.x=c('municipality','date'), by.y=c('COD_MUNI__','year'))
data = data[, .(mortality_rate=weighted.mean(mortality_rate, population), 
				case_notification_rate=weighted.mean(case_notification_rate, population),
				population=sum(population, na.rm=T)), 
				by=c('date','department')]
# -----------------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------------
# load/prep GBD mortality estimates data:
gbd = readRDS(inFile_GBD)
gbd = gbd[country == 'Guatemala']
# collapse population to natl level and merge
populations_natl = populations[, .(population = sum(population)), by = 'year']
gbd = merge(gbd, populations_natl, by = 'year')
# -----------------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------------
# extrapolate where necessary using GLM (better would be to use multiple imputation)
i=1
for(v in c('mortality_rate','case_notification_rate')) {
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
  }
}
data$tmp = NULL

# collapse to national level
national = data[, .(mortality_rate=weighted.mean(mortality_rate, population, na.rm=T), 
				case_notification_rate=weighted.mean(case_notification_rate, population, na.rm=T)), 
				by=c('date')]

# put rates in per 100,000 population
national[, mortality_rate:=mortality_rate*100000]
national[, case_notification_rate:=case_notification_rate*100000]
data[, mortality_rate:=mortality_rate*100000]
data[, case_notification_rate:=case_notification_rate*100000]

# compute MI ratio
# data[mortality_rate>250, mortality_rate:=NA]
# data = data[date%%1==0 & date<=2015]
data[, mi_ratio:=mortality_rate/case_notification_rate]

# drop municipalities that are completely missing or zero
data[, mortality_rate_sum:=sum(mortality_rate, na.rm=T), by='department']
data[, case_notification_rate_sum:=sum(case_notification_rate, na.rm=T), by='department']
data = data[mortality_rate_sum!=0 & case_notification_rate_sum!=0]
data$mortality_rate_sum = NULL
data$case_notification_rate_sum = NULL

# graph data
# ggpairs(data[, c('mortality_rate','case_notification_rate','mi_ratio'), with=F])

# define smithsonTransform function
smithsonTransform = function(x) { 
	N=length( x[!is.na(x)] )
	prop_lsqueeze = logit(((x*(N-1))+0.5)/N)
}

# use GBD data
if (use_GBD == TRUE) { 
  gbd[ , department := 'all']
  setnames(gbd, 'Deaths', 'mortality_rate')
  setnames(gbd, 'Incidence', 'cases_var')
  
  data = copy(gbd)
  national = copy(gbd)
}

if (use_GBD != TRUE) setnames(data, 'case_notification_rate', 'cases_var')

# transform
offset1 = quantile(data[mortality_rate>0]$mortality_rate,.01)
offset2 = quantile(data[cases_var>0]$cases_var,.01)
offset3 = quantile(data[mi_ratio>0]$mi_ratio,.01)
data[, log_mortality_rate:=log(mortality_rate+offset1)]
data[, log_cases_var:=log(cases_var+offset2)]
data[, tmp:=mi_ratio]
# data[tmp==0, tmp:=offset3]
data[tmp>=1, tmp:=1]
data[, logit_mi_ratio:=smithsonTransform(tmp)]
data$tmp=NULL

# z-standardize
data[, mortality_rate_std:=(mortality_rate-mean(mortality_rate))/sd(mortality_rate)]
data[, log_cases_var_std:=(log_cases_var-mean(log_cases_var))/sd(log_cases_var)]
data[, logit_mi_ratio_std:=(logit_mi_ratio-mean(logit_mi_ratio))/sd(logit_mi_ratio)]

# graph transformed data
ggpairs_fig = ggpairs(data[, c('mortality_rate_std','log_cases_var_std','logit_mi_ratio_std'), with=F])
# -----------------------------------------------------------------------------------------


# ----------------------------------------------------

# Get glm estimate

lmFits = lapply(unique(data$department), function(m) { 
	lm(mortality_rate_std ~ log_cases_var_std + logit_mi_ratio_std, data[department==m])
})

# loop over runs and compute explained variance
evs = NULL
for(i in seq(length(lmFits))) {
	fitObject = lmFits[[i]]
	inputData = copy(data[department==unique(data$department)[[i]]])
	tmp = data.table(variable=names(coef(fitObject))[-1])
	values = sapply(tmp$variable, function(v) {
		# test for standardization
		if (round(mean(inputData[[v]]),5)!=0 | round(sd(inputData[[v]]),5)!=1) stop(paste('Variable', v, 'is not z-standardized'))
		coef(fitObject)[[v]] * cov(inputData[[v]], fitObject$fitted.values)
	})
	tmp[, explained_variance := values]
	tmp = rbind(tmp, data.table(variable='Residuals', explained_variance=1-sum(values)))
	tmp[,department:=unique(data$department)[[i]]]
	if (i==1) evs = copy(tmp)
	if (i>1) evs = rbind(evs, tmp)
}

evs_mean = evs[, .(explained_variance=mean(explained_variance)), by='variable']
options(scipen=999)
evs_mean
# ----------------------------------------------------


# ----------------------------------------------------
# Set up to graph

# set up graph data
graphData = evs_mean
graphData[variable=='log_cases_var_std', 
	label:=paste('Incidence -', round(explained_variance*100, 1),'%')]
graphData[variable=='logit_mi_ratio_std', 
	label:=paste('Case Fatality -', round(explained_variance*100, 1),'%')]
graphData[variable=='Residuals', 
	label:=paste('Unexplained by Model -', round(explained_variance*100, 1),'%')]
	
# set up national
if (use_GBD == TRUE) {
  id_vars = c('year', 'country', 'department', 'disease')
} else {
    id_vars = 'date'
}

national = melt(national, id.vars=id_vars)
national[variable=='mortality_rate', variable:='TB Mortality Rate (per 100,000)']
national[variable=='cases_var', variable:='TB Case Notification Rate (per 100,000)']
	
# colors
cols = brewer.pal(3, 'Paired')
cols = c(cols[c(3,2)], '#969696')
# ----------------------------------------------------


# ----------------------------------------------------
# Graph

# open pdf
pdf(outFile, height=5.5, width=8)

print(ggpairs_fig)

if(use_GBD == TRUE){
  cap ='Case fatality approximated by mortality:incidence ratio\nMortality and incidence rates come from GBD 2017 national estimates'
} else {
  cap = 'Case fatality approximated by mortality:incidence ratio\nIncidence approximated by case notification rate'
}

# graph national EV
ggplot(graphData, aes(y=explained_variance, x=1, fill=label)) + 
	geom_bar(width=1, color='gray90', stat='identity', position='stack') + 
	geom_text(aes(label=label), size=3, position=position_stack(vjust=.5)) +
	annotate('text', label='Declining\nMortality\nRates', y=0, x=-0.5, size=5) +
	coord_polar(theta='y') + 
	scale_fill_manual('', values=cols) +
	labs(title='Impact on Mortality Rate', 
		caption=cap) + 
	theme_void() + 
	theme(legend.position='none')


# graph national trends
x_var = ifelse(use_GBD == TRUE, 'year', 'date')
ggplot(national, aes(y=value, x=get(x_var))) + 
	geom_point() +
	geom_smooth() + 
	facet_wrap(~variable, scales='free') + 
	labs(title='National Trends in Reported Mortality and Case Notification', 
		caption='2017 mortality rate estimated based on trend') + 
	theme_bw()
  
if (use_GBD != TRUE){  
  # graph example municipalities
  miExamples = unique(evs[variable=='logit_mi_ratio'][order(-explained_variance)]$department)[1:5]
  incExamples = unique(evs[variable=='log_cases_var'][order(-explained_variance)]$department)[1:5]
  for(h in c(miExamples, incExamples)) { 		
  	evmi = round(evs[department==h & variable=='logit_mi_ratio']$explained_variance,3)
  	evinc = round(evs[department==h & variable=='log_cases_var']$explained_variance,3)
  	tmp = melt(data[department==h], id.vars=c('department','date'))
  	tmp = tmp[!grepl('log',variable)]
  	tmp = tmp[variable!='population']
  	p=ggplot(tmp, aes(y=value, x=date)) + 
  		geom_point() + 
  		geom_line() + 
  		facet_wrap(~variable, scales='free_y') + 
  		labs(title=paste('department:', h), 
  			subtitle=paste('Explained Variance by MI Ratio:',evmi,'\nExplained Variance by Incidence Rate:',evinc),
  			x='') + 
  		theme_bw()
  	print(p)
  }
}
# close pdf
dev.off()
# ----------------------------------------------------

