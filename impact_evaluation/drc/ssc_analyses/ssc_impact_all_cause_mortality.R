# ------------------------------------------------------------------------------
# David Phillips
# 
# 6/19/2019
# Analysis to assess the impact of having the full package of iCCM services on mortality
# ------------------------------------------------------------------------------


# -------------------
# Set up R
rm(list=ls())
library(data.table)
library(lme4)
library(ggplot2)
# -------------------


# ---------------------------------------------------------------------------------------
# Files and directories

# root directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/')

# input file
inFile = paste0(dir, '/impact_evaluation/cod/prepped_data/second_half_data_pre_model.rdata')

# file listing health zones
hzFile = paste0(dir, '/outcome_measurement/cod/prepped_data/Unicef HZ extraction_BH.csv')
# ---------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Load/prep data

# load input data
load(inFile)

# load list of health zones with full package
hzList = fread(hzFile)

# subset columns
hzList = hzList[,'health_zone']

# get rate of deaths for all cause mortality
data[ , allDeaths_under5_rate := totalDeathsAllDiseases_under5/population*100000]

# data = untransformed[, c('health_zone','date','malariaDeaths_under5','allDeaths_under5_rate','completeness_totalPatientsTreated')]
data[allDeaths_under5_rate<0, allDeaths_under5_rate:=0]

# check for health zones in the list that aren't in the data 
# (there are 3, but really 2 because kikwit-nord is combined with kikwit-sud in the data)
hzList$health_zone[!hzList$health_zone %in% untransformed$health_zone]

# identify "intervention" health zones in the data
hzList[, intervention:=1]
data = merge(data, hzList, by='health_zone', all.x=TRUE)
data[is.na(intervention), intervention:=0]
data[, intervention_label:=ifelse(intervention==1, '2. Intervention', '1. Control')]

# identify before/after
data[, period:=ifelse(date<2017, 0, 1)]
data[, period_label:=ifelse(period==1, '2. After 2017', '1. Before 2017')]

# take averages for graph
means = data[, .(allDeaths_under5_rate=mean(allDeaths_under5_rate), 
	lower_pctle=quantile(allDeaths_under5_rate, 0.025), upper_pctle=quantile(allDeaths_under5_rate, 0.975)), 
	by=c('period_label','intervention_label', 'period','intervention')]
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Run analysis

# offset log transform
offset = quantile(data[allDeaths_under5_rate>0]$allDeaths_under5_rate, .01)
data[, log_allDeaths_under5_rate:=log(allDeaths_under5_rate+offset)]

# difference in differences with OLS
lmFit = lm(allDeaths_under5_rate~intervention*period, data)
summary(lmFit)

# difference in differences with OLS on log-mortality rate
lmFitLog = lm(log_allDeaths_under5_rate~intervention*period, data)
summary(lmFit)

# difference in differences with health zone random effect for correlated errors
lmeFit = lmer(log_allDeaths_under5_rate~intervention*period + (1|health_zone), data)
summary(lmeFit)

# predict from lmFit for graph
means = cbind(means, (predict(lmFit, interval='confidence', newdata=means)))
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Graph

# histogram
ggplot(data[allDeaths_under5_rate<100], aes(x=(allDeaths_under5_rate))) + 
	geom_histogram() + 
	facet_grid(intervention_label~period_label, scales='free') 

# traditional DiD graph
ggplot(means, aes(y=allDeaths_under5_rate, ymin=lower_pctle, ymax=upper_pctle, x=period_label, color=intervention_label)) + 
	geom_pointrange(position=position_dodge(width=.2), size=1) + 
	geom_line(aes(group=intervention_label), position=position_dodge(width=.2), size=1) + 
	labs(title='Comparison of Health Zones', 
		subtitle='With and Without Full Package of iCCM Services', 
		y='Under-5 All-Cause Mortality Rate (per 10,000 population)', x='Period', color='Health Zones', 
		caption='Points and ranges show mean, \n 2.5th and 97.5th percentiles of health zones') + 
	theme_bw()

# traditional DiD graph with model estimates on it
ggplot(means, aes(y=fit, ymin=lwr, ymax=upr, x=period_label, color=intervention_label)) + 
	geom_pointrange(position=position_dodge(width=.2), size=.65) + 
	geom_line(aes(group=intervention_label), position=position_dodge(width=.2), size=1) + 
	labs(y='Under-5 All-Cause Mortality Rate', x='Period', color='Health Zones', 
		caption='Points and ranges show midpoint, \n upper and lower 95% confidence interval from model') + 
	theme_bw()
# ------------------------------------------------------------------------------
