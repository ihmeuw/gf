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
# Files, directories and settings

# whether or not to run analysis among UNICEF health zones only
unicefOnly = TRUE

# root directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/')

# input file
inFile = paste0(dir, '/impact_evaluation/cod/prepped_data/second_half_data_pre_model.rdata')

# file listing health zones
hzFile = paste0(dir, '/mapping/cod/ssc_lists/prepped_hz_list.csv')

# output files
outFile = paste0(dir, '/impact_evaluation/cod/prepped_data/ssc_analyses/DiD_results.rdata')
graphFile = paste0(dir, '/impact_evaluation/cod/visualizations/ssc_analyses/DiD_results.pdf')

# modify output file names if we're running analysis among UNICEF health zones only
if(unicefOnly) graphFile = gsub('.pdf', '_UNICEF_HZs_only.pdf', graphFile)
if(unicefOnly) outFile = gsub('.rdata', '_UNICEF_HZs_only.rdata', outFile)
# ---------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Load/prep data

# load input data
load(inFile)

# load list of health zones with full package
hzList = fread(hzFile)
if(unicefOnly) hzList = hzList[unicef_supported==1]

# compute all cause mortality rate
untransformed[ , allDeaths_under5_rate := totalDeathsAllDiseases_under5/population*100000]

# subset columns
hzList = hzList[,c('health_zone','unicef_supported')]
data = untransformed[, c('health_zone','date','malariaDeaths_under5','malariaDeaths_under5_rate','completeness_totalPatientsTreated', 'totalDeathsAllDiseases_under5', 'allDeaths_under5_rate')]

# drop rows post-2017 because DHIS doesn't have age-specific mortality
data = data[date<2018]

# check for health zones in the list that aren't in the data 
# kikwit-nord is ok because it's combined with kikwit-sud in the data
# there are 10 others that aren't explained
hzList$health_zone[!hzList$health_zone %in% untransformed$health_zone]

# list health zones that will get counted as the control group
unique(untransformed$health_zone[!untransformed$health_zone %in% hzList$health_zone])

# identify "intervention" health zones in the data
hzList[, intervention:=1]
data = merge(data, hzList, by='health_zone', all.x=TRUE)
data[is.na(intervention), intervention:=0]
data[, intervention_label:=ifelse(intervention==1, '2. Intervention', '1. Control')]

# identify before/after
data[, period:=ifelse(date<2017, 0, 1)]
data[, period_label:=ifelse(period==1, '2. After 2017', '1. Before 2017')]
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Aggregate data

# take averages by intervention/period to have a data frame to predict amongst
means = data[, .(malariaDeaths_under5_rate=mean(malariaDeaths_under5_rate), 
	allDeaths_under5_rate=mean(allDeaths_under5_rate),	
	lower_pctle_malaria=quantile(malariaDeaths_under5_rate, 0.2), 
	upper_pctle_malaria=quantile(malariaDeaths_under5_rate, 0.8), 
	lower_pctle_all_cause=quantile(allDeaths_under5_rate, 0.2), 
	upper_pctle_all_cause=quantile(allDeaths_under5_rate, 0.8)), 
	by=c('period_label','intervention_label', 'period', 'intervention')]
	
# take averages by intervention/date for time series graph of data
means_ts = data[, .(malariaDeaths_under5_rate=median(malariaDeaths_under5_rate), 
	allDeaths_under5_rate=median(allDeaths_under5_rate),	
	lower_pctle_malaria=quantile(malariaDeaths_under5_rate, 0.2), 
	upper_pctle_malaria=quantile(malariaDeaths_under5_rate, 0.8), 
	lower_pctle_all_cause=quantile(allDeaths_under5_rate, 0.2), 
	upper_pctle_all_cause=quantile(allDeaths_under5_rate, 0.8)), 
	by=c('intervention_label', 'date', 'intervention')]
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Run analysis

# difference in differences with OLS on malaria mortality
lmFit1 = lm(malariaDeaths_under5_rate~intervention*period, data)
summary(lmFit1)

# difference in differences with OLS on all cause mortality
lmFit2 = lm(allDeaths_under5_rate~intervention*period, data)
summary(lmFit2)

# predict from lmFit and glmFit for graph
preds1 = data.table(predict(lmFit1, interval='confidence', newdata=means))
preds2 = data.table(predict(lmFit2, interval='confidence', newdata=means))
setnames(preds1, paste0(names(preds1), '_malaria'))
setnames(preds2, paste0(names(preds2), '_all_cause'))
means = cbind(means, preds1)
means = cbind(means, preds2)
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Graph histograms

# histogram of malaria mortality
cut = 100
n_trunc1 = sum(data$malariaDeaths_under5_rate>=cut)
p1 = ggplot(data[malariaDeaths_under5_rate<cut], aes(x=malariaDeaths_under5_rate)) + 
	geom_histogram() + 
	geom_vline(data=means, aes(xintercept=malariaDeaths_under5_rate)) + 
	facet_grid(intervention_label~period_label, scales='free') + 
	labs(title='Under-5 Malaria Mortality Rates', 
		subtitle='Comparison between Health Zones with and without Full Package of iCCM Services', 
		y='Frequency', x='Under-5 Malaria Mortality Rate (per 100,000 population)',
		caption=paste0('Unit of analysis is health zone-quarters, 2010-2019\n(', 
		n_trunc1, ' values >', cut, ' not displayed)')) + 
	theme_bw()

# histogram of all-cause mortality
cut = 300
n_trunc2 = sum(data$allDeaths_under5_rate>=cut)
p2 = ggplot(data[allDeaths_under5_rate<cut], aes(x=allDeaths_under5_rate)) + 
	geom_histogram() + 
	geom_vline(data=means, aes(xintercept=allDeaths_under5_rate)) + 
	facet_grid(intervention_label~period_label, scales='free') + 
	labs(title='Under-5 All-Cause Mortality Rates', 
		subtitle='Comparison between Health Zones with and without Full Package of iCCM Services', 
		y='Frequency', x='Under-5 All-Cause Mortality Rate (per 100,000 population)',
		caption=paste0('Unit of analysis is health zone-quarters, 2010-2019\n(', 
		n_trunc2, ' values >', cut, ' not displayed)')) + 
	theme_bw()
# ------------------------------------------------------------------------------
	
	
# ------------------------------------------------------------------------------
# Graph time series plots

# time series graph of the malaria data
p3 = ggplot(means_ts, aes(y=malariaDeaths_under5_rate, ymin=lower_pctle_malaria, 
		ymax=upper_pctle_malaria, x=date, color=intervention_label, fill=intervention_label)) + 
	geom_ribbon(alpha=.5) + 
	geom_line(size=1.25) + 
	scale_fill_manual(values=c('#33a02c', '#1f78b4')) + 
	scale_color_manual(values=c('#33a02c', '#1f78b4')) + 
	labs(title='Malaria Mortality Under 5', 
		subtitle='Comparing Health Zones with and without Full Package of iCCM Services', 
		y='Under-5 Malaria Mortality Rate (per 10,000 population)', x='Period', 
		color='Health Zones', fill='Health Zones', 
		caption='Lines and intervals show median, 20th and 80th percentiles of health zones') + 
	theme_bw()

# time series graph of the all-cause data
p4 = ggplot(means_ts, aes(y=allDeaths_under5_rate, ymin=lower_pctle_all_cause, 
		ymax=upper_pctle_all_cause, x=date, color=intervention_label, fill=intervention_label)) + 
	geom_ribbon(alpha=.5) + 
	geom_line(size=1.25) + 
	scale_fill_manual(values=c('#33a02c', '#1f78b4')) + 
	scale_color_manual(values=c('#33a02c', '#1f78b4')) + 
	labs(title='All-Cause Mortality Under 5', 
		subtitle='Comparing Health Zones with and without Full Package of iCCM Services', 
		y='Under-5 All-Cause Mortality Rate (per 10,000 population)', x='Period', 
		color='Health Zones', fill='Health Zones', 
		caption='Lines and intervals show median, 20th and 80th percentiles of health zones') + 
	theme_bw()
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Graph DiD results

# traditional DiD graph with malaria model estimates
p5 = ggplot(means, aes(y=fit_malaria, ymin=lwr_malaria, ymax=upr_malaria, 
		x=period_label, color=intervention_label)) + 
	geom_pointrange(position=position_dodge(width=.2), size=1) + 
	geom_line(aes(group=intervention_label), position=position_dodge(width=.2), size=1) + 
	scale_color_manual(values=c('#33a02c', '#1f78b4')) + 
	labs(y='Under-5 Malaria Mortality Rate', x='Period', color='Health Zones', 
		caption='Points and ranges show midpoint, upper and lower 95% confidence interval from model') + 
	theme_bw()

# traditional DiD graph with all-cause model estimates
p6 = ggplot(means, aes(y=fit_all_cause, ymin=lwr_all_cause, ymax=upr_all_cause, 
		x=period_label, color=intervention_label)) + 
	geom_pointrange(position=position_dodge(width=.2), size=1) + 
	geom_line(aes(group=intervention_label), position=position_dodge(width=.2), size=1) + 
	scale_color_manual(values=c('#33a02c', '#1f78b4')) + 
	labs(y='Under-5 Malaria Mortality Rate', x='Period', color='Health Zones', 
		caption='Points and ranges show midpoint, upper and lower 95% confidence interval from model') + 
	theme_bw()
# ------------------------------------------------------------------------------


# ----------------------------------------------------
# Save results

# save graphs
pdf(graphFile, height=5.5, width=8)
p1
p2
p3
p4
p5
p6
dev.off()

# save workspace (excluding temporary objects)
dropObjs = c('cut','preds1','preds2','untransformed')
rm(list=dropObjs)
save(list=ls(), file=outFile)
# ----------------------------------------------------
