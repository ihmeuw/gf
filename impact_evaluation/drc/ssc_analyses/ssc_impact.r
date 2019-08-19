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
library(ggplot2)
# -------------------


# ---------------------------------------------------------------------------------------
# Files, directories and settings

# whether or not to run analysis among full-iccm-package health zones only
fullpackageOnly = TRUE

# root directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/')

# input file
inFile = paste0(dir, '/impact_evaluation/cod/prepped_data/ssc_analyses/DiD_input_data.rdata')
if(fullpackageOnly) inFile = gsub('.rdata', '_full_package_HZs_only.rdata', inFile)

# file listing health zones
hzFile = paste0(dir, '/mapping/cod/ssc_lists/prepped_hz_list.csv')

# output files
outFile = paste0(dir, '/impact_evaluation/cod/prepped_data/ssc_analyses/DiD_results.rdata')
graphFile = paste0(dir, '/impact_evaluation/cod/visualizations/ssc_analyses/DiD_results.pdf')

# modify output file names if we're running analysis among UNICEF health zones only
if(fullpackageOnly) graphFile = gsub('.pdf', '_full_package_HZs_only.pdf', graphFile)
if(fullpackageOnly) outFile = gsub('.rdata', '_full_package_HZs_only.rdata', outFile)
# ---------------------------------------------------------------------------------------


# -------------------------------------
# Load

# load data produced by set_up_data.r
load(inFile)
# -------------------------------------


# ------------------------------------------------------------------------------
# Run analysis

# difference in differences with OLS on malaria treatment coverage (uncomplicated)
lmFit1 = lm(mildMalariaTreated_under5_rate~intervention*period, data)
summary(lmFit1)

# difference in differences with OLS on malaria mortality
lmFit2 = lm(malariaDeaths_under5_rate~intervention*period, data)
summary(lmFit2)

# difference in differences with OLS on all cause mortality
lmFit3 = lm(allDeaths_under5_rate~intervention*period, data)
summary(lmFit3)

# difference in differences with OLS on malaria case detection rate
lmFit4 = lm(newCasesMalariaMild_under5_rate~intervention*period, data)
summary(lmFit4)

# predict from lmFit and glmFit for graph
preds1 = data.table(predict(lmFit1, interval='confidence', newdata=means))
preds2 = data.table(predict(lmFit2, interval='confidence', newdata=means))
preds3 = data.table(predict(lmFit3, interval='confidence', newdata=means))
preds4 = data.table(predict(lmFit4, interval='confidence', newdata=means))
setnames(preds1, paste0(names(preds1), '_coverage'))
setnames(preds2, paste0(names(preds2), '_malaria'))
setnames(preds3, paste0(names(preds3), '_all_cause'))
setnames(preds3, paste0(names(preds4), '_detection'))
means = cbind(means, preds1)
means = cbind(means, preds2)
means = cbind(means, preds3)
means = cbind(means, preds4)
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Graph histograms

# switch title if specified
if (fullpackageOnly==FALSE) intlab = 'SSCs'
if (fullpackageOnly==TRUE) intlab = 'iCCM'

# histogram of malaria treatment coverage
cut = 100
n_trunc1 = sum(data$mildMalariaTreated_under5_rate>=cut)
p1 = ggplot(data[mildMalariaTreated_under5_rate<cut], aes(x=mildMalariaTreated_under5_rate)) + 
	geom_histogram() + 
	geom_vline(data=means, aes(xintercept=mildMalariaTreated_under5_rate)) + 
	facet_grid(intervention_label~period_label, scales='free') + 
	labs(title='Under-5 Antimalarial Treatment Coverage', 
		subtitle=paste('Comparison between Health Zones with and without', intlab), 
		y='Frequency', x='Under-5 Antimalarial Treatment Coverage (proportion of cases)',
		caption=paste0('Unit of analysis is health zone-quarters, 2010-2018\n(', 
		n_trunc1, ' values >', cut, ' not displayed)')) + 
	theme_bw()

# histogram of malaria mortality
cut = 100
n_trunc2 = sum(data$malariaDeaths_under5_rate>=cut)
p2 = ggplot(data[malariaDeaths_under5_rate<cut], aes(x=malariaDeaths_under5_rate)) + 
	geom_histogram() + 
	geom_vline(data=means, aes(xintercept=malariaDeaths_under5_rate)) + 
	facet_grid(intervention_label~period_label, scales='free') + 
	labs(title='Under-5 Malaria Mortality Rates', 
		subtitle=paste('Comparison between Health Zones with and without', intlab), 
		y='Frequency', x='Under-5 Malaria Mortality Rate (per 100,000 population)',
		caption=paste0('Unit of analysis is health zone-quarters, 2010-2018\n(', 
		n_trunc2, ' values >', cut, ' not displayed)')) + 
	theme_bw()

# histogram of all-cause mortality
cut = 300
n_trunc3 = sum(data$allDeaths_under5_rate>=cut)
p3 = ggplot(data[allDeaths_under5_rate<cut], aes(x=allDeaths_under5_rate)) + 
	geom_histogram() + 
	geom_vline(data=means, aes(xintercept=allDeaths_under5_rate)) + 
	facet_grid(intervention_label~period_label, scales='free') + 
	labs(title='Under-5 All-Cause Mortality Rates', 
		subtitle=paste('Comparison between Health Zones with and without', intlab), 
		y='Frequency', x='Under-5 All-Cause Mortality Rate (per 100,000 population)',
		caption=paste0('Unit of analysis is health zone-quarters, 2010-2018\n(', 
		n_trunc3, ' values >', cut, ' not displayed)')) + 
	theme_bw()
# ------------------------------------------------------------------------------
	
	
# ------------------------------------------------------------------------------
# Graph time series plots

# time series graph of the malaria treatment data
p4 = ggplot(means_ts, aes(y=mildMalariaTreated_under5_rate, ymin=lower_pctle_mild_coverage, 
		ymax=upper_pctle_mild_coverage, x=date, color=intervention_label, fill=intervention_label)) + 
	geom_ribbon(alpha=.5) + 
	geom_line(size=1.25) + 
	scale_fill_manual(values=c('#33a02c', '#1f78b4')) + 
	scale_color_manual(values=c('#33a02c', '#1f78b4')) + 
	labs(title='Antimalarial Treatment Coverage Under 5', 
		subtitle=paste('Comparing Health Zones with and without', intlab), 
		y='Under-5 Antimalarial Treatment Coverage (proportion of cases)', x='Period', 
		color='', fill='', 
		caption='Lines and intervals show median, 20th and 80th percentiles of health zones') + 
	theme_bw()

# time series graph of the malaria treatment data excluding the control group
p4b = ggplot(means_ts[intervention==1], aes(y=mildMalariaTreated_under5_rate, ymin=lower_pctle_mild_coverage, 
		ymax=upper_pctle_mild_coverage, x=date)) + 
	geom_ribbon(alpha=.5, fill='#1f78b4') + 
	geom_line(size=1.25, color='#1f78b4') + 
	labs(title='Antimalarial Treatment Coverage Under 5', 
		subtitle=paste('Health Zones with', intlab), 
		y='Under-5 Antimalarial Treatment Coverage (proportion of cases)', x='Period', 
		color='', fill='', 
		caption='Lines and intervals show median, 20th and 80th percentiles of health zones') + 
	theme_bw()

# time series graph of the malaria case detection rate
p5 = ggplot(means_ts, aes(y=newCasesMalariaMild_under5_rate, ymin=lower_pctle_mild_detection, 
		ymax=upper_pctle_mild_detection, x=date, color=intervention_label, fill=intervention_label)) + 
	geom_ribbon(alpha=.5) + 
	geom_line(size=1.25) + 
	scale_fill_manual(values=c('#33a02c', '#1f78b4')) + 
	scale_color_manual(values=c('#33a02c', '#1f78b4')) + 
	labs(title='Malaria Case Detection Rate Under 5', 
		subtitle=paste('Comparing Health Zones with and without', intlab), 
		y='Under-5 Malaria Case Detection Rate (per 10,000 population)', x='Period', 
		color='', fill='', 
		caption='Lines and intervals show median, 20th and 80th percentiles of health zones') + 
	theme_bw()

# time series graph of the malaria case detection rate excluding the control group
p5b = ggplot(means_ts[intervention==1], aes(y=newCasesMalariaMild_under5_rate, ymin=lower_pctle_mild_detection, 
		ymax=upper_pctle_mild_detection, x=date)) + 
	geom_ribbon(alpha=.5, fill='#1f78b4') + 
	geom_line(size=1.25, color='#1f78b4') + 
	labs(title='Malaria Case Detection Rate Under 5', 
		subtitle=paste('Health Zones with', intlab), 
		y='Under-5 Malaria Case Detection Rate (per 10,000 population)', x='Period', 
		color='', fill='', 
		caption='Lines and intervals show median, 20th and 80th percentiles of health zones') + 
	theme_bw()

# time series graph of the malaria case detection fraction of estimates
p6 = ggplot(means_ts, aes(y=proportion_estimated_cases_detected, ymin=lower_pctle_detection_prop, 
		ymax=upper_pctle_detection_prop, x=date, color=intervention_label, fill=intervention_label)) + 
	geom_ribbon(alpha=.5) + 
	geom_line(size=1.25) + 
	scale_fill_manual(values=c('#33a02c', '#1f78b4')) + 
	scale_color_manual(values=c('#33a02c', '#1f78b4')) + 
	labs(title='Ratio of Detected Malaria Cases to Estimated Malaria Cases', 
		subtitle=paste('Comparing Health Zones with and without', intlab), 
		y='Ratio of Detected Cases to Estimated Cases (all ages)', x='Period', 
		color='', fill='', 
		caption='Lines and intervals show median, 20th and 80th percentiles of health zones') + 
	theme_bw()

# time series graph of the malaria case detection fraction of estimates excluding the control group
p6b = ggplot(means_ts[intervention==1], aes(y=proportion_estimated_cases_detected, ymin=lower_pctle_detection_prop, 
		ymax=upper_pctle_detection_prop, x=date)) + 
	geom_ribbon(alpha=.5, fill='#1f78b4') + 
	geom_line(size=1.25, color='#1f78b4') + 
	labs(title='Ratio of Detected Malaria Cases to Estimated Malaria Cases', 
		subtitle=paste('Health Zones with', intlab), 
		y='Ratio of Detected Cases to Estimated Cases (all ages)', x='Period', 
		color='', fill='', 
		caption='Lines and intervals show median, 20th and 80th percentiles of health zones') + 
	theme_bw()

# time series graph of the malaria data
p7 = ggplot(means_ts, aes(y=malariaDeaths_under5_rate, ymin=lower_pctle_malaria, 
		ymax=upper_pctle_malaria, x=date, color=intervention_label, fill=intervention_label)) + 
	geom_ribbon(alpha=.5) + 
	geom_line(size=1.25) + 
	scale_fill_manual(values=c('#33a02c', '#1f78b4')) + 
	scale_color_manual(values=c('#33a02c', '#1f78b4')) + 
	labs(title='Malaria Mortality Under 5', 
		subtitle=paste('Comparing Health Zones with and without', intlab), 
		y='Under-5 Malaria Mortality Rate (per 10,000 population)', x='Period', 
		color='', fill='', 
		caption='Lines and intervals show median, 20th and 80th percentiles of health zones') + 
	theme_bw()

# time series graph of the malaria data excluding the control group
p7b = ggplot(means_ts[intervention==1], aes(y=malariaDeaths_under5_rate, ymin=lower_pctle_malaria, 
		ymax=upper_pctle_malaria, x=date)) + 
	geom_ribbon(alpha=.5, fill='#1f78b4') + 
	geom_line(size=1.25, color='#1f78b4') + 
	labs(title='Malaria Mortality Under 5', 
		subtitle=paste('Health Zones with', intlab), 
		y='Under-5 Malaria Mortality Rate (per 10,000 population)', x='Period', 
		color='', fill='', 
		caption='Lines and intervals show median, 20th and 80th percentiles of health zones') + 
	theme_bw()

# time series graph of the all-cause data
p8 = ggplot(means_ts, aes(y=allDeaths_under5_rate, ymin=lower_pctle_all_cause, 
		ymax=upper_pctle_all_cause, x=date, color=intervention_label, fill=intervention_label)) + 
	geom_ribbon(alpha=.5) + 
	geom_line(size=1.25) + 
	scale_fill_manual(values=c('#33a02c', '#1f78b4')) + 
	scale_color_manual(values=c('#33a02c', '#1f78b4')) + 
	labs(title='All-Cause Mortality Under 5', 
		subtitle=paste('Comparing Health Zones with and without', intlab), 
		y='Under-5 All-Cause Mortality Rate (per 10,000 population)', x='Period', 
		color='', fill='', 
		caption='Lines and intervals show median, 20th and 80th percentiles of health zones') + 
	theme_bw()
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Graph DiD results

# traditional DiD graph with malaria model estimates
p9 = ggplot(means, aes(y=fit_coverage, ymin=lwr_coverage, ymax=upr_coverage, 
		x=period_label, color=intervention_label)) + 
	geom_pointrange(position=position_dodge(width=.2), size=1) + 
	geom_line(aes(group=intervention_label), position=position_dodge(width=.2), size=1) + 
	scale_color_manual(values=c('#33a02c', '#1f78b4')) + 
	labs(title='Difference-in-Difference Analysis of Health Zones with SSCs', subtitle='Antimalarial Treatment Coverage', 
		y='Under-5 Antimalarial Treatment Coverage', x='Period', color='Health Zones', 
		caption='Points and ranges show midpoint, upper and lower 95% confidence interval from model') + 
	theme_bw()

# traditional DiD graph with malaria model estimates
p10 = ggplot(means, aes(y=fit_malaria, ymin=lwr_malaria, ymax=upr_malaria, 
		x=period_label, color=intervention_label)) + 
	geom_pointrange(position=position_dodge(width=.2), size=1) + 
	geom_line(aes(group=intervention_label), position=position_dodge(width=.2), size=1) + 
	scale_color_manual(values=c('#33a02c', '#1f78b4')) + 
	labs(title='Difference-in-Difference Analysis of Health Zones with SSCs', subtitle='Malaria-Specific Mortality', 
		y='Under-5 Malaria Mortality Rate', x='Period', color='Health Zones', 
		caption='Points and ranges show midpoint, upper and lower 95% confidence interval from model') + 
	theme_bw()

# traditional DiD graph with all-cause model estimates
p11 = ggplot(means, aes(y=fit_all_cause, ymin=lwr_all_cause, ymax=upr_all_cause, 
		x=period_label, color=intervention_label)) + 
	geom_pointrange(position=position_dodge(width=.2), size=1) + 
	geom_line(aes(group=intervention_label), position=position_dodge(width=.2), size=1) + 
	scale_color_manual(values=c('#33a02c', '#1f78b4')) + 
	labs(title='Difference-in-Difference Analysis of Health Zones with SSCs', subtitle='All-Cause Mortality', 
		y='Under-5 All-Cause Mortality Rate', x='Period', color='Health Zones', 
		caption='Points and ranges show midpoint, upper and lower 95% confidence interval from model') + 
	theme_bw()
# ------------------------------------------------------------------------------


# -----------------------------------
# Save results

# save graphs
pdf(graphFile, height=5.5, width=8)
p1
p2
p3
p4
p4b
p5
p5b
p6
p6b
p7
p7b
p8
p9
p10
p11
dev.off()

# save results
dropObjs = c('inFile', 'outFile', 'graphFile')
save(list=ls()[!ls() %in% dropObjs], file=outFile)
# -----------------------------------
