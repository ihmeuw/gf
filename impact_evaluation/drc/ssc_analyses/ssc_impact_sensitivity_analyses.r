# ------------------------------------------------------------------------------
# David Phillips
# 
# 7/22/2019
# Sensitivity/secondary analyses based on ssc_impact.r
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
inFile = paste0(dir, '/impact_evaluation/cod/prepped_data/ssc_analyses/DiD_results.rdata')

# output files
graphFile = paste0(dir, '/impact_evaluation/cod/visualizations/ssc_analyses/DiD_sensitivity_analysis.pdf')
# ---------------------------------------------------------------------------------------


# ------------------------------------
# Load/prep data

# load data produced by ssc_impact.r
load(inFile)
# ------------------------------------


# ------------------------------------------------------------------------------
# Run alternative analyses

# offset log transform
offset = quantile(data[malariaDeaths_under5_rate>0]$malariaDeaths_under5_rate, .01)
data[, log_malariaDeaths_under5_rate:=log(malariaDeaths_under5_rate+offset)]

# difference in differences with OLS on log-mortality rate
lmFitLog = lm(log_malariaDeaths_under5_rate~intervention*period, data)
summary(lmFitLog)

# difference in differences with Poisson regression on mortality rate
glmFit = glm(malariaDeaths_under5_rate~intervention*period, data, family='poisson')
summary(glmFit)

# difference in differences with health zone random effect for correlated errors
lmeFit = lmer(log_malariaDeaths_under5_rate~intervention*period + (1|health_zone), data)
summary(lmeFit)

# predict from poisson
preds = predict(glmFit, se.fit=TRUE, newdata=means)
means[, fit_poisson:=exp(preds$fit)]
means[, lwr_poisson:=exp(preds$fit - 1.96*preds$se.fit)]
means[, upr_poisson:=exp(preds$fit + 1.96*preds$se.fit)]

# predict from log transformed ols
preds = predict(lmFitLog, se.fit=TRUE, newdata=means)
means[, fit_log:=exp(preds$fit)]
means[, lwr_log:=exp(preds$fit - 1.96*preds$se.fit)]
means[, upr_log:=exp(preds$fit + 1.96*preds$se.fit)]

# predict from log transformed ols
preds = predict(lmeFit, se.fit=TRUE, newdata=means, re.form=NA)
bootStraps = data.table(bootMer(lmeFit, nsim=100, FUN=function(x)predict(x, newdata=means, re.form=NA))$t)
lower = bootStraps[, lapply(.SD, quantile, .025)]
upper = bootStraps[, lapply(.SD, quantile, .975)]
means[, fit_lme:=exp(as.numeric(preds))]
means[, lwr_lme:=exp(as.numeric(lower))]
means[, upr_lme:=exp(as.numeric(upper))]
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Graph DiD results

# melt models long
means = melt(means, id.vars=c('period_label', 'intervention_label', 'period', 'intervention', 'malariaDeaths_under5_rate', 'allDeaths_under5_rate'))
means = means[!grepl('all_cause|pctle',variable)]
means[grepl('fit',variable), est:='fit']
means[grepl('lwr|lower',variable), est:='lwr']
means[grepl('upr|upper',variable), est:='upr']
means[grepl('malaria',variable), model:='Linear Regression']
means[grepl('lme',variable), model:='Log-Linear Mixed Effects']
means[grepl('log',variable), model:='Log-Linear Regression']
means[grepl('poisson',variable), model:='Poisson']
means = dcast.data.table(means, period_label+intervention_label+period+intervention+malariaDeaths_under5_rate+allDeaths_under5_rate+model~est)

# traditional DiD graph for each type of model
ggplot(means, aes(y=fit, ymin=lwr, ymax=upr, x=period_label, color=intervention_label)) + 
	geom_pointrange(position=position_dodge(width=.2), size=1) + 
	geom_line(aes(group=intervention_label), position=position_dodge(width=.2), size=1) + 
	scale_color_manual(values=c('#33a02c', '#1f78b4')) + 
	facet_wrap(~model) + 
	labs(y='Under-5 Malaria Mortality Rate', x='Period', color='Health Zones', 
		caption='Points and ranges show midpoint, upper and lower 95% confidence interval from model') + 
	theme_bw()
# ------------------------------------------------------------------------------
