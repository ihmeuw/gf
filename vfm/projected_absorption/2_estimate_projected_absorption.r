# ------------------------------------------------------
# David Phillips
#
# 11/2/2019
# Estimate projected absorption based on historical data
# ------------------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(GGally)
# ------------------


# -----------------------------------------------------------------
# Files and directories

# root directory for input/output
dir = 'J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/combined_prepped_data'

# input data
inFile = paste0(dir, '/projected_absorption_input.rds')

# output file
outFile = paste0(dir, '/projected_absorption_estimates.csv')
# -----------------------------------------------------------------


# --------------------
# Load/prep data
data = readRDS(inFile)
# --------------------


# ----------------------------------------------------------------------------
# Estimate projected absorption

# fit regression
# PAFit = lm(absorption ~ gf_module*days_until_end + budget + num_modules, data)
PAFit = lm(absorption ~ gf_module*days_until_end, data)

# define reverse lemon squeeze function
reverseLemonSqueeze = function(x) { 
	N = length(x[!is.na(x)])
	return(((inv.logit(x)*N)-0.5)/(N-1))
}

# extract results
modules = unique(data$gf_module)
days = seq(0, 365*3, by=10)
prediction_frame = data.table(expand.grid(modules, days))
setnames(prediction_frame, c('gf_module', 'days_until_end'))
prediction_frame[, projected_absorption:=predict(PAFit, newdata=prediction_frame)]
prediction_frame[, projected_absorption:=reverseLemonSqueeze(projected_absorption)]
p05 = quantile(prediction_frame$projected_absorption, .05)
p95 = quantile(prediction_frame$projected_absorption, .95)
prediction_frame[projected_absorption<p05, projected_absorption:=p05]
prediction_frame[projected_absorption>p95, projected_absorption:=p95]
# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------
# Display data

# pairwise plot of continuous explanatory variables
numVars = c('absorption','days_until_end','budget','num_modules')
p1 = ggpairs(data[, numVars, with=FALSE])

# graph of categorical explanatory variables
p2 = ggplot(data, aes(y=absorption, x=gf_module)) + 
	geom_jitter(height=0, width=.1) + 
	theme_bw() + 
	theme(axis.text.x=element_text(angle=315, hjust=0))
	
# confirm that projected absorption has appropriate patterns
p3 = ggplot(prediction_frame, aes(y=projected_absorption, x=days_until_end)) + 
	geom_point() + 
	geom_smooth()
	
# graph absorption over the life of a grant
data[, disease:=toupper(disease)]
data[disease=='MALARIA', disease:='Malaria']
ggplot(data[raw_absorption<5], aes(y=raw_absorption*100, x=days_since_start)) + 
	geom_jitter(height=0, width=50) + 
	geom_hline(yintercept=100, color='black') + 
	geom_smooth() + 
	facet_wrap(~disease, scales='free') + 
	labs(title='', y='Absorption (%)', x='Days Since Start of Grant', 
		caption='Data Source: All available PUDRs from DRC, Guatemala, Senegal and Uganda, 2013-2019\n
				Values greater than 500% absorption not displayed\n
				Data points jittered to avoid overlap') +
	theme_bw()
# ----------------------------------------------------------------------


# --------------------------------------------------
# Save regression fit
write.csv(prediction_frame, outFile, rownames=FALSE)
# --------------------------------------------------

