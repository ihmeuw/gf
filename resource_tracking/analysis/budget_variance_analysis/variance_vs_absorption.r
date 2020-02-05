# -------------------------------------------------------------------
# David Phillips
# 
# 1/2/2019
# Check if PRs gradually shift money away from low-absorbing modules
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Set up R
rm(list=ls())
library(data.table)
library(viridis)
library(ggplot2)
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Files and directories

# input file
dir = 'J:/Project/Evaluation/GF/resource_tracking'
inFile =  paste0(dir, '/_gf_files_gos/combined_prepped_data/budget_exp_wide.rds')

# output file
outDir = paste0(dir, '/visualizations/budget_variance_analysis/')
graphFile = paste0(outDir, 'variance_vs_absorption.pdf')
outFile = paste0(outDir, 'module_budget_variance.csv')
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Load/prep data

# load
data = readRDS(inFile)

# melt semesters long
idVars = c('loc_name','grant','grant_period','disease','gf_module')
data = melt(data, id.vars=idVars, 
	measure=patterns('budget_S*', 'original_budget_S*', 'expenditure_S*'), 
	variable.name='semester')
setnames(data, paste0('value',seq(3)), c('budget', 'original_budget', 'expenditure'))

# generate new variables
data[, variance:=budget-original_budget]
data[, variance_normalized:=(budget-original_budget)/original_budget]
data[, absorption:=expenditure/budget]

# save here
write.csv(data[, -'variance_normalized', with=FALSE], outFile)

# transform data
data[, absorption_transformed:=absorption]
data[, variance_transformed:=variance]
data[, variance_normalized_transformed:=variance_normalized]
data[absorption>9 | absorption<0, absorption_transformed:=NA]
data[variance>3e6 | variance < -4e6, variance_transformed:=NA]

# lags
data[, lag_absorption:=shift(absorption), by=idVars]
data[, lag_absorption_transformed:=shift(absorption_transformed), by=idVars]
data[, lag_variance:=shift(variance), by=idVars]
data[, lag_variance_transformed:=shift(variance_transformed), by=idVars]
data[, lag_variance_normalized_transformed:=shift(variance_normalized_transformed), by=idVars]

# absolute values
data[, lag_variance_transformed_abs:=abs(lag_variance_transformed)]
data[, lag_variance_normalized_transformed_abs:=abs(lag_variance_normalized_transformed)]
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Analysis

# correlate S3 variance with S2 absorption
# lm(variance_S3_transformed ~ absorption_S2_transformed)
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Graph

# are they moving moving away from low-absorbing modules?
graphData = na.omit(data[,c('variance_normalized_transformed','lag_absorption_transformed')])
p1 = ggplot(graphData, aes(y=variance_normalized_transformed, x=lag_absorption_transformed*100)) + 
	geom_point() +
	geom_smooth(method='lm') + 
	labs(title='Do PRs Tend to Reallocate Resources Away from Low-Absorbing Modules?', 
		y='Budget Variance as a Proportion of Original Budget\n(Semesters 2-3)', 
		x='Absorption Percentage\n(Semester 1-2)', 
		caption='Values >900% and <0% absorption not displayed\n
				Points represent grant-module-semesters.\n
				Semester-1 absorption is compared to semester-2 variance etc.') + 
	theme_bw()

# does variance (in either direction) help them absorb later?
graphData = na.omit(data[,c('lag_variance_transformed_abs','absorption_transformed')])
p2 = ggplot(graphData, aes(y=absorption_transformed*100, x=lag_variance_transformed_abs)) + 
	geom_point() +
	geom_smooth(method='lm') + 
	labs(title='Does Budget Variance Correspond with Higher Absorption in the Next Semester?', 
		x='Budget Variance (absolute value in dollars)\n(Semesters 1-2)', 
		y='Absorption Percentage\n(Semester 2-3)', 
		caption='Values >900% and <0% absorption not displayed\n
				Points represent grant-module-semesters.\n
				Semester-1 variance is compared to semester-2 absorption etc.') + 
	theme_bw()

# does variance (in either direction normalized by budget size) help them absorb later?
graphData = na.omit(data[,c('lag_variance_normalized_transformed_abs','absorption_transformed')])
p3 = ggplot(graphData, aes(y=absorption_transformed*100, x=lag_variance_normalized_transformed_abs)) + 
	geom_point() +
	geom_smooth(method='lm') + 
	labs(title='Does Budget Variance Correspond with Higher Absorption in the Next Semester?', 
		x='Budget Variance as a Proportion of Original Budget\n(Semesters 1-2)', 
		y='Absorption Percentage\n(Semester 2-3)', 
		caption='Values >900% and <0% absorption not displayed\n
				Points represent grant-module-semesters.\n
				Semester-1 variance is compared to semester-2 absorption etc.') + 
	theme_bw()
	
pdf(outFile, height=5.5, width=8)
p1
p2
p3
dev.off()
# -------------------------------------------------------------------

