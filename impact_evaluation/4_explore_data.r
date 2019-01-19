# ------------------------------------------------
# David Phillips
# 
# 1/18/2019
# Exploratory visualizations to get a good sense 
# of the impact evaluation data
# ------------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
# --------------------


# --------------------------------------------------------------------
# Files and directories

# input file
dir = 'J:/Project/Evaluation/GF/impact_evaluation/cod/'
inFile = paste0(dir, 'prepped_data/pilot_data.RDS')

# output files
graphFile = paste0(dir, 'visualizations/pilot_data_exploratory_graphs.pdf')
# --------------------------------------------------------------------


# ----------------------------------------------------------------------------
# Load/prep data

# load
data = readRDS(inFile)

# test unique identifiers
idVars = c('year','quarter','code','module','intervention','indicator','indicator_type')
test = nrow(data)==nrow(unique(data[,idVars, with=F]))
if (test==FALSE) stop(paste('Something is wrong.', paste(idVars, collapse=' '), 'do not uniquely identify rows.'))

# prep work that shouldn't be necessary once bugs are fixed
data = data[!is.na(value)]
data = data[, .(budget=sum(budget,na.rm=T), 
				value=sum(value,na.rm=T), 
				completeness=mean(completeness,na.rm=T)), by=idVars]

# compute lags of budget
lagVars = c('budget_lag1','budget_lag4')
data[, (lagVars):=shift(budget, c(1,4), type='lag'), by='code']

# compute cumulative budget
data[, budget_cumulative:=cumsum(budget), by='code']

# set aside results chain sections
inputs = unique(data[, c('year','quarter','code','intervention','budget','budget_lag1','budget_lag4','budget_cumulative')])
activities = unique(data[indicator_type=='activity', c('year','quarter','indicator','value', 'completeness')])
outputs = unique(data[indicator_type=='output', c('year','quarter','indicator','value', 'completeness')])

# assemble wide-format data
wideInputs = copy(inputs)

# ----------------------------------------------------------------------------


# ----------------------------------------------
# Set up to graph

# ----------------------------------------------


# ----------------------------------------------
# Make graphs

# time series of inputs
p1 = ggplot(inputs, aes(y=budget, x=year+(quarter/4), color=intervention)) + 
	geom_line() + 
	geom_point() + 
	labs(y='Budget', x='Quarter', color='Intervention') + 
	theme_bw(base_size=16)

# time series of activities
p2 = ggplot(activities, aes(y=value, x=year+(quarter/4), color=indicator)) + 
	geom_line() + 
	geom_point() + 
	labs(y='Budget', x='Quarter', color='Activity') + 
	theme_bw(base_size=16)

# time series of outputs
p3 = ggplot(outputs, aes(y=value, x=year+(quarter/4), color=indicator)) + 
	geom_line() + 
	geom_point() + 
	labs(y='Budget', x='Quarter', color='Output') + 
	theme_bw(base_size=16)

# histograms of distributions
p4 = ggplot(inputs, aes(x=budget)) + 
	geom_histogram() + 
	facet_wrap(~intervention, scales='free') + 
	labs(y='Frequency (Quarters)', x='Budget') + 
	theme_bw(base_size=16)

# scatterplot of correlations
p5a = ggplot(data[indicator_type=='activity'], aes(y=value, x=budget)) + 
	geom_point() + 
	facet_wrap(~intervention, scales='free') + 
	geom_smooth(method='lm', se=FALSE) + 
	labs(y='Activity Value', x='Budget') + 
	theme_bw(base_size=16)
p5o = ggplot(data[indicator_type=='outputs'], aes(y=value, x=budget)) + 
	geom_point() + 
	facet_wrap(~intervention, scales='free') + 
	geom_smooth(method='lm', se=FALSE) + 
	labs(y='Output Value', x='Budget') + 
	theme_bw(base_size=16)

# scatterplot of lag-correlations
p6a1 = ggplot(data[indicator_type=='activity'], aes(y=value, x=budget_lag1)) + 
	geom_point() + 
	facet_wrap(~intervention, scales='free') + 
	geom_smooth(method='lm', se=FALSE) + 
	labs(y='Activity Value', x='Budget (1-Quarter Lag)') + 
	theme_bw(base_size=16)
p6a4 = ggplot(data[indicator_type=='activity'], aes(y=value, x=budget_lag4)) + 
	geom_point() + 
	facet_wrap(~intervention, scales='free') + 
	geom_smooth(method='lm', se=FALSE) + 
	labs(y='Activity Value', x='Budget (1-Year Lag)') + 
	theme_bw(base_size=16)
# ----------------------------------------------


# --------------------------------
# Save file
pdf(graphFile, height=5.5, width=9)
p1
p2
p3
p4
p5a
p5o
p6a1
p6a4
dev.off()
# --------------------------------
