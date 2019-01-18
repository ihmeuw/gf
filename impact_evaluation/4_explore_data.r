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
dir = 'J:/Project/Evaluation/GF/impact_evaluation/cod/visualizations'
inFile = paste0(dir, '/pilot_data.RDS')

# output files
graphFile = paste0(dir, 'pilot_data_pre_model.rds')
# --------------------------------------------------------------------


# ----------------------------------------------
# Load/prep data

# load
data = readRDS(inFile)

# test unique identifiers
idVars = c('year','quarter','module','intervention','indicator')
test = nrow(data)==nrow(unique(data[,idVars, with=F]))
if (test==FALSE) stop(paste('Something is wrong.', paste(idVars, collapse=' '), 'do not uniquely identify rows.'))

# set aside results chain sections
inputs = unique(data[, c('year','quarter','intervention','budget')])
activities = unique(data[indicator_type=='activity', c('year','quarter','indicator','value', 'completeness')])
outputs = unique(data[indicator_type=='output', c('year','quarter','indicator','value', 'completeness')])

# ----------------------------------------------


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
p5 = ggplot(data, aes(y=value, x=budget)) + 
	geom_point() + 
	facet_wrap(indicator_type~intervention, scales='free') + 
	geom_smooth(method='lm', se=FALSE) + 
	labs(y='Activity or Output Value', x='Budget') + 
	theme_bw(base_size=16)

# scatterplot of lag-correlations
# p6 = ggplot(data, aes(y=value, x=budget)) + 
	# geom_point() + 
	# facet_wrap(indicator_type~intervention, scales='free') + 
	# geom_smooth(method='lm', se=FALSE) + 
	# labs(y='Activity or Output Value', x='Budget') + 
	# theme_bw(base_size=16)
# ----------------------------------------------


# --------------------------------
# Save file
pdf(graphFile, height=5.5, width=9)
p1
p2
p3
p4
p5
p6
dev.off()
# --------------------------------
