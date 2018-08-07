# -----------------------------------------------------------------------------
# David Phillips
# 
# 7/19/2018
# Figure showing the overall landscape of TB funding in GTM from SICOIN and FGH
# -----------------------------------------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(RColorBrewer)
library(ggplot2)
# --------------------


# ------------------------------------------------------------------------
# Files and directories

# input directory
dir = 'J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/'

# input file
inFile = paste0(dir, 'total_resource_tracking_data.csv')

# output file

# ------------------------------------------------------------------------


# -----------------------------------------------------------------------------------------------------------
# Load/prep data

# load
data = fread(inFile)

# subset to Guatemala
data = data[country=='Guatemala']

# subset to TB and total health expenditure
data = data[disease %in% c('tb','all')]

# reshape variables (exp/dis/budg) long
varLong = melt(data, measure.vars=c('budget','disbursement','expenditure'))
varLong = varLong[, .(value=sum(value)), by=c('financing_source','data_source','year','disease','variable')]
varLong[, financing_source:=gsub('_forecasted', '', financing_source)]

# quick graph just to see it all
# ggplot(varLong[disease%in%c('tb','all')], aes(y=value, x=year, color=variable,shape=disease)) + 
# 	geom_point() + geom_line() + facet_grid(financing_source~data_source, scales='free')
# -----------------------------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------- 
# Set up to graph

# subset data down to just TB budgets
graphData = varLong[variable=='budget' & disease=='tb']

# subset data down to just ghe/dah from SICOIN and GF from detailed budgets
graphData = graphData[!(data_source=='sicoin' & financing_source=='gf')]
graphData = graphData[!data_source %in% c('gos','pudr')]

# clean up labels
graphData[financing_source=='donacions', financing_source:='All External Donors']
graphData[financing_source=='gf', financing_source:='Global Fund']
graphData[financing_source=='ghe', financing_source:='Government']
graphData[data_source=='fpm', data_source:='Global Fund Detailed Budgets']
graphData[data_source=='sicoin', data_source:='National Accounting System (SICOIN)']
# ----------------------------------------------------------------------------------


# -----------------------------------------------------------------------------------
# Graph

ggplot(graphData, aes(y=value/1000000, x=year, shape=data_source, color=financing_source)) + 
	geom_line(size=1) + 
	geom_point(size=2.5) + 
	labs(title='Landscape of TB Funding', y='Budget (In Millions of USD)', 
		shape='Data Source', color='Financing Source') + 
	theme_bw()

# -----------------------------------------------------------------------------------
