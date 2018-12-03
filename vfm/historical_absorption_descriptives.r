# ------------------------------------------------------
# David Phillips
#
# 10/5/2018
# Compare historical data on absorption to new data module-by-module
# This is intended to be the simplification of expected_absorption.r
# ------------------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(gridExtra)
# ------------------


# -----------------------------------------------------------------
# Files and directories

# root directory for input/output
dir = 'J:/Project/Evaluation/GF/resource_tracking/multi_country/'

# input data
inFile = paste0(dir, 'mapping/total_resource_tracking_data.csv')

# place to store the regression output
regInFile = paste0(dir, '../../vfm/outputs/absorption_correlates_model_fit.rdata')

# output graphs
outFile = paste0(dir, '../../vfm/visualizations/average_absorption.pdf')
# -----------------------------------------------------------------


# ----------------------------------------------------------------------
# Load/prep data

# load
allData = fread(inFile)

# subset to GOS and PUDRs
data = allData[data_source %in% c('gos','pudr')]

# aggregate the PUDRs by grant but GOS overall
data[data_source=='gos', grant_number:='all']
data[data_source=='gos', grant_period:='all']

# identify RSSH modules
data[, rssh:=grepl('R',code)]

# compute absorption by module
byVars = c('country','grant_number','grant_period','abbrev_module','rssh','data_source')
agg = data[, .(expenditure=sum(expenditure, na.rm=T), budget=sum(budget,na.rm=T)), by=byVars]
agg[, absorption:=expenditure/budget]

# delete erroneous module from Uganda
# agg = agg[!(grant_number=='UGA-M-MoFPED' & abbrev_module=='HR & health workers')]
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
# Set up to graph

# subset to the grant-windows that we want to display
graphData = agg[(data_source=='gos' & country=='Uganda') | 
				(grant_period=='2018-2020' & country=='Uganda')]

# reshape data sources wide
keepVars = c('country','abbrev_module','rssh','budget','expenditure','absorption')
gos = graphData[data_source=='gos', keepVars, with=FALSE]
graphData = graphData[data_source!='gos']
graphData$data_source = NULL
vars = c('budget','expenditure','absorption')
setnames(gos, vars, paste0(vars,'_gos'))
setnames(graphData, vars, paste0(vars,'_pudr'))
graphData = merge(graphData, gos, by=c('country','abbrev_module','rssh'))

# make expected expenditure
graphData[, expected_expenditure:=absorption_gos*budget_pudr]

# melt metrics long
idVars = c('country','abbrev_module','rssh','grant_number','grant_period')
graphData = melt(graphData, id.vars=idVars)

# make numeric abbrev_module for line segments
graphData[, module_id:=as.numeric(as.factor(abbrev_module)), by=c('grant_number','rssh')]

# graph labels
graphData[variable=='budget_pudr', label:='Budget']
graphData[variable=='expenditure_pudr', label:='Expenditure']

# other settings
f=1000000
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
# Graph

# module-level bar charts of absorption
p1 = ggplot(graphData[variable=='absorption_pudr'], aes(y=value*100, x=abbrev_module)) + 
	geom_bar(stat='identity', position='stack', fill='#1c9099') + 
	geom_segment(data=graphData[variable=='absorption_gos'], aes(y=value*100, yend=value*100, 
		x=module_id-.5, xend=module_id+.5, color='Average\nAbsorption*'), 
		linetype='dashed', size=1.25) + 
	facet_wrap(~grant_number, scales='free', ncol=5) + 
	scale_color_manual(values=c('Average\nAbsorption*'='grey25')) + 
	labs(title='Absorption Compared to Historical Data', subtitle='Uganda January 2018 - June 2018', 
		y='% Absorption', x='', fill='', color='', 
		caption='*Average based on absorption rate from all previous grants with this module in Uganda') + 
	theme_bw() + 
	theme(axis.text.x = element_text(angle=45, hjust=1))

# module-level bar charts of execution
p2 = ggplot(data=NULL, aes(y=value/f, x=abbrev_module, fill=label)) + 
	geom_bar(data=graphData[variable=='budget_pudr'], stat='identity', position='identity') + 
	geom_bar(data=graphData[variable=='expenditure_pudr'], stat='identity', position='identity') + 
	geom_segment(data=graphData[variable=='expected_expenditure'], aes(y=value/f, yend=value/f, 
		x=module_id-.5, xend=module_id+.5, color='Average\nExpenditure*'), 
		linetype='dashed', size=1.25) + 
	facet_wrap(~grant_number, scales='free', ncol=5) + 
	scale_color_manual(values=c('Average\nExpenditure*'='grey25')) + 
	scale_fill_manual(values=c('#3182bd','#31a354')) + 
	labs(title='Budget Compared to Expenditure', subtitle='Uganda January 2018 - June 2018', 
		y='Execution (in Millions)', x='', fill='', color='', 
		caption='*Average based on absorption rate from all previous grants with this module in Uganda') + 
	theme_bw() + 
	theme(axis.text.x = element_text(angle=45, hjust=1))

# module-level bar charts of execution for RSSH modules only
p3 = ggplot(data=NULL, aes(y=value/f, x=abbrev_module, fill=label)) + 
	geom_bar(data=graphData[variable=='budget_pudr' & rssh==1], stat='identity', position='identity') + 
	geom_bar(data=graphData[variable=='expenditure_pudr' & rssh==1], stat='identity', position='identity') + 
	geom_segment(data=graphData[variable=='expected_expenditure' & rssh==1], aes(y=value/f, yend=value/f, 
		x=module_id-.5, xend=module_id+.5, color='Average\nExpenditure*'), 
		linetype='dashed', size=1.25) + 
	facet_grid(.~grant_number, scales='free', space='free_x') + 
	scale_color_manual(values=c('Average\nExpenditure*'='grey25')) + 
	scale_fill_manual(values=c('#3182bd','#31a354')) + 
	labs(title='Budget Compared to Expenditure - RSSH Modules', subtitle='Uganda January 2018 - June 2018', 
		y='Execution (in Millions)', x='', fill='', color='', 
		caption='*Average based on absorption rate from all previous grants with this module in Uganda') + 
	theme_bw() + 
	theme(axis.text.x = element_text(angle=45, hjust=1))
# ----------------------------------------------------------------------


# ---------------------------------
# Save
pdf(outFile, height=5.5, width=10)
p1
p2
p3
dev.off()
# ---------------------------------
