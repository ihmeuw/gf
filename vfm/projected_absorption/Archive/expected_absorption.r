# ------------------------------------------------------
# David Phillips
#
# 10/5/2018
# Predict expected (aka projected) absorption based on past trends
# Loads a file created by ./vfm/projected_absorption/prep_expected_absorption.r
# ------------------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(boot)
library(data.table)
library(ggplot2)
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
outFile = paste0(dir, '../../vfm/visualizations/expected_absorption.pdf')

# output table
tableFile = paste0(dir, '../../vfm/visualizations/RT_synthesis_summary_table.csv')
# -----------------------------------------------------------------


# ----------------------------------------------------------------------
# Load/prep data

# load
allData = fread(inFile)

# subset to budgets
data = allData[data_source=='fpm']

# subset to pudrs
pudrs = allData[data_source=='pudr']

# subset to upcoming budgets
data[, maxyear:=max(year), by=grant_number]
data = data[maxyear>2016]

# keep only matching pudrs
pudrs = pudrs[grant_number %in% unique(data$grant_number)]
pudrs = pudrs[abbrev_module!='Unspecified']

# collapse to intervention-quarter level
data[, quarter:=quarter(start_date)]
pudrs[, quarter:=quarter(start_date)]
byVars = c('disease','country','grant_number','year','quarter','abbrev_module')
data=data[, .('budget'=sum(budget,na.rm=TRUE)), by=byVars]
pudrs=pudrs[, .('budget'=sum(budget,na.rm=TRUE), 
				'expenditure'=sum(expenditure,na.rm=TRUE)), by=byVars]

# use pudrs wherever we have them
data = merge(data, pudrs, by=byVars, all=TRUE, suffixes=c('_fpm','_pudr'))
data[, budget:=budget_fpm]
data[!is.na(budget_pudr), budget:=budget_pudr]
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
# Generate extra predictor variables

# year within grant and years from end of grant
data[, grant_year:=as.numeric(as.factor(year)), by='grant_number']
data[, years_from_end:=max(grant_year)-grant_year+1, by='grant_number']
data[, yearid:=as.numeric(as.factor(year)), by='grant_number']
data[, quarterid:=(((yearid-1)*4))+quarter]
data[, quarters_from_end:=max(quarterid)-quarterid+1, by='grant_number']

# total budget of the grant
data[, total_budget:=sum(budget,na.rm=TRUE), by='grant_number']

# number of modules within grant
data[, num_modules:=length(unique(abbrev_module)), by='grant_number']
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
# Load/prep regression output

# load regression output
load(regInFile)

# identify modules that weren't in the model data (Care & prevention is the intercept)
origModules = names(coef(lmFit2))
origModules = gsub('abbrev_module', '', origModules)
missing = unique(data$abbrev_module)
missing = missing[!missing %in% origModules]

# use substitutes for missing modules
data[, true_module:=abbrev_module]
data[abbrev_module=='Prevention programs for IJU', 
	abbrev_module:='Prevention programs for CSW & clients']
data[abbrev_module=='Prevention programs for prisoners', 
	abbrev_module:='Prevention programs for CSW & clients']
data[abbrev_module=='Performance Based Financing', 
	abbrev_module:='Nat. health strategies']
# ----------------------------------------------------------------------


# ----------------------------------------------------------
# Predict from regression

# predict absorption
data[, expected_absorption:=predict(lmFit2, newdata=data)]

# reverse lemon-squeeze 
# (to reverse, we need the same N that was used in the original regression)
reverseLemonSqueeze = function(x,N) { 
	N = length(x[!is.na(x)])
	return(((inv.logit(x)*N)-0.5)/(N-1))
}
data[, expected_absorption:=reverseLemonSqueeze(expected_absorption,N)]

# compute actual absorption
data[, observed_absorption:=expenditure/budget]

# compute expenditure
data[, expected_expenditure:=budget*expected_absorption]

# set substitutes back to their true module
data[, abbrev_module:=true_module]
# ----------------------------------------------------------


# -------------------------------------------------------------------------
# Aggregate to grant level

# aggregate
byVars = c('country','grant_number','year','quarter')
data[, has_pudr:=any(!is.na(expenditure)), by=byVars]
agg = data[, .(budget=sum(budget,na.rm=TRUE), 
				expenditure=sum(expenditure, na.rm=TRUE), has_pudr=max(has_pudr),
				expected_expenditure=sum(expected_expenditure,na.rm=TRUE)), 
				by=byVars]

# keep track of where pudrs are missing
agg[has_pudr==0, expenditure:=NA]
				
# recompute expected and observed absorption
agg[, observed_absorption:=expenditure/budget]
agg[, expected_absorption:=expected_expenditure/budget]
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------------
# Set up to graph

# color palette
cols = c('#008080','#70a494','#b4c8a8','#f6edbd','#edbb8a','#de8a5a','#ca562c')

# format dates
agg[, date:=as.Date(paste0(year, '-', quarter*3, '-01', '%Y-%m-%d'))]

# melt
melt = melt(agg, id.vars=c('country','grant_number','year','quarter','date','has_pudr'))
melt[variable=='expected_absorption', label:='Expected']
melt[variable=='observed_absorption', label:='Observed']
melt[variable=='budget', label:='Budget']
melt[variable=='expenditure', label:='Expenditure']

# country subsets
ugaSubset = melt[country=='Uganda' & year>=2018 & has_pudr==1]
codSubset = melt[country=='Congo (Democratic Republic)' & year>=2018 & has_pudr==1]
gtmSubset = melt[country=='Guatemala' & year>=2017 & has_pudr==1]
# -------------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------------
# Graphs

# store plots in list
plots=list()
i=0

# UGANDA
# bar graph of execution
i=i+1
f = 1000000
plots[[i]] = ggplot(ugaSubset[variable %in% c('budget','expenditure')], 
		aes(y=value/f, x=grant_number, fill=label)) + 
	geom_bar(stat='identity', position='identity') + 
	geom_hline(data=ugaSubset[variable=='expected_expenditure'], aes(yintercept=value/f, 
		color='Projected\nExpenditure*'), linetype='dashed', size=1.25) + 
	facet_wrap(~grant_number, scales='free', ncol=5) + 
	scale_fill_manual(values=cols[c(1,6)]) + 
	scale_color_manual(values=c('Projected\nExpenditure*'='grey25')) + 
	labs(title='Budget Compared to Expenditure', subtitle='Uganda January 2018 - June 2018', 
		y='Execution (in Millions of USD)', x='', fill='', color='', 
		caption='*Projected expenditure adjusted for mix of interventions, grant phase and size') + 
	theme_bw(base_size=16) + 
	theme(strip.background=element_blank(), strip.text.x=element_blank()) + 
	guides(color = guide_legend(override.aes = list(size=.75)))

# bar graph of % absorption
i=i+1
plots[[i]] = ggplot(ugaSubset[grepl('absorption', variable)], aes(y=value*100, x=grant_number, fill=label)) + 
	geom_bar(stat='identity', position='dodge') + 
	scale_fill_manual(values=cols[c(1,6)]) + 
	labs(title='Absorption Compared to Historical Data', subtitle='Uganda January 2018 - June 2018', 
		y='Percent Absorption', x='', fill='', 
		caption='Historical expenditure adjusted for mix of interventions, grant phase and size') + 
	theme_bw(base_size=16)

# graph time series
i=i+1
plots[[i]] = ggplot(agg[country=='Uganda' & year>=2018], aes(y=expected_absorption, x=date)) + 
	geom_hline(yintercept=1, linetype='dashed', color='grey40') + 
	geom_point() + 
	geom_line() + 
	geom_point(aes(y=observed_absorption), color='red') + 
	facet_wrap(~grant_number) + 
	theme_bw()
# -------------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------------
# DRC
# bar graph of execution
i=i+1
f = 1000000
plots[[i]] = ggplot(codSubset[variable %in% c('budget','expenditure')], 
		aes(y=value/f, x=grant_number, fill=label)) + 
	geom_bar(stat='identity', position='identity') + 
	geom_hline(data=codSubset[variable=='expected_expenditure'], aes(yintercept=value/f, 
		color='Historical\nExpenditure*'), linetype='dashed', size=1.25) + 
	facet_wrap(~grant_number, scales='free', ncol=5) + 
	scale_fill_manual(values=cols[c(1,6)]) + 
	scale_color_manual(values=c('Historical\nExpenditure*'='grey25')) + 
	labs(title='Budget Compared to Expenditure', subtitle='DRC January 2018 - June 2018', 
		y='Execution (in Millions of USD)', x='', fill='', color='', 
		caption='*Historical expenditure adjusted for mix of interventions, grant phase and size') + 
	theme_bw(base_size=16) + 
	theme(strip.background=element_blank(), strip.text.x=element_blank()) + 
	guides(color = guide_legend(override.aes = list(size=.75)))

# bar graph of % absorption
i=i+1
plots[[i]] = ggplot(codSubset[grepl('absorption', variable)], aes(y=value*100, x=grant_number, fill=label)) + 
	geom_bar(stat='identity', position='dodge') + 
	scale_fill_manual(values=cols[c(1,6)]) + 
	labs(title='Absorption Compared to Historical Data', subtitle='DRC January 2018 - June 2018', 
		y='Percent Absorption', x='', fill='', 
		caption='Historical expenditure adjusted for mix of interventions, grant phase and size') + 
	theme_bw(base_size=16)

# graph time series
i=i+1
plots[[i]] = ggplot(agg[country=='DRC' & year>=2018], aes(y=expected_absorption, x=date)) + 
	geom_hline(yintercept=1, linetype='dashed', color='grey40') + 
	geom_point() + 
	geom_line() + 
	geom_point(aes(y=observed_absorption), color='red') + 
	facet_wrap(~grant_number) + 
	theme_bw()
# -------------------------------------------------------------------------------------------------


# -----------------------------
# Save graphs
pdf(outFile, height=5.5, width=10)
for(i in seq(length(plots))) print(plots[[i]])
dev.off()
# -----------------------------
