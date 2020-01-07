# ------------------------------------------------------------------------------------------------------------
# David Phillips
#
# 12/5/2019
# "Budget variance analysis", 
# i.e. analysis of how far the budgets in PUDRs have drifted away from the original budgets from grant-making
# ------------------------------------------------------------------------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(data.table)
library(stringr)
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(scales)
# ------------------


# --------------------------------------------------------------------------------
# Files and directories

# directory for the data
dir = 'J:/Project/Evaluation/GF/resource_tracking/_other_data_sources/multi_country/2019-2020_synthesis/'

# file prepped by Emily
inFileModule = paste0(dir, 'all_modules.rds')
inFileCostCats = paste0(dir, 'all_cost_categories.rds')

# output file
outFile = paste0(dir, '../../../visualizations/budget_variance_analysis/budget_variance_analysis.pdf')
# --------------------------------------------------------------------------------


# --------------------------------------------------
# Load/prep module data

# load
moduleData = readRDS(inFileModule)

# test unique identifiers
idVars = c('loc_name','grant','gf_module')
test = nrow(moduleData)==nrow(unique(moduleData[, idVars,with=FALSE]))
if(!test) stop(paste(paste(idVars, collapse=' '), 'do not unique identify rows!'))

# collapse to country level
byVars = c('loc_name', 'gf_module', 'abbrev_mod', 'semester', 'grant_disease')
sdcols = c('cumulative_budget','cumulative_expenditure', 'original_budget', 'expenditure_incl_commitments')
moduledata = moduleData[, lapply(.SD, sum, na.rm=TRUE), by=byVars, .SDcols=sdcols]

# compute difference
moduleData[, difference:=cumulative_budget-original_budget]

# drop rows with no original_budget
moduleData = moduleData[!is.na(original_budget)]

# compute net difference by module and disease group
moduleData[grant_disease!='malaria', disease_group:='HIV and TB']
moduleData[grant_disease=='malaria', disease_group:='Malaria']
moduleData[, net_difference:=sum(difference), by=c('disease_group','gf_module')]
moduleData[, net_difference_overall:=sum(difference), by='gf_module']
moduleData[, net_difference_country:=sum(difference), by='loc_name']

# clean up disease names
moduleData[grant_disease=='hiv', grant_disease:='HIV']
moduleData[grant_disease=='hiv/tb', grant_disease:='HIV/TB']
moduleData[grant_disease=='malaria', grant_disease:='Malaria']
moduleData[grant_disease=='tb', grant_disease:='TB']
# --------------------------------------------------


# --------------------------------------------------
# Analysis
bigChanges = moduleData[abs(difference)>1000,c('abbrev_mod','difference','loc_name','grant_disease')]
bigChanges[, loc_name:=paste0(loc_name, ' (',round(difference), ')')]
increases = bigChanges[difference>0]
increases[, loc_name:=gsub('\\(', '\\(\\+', loc_name)]
decreases = bigChanges[difference<0]
setnames(increases, 'loc_name', 'increases')
setnames(decreases, 'loc_name', 'decreases')
increases[, increases:=paste(increases, collapse=', '), by=c('grant_disease','abbrev_mod')]
decreases[, decreases:=paste(decreases, collapse=', '), by=c('grant_disease','abbrev_mod')]
increases$difference = NULL
decreases$difference = NULL
increases = unique(increases)
decreases = unique(decreases)
bigChanges = merge(increases, decreases, by=c('grant_disease','abbrev_mod'), all=TRUE)
bigChanges
# --------------------------------------------------


# --------------------------------------------------
# Set up to graph

# country colors
colors = brewer.pal(length(unique(moduleData$loc_name))+2, 'Paired')[c(1,2,3,4,7,8)]
countries = unique(moduleData$loc_name)
names(colors) = countries[order(countries)]

# titles
t = 'Budget Variance from Original Approved Budget (by module)'
y = 'Difference from Original Budget through 18 Months ($)'
c = 'Values compare the budget as reported in semester 3 PUDRs
	to the budget as reported in the original, approved grant documentation'
# --------------------------------------------------


# --------------------------------------------------
# Graph
pdf(outFile, height=5.5, width=9)
	
# graph by country
ggplot(moduleData, aes(x=reorder(loc_name, net_difference_country), y=difference, fill=grant_disease)) + 
	geom_bar(position='stack', stat='identity') +
	geom_hline(yintercept=0) + 
	coord_flip() + 
	scale_fill_viridis(discrete=TRUE) + 
	scale_y_continuous(labels = comma) + 
	labs(title=t, y=y, x='', caption=c, fill='') + 
	theme_bw()
	
# graph HIV/TB modules
ggplot(moduleData[disease_group!='Malaria'], aes(x=reorder(abbrev_mod, net_difference), y=difference, fill=loc_name)) + 
	geom_bar(position='stack', stat='identity') +
	geom_hline(yintercept=0) + 
	coord_flip() + 
	scale_fill_manual('', values=colors) + 
	scale_y_continuous(labels = comma) + 
	labs(title=t, subtitle='HIV and TB Grants', y=y, x='', caption=c) + 
	theme_bw()
	
# graph HIV modules
ggplot(moduleData[grant_disease=='HIV'], aes(x=reorder(abbrev_mod, net_difference), y=difference, fill=loc_name)) + 
	geom_bar(position='stack', stat='identity') +
	geom_hline(yintercept=0) + 
	coord_flip() + 
	scale_fill_manual('', values=colors) + 
	scale_y_continuous(labels = comma) + 
	labs(title=t, subtitle='HIV Grants', y=y, x='', caption=c) + 
	theme_bw()
	
# graph TB modules
ggplot(moduleData[grant_disease=='TB'], aes(x=reorder(abbrev_mod, net_difference), y=difference, fill=loc_name)) + 
	geom_bar(position='stack', stat='identity') +
	geom_hline(yintercept=0) + 
	coord_flip() + 
	scale_fill_manual('', values=colors) + 
	scale_y_continuous(labels = comma) + 
	labs(title=t, subtitle='TB Grants', y=y, x='', caption=c) + 
	theme_bw()
	
# graph HIV/TB modules
ggplot(moduleData[grant_disease=='HIV/TB'], aes(x=reorder(abbrev_mod, net_difference), y=difference, fill=loc_name)) + 
	geom_bar(position='stack', stat='identity') +
	geom_hline(yintercept=0) + 
	coord_flip() + 
	scale_fill_manual('', values=colors) + 
	scale_y_continuous(labels = comma) + 
	labs(title=t, subtitle='HIV/TB Grants', y=y, x='', caption=c) + 
	theme_bw()
	
# graph malaria modules
ggplot(moduleData[disease_group=='Malaria'], aes(x=reorder(abbrev_mod, net_difference), y=difference, fill=loc_name)) + 
	geom_bar(position='stack', stat='identity') +
	geom_hline(yintercept=0) + 
	coord_flip() + 
	scale_fill_manual('', values=colors) + 
	scale_y_continuous(labels = comma) + 
	labs(title=t, subtitle='Malaria Grants', y=y, x='', caption=c) + 
	theme_bw()
	
# graph modules for all diseases
ggplot(moduleData, aes(x=reorder(abbrev_mod, net_difference_overall), y=difference, fill=loc_name)) + 
	geom_bar(position='stack', stat='identity') +
	geom_hline(yintercept=0) + 
	coord_flip() + 
	scale_fill_manual('', values=colors) + 
	scale_y_continuous(labels = comma) + 
	labs(title=t, subtitle='All Grants', y=y, x='', caption=c) + 
	theme_bw()

# graph HIV modules separate by country
ggplot(moduleData[grant_disease=='HIV' & abs(difference)>1], aes(x=reorder(abbrev_mod, net_difference), y=difference, fill=loc_name)) + 
	geom_bar(position='stack', stat='identity') +
	geom_hline(yintercept=0) + 
	coord_flip() + 
	facet_wrap(~loc_name, scales='free', ncol=2) + 
	scale_fill_manual('', values=colors) + 
	scale_y_continuous(labels=comma, breaks=pretty_breaks(n=3)) + 
	labs(title=t, subtitle='HIV Grants', y=y, x='', caption=c) + 
	theme_bw()

# graph TB modules separate by country
ggplot(moduleData[grant_disease=='TB' & abs(difference)>1], aes(x=reorder(abbrev_mod, net_difference), y=difference, fill=loc_name)) + 
	geom_bar(position='stack', stat='identity') +
	geom_hline(yintercept=0) + 
	coord_flip() + 
	facet_wrap(~loc_name, scales='free', ncol=2) + 
	scale_fill_manual('', values=colors) + 
	scale_y_continuous(labels=comma, breaks=pretty_breaks(n=3)) + 
	labs(title=t, subtitle='TB Grants', y=y, x='', caption=c) + 
	theme_bw()

# graph HIV/TB modules separate by country
ggplot(moduleData[grant_disease=='HIV/TB' & abs(difference)>1], aes(x=reorder(abbrev_mod, net_difference), y=difference, fill=loc_name)) + 
	geom_bar(position='stack', stat='identity') +
	geom_hline(yintercept=0) + 
	coord_flip() + 
	facet_wrap(~loc_name, scales='free', ncol=2) + 
	scale_fill_manual('', values=colors) + 
	scale_y_continuous(labels=comma, breaks=pretty_breaks(n=3)) + 
	labs(title=t, subtitle='HIV/TB Grants', y=y, x='', caption=c) + 
	theme_bw()

# graph malaria modules separate by country
ggplot(moduleData[disease_group=='Malaria' & abs(difference)>1], aes(x=reorder(abbrev_mod, net_difference), y=difference, fill=loc_name)) + 
	geom_bar(position='stack', stat='identity') +
	geom_hline(yintercept=0) + 
	coord_flip() + 
	facet_wrap(~loc_name, scales='free', ncol=2) + 
	scale_fill_manual('', values=colors) + 
	scale_y_continuous(labels=comma, breaks=pretty_breaks(n=3)) + 
	labs(title=t, subtitle='Malaria Grants', y=y, x='', caption=c) + 
	theme_bw()

dev.off()
# --------------------------------------------------
