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


#------------------------------------------------
# Alternate figure for the synthesis report, created by Emily Linebarger 
plotData = moduleData[, .(gf_module, disease_group, grant, loc_name, grant_period, cumulative_budget, original_budget)]

# Pull in abbreviated modules 
abbrev_mods = readRDS("J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/all_interventions.rds")
abbrev_mods = unique(abbrev_mods[, .(module_eng, abbrev_mod_eng)])
setnames(abbrev_mods, c('module_eng', 'abbrev_mod_eng'), c('gf_module', 'abbrev_mod'))
plotData = merge(plotData, abbrev_mods, by='gf_module', all.x=T)

#Tag abbrev_mod with its disease. 
plotData[, abbrev_mod:=paste0(disease_group, ": ",  abbrev_mod)]

# Generate a normalized variance.
plotData[, variance_normalized:=round(((cumulative_budget-original_budget)/original_budget)*100, 1)]

# Generate ranges for variance to fall into, to make graph easier to interpret.
plotData[variance_normalized==0, variance_category:="No difference"]
plotData[variance_normalized<0 & variance_normalized>-25, variance_category:="Small reduction (0-25% of original budget)"]
plotData[variance_normalized<=-25, variance_category:="Large reduction (>25% of original budget)"]
plotData[variance_normalized>0 & variance_normalized<25, variance_category:="Small increase (0-25% of original budget)"]
plotData[variance_normalized>25, variance_category:="Large increase (>25% of original budget)"]

plotData[, num_grants_per_cat:=.N, by=c('gf_module', 'variance_category')]
plotData = unique(plotData[, .(abbrev_mod, variance_category, num_grants_per_cat)])

plotData[variance_category%in%c("Small reduction (0-25% of original budget)", "Large reduction (>25% of original budget)"), num_grants_per_cat:=-num_grants_per_cat]

# Drop unnecessary categories 
plotData = plotData[!variance_category%in%c(NA, 'No difference')]

# Calculate overall change, and sort by this
plotData[, overall_diff:=sum(num_grants_per_cat), by='abbrev_mod']

# Factor plot data 
plotData$variance_category <- factor(plotData$variance_category, 
                                     levels=c("Large reduction (>25% of original budget)",
                                              "Small reduction (0-25% of original budget)", 
                                              "Small increase (0-25% of original budget)", 
                                              "Large increase (>25% of original budget)"))

p = ggplot(plotData, aes(x=reorder(abbrev_mod, overall_diff), y=num_grants_per_cat, fill=variance_category, label=abs(num_grants_per_cat))) + 
  geom_bar(stat="identity", position="stack") + 
  geom_text(position = position_stack(vjust=0.5), size=6) + 
  theme_bw(base_size=20) + 
  coord_flip() + 
  scale_fill_manual(values=c("coral2", "khaki1", "lightgreen", "green4")) + 
  scale_y_continuous(limits=c(-8, 8), breaks=seq(-8, 8, by=2)) + 
  labs(title="Budget variance by module", x="Module", y="Number of grants in each category", fill="Category")
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
