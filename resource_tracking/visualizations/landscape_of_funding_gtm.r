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


# --------------------------------------------------------------------------
# Files and directories

# input directory
dir = 'J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/'

# input file
inFile = paste0(dir, 'total_resource_tracking_data.csv')

# output file
outFile = paste0(dir, '../../gtm/visualizations/landscape_of_funding.pdf')
# --------------------------------------------------------------------------


# -----------------------------------------------------------------------------------------------------------
# Load/prep data

# load
data = fread(inFile)

# subset to Guatemala after 2010
data = data[country=='Guatemala' & year>2010 & year<2019 & disease!='all']

# subset to exclude FGH estimates and initial budgets
data = data[!grepl('init_fpm', data_source)]
# data = data[!(data_source=='fgh' & (grepl('ensemble',financing_source) | grepl('lower',financing_source) | grepl('upper',financing_source) | grepl('domestic',financing_source) | grepl('ghe',financing_source) | grepl('the',financing_source)))]
data = data[data_source!='fgh']

# use FGH disbursement as budget
data[data_source=='fgh', budget:=disbursement]
data[financing_source=='bil_usa', financing_source:='other_dah']

# reshape variables (exp/dis/budg) long
varLong = melt(data, measure.vars=c('budget','disbursement','expenditure'))
varLong = varLong[, .(value=sum(value)), by=c('financing_source','data_source','year','disease','variable')]

# quick graph just to see it all
ggplot(varLong[disease%in%c('tb')], aes(y=value, x=year, color=variable,shape=disease)) + 
	geom_point() + geom_line() + facet_grid(financing_source~data_source, scales='free')
# -----------------------------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------- 
# Set up to graph

# subset data down to just TB budgets post 2010
graphData = varLong[variable=='budget']

# subset data down to just ghe/dah from SICOIN and GF from detailed budgets
graphData = graphData[!(data_source=='sicoin' & financing_source=='gf')]
graphData = graphData[!data_source %in% c('gos','pudr')]

# clean up labels
graphData[financing_source=='other_dah', data_source:='OECD DAC/CRS Databases']
graphData[financing_source=='other_dah', financing_source:='Other External Donors']
graphData[financing_source=='gf', financing_source:='Global Fund']
graphData[financing_source=='ghe', financing_source:='Government']
graphData[data_source=='fpm', data_source:='Global Fund Detailed Budgets']
graphData[data_source=='fpm', data_source:='Global Fund Detailed Budgets']
graphData[data_source=='sicoin', data_source:='National Accounting System (SICOIN)']

# compute percentages
graphData[, total:=sum(value), by=c('disease','year')]
graphData[, pct:=value/total]

# drop percentages that doing have ghe data
graphData[, has_ghe:=ifelse(financing_source=='Government',1,0)]
graphData[, has_ghe:=max(has_ghe), by=c('disease','year')]
graphData[has_ghe==0, pct:=NA]
# ----------------------------------------------------------------------------------


# -----------------------------------------------------------------------------------
# Graph

# start a list to store graphs in
plots = list()

# loop over diseases
i=1
for(d in c('hiv','tb','malaria')) { 
	# store label for title
	lab = toupper(d)
	if (d=='malaria') lab = 'Malaria'

	# store graph of numbers in list
	plots[[i]] = ggplot(graphData[disease==d], aes(y=value/1000000, x=year, 
			shape=data_source, color=financing_source)) + 
		geom_line(size=1.5) + 
		geom_point(size=3.5, color='grey45') + 
		labs(title=paste('Landscape of', lab, 'Funding'), y='Budget (In Millions of USD)', 
			shape='Data Source', color='Financing Source') + 
		theme_bw()
	i=i+1

	# store graph of percentages in list
	plots[[i]] = ggplot(graphData[disease==d], aes(y=pct, x=year, 
			shape=data_source, color=financing_source)) + 
		geom_line(size=1.5) + 
		geom_point(size=3.5, color='grey45') + 
		labs(title=paste('Landscape of', lab, 'Funding'), y='Percentage of Budget', 
			shape='Data Source', color='Financing Source') + 
		theme_bw()
	i=i+1
}
# -----------------------------------------------------------------------------------


# --------------------------------
# Save graphs
pdf(outFile, height=5.5, width=8)
for(i in seq(length(plots))) { 
	print(plots[[i]])
}
dev.off()
# --------------------------------
