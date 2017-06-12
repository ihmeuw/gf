# ------------------------------------------------------------
# David Phillips
#
# 2/3/2017
# Country-level Sankey diagrams for TERG meeting
# The current directory should be the same as sankey_diagram.r
# ------------------------------------------------------------

# to-do
# - perfect the order of edges. there is still unnecessary crossing
# - map colors to channels by name so that they are consistent regardless of order
# - figure out a better way to place the source/channel/outcome titles

# -------------------
# Set up R
rm(list=ls())
library(data.table)
library(riverplot)
library(scales)
library(RColorBrewer)
# -------------------


# ---------------------------------------------------------------------------------
# Files and directories

# main directory
root = 'J:/Project/Evaluation/GF/miscellaneous/IHME_DAH_DATABASE_1990_2015_CSV_0'

# data file, this is an extraction from the GHDx
# http://ghdx.healthdata.org/record/development-assistance-health-database-1990-2015
inFile = paste0(root, '/IHME_DAH_DATABASE_1990_2015_Y2016M04D25.csv')

# output file
outFile = paste0(root, '/sankey_diagrams.pdf')

# load sankey function
source('./sankey_diagram.r')
# ---------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------
# Load/prep data

# load
data = fread(inFile)

# sum HIV
hivVars = names(data)[grepl('hiv', names(data))]
data[, hiv_dah_15:=rowSums(.SD), .SDcols=hivVars]

# subset rows
data = data[year==2013]

# subset columns
vars = c('recipient_isocode', 'source', 'channel', 'hiv_dah_15', 'mal_dah_15', 'tb_dah_15')
data = data[, vars, with=FALSE]

# group together sources/format names
otherCountries = c('Ireland', 'Japan', 'Italy', 'Korea', 'Switzerland', 'Austria',
					'Non_OECD_DAC_countries', 'Greece', 'Spain', 'Netherlands', 
					'Belgium', 'Denmark', 'Finland', 'Sweden', 'Norway', 
					'New_Zealand', 'Luxembourg', 'Portugal', 'Canada', 
					'United Kingdom', 'Australia', 'Germany', 'France')
otherSources = c('Debt_repayments', 'Other', 'Unallocable')
data[source %in% otherCountries, source_cat:='Other Governments']
data[source %in% otherSources, source_cat:='Other Sources']
data[is.na(source_cat), source_cat:=source]
data[source_cat=='Private_other', source_cat:='Private Philanthropy']
data[source_cat=='United_Kingdom', source_cat:='United Kingdom']
data[source_cat=='United_States', source_cat:='United States']

# group together channels
otherBilaterals = c('BIL_IRL', 'BIL_AUT', 'BIL_NLD', 'BIL_ITA', 'BIL_ESP', 'BIL_KOR',
					'BIL_FIN', 'BIL_CHE', 'BIL_DNK', 'BIL_JPN', 'Other_governments',
					'BIL_NOR', 'BIL_SWE', 'BIL_BEL', 'BIL_NZL', 'BIL_LUX', 'BIL_GRC', 
					'BIL_CAN', 'BIL_GBR', 'BIL_AUS', 'BIL_DEU', 'BIL_FRA')
unAgencies = c('WHO', 'PAHO', 'UNICEF', 'UNFPA', 'UNAIDS')
developmentBanks = c('WB_IDA', 'AsDB', 'AfDB', 'ADB', 'WB_IBRD', 'IDB', 'EC')
ngos = c('INTLNGO', 'NGO')
data[channel %in% otherBilaterals, channel_cat:='Other Bilateral Aid Agencies']
data[channel %in% developmentBanks, channel_cat:='Development Banks']
data[channel %in% unAgencies, channel_cat:='UN Agencies']
data[channel %in% ngos, channel_cat:='NGOs and Foundations']
data[is.na(channel_cat), channel_cat:=channel]
data[channel_cat=='BMGF', channel_cat:='BMGF-C']
data[channel_cat=='BIL_USA', channel_cat:='United States-C']
data[channel_cat=='GFATM', channel_cat:='Global Fund']
data[channel_cat=='BIL_CAN', channel_cat:='Canada-C']
data[channel_cat=='BIL_GBR', channel_cat:='United Kingdom-C']
data[channel_cat=='BIL_AUS', channel_cat:='Australia-C']
data[channel_cat=='BIL_DEU', channel_cat:='Germany-C']
data[channel_cat=='BIL_FRA', channel_cat:='France-C']
data[channel_cat=='EC', channel_cat:='European Commission']

# collapse to groups of sources/channels
data = data[, lapply(.SD, sum), by=c('recipient_isocode', 'source_cat', 'channel_cat'), 
	.SDcols=c('hiv_dah_15', 'mal_dah_15', 'tb_dah_15')]
	
# reshape outcomes long
data = melt(data, id.vars=c('recipient_isocode', 'source_cat', 'channel_cat'), 
				variable.name='outcome', value.name='dah')
				
# format outcome variable
data[outcome=='hiv_dah_15', outcome:='HIV']
data[outcome=='tb_dah_15', outcome:='TB']
data[outcome=='mal_dah_15', outcome:='Malaria']
data = droplevels(data)
data[, outcome:=as.character(outcome)]
# ---------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------------
# Make plot

# DRC
# manually specify the order of nodes because it's not as simple as just descending order of size
nodeOrder = c('United States', 'BMGF', 'Other Governments', 
		'Canada', 'Other Sources', 'Private Philanthropy', 'United Kingdom', 'Australia', 
		'Germany', 'France', 'Unallocable', 'NGOs and Foundations', 'United States-C', 
		'Global Fund', 'BMGF-C', 'UN Agencies', 'Other Bilateral Aid Agencies', 
		'Development Banks', 'Canada-C', 'GAVI', 'United Kingdom-C', 'European Commission', 
		'Australia-C', 'Germany-C', 'France-C', 'HIV', 'TB' ,'Malaria')

# use the new function to make a single sankey
p1 = makeSankey(inputData=data[recipient_isocode=='COD'], nodeOrder=nodeOrder)		

# GTM
# manually specify the order of nodes because it's not as simple as just descending order of size
nodeOrder = c('United States', 'Private Philanthropy', 'BMGF', 'Other Governments', 
		'Canada', 'Other Sources', 'United Kingdom', 'Australia', 
		'Germany', 'France', 'Unallocable', 'United States-C', 'NGOs and Foundations', 
		'Global Fund', 'BMGF-C', 'UN Agencies', 'Other Bilateral Aid Agencies', 
		'Development Banks', 'Canada-C', 'GAVI', 'United Kingdom-C', 'European Commission', 
		'Australia-C', 'Germany-C', 'France-C', 'HIV', 'TB' ,'Malaria')

# use the new function to make a single sankey
p2 = makeSankey(inputData=data[recipient_isocode=='GTM'], nodeOrder=nodeOrder)		

# UGA
# manually specify the order of nodes because it's not as simple as just descending order of size
nodeOrder = c('United States', 'Private Philanthropy', 'BMGF', 'Other Governments', 
		'Canada', 'Other Sources', 'United Kingdom', 'Australia', 
		'Germany', 'France', 'Unallocable', 'United States-C', 'NGOs and Foundations', 
		'Global Fund', 'BMGF-C', 'UN Agencies', 'Other Bilateral Aid Agencies', 
		'Development Banks', 'Canada-C', 'GAVI', 'United Kingdom-C', 'European Commission', 
		'Australia-C', 'Germany-C', 'France-C', 'HIV', 'TB' ,'Malaria')

# use the new function to make a single sankey
p3 = makeSankey(inputData=data[recipient_isocode=='UGA'], nodeOrder=nodeOrder)		

# open pdf
pdf(outFile, height=6, width=12)

# make graph for DRC
plot(p1, srt=0, node_margin=3, nodewidth=1, plot_area=.75, nsteps=200)
title(main='Democratic Republic of the Congo', line = -1.5)
textY = max(p1$nodes$y)+3
text(1, textY, bquote(underline('Source')), adj=c(.5,0), cex=1.25)
text(2, textY, bquote(underline('Channel')), adj=c(.5,0), cex=1.25)
text(3, textY, bquote(underline('Outcome')), adj=c(.5,0), cex=1.25)

# make graph for GTM
plot(p2, srt=0, node_margin=3, nodewidth=1, plot_area=.75, nsteps=200)
title(main='Guatemala', line = -1.5)
textY = max(p1$nodes$y)-2
text(1, textY, bquote(underline('Source')), adj=c(.5,0), cex=1.25)
text(2, textY, bquote(underline('Channel')), adj=c(.5,0), cex=1.25)
text(3, textY, bquote(underline('Outcome')), adj=c(.5,0), cex=1.25)

# make graph for UGA
plot(p3, srt=0, node_margin=3, nodewidth=1, plot_area=.75, nsteps=200)
title(main='Uganda', line = -1.5)
textY = max(p1$nodes$y)+2.5
text(1, textY, bquote(underline('Source')), adj=c(.5,0), cex=1.25)
text(2, textY, bquote(underline('Channel')), adj=c(.5,0), cex=1.25)
text(3, textY, bquote(underline('Outcome')), adj=c(.5,0), cex=1.25)

# close pdf
dev.off()
# -------------------------------------------------------------------------------------------------
