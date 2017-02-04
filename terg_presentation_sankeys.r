# -----------------------------------------------
# David Phillips
#
# 2/3/2017
# Country-level Sankey diagrams for TERG meeting
# -----------------------------------------------

# to-do
# - perfect the order of edges. there is still unnecessary crossing
# - map colors to channels by name so that they are consistent regardless of order


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
					'New_Zealand', 'Luxembourg', 'Portugal')
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
					'BIL_NOR', 'BIL_SWE', 'BIL_BEL', 'BIL_NZL', 'BIL_LUX', 'BIL_GRC')
unAgencies = c('WHO', 'PAHO', 'UNICEF', 'UNFPA', 'UNAIDS')
developmentBanks = c('WB_IDA', 'AsDB', 'AfDB', 'ADB', 'WB_IBRD', 'IDB')
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


# --------------------------------------------
# Define function that will make one Sankey
# --------------------------------------------
makeSankey = function(inputData, nodeOrder) { 

	# ---------------------------------------------------------------------------------
	# Set up for graphs

	# isolate nodes
	ID = unique(c(inputData$source_cat, inputData$channel_cat, inputData$outcome))

	# manually order nodes
	ID = ID[order(match(ID, nodeOrder))]

	# assign x-axis coordinates to nodes based on source-channel-outcome
	x = rep(1, length(ID))
	x[ID %in% inputData$channel_cat] = 2
	x[ID %in% inputData$outcome] = 3

	# assign y-axis coordinates to nodes based on the ammount of space the node will take up in its column
		# each column is going to sequence from 0 to ymax
		ymax = max(table(x))
		
		# identify how big each node is
		sourceTotals = inputData[, list(dah=sum(dah)), by='source_cat']
		channelTotals = inputData[, list(dah=sum(dah)), by='channel_cat']
		outcomeTotals = inputData[, list(dah=sum(dah)), by='outcome']
		sourceTotals =  sourceTotals[rev(order(match(source_cat, nodeOrder[nodeOrder%in%inputData$source_cat])))]
		channelTotals = channelTotals[rev(order(match(channel_cat, nodeOrder[nodeOrder%in%inputData$channel_cat])))]
		outcomeTotals = outcomeTotals[rev(order(match(outcome, nodeOrder[nodeOrder%in%inputData$outcome])))]
		
		# seq from 0 to ymax
		ySources = seq(0, ymax+6, length.out=nrow(sourceTotals))
		yChannels = seq(0, ymax+10, length.out=nrow(channelTotals)) # add a little bulge to the middle column for aesthetics
		yOutcomes = seq(0, ymax, length.out=nrow(outcomeTotals))
		
		# add an arbitrary amount of padding to large nodes
		ySources = ySources + cumsum(ifelse(sourceTotals$dah>median(sourceTotals$dah), 2, 0)) 
		yChannels = yChannels + cumsum(ifelse(channelTotals$dah>median(channelTotals$dah), 2, 0))
		yOutcomes = yOutcomes + cumsum(ifelse(outcomeTotals$dah>median(outcomeTotals$dah), 2, 0))
		ySources[length(ySources)] = ySources[length(ySources)] + 3 # specially padding for USA-S
		
		# now that the spacing is more appropriate, re-center nodes relative to the channel column
		ySources = ySources + (median(range(yChannels))-median(range(ySources)))
		yOutcomes = yOutcomes + (median(range(yChannels))-median(range(yOutcomes)))

	y = c(rev(ySources), rev(yChannels), rev(yOutcomes))
	nodes = data.frame(ID, x, y, stringsAsFactors=FALSE)
	ID = nodes$ID

	# edge colors based on the channel they connect to
	sources = ID[ID %in% inputData$source_cat]
	channels = ID[ID %in% inputData$channel_cat]
	channelColors = suppressWarnings(brewer.pal(n=length(channels), 'Paired'))
	channelColors = c(channelColors, '#a6611a', '#bdbdbd')

	# set up edges
	edges = data.frame()
	for(n in seq_along(ID)) {
		node = ID[n]
		if (node %in% inputData$outcome) next
		if (node %in% inputData$source_cat) origin = 'source_cat'
		if (node %in% inputData$channel_cat) origin = 'channel_cat'
		if (node %in% inputData$source_cat) destination = 'channel_cat'
		if (node %in% inputData$channel_cat) destination = 'outcome'
		for(e in unique(inputData[get(origin)==node][[destination]])) {
			v = sum(inputData[get(origin)==node & get(destination)==e]$dah)
			if (destination=='channel_cat') c = channelColors[channels==e]
			if (destination=='outcome') c = channelColors[channels==node]
			newEdge = data.frame(N1=node, N2=e, Value=v, col=c, edgecol='col', stringsAsFactors=FALSE)
			edges = rbind(edges, newEdge)
		}
	}

	# order edges within starting nodes
	for(n in edges$N1) {
		tmp = edges[edges$N1==n,]
		tmp = tmp[order(tmp[,'Value']),]
		edges[edges$N1==n,] = tmp
	}
	
	# this reverses the order that edges enter the channel nodes
	for (c in channels) {
		edges[edges$N2==c,] = edges[edges$N2==c,][rev(seq(1,nrow(edges[edges$N2==c,]))),]
	}
	
	# this reverses the order that edges enter the outcome nodes
	edges[edges$N2=='HIV',] = edges[edges$N2=='HIV',][rev(seq(1,nrow(edges[edges$N2=='HIV',]))),]
	edges[edges$N2=='TB',] = edges[edges$N2=='TB',][rev(seq(1,nrow(edges[edges$N2=='TB',]))),]
	edges[edges$N2=='Malaria',] = edges[edges$N2=='Malaria',][rev(seq(1,nrow(edges[edges$N2=='Malaria',]))),]
	
	# this reverses the order that edges exit the source nodes
	for (s in sources) {
		edges[edges$N1==s,] = edges[edges$N1==s,][rev(seq(1,nrow(edges[edges$N1==s,]))),]
	}
	# ---------------------------------------------------------------------------------


	# ----------------------------------------------------------------------------
	# Make graphs

	# make initial graph
	r1 = makeRiver(nodes, edges)

	# use the output from the graph to hack a few alterations
	edgeStyles = r1$styles
	for(e in names(edgeStyles)) { 
		a = 1- r1$edges[r1$edges$ID==e,'Value']/sum(r1$edges$Value)
		edgeStyles[[e]]$col=alpha(edgeStyles[[e]]$col, a) # add some alpha to larger edges
	}

	# remake graph
	r2 = makeRiver(nodes, edges, edge_styles=edgeStyles)
	# ----------------------------------------------------------------------------
	
	
	# ------------
	# Return plot
	return(r2)
	# ------------
} # end of function


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
pdf(outFile, height=6, width=10)

# make graph for DRC
plot(p1, srt=0, node_margin=3, nodewidth=1, plot_area=.75, nsteps=10)
title(main='Democratic Republic of the Congo', line = -1.5)
textY = max(p1$nodes$y)+3.5
text(1, textY, bquote(underline('Source')), adj=c(.5,0), cex=1.25)
text(2, textY, bquote(underline('Channel')), adj=c(.5,0), cex=1.25)
text(3, textY, bquote(underline('Outcome')), adj=c(.5,0), cex=1.25)

# make graph for GTM
plot(p2, srt=0, node_margin=3, nodewidth=1, plot_area=.75, nsteps=200)
title(main='Guatemala', line = -1.5)
textY = max(p1$nodes$y)-1.5
text(1, textY, bquote(underline('Source')), adj=c(.5,0), cex=1.25)
text(2, textY, bquote(underline('Channel')), adj=c(.5,0), cex=1.25)
text(3, textY, bquote(underline('Outcome')), adj=c(.5,0), cex=1.25)

# make graph for UGA
plot(p3, srt=0, node_margin=3, nodewidth=1, plot_area=.75, nsteps=200)
title(main='Uganda', line = -1.5)
textY = max(p1$nodes$y)+3.5
text(1, textY, bquote(underline('Source')), adj=c(.5,0), cex=1.25)
text(2, textY, bquote(underline('Channel')), adj=c(.5,0), cex=1.25)
text(3, textY, bquote(underline('Outcome')), adj=c(.5,0), cex=1.25)

# close pdf
dev.off()
# -------------------------------------------------------------------------------------------------
