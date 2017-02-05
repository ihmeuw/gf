# ------------------------------------------------------------------------------------------------
# David Phillips
# 
# 2/4/2017
# Function that makes a single static Sankey diagram similar to http://vizhub.healthdata.org/fgh/
# ------------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Inputs
# * inputData - data.frame with four columns:
# 	- source - (character) sources of DAH
# 	- channel - (character) channels (intermediaries) of DAH
# 	- outcome - (character) outcomes (destinations) of DAH
# 	- dah - (numeric) quantity of DAH
# where rows are individual financial flows
# * nodeOrder - (character) user-supplied list of node names, 
# 				in the order that they should be plotted (top to bottom)
#
# Outputs
# * r2 - (object of class "riverplot") ready to be plotted with base plot() 
# -------------------------------------------------------------------------


# to-do
# - make the input data.table compatible
# - figure out a better way to place the source/channel/outcome titles
# - improve assignment of y-axis coordinates
# - change input variables from source_cat/channel_cat to source/channel


# --------------------------------------------
# Define function
# --------------------------------------------
makeSankey = function(inputData, nodeOrder) { 
	
	# ---------------------------------------------------------------------------------
	# Set up node basics
	
	# isolate node names
	ID = unique(c(inputData$source_cat, inputData$channel_cat, inputData$outcome))
	
	# isolate sources, channels and outcomes for later use
	sources = ID[ID %in% inputData$source_cat]
	channels = ID[ID %in% inputData$channel_cat]
	outcomes = ID[ID %in% inputData$outcome]
	
	# manually order nodes
	ID = ID[order(match(ID, nodeOrder))]
	
	# assign x-axis coordinates to nodes based on source-channel-outcome
	x = rep(1, length(ID))
	x[ID %in% channels] = 2
	x[ID %in% outcomes] = 3
	# ---------------------------------------------------------------------------------

	
	# ----------------------------------------------------------------------------------------------------
	# Assign y-axis coordinates to nodes	
	# coordinates should respect the ammount of space the node will take up in its column
	# and the order specified in nodeOrder
	
	# each column is going to sequence from 0 to ymax
	ymax = max(table(x))
	
	# identify how big each node is
	sourceTotals = inputData[, list(dah=sum(dah)), by='source_cat']
	channelTotals = inputData[, list(dah=sum(dah)), by='channel_cat']
	outcomeTotals = inputData[, list(dah=sum(dah)), by='outcome']
	sourceTotals =  sourceTotals[rev(order(match(source_cat, nodeOrder[nodeOrder %in% sources])))]
	channelTotals = channelTotals[rev(order(match(channel_cat, nodeOrder[nodeOrder %in% channels])))]
	outcomeTotals = outcomeTotals[rev(order(match(outcome, nodeOrder[nodeOrder %in% outcomes])))]
	
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
	
	# reverse y coordinates
	y = c(rev(ySources), rev(yChannels), rev(yOutcomes))
	
	# finally put together data.frame of nodes
	nodes = data.frame(ID, x, y, stringsAsFactors=FALSE)
	# ----------------------------------------------------------------------------------------------------
	

	# -------------------------------------------------------------------------------------------------
	# Set up edges
	
	# set up a vector of colors
	# edges should be colored according to the channel they connect to
	channelColors = suppressWarnings(brewer.pal(n=length(channels), 'Paired'))
	if (length(channels)>length(channelColors)) channelColors = c(channelColors, '#a6611a', '#bdbdbd')
	
	# map colors to channels by name so they're agnostic to node order
	possibleChannels = c("United States-C", "NGOs and Foundations", "Global Fund", "UN Agencies", 
						"Other Bilateral Aid Agencies", "GAVI", "Development Banks", "BMGF-C", 
						'United Kingdom-C' ,'France-C', 'Canada-C', 'Australia-C', 'Germany-C')
	possibleColors = c(brewer.pal(n=12, 'Paired'), '#a6611a', '#bdbdbd')
	channelColors = data.table(channel=possibleChannels[1:length(possibleColors)], color=possibleColors[1:length(possibleChannels)])
	channelColors = channelColors[channel %in% channels] # reduce to channels actually observed in inputData
	
	# set up edges
	edges = data.frame()
	for(n in seq_along(ID)) {
		node = ID[n]
		if (node %in% outcomes) next
		if (node %in% sources) origin = 'source_cat'
		if (node %in% channels) origin = 'channel_cat'
		if (node %in% sources) destination = 'channel_cat'
		if (node %in% channels) destination = 'outcome'
		for(e in unique(inputData[get(origin)==node][[destination]])) {
			v = sum(inputData[get(origin)==node & get(destination)==e]$dah)
			if (destination=='channel_cat') c = channelColors[channel==e]$color
			if (destination=='outcome') c = channelColors[channel==node]$color
			newEdge = data.frame(N1=node, N2=e, Value=v, col=c, edgecol='col', stringsAsFactors=FALSE)
			edges = rbind(edges, newEdge)
		}
	}
	# -------------------------------------------------------------------------------------------------
	
	
	# --------------------------------------------------------------------------------------------------------------
	# Put edges in more logical order
	
	# 1. edges coming out of sources should be in descending value, 
	# 2. edges entering channels should be in descending order of their source's y-value, 
	# 3. edges exiting channels should be in descending order of their outcome's y-value
	# 4. edges entering outcomes should be in descending order of their channel's y-value
	
	# order edges going into outcomes/coming out of channels
	nodeYs = nodes[, c('ID', 'y')]
	outcomeEdges = edges[edges$N2 %in% outcomes,]
	outcomeEdges = merge(outcomeEdges, nodeYs, by.x='N1', by.y='ID', all.x=TRUE)
	outcomeEdges = merge(outcomeEdges, nodeYs, by.x='N2', by.y='ID', all.x=TRUE, suffixes=c('_channel', '_outcome'))
	outcomeEdges = outcomeEdges[order(outcomeEdges$y_channel, outcomeEdges$y_outcome),]
	
	# order edges going into channels/coming out of sources
	channelEdges = edges[edges$N2 %in% channels,]
	channelEdges = merge(channelEdges, nodeYs, by.x='N1', by.y='ID', all.x=TRUE)
	channelEdges = channelEdges[order(channelEdges$y, channelEdges$Value),]
	
	# re-assemble sorted edges
	edges = rbind(channelEdges[, names(edges)], outcomeEdges[, names(edges)])
	# --------------------------------------------------------------------------------------------------------------


	# --------------------------------------------------------------
	# Make graphs
	
	# make initial graph
	r1 = makeRiver(nodes, edges)
	
	# use the output from the graph to hack a few alterations 
	# because it's hard to make an object with the appropriate dimensions/names for edge_styles
	edgeStyles = r1$styles
	for(e in names(edgeStyles)) { 
		a = 1- r1$edges[r1$edges$ID==e,'Value']/sum(r1$edges$Value)
		edgeStyles[[e]]$col=alpha(edgeStyles[[e]]$col, a) # add some alpha to larger edges
	}
	
	# remake graph
	r2 = makeRiver(nodes, edges, edge_styles=edgeStyles)
	# --------------------------------------------------------------
	
	
	# ------------
	# Return plot
	plot(r2, srt=0, node_margin=3, nodewidth=1, plot_area=.75, nsteps=10)

	# return(r2)
	# ------------
} # end of function
