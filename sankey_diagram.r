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
#
# Outputs
# * r2 - (object of class "riverplot") ready to be plotted with base plot() 
# -------------------------------------------------------------------------


# to-do
# - make the input data.table compatible
# - perfect the order of edges. there is still unnecessary crossing
# - map colors to channels by name so that they are consistent regardless of order
# - figure out a better way to place the source/channel/outcome titles


# --------------------------------------------
# Define function
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
	if (length(channels)>length(channelColors)) channelColors = c(channelColors, '#a6611a', '#bdbdbd')

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
	# because it's hard to make an object with the appropriate dimensions/names for edge_styles
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

