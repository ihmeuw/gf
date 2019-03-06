# Function that makes a diagram of SEM output
# Arguments: 
# fitObject, a lavaan or blavaan object
# infoTable, a data.table containing 4 columns (rows correspond to model variables in any order): 
# 	variable (exactly as in model), x, y (where it should appear in graph) and label (optional)
# edgeLabels, (logical) whether or not to label coefficients
# labSize1, labSize2, (numeric) large and small text sizes
# boxHeight, boxWidth, (numeric) height and width of boxes
# variances, (logical) whether or not to show variances
# curved, (numeric) 0=straight lines, 1=1 bend, 2=2 bends
# tapered, (logical) whether to taper edges from start to finish
# Returns: a graph
# Rquires: data.table, ggplot2, stringr

# TO DO


# rm(list=ls())
# fitObject = readRDS('C:/local/tmpsemfit.rds')
# nodeTable = fread('C:/local/gf/impact_evaluation/visualizations/vartable.csv')
# labSize1=5
# labSize2=3
# variances = TRUE
# edgeLabels = TRUE
# boxWidth=4
# boxHeight=1

semGraph = function(fitObject=NULL, nodeTable=NULL, edgeLabels=TRUE, variances=TRUE, labSize1=5, labSize2=3, 
	boxWidth=4, boxHeight=1) {

	# -------------------------------------------------------------------------------
	# Set up node table
	
	# test any variables not in model
	
	# test any variables not in nodeTable
	modelVars = unique(c(fitObject@ParTable$lhs, fitObject@ParTable$rhs))
	modelVars = modelVars[modelVars!='']
	exclVars = modelVars[!modelVars %in% nodeTable$variable]
	
	# check all labels available
	nodeTable[, label:=as.character(label)]
	nodeTable[is.na(label), label:=variable]
	# -------------------------------------------------------------------------------
	
	
	# -------------------------------------------------------------------------------
	# Set up edge table
	
	# extract edges from lavaan table
	# edgeTable = data.table(sapply(fitObject@ParTable,c))
	# edgeTable[, est:=as.numeric(est)]
	# edgeTable[, se:=as.numeric(se)]
	# edgeTable$label = NULL
	edgeTable = data.table(standardizedSolution(fitObject))
	setnames(edgeTable, 'est.std', 'est')
	
	# identify start and end locations
	edgeTable = merge(edgeTable, nodeTable, by.x='rhs', by.y='variable')
	setnames(edgeTable, c('x','y','label'), c('xstart','ystart','labelstart'))
	edgeTable = merge(edgeTable, nodeTable, by.x='lhs', by.y='variable')
	setnames(edgeTable, c('x','y','label'), c('xend','yend','labelend'))
	
	# identify middle of each path for coefficient labels
	edgeTable[, xmid:=(xstart+boxWidth+xend)/2]
	edgeTable[, ymid:=(ystart+yend)/2]
	
	# identify the length of each path
	edgeTable[,edge_length:=sqrt(((yend-ystart)^2)+((xend-xstart)^2))]
	
	# add buffers to the end of the edges (assuming all arrows are going left to right)
	edgeTable[, vx:=xend-xstart]
	# edgeTable[, xend:=xend-(labSize2*.05*vx)]
	# edgeTable[op!='~~', xend:=xend-labSize2*.1]
	
	# drop variances if specified
	if (variances==FALSE) { 
		edgeTable = edgeTable[op!='~~']
	}
	
	# always drop variances that are exactly zero assuming they've been manually excluded
	edgeTable = edgeTable[!(op=='~~' & est==0)]
	# -------------------------------------------------------------------------------
	
	
	# -------------------------------------------------------------------------------
	# Graph
	
	# initialize plot
	p = ggplot() 

	
	# add edges
	p = p + 
		geom_segment(data=edgeTable[op!='~~'], aes(x=xstart+boxWidth, y=ystart, xend=xend, yend=yend, color=est), 
			arrow=arrow(), size=labSize2)
			
	# add covariances with curvature based on edge length
	if (variances==TRUE) { 
		for(i in which(edgeTable$op=='~~' & edgeTable$rhs!=edgeTable$lhs)) { 		
			# identify edge length
			el = edgeTable[i]$edge_length
			# set direction of curve
			di = ifelse(edgeTable[i]$xstart<mean(edgeTable$xstart), -1, 1)
			# curved arrows going downward need to bend the other way
			if (edgeTable[i]$yend < edgeTable[i]$ystart) di = -di 
			
			p = p + 
				geom_curve(data=edgeTable[i], 
					aes(x=xstart, y=ystart, xend=xend, yend=yend, color=est), 
					size=labSize2*.25, curvature=di*(1/(0.4*el)), angle=90)
		}
	}
	
	# add edge labels
	if (edgeLabels) { 
		p = p + geom_text(data=edgeTable[edgeTable$op!='~~'], aes(x=xmid, y=ymid+(min(edgeTable$ystart)*.25), 
			label=round(est,2)), size=labSize2*.8, lwd=0)
	}
	
	# add nodes
	p = p + 
		# geom_point(data=nodeTable, aes(y=y, x=x), size=labSize2*5, shape=22, fill='white') + 
		geom_rect(data=nodeTable, aes(ymin=y-(boxHeight*.5), ymax=y+(boxHeight*.5), xmin=x, xmax=x+boxWidth), 
			fill='white', color='black') + 
		geom_text(data=nodeTable, aes(y=y, x=x+(0.05*boxWidth), label=str_wrap(label,20)), size=labSize2, hjust=0) 
	
	# improve legend
	p = p + 
		scale_color_viridis(direction=-1) 
	
	# add buffer space to axes
	ymax = max(nodeTable$y)+(0.25*sd(nodeTable$y))
	ymin = min(nodeTable$y)-(0.25*sd(nodeTable$y))
	xmax = max(nodeTable$x)+(0.25*sd(nodeTable$x))
	xmin = min(nodeTable$x)-(0.25*sd(nodeTable$x))
	p = p + 
		expand_limits(y=c(ymin, ymax), x=c(xmin, xmax)) 
	
	# labels
	p = p + 
		labs(color='Effect\nSize', caption=paste('Control variables not displayed:', paste(exclVars, collapse =',')))
	
	# clean up plot
	p = p + theme_void()
	
	# -------------------------------------------------------------------------------
	return(p)
}
