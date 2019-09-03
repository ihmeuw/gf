# Function that makes a diagram of SEM output
# Arguments: 
# fitObject, a lavaan or blavaan object (this or parTable is required)
# parTable, the result of parTable(fitObject) or standardizedSolution(fitObject) (must match "standardized"), if fitObject isn't provided
# infoTable, a data.table containing 4 columns (rows correspond to model variables in any order): 
# 	variable (exactly as in model), x, y (where it should appear in graph) and label (optional)
# scaling_factors, optional data.table containing one column per model variable, data represents factors used to rescale variables
# edgeLabels, (logical) whether or not to label coefficients
# variances, (logical) whether or not to show variances
# standardized, (logical) whether or not to display standardized coefficients
# uncertainty, (logical) whether or not to display standard errors next to coefficients
# labSize1, labSize2, (numeric) large and small text sizes
# boxHeight, boxWidth, (numeric) height and width of boxes
# lineWidth, (numeric) edge thickness and arrow size
# midpoint, (numeric [0,1]), user-specified midpoint for edge bending
# buffer, (numeric vector length 4), multipliers to add extra space around plot (0=no space), order is xmin, xmax, ymin, ymax
# curved, (numeric) 0=straight lines, 1=1 bend, 2=2 bends, 3=step-wise (1 and 2 NOT IMPLEMENTED)
# tapered, (logical) whether to taper edges from start to finish NOT IMPLEMENTED
# dim, (logical) whether to display the base diagram with an alpha < 1
# highlight (character) what boxes and the relationships between them should be highlighted? Best if used with 'dim' option. 
#colScaleMin is the minimum value to show on viridis map scale 
#colScaleMax is the maximum value to show on viridis map scale 
# Returns: a graph
# Rquires: data.table, ggplot2, stringr

# TO DO
# make "repel" code respect the xstart order
# make uncertainty=TRUE work with standardized=FALSE


# rm(list=ls())
# fitObject = semFit
# parTable = NULL
# nodeTable = fread('./impact_evaluation/visualizations/vartable.csv')
# labSize1=5
# labSize2=3
# variances = TRUE
# edgeLabels = TRUE
# standardized = TRUE
# uncertainty = TRUE
# boxWidth=4
# boxHeight=1
# lineWidth=1
# buffer=c(.25,.25,.25,.25)
# midpoint=0.4
# curved=0

semGraph = function(fitObject=NULL, parTable=NULL, nodeTable=NULL, scaling_factors=NA, 
	edgeLabels=TRUE, variances=TRUE, standardized=FALSE, uncertainty=TRUE, 
	labSize1=3, labSize2=2.4, boxWidth=4, boxHeight=1, lineWidth=3, midpoint=.5, buffer=c(.25,.25,.25,.25),
	curved=0, tapered=TRUE, dim=FALSE, highlight=NULL, colScaleMin=-0.5, colScaleMax=1.1) {

	# ------------------------------------------------------
	# Load functions/parameters
	source('./impact_evaluation/_common/drawPaths.r')
	# ------------------------------------------------------


	# -------------------------------------------------------------------------------
	# Set up node table
	
	# test any variables not in model
	
	# test any variables not in nodeTable
	if (!is.null(fitObject)) modelVars = unique(c(fitObject@ParTable$lhs, fitObject@ParTable$rhs))
	parTable[,lhs:=as.character(lhs)]
	parTable[,rhs:=as.character(rhs)]
	if (!is.null(parTable)) modelVars = unique(c(parTable$lhs, parTable$rhs))
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
	if (standardized==TRUE) {
		vars = c('lhs','op','rhs','est.std', 'se.std')
		if (!is.null(fitObject)) edgeTable = data.table(standardizedSolution(fitObject, se=TRUE))
		if (!is.null(parTable)) edgeTable = data.table(parTable[, vars, with=FALSE])
		setnames(edgeTable, c('est.std', 'se.std'), c('est', 'se'))
	}
	if (standardized==FALSE) { 
		vars = c('lhs','op','rhs','est', 'se')
		if (!is.null(fitObject)) edgeTable = data.table(parTable(fitObject))[, vars, with=FALSE]
		if (!is.null(parTable)) edgeTable = data.table(parTable[, vars, with=FALSE])
	}
	# multiply by scaling factors if we're showing actual coefficients (if possible)
	if (!all(is.na(scaling_factors))) {
		tmp = unique(melt(scaling_factors, value.name='scaling_factor'))
		edgeTable = merge(edgeTable, tmp, by.x='rhs', by.y='variable', all.x=TRUE)
		edgeTable = merge(edgeTable, tmp, by.x='lhs', by.y='variable', all.x=TRUE)
		edgeTable[is.na(scaling_factor.x), scaling_factor.x:=1]
		edgeTable[is.na(scaling_factor.y), scaling_factor.y:=1]
		edgeTable[, est:=est/scaling_factor.x*scaling_factor.y]
	}
	
	# identify start and end locations
	edgeTable = merge(edgeTable, nodeTable, by.x='rhs', by.y='variable')
	setnames(edgeTable, c('x','y','label'), c('xstart','ystart','labelstart'))
	edgeTable = merge(edgeTable, nodeTable, by.x='lhs', by.y='variable')
	setnames(edgeTable, c('x','y','label'), c('xend','yend','labelend'))
	
	# make start and end y-values repel eachother a little
	if (curved>0) { 
		edgeTable[, grp:=.GRP, by=c('yend','xend')]
		for (g in unique(edgeTable$grp)) {
			N = nrow(edgeTable[grp==g & op!='~~'])
			if (N==1) next
			edgeTable[grp==g & op!='~~', half:=rep(seq(N/2), each=N/2)]
			edgeTable[grp==g & op!='~~', n:=seq(.N), by=half]
			edgeTable[grp==g & op!='~~' & half==1, yend:=yend-n*(boxHeight*.15)]
			edgeTable[grp==g & op!='~~' & half==2, yend:=yend+n*(boxHeight*.15)]
		}
	}
	
	# identify middle of each path for coefficient labels (plus a user-specified midpoint "s")
	edgeTable[, xmid:=(xstart+boxWidth+xend)/2]
	edgeTable[, ymid:=(ystart+yend)/2]
	edgeTable[, xmid_s:=xmid-((0.5-midpoint)*(xend-xstart))]
	edgeTable[, ymid_s:=ymid-((0.5-midpoint)*(yend-ystart))]
	
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
	
	# make nice labels for edges if necessary
	if (edgeLabels==TRUE) {
		# estimate 1.96*std error assuming symmetrical
		if (uncertainty==TRUE) edgeTable[, se196:=se*1.96]
		edgeTable[, edge_label:=as.character(round(est,2))]
		if (uncertainty==TRUE) edgeTable[, edge_label:=paste0(edge_label, ' (\u00b1', round(se196,2),')')] 
	}
	# -------------------------------------------------------------------------------
	
	
	# --------------------------------------------------------------------------------------------------
	# Generate paths for edges
	if (curved %in% c(1,2)) { 
		# generate an edge path for each pair of connected nodes using edgeMaker
		len = 500
		paths = lapply(1:nrow(edgeTable[op!='~~']), drawPaths, edges=edgeTable, len=len, curved=curved, boxWidth=boxWidth)
		paths = data.table(do.call(rbind, paths))
		
		# identify the type of the end of each edge
		paths[, c('start', 'end'):=tstrsplit(group, '>', fixed=TRUE)]
		
		# add weights/operators to paths
		vars = c('rhs', 'lhs', 'est', 'op')
		paths = merge(paths, edgeTable[, vars, with=FALSE], by.x=c('start', 'end'), by.y=c('rhs', 'lhs'))	
		
		# identify last segment of each path for arrows
		paths[, x_prev:=shift(x), by='group']
		paths[, y_prev:=shift(y), by='group']
		paths[, max:=max(sequence), by='group'] 
		
		# identify middle of each path for coefficient labels
		paths[, mid:=floor(median(sequence)), by='group']
		paths[sequence==mid, mid_id:=seq_len(.N), by=group]
		paths[mid_id==2, mid:=NA]
		
		# Discretize path colors into 12 bins for better control over colors
		# min = floor(min(edgeTable$est))
		# max = ceiling(max(edgeTable$est))
		# breaks = c(min, quantile(c(min, 0), c(.5, .9)), -.01, quantile(c(0, max), c(.1, .5, .75)), max)
		# paths[, est_cat:=.bincode(est, breaks, right=FALSE, include.lowest=TRUE)]
		# means = paths[, mean(est), by='est_cat'][order(est_cat)]
		# paths = merge(paths, means, by='est_cat')
		# paths[, est_cat:=V1]
		# paths[, V1:=NULL]
	}
	# --------------------------------------------------------------------------------------------------
	
	
	# -------------------------------------------------------------------------------
	# Graph
	
	# initialize plot
	p = ggplot() 

	
	# add edges
	# straight
	if (curved==0) { 
	  if (dim == TRUE) {
		p = p + 
			geom_segment(data=edgeTable[!((lhs%in%highlight & rhs%in%highlight) & op!='~~')], aes(x=xstart+boxWidth, y=ystart, xend=xend, yend=yend), 
				arrow=arrow(length=unit(lineWidth*.25,'cm')), size=lineWidth, color="gray89") + 
		  geom_segment(data=edgeTable[(lhs%in%highlight & rhs%in%highlight) & op!='~~'], aes(x=xstart+boxWidth, y=ystart, xend=xend, yend=yend, color=est), 
		               arrow=arrow(length=unit(lineWidth*.25,'cm')), size=lineWidth)
	  } else {
	    p = p + 
	      geom_segment(data=edgeTable[op!='~~'], aes(x=xstart+boxWidth, y=ystart, xend=xend, yend=yend, color=est), 
	                   arrow=arrow(length=unit(lineWidth*.25,'cm')), size=lineWidth)
	  }
	}
	if (curved%in%c(1,2)) { 
		if (tapered) { 
			p = p + geom_path(data=paths[op!='~~'], aes(x=x, y=y, group=group, color=est, size=-sequence*(.25*lineWidth)))
			p = p + geom_segment(data=paths[op!='~~' & sequence==max], aes(x=x_prev, y=y_prev, xend=x, yend=y, group=group, color=est), 
				arrow=arrow(length=unit(lineWidth*.25,'cm')), size=0)
		}
		if (!tapered) { 
			p = p + geom_path(data=paths[op!='~~'], aes(x=x, y=y, group=group, color=est), size=lineWidth)
			p = p + geom_segment(data=paths[op!='~~' & sequence==max], aes(x=x_prev, y=y_prev, xend=x, yend=y, group=group, color=est), 
				arrow=arrow(length=unit(lineWidth*.25,'cm')), size=0)
		}
	}
	# stepwise
	if (curved==3) { 
		p = p + 
			geom_segment(data=edgeTable[op!='~~'], aes(x=xstart, y=ystart, xend=xmid_s, yend=ystart, color=est), size=lineWidth, alpha=.5) + 
			geom_segment(data=edgeTable[op!='~~'], aes(x=xmid_s, y=ystart, xend=xmid_s, yend=yend, color=est), size=lineWidth, alpha=.5) + 
			geom_segment(data=edgeTable[op!='~~'], aes(x=xmid_s, y=yend, xend=xend, yend=yend, color=est), size=lineWidth, alpha=.5, 
				arrow=arrow(length=unit(lineWidth*.25,'cm')))
	}
	
	# add covariances with curvature based on edge length
	if (variances==TRUE) { 
		for(i in which(edgeTable$op=='~~' & edgeTable$rhs!=edgeTable$lhs)) { 		
			# identify edge length
			el = edgeTable[i]$edge_length
			# set direction of curve
			side = ifelse(edgeTable[i]$xstart<mean(edgeTable$xstart), 'left', 'right')
			di = ifelse(side=='left', -1, 1)
			# curved arrows going downward need to bend the other way
			if (edgeTable[i]$yend < edgeTable[i]$ystart) di = -di 
			# add curves to graph (different depending on direction of curve)
			if (side=='right') {
				p = p + 
					geom_curve(data=edgeTable[i], 
						aes(x=xstart+boxWidth, y=ystart, xend=xend+boxWidth, yend=yend, color=est), 
						size=labSize2*.25, curvature=di*(1/(0.4*el)), angle=90)
			}
			if (side=='left') { 
				p = p + 
					geom_curve(data=edgeTable[i], 
						aes(x=xstart, y=ystart, xend=xend, yend=yend, color=est), 
						size=labSize2*.25, curvature=di*(1/(0.4*el)), angle=90)			
			}
		}
	}
	
	# add edge labels
	if (edgeLabels) { 
		if (curved!=3) { 
		  if (dim==TRUE){
		    if (is.null(highlight)){
		      stop("Highlight must not be null if dim is TRUE.")
		    }
		    p = p + geom_text(data=edgeTable[(lhs%in%highlight & rhs%in% highlight) & edgeTable$op!='~~'], aes(x=xmid, y=ymid+(min(edgeTable$ystart)*.25), 
		                                                              label=edge_label), size=labSize2, lwd=0)
		  } else {
			  p = p + geom_text(data=edgeTable[edgeTable$op!='~~'], aes(x=xmid, y=ymid+(min(edgeTable$ystart)*.25), 
				label=edge_label), size=labSize2, lwd=0)
		  }
		}
		if (curved==3) { 
			p = p + geom_text(data=edgeTable[edgeTable$op!='~~'], aes(x=xmid, y=yend, 
				label=edge_label), size=labSize2, lwd=0)
		}
	}
	
	# add nodes
	if (dim == TRUE) {
  	p = p + 
  		# geom_point(data=nodeTable, aes(y=y, x=x), size=labSize2*5, shape=22, fill='white') + 
  		geom_rect(data=nodeTable[!variable%in%highlight], aes(ymin=y-(boxHeight*.5), ymax=y+(boxHeight*.5), xmin=x, xmax=x+boxWidth), 
  			fill='white', color='gray89') + 
  		geom_text(data=nodeTable[!variable%in%highlight], aes(y=y, x=x+(0.05*boxWidth), label=str_wrap(label,19)), size=labSize1, hjust=0, alpha=0.5) + 
  	  geom_rect(data=nodeTable[variable%in%highlight], aes(ymin=y-(boxHeight*.5), ymax=y+(boxHeight*.5), xmin=x, xmax=x+boxWidth), 
  	            fill='white', color='black') + 
  	  geom_text(data=nodeTable[variable%in%highlight], aes(y=y, x=x+(0.05*boxWidth), label=str_wrap(label,19)), size=labSize1, hjust=0)
	} else {
	  p = p + 
	    # geom_point(data=nodeTable, aes(y=y, x=x), size=labSize2*5, shape=22, fill='white') + 
	    geom_rect(data=nodeTable, aes(ymin=y-(boxHeight*.5), ymax=y+(boxHeight*.5), xmin=x, xmax=x+boxWidth), 
	              fill='white', color='black') + 
	    geom_text(data=nodeTable, aes(y=y, x=x+(0.05*boxWidth), label=str_wrap(label,19)), size=labSize1, hjust=0)
	}
	
	# improve legend
	p = p + 
		scale_color_viridis(direction=-1, breaks=seq(colScaleMin, colScaleMax, by=0.5), labels=as.character(seq(colScaleMin, colScaleMax, by=0.5)), limits=c(colScaleMin, colScaleMax)) 
	
	# add buffer space to axes
	ymax = max(nodeTable$y)+(buffer[4]*sd(nodeTable$y))
	ymin = min(nodeTable$y)-(buffer[3]*sd(nodeTable$y))
	xmax = max(nodeTable$x)+(buffer[2]*sd(nodeTable$x))
	xmin = min(nodeTable$x)-(buffer[1]*sd(nodeTable$x))
	p = p + 
		expand_limits(y=c(ymin, ymax), x=c(xmin, xmax)) 
	
	# labels
	capt = paste('Control variables not displayed:', paste(exclVars, collapse =', '))
	capt = str_wrap(capt, 100)
	if (length(exclVars)==0) capt='' 
	p = p + 
		labs(color='Effect\nSize', caption=capt)
	
	# clean up plot
	p = p + theme_void(base_size=16) + theme(legend.position=c(0.5, 0), legend.direction='horizontal', plot.margin=unit(c(t=-.5,r=.75,b=.25,l=-1.5), 'cm'), 
	plot.caption=element_text(size=6))
	if (curved %in% c(1,2)) p = p + scale_size_continuous(guide = FALSE)
	
	# -------------------------------------------------------------------------------
	return(p)
}
