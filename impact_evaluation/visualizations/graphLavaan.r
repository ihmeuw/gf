# Function that makes a diagram of SEM output
# Arguments: 
# fitObject, a lavaan or blavaan object
# edgeLabels, (logical) whether or not to label coefficients
# curved, (numeric) 0=straight lines, 1=1 bend, 2=2 bends
# tapered, (logical) whether to taper edges from start to finish
# Returns: a graph

semGraph = function(fitObject=NULL, modelVarLabels, latentVarLabels, edgeLabels=FALSE, curved=2, tapered=TRUE, title=NULL) {

	# ------------------------
	# Parameters
	
	# appearance of nodes
	size1 = 28
	size2 = 14
	shape1 = 21
	shape2 = 22
	
	# appearance of edges
	smooth = FALSE
	
	# appearance of labels
	labSize1 = 5
	labSize2 = 2
	
	# style 
	style = 'tree'
	style = 'converging'
	# ------------------------
	
	
	# ---------------------------------------------------------------------------------------------
	# Load functions/files
	source('../visualizations/drawPaths.r')
	
	# coordinates
	coordDir = '../supporting_files/layout_coordinates/'
	if (style=='tree') coordFile = paste0(coordDir, 'layout_coordinates_expirmental.csv')
	if (style=='converging') coordFile = paste0(coordDir, 'layout_coordinates_', modelName, '.csv')
	manualCoordinates = fread(coordFile)
	# ---------------------------------------------------------------------------------------------
	
	
	# --------------------------------------------------------------------------------------------
	# Set up data
	
	# extract edges from sem fit
	edges = data.table(lhs=fitObject@ParTable$lhs, op=fitObject@ParTable$op, 
					rhs=fitObject@ParTable$rhs, est=standardizedSolution(fitObject)$est.std)
	edges[, est:=as.numeric(est)]
	edges = edges[op %in% c('~', '=~', '<~')]
	
	# identify the 'outcome'
	outcome = unique(edges$lhs[!edges$lhs %in% edges$rhs])
	
	# extract nodes from sem fit
	nodes = rbind(modelVarLabels[, 1:2, with=FALSE], latentVarLabels)
	nodes[Variable %in% latentVarLabels$Variable, type:='latent']
	nodes[Variable %in% modelVarLabels$Variable, type:='measurement']
	nodes[Variable==outcome, type:='outcome']
	nodes[, Label:=str_wrap(Label, 9)]
	nodes[, id:=seq_len(.N)]
	
	# label nodes with their display size
	nodes[type %in% c('latent', 'outcome'), size:=size1]
	nodes[type=='measurement', size:=size2]
	
	# label edges with ids
	ids = nodes[, c('id', 'Variable'), with=FALSE]
	edges = merge(edges, ids, by.x='rhs', by.y='Variable')
	edges = merge(edges, ids, by.x='lhs', by.y='Variable')
	setnames(edges, c('id.x', 'id.y'), c('id_rhs', 'id_lhs'))
	# --------------------------------------------------------------------------------------------
	
	
	# -------------------------------------------------------------------------
	# Lay out coordinates for nodes
	
	# manual layout
	manualCoordinates[,variable:=gsub('penta1',outcomeStub,variable)]
	manualCoordinates = manualCoordinates[,c('variable','x','y'),with=FALSE]
	manualCoordinates = manualCoordinates[match(ids$Variable, variable),]
	manualCoordinates = manualCoordinates[, c('x','y'), with=FALSE]
	layoutCoordinates = manualCoordinates
	
	# store coordinates with the nodes for ggplot
	nodes = cbind(nodes, data.table(layoutCoordinates))
	# -------------------------------------------------------------------------
	
	
	# ---------------------------------------------------------------------------------------------------
	# Identify start/end coordinates of edges
	
	# store edge coordinates with edges (also label the type of each variable)
	vars = c('id', 'x', 'y', 'size')
	edges = merge(edges, nodes[, vars, with=FALSE], by.x='id_rhs', by.y='id')
	edges = merge(edges, nodes[, vars, with=FALSE], by.x='id_lhs', by.y='id', suffixes=c('_rhs','_lhs'))

	# figure out the direction of each group of edges 
	# FIX ME DOING THIS MANUALLY FOR NOW
	edges[lhs==outcome & op=='~', direction:='horizontal']	
	edges[lhs==outcome & op=='~' & rhs %in% c('child_age', 'access'), direction:='vertical']	
	edges[lhs=='readiness' & op=='<~', direction:='horizontal']	
	edges[lhs=='access' & op=='<~', direction:='vertical']	
	edges[lhs=='intent' & op=='<~', direction:='horizontal']	
	edges[lhs=='attitudes' & op=='=~', direction:='horizontal']	
	edges[lhs=='norms' & op=='<~', direction:='horizontal']	
	edges[lhs=='norms' & grepl('community_', rhs) & grepl('_cvg', rhs), direction:='vertical']	
	edges[lhs=='perceived_control' & op=='~', direction:='vertical']	
	edges[lhs=='perceived_control' & op=='=~', direction:='horizontal']	
	edges[lhs=='supply' & op=='~', direction:='vertical']	
	edges[lhs=='supply' & op=='=~', direction:='horizontal']	
	edges[lhs=='workforce' & op=='~', direction:='vertical']	
	edges[lhs=='workforce' & op=='=~', direction:='horizontal']	
	
	# swap coordinates and labels for measurement variables (this makes the lhs always the side of the arrow)
	edges[op=='=~', c('id_lhs', 'id_rhs') := .(id_rhs, id_lhs)]
	edges[op=='=~', c('lhs', 'rhs') := .(rhs, lhs)]
	edges[op=='=~', c('x_lhs', 'x_rhs') := .(x_rhs, x_lhs)]
	edges[op=='=~', c('y_lhs', 'y_rhs') := .(y_rhs, y_lhs)]
	edges[op=='=~', c('size_lhs', 'size_rhs') := .(size_rhs, size_lhs)]
	
	# trim edges by fixed coordinates
	if (size1==32) {
		edges[direction=='horizontal' & size_lhs==size1 & x_lhs>0, x_lhs:=x_lhs+.03]
		edges[direction=='horizontal' & size_lhs==size1 & x_lhs<0, x_lhs:=x_lhs-.03]
		edges[direction=='vertical' & size_lhs==size1 & y_lhs>0, y_lhs:=y_lhs+.165]
		edges[direction=='vertical' & size_lhs==size1 & y_lhs<0, y_lhs:=y_lhs-.165]
		edges[lhs==outcome & rhs=='intent', x_lhs:=x_lhs-.025]
		edges[lhs==outcome & rhs=='readiness', x_lhs:=x_lhs+.025]
		edges[lhs==outcome & rhs=='child_age', y_lhs:=y_lhs+.15]
		edges[lhs==outcome & rhs=='access', y_lhs:=y_lhs-.15]
	}
	if (size1==28) { 
		edges[direction=='horizontal' & size_lhs==size1 & x_lhs>0, x_lhs:=x_lhs+.025]
		edges[direction=='horizontal' & size_lhs==size1 & x_lhs<0, x_lhs:=x_lhs-.025]
		edges[direction=='vertical' & size_lhs==size1 & y_lhs>0, y_lhs:=y_lhs+.15]
		edges[direction=='vertical' & size_lhs==size1 & y_lhs<0, y_lhs:=y_lhs-.15]
		edges[lhs==outcome & rhs=='intent', x_lhs:=x_lhs-.0225]
		edges[lhs==outcome & rhs=='readiness', x_lhs:=x_lhs+.0225]
		edges[lhs==outcome & rhs=='child_age', y_lhs:=y_lhs+.13]
		edges[lhs==outcome & rhs=='access', y_lhs:=y_lhs-.13]
	}
	if (size2==14) { 
		edges[direction=='horizontal' & size_lhs==size2 & x_lhs>0, x_lhs:=x_lhs-.012]
		edges[direction=='horizontal' & size_lhs==size2 & x_lhs<0, x_lhs:=x_lhs+.012]
		edges[direction=='vertical' & size_lhs==size2 & y_lhs>0, y_lhs:=y_lhs-.175]
		edges[direction=='vertical' & size_lhs==size2 & y_lhs<0, y_lhs:=y_lhs+.175]
	}
	# ---------------------------------------------------------------------------------------------------
	
	
	# --------------------------------------------------------------------------------------------------
	# Generate paths for edges
	
	# generate an edge path for each pair of connected nodes using edgeMaker
	len = 500
	paths = lapply(1:nrow(edges), drawPaths, edges=edges, len=len, curved=curved, smooth=smooth)
	paths = data.table(do.call(rbind, paths))
	
	# identify the type of the end of each edge
	paths[, c('start', 'end'):=tstrsplit(group, '>', fixed=TRUE)]
	paths[, start:=as.numeric(start)]
	paths[, end:=as.numeric(end)]
	paths = merge(paths, nodes[, c('id', 'type'), with=FALSE], by.x='end', by.y='id')	
	
	# add weights/operators to paths
	vars = c('id_rhs', 'id_lhs', 'est', 'op')
	paths = merge(paths, edges[, vars, with=FALSE], by.x=c('start', 'end'), by.y=c('id_rhs', 'id_lhs'))	
	
	# make the paths associated with observed variables smaller by altering the "sequence" variable
	mIds = nodes[type=='measurement']$id
	newScale = seq(len/2,len,length=len)
	paths[start %in% mIds | end %in% mIds, sequence:=newScale]
	
	# identify last segment of each path for arrows
	paths[, x_prev:=shift(x), by='group']
	paths[, y_prev:=shift(y), by='group']
	paths[, max:=max(sequence), by='group'] 
	
	# identify middle of each path for coefficient labels
	paths[, mid:=floor(median(sequence)), by='group']
	paths[sequence==mid, mid_id:=seq_len(.N), by=group]
	paths[mid_id==2, mid:=NA]
	# --------------------------------------------------------------------------------------------------
	
	
	# --------------------------------------------------------------------------------------------------
	# Discretize path colors into 12 bins for better control over colors
	min = floor(min(edges$est))
	max = ceiling(max(edges$est))
	breaks = c(min, quantile(c(min, 0), c(.5, .9)), -.01, quantile(c(0, max), c(.1, .5, .75)), max)
	paths[, est_cat:=.bincode(est, breaks, right=FALSE, include.lowest=TRUE)]
	means = paths[, mean(est), by='est_cat'][order(est_cat)]
	paths = merge(paths, means, by='est_cat')
	paths[, est_cat:=V1]
	paths[, V1:=NULL]
	# --------------------------------------------------------------------------------------------------
	
	
	# -------------------------------------------------------------------------------
	# Make Graph
	
	# empty ggplot2 theme
	new_theme_empty = theme_bw()
	new_theme_empty$line = element_blank()
	new_theme_empty$rect = element_blank()
	new_theme_empty$strip.text = element_blank()
	new_theme_empty$axis.text = element_blank()
	new_theme_empty$axis.title = element_blank()
	new_theme_empty$plot.margin = structure(c(0,-.5,0,-.5), unit='lines', valid.unit=3L, class='unit')
	# new_theme_empty$legend.position = c(0.8, 0.2)
	new_theme_empty$legend.position = 'none'
	
	# start graph
	p = ggplot() 
	
	# add edges
	a = arrow(length=unit(.6, 'cm'), type='closed')
	if (tapered) { 
		p = p + geom_path(data=paths, aes(x=x, y=y, group=group, color=factor(est_cat), size=-sequence))
		p = p + geom_segment(data=paths[sequence==max], aes(x=x_prev, y=y_prev, xend=x, yend=y, group=group, color=factor(est_cat)), arrow=a, size=0)
	}
	if (!tapered) { 
		p = p + geom_path(data=paths, aes(x=x, y=y, group=group, color=factor(est_cat)), size=2.5)
		p = p + geom_segment(data=paths[sequence==max], aes(x=x_prev, y=y_prev, xend=x, yend=y, group=group, color=factor(est_cat)), arrow=a, size=0)
	}
	
	# add nodes
	# use separate geoms for each type to have more control (ggplot gets confused with multiple scales)
	latColor = 'white'
	obsColor = 'white'
	outColor = '#f0f0f0' 
	p = p + geom_point(data=nodes[type=='latent'], aes(x=x, y=y), size=size1, shape=shape1, fill=latColor) 
	p = p + geom_point(data=nodes[type=='measurement'], aes(x=x, y=y), size=size2, shape=shape2, fill=obsColor)
	p = p + geom_point(data=nodes[type=='outcome'], aes(x=x, y=y), size=size1, shape=shape2, fill=outColor)
	
	# label nodes
	p = p + geom_text(data=nodes[type %in% c('latent', 'outcome')], aes(x=x, y=y, label=Label), size=labSize1)
	p = p + geom_text(data=nodes[type=='measurement'], aes(x=x, y=y, label=Label), size=labSize2)
	
	# label edges if specified
	if (edgeLabels) { 
		p = p + geom_label(data=paths[sequence==mid], aes(x=x, y=y, label=round(est, 2)), fill='white', color='white', label.padding=unit(0.1, 'lines'), label.r=unit(0.01, 'lines'), size=2) # this makes a white background for the text to sit on
		p = p + geom_text(data=paths[sequence==mid], aes(x=x, y=y, label=round(est, 2)), size=3)
	}
	
	# modify line color
	legendBreaks = c(min, quantile(c(min, 0), .75), 0, quantile(c(0, max), .25), max)
    greens = brewer.pal(9, 'Blues')[c(3,3,5,6,7,8,9)]
    reds = brewer.pal(9, 'Reds')[c(3,5,6,7,8)]
	colors = c(rev(reds), greens)
	colorNames = c(unique(paths$est_cat), legendBreaks)
	names(colors) = colorNames[order(colorNames)]
	p = p + scale_color_manual(values=colors)
	
	# manual legend
	# min = floor(min(edges$est))
	# max = ceiling(max(edges$est))
	if (style=='tree') { 
		inc = .24
		legendLoc = data.table(x=max(nodes$x-(.75*inc)), y=max(nodes$y-(.45*inc)))
		p = p + geom_point(data=legendLoc, aes(x=x+(.1*inc), y=y-.25*inc), shape=shape1, size=size1-10, fill=latColor)
		p = p + geom_point(data=legendLoc, aes(x=x+(.1*inc), y=y-1.75*inc), shape=shape2, size=size1-10, fill=obsColor)
		p = p + geom_text(data=legendLoc, aes(x=x+(.1*inc), y=y-.25*inc), label='Latent/nVariable', size=labSize1-2)
		p = p + geom_text(data=legendLoc, aes(x=x+(.1*inc), y=y-1.75*inc), label='Observed/nVariable', size=labSize1-2)
		p = p + geom_text(data=legendLoc, aes(x=x, y=y-2.6*inc), label='Effect', size=labSize1-2)
		pathLen = 10
		legendPathsy = seq(from=legendLoc$y-(3*inc), to=legendLoc$y-(length(legendBreaks)*inc*.95), len=length(legendBreaks))
		legendPaths = data.table(group=seq(length(legendBreaks)), y=legendPathsy)
		
		legendPaths = data.table(group=rep(legendPaths$group, each=pathLen), 
			y=rep(legendPaths$y, each=pathLen), 
			breaks=rep(round(legendBreaks,1),each=pathLen), 
			sequence=rep(seq(from=1,to=len,len=pathLen),pathLen))
		
		legendPaths[, x:=rep(seq(from=legendLoc$x-(.25*inc), to=legendLoc$x+(.4*inc), len=pathLen), pathLen)]
		
		a = arrow(length=unit(.25, 'cm'), type='closed')
		p = p + geom_path(data=legendPaths, aes(x=x, y=y, group=group, color=factor(breaks)), arrow=a, size=1.15)
		p = p + geom_text(data=NULL, aes(x=legendLoc$x-(.4*inc), y=legendPathsy, label=round(breaks,1)), size=labSize1-2)
	}
	if (style=='converging') { 
		inc = .12
		legendLoc = data.table(x=quantile(nodes$x,.275), y=max(nodes$y-(1.2*inc)))
		p = p + geom_point(data=legendLoc, aes(x=x+(2.75*inc), y=y-.25*inc), shape=shape1, size=size1-10, fill=latColor)
		p = p + geom_point(data=legendLoc, aes(x=x+(2.75*inc), y=y-2.25*inc), shape=shape2, size=size1-10, fill=obsColor)
		p = p + geom_text(data=legendLoc, aes(x=x+(2.75*inc), y=y-.25*inc), label='Latent\nVariable', size=labSize1-2)
		p = p + geom_text(data=legendLoc, aes(x=x+(2.75*inc), y=y-2.25*inc), label='Observed\nVariable', size=labSize1-2)
		p = p + geom_text(data=legendLoc, aes(x=x+(1.45*inc), y=y+.35*inc), label='Effect', size=labSize1-2)
		pathLen = 10
		legendPathsy = seq(from=legendLoc$y-(.15*inc), to=legendLoc$y-(length(legendBreaks)*inc*.425), len=length(legendBreaks))
		legendPaths = data.table(group=seq(length(legendBreaks)), y=legendPathsy)
		
		legendPaths = data.table(group=rep(legendPaths$group, each=pathLen), 
			y=rep(legendPaths$y, each=pathLen), 
			breaks=rep(legendBreaks,each=pathLen), 
			sequence=rep(seq(from=1,to=len,len=pathLen),pathLen))
		
		legendPaths[, x:=rep(seq(from=legendLoc$x+(1.3*inc), to=legendLoc$x+(1.6*inc), len=pathLen), pathLen)]
		
		a = arrow(length=unit(.25, 'cm'), type='closed')
		p = p + geom_path(data=legendPaths, aes(x=x, y=y, group=group, color=factor(breaks)), arrow=a, size=1.15)
		p = p + geom_text(data=NULL, aes(x=legendLoc$x+(1.25*inc), y=legendPathsy, label=legendBreaks), size=labSize1-2)
	}
	
	# modify line taper
	if (tapered) p = p + scale_size(range=c(.01, 5), guide='none')
	
	# apply empty theme
	p = p + new_theme_empty
	
	# add title
	if (!is.null(title)) p = p + geom_text(aes(y=max(nodes$y), x=0, label=title), hjust='center', size=8)
	# -------------------------------------------------------------------------------
	
	return(p)
}
