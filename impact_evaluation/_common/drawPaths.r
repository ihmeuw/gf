# David Phillips
#
# 8/18/2016
# A function that takes a single edge from a data.table of edges draws a path along it for fancier plotting
# Aadapted from https://gist.github.com/dsparks/4331058
# Intended to be called by semGraphForce.r
# Inputs:
# whichRow (must be first argument) - a numeric identifier for the row of the edge to draw
# edges - a data.table with the following columns: lhs rhs op est xstart ystart size_rhs xend yend size_lhs
# len - number of discrete points to draw along the path (i.e. "resolution")
# curved - (numeric) 0=no curve in the paths, 1=one curve, 2=two curves
# boxWidth - (numeric) width of boxes so arrows can connect to the right edge of boxes

# function
drawPaths = function(whichRow, edges, len=100, curved=2, boxWidth=4, lineWidth) {
	# store id's so row numbers don't matter
	edges = edges[op!='~~']
	start = edges[whichRow]$rhs
	end = edges[whichRow]$lhs
	
	# test
	check = any(is.na(edges$xend)) | any(is.na(edges$yend)) | any(is.na(edges$xstart)) | any(is.na(edges$ystart))
	if (check==TRUE) stop('Missing values in coordinates!')
	
	# store the origin and terminus of each edge
	idx = which(edges$rhs==start & edges$lhs==end)
	fromC = as.matrix(edges[idx, c('xstart', 'ystart'), with=FALSE])
	fromC[,'xstart'] = fromC[,'xstart']+boxWidth
	toC = as.matrix(edges[idx, c('xend', 'yend'), with=FALSE])
	
	# no curve, i.e. bezier midpoint that is halfway along the line
	if (curved==0) bezierMid = (fromC + toC) / 2
	
	# add curve
	if (curved==1) {
		graphCenter = as.matrix(nodeTable[, lapply(.SD, mean), .SDcols=c('x','y')]) # Center of the overall graph
		bezierMid = c(fromC[1], toC[2]) # A midpoint, for bended edges (x1, y2)
		distance1 = sum((graphCenter - bezierMid)^2)
		if (distance1 < sum((graphCenter - c(toC[1], fromC[2]))^2)) {
			bezierMid = c(toC[1], fromC[2]) # (x2, y1) for bending the other way
		} # To select the best Bezier midpoint
	}
	
	# add double-curve
	if (curved==2) {
		bezierMid = c(fromC[1], toC[2], toC[1], fromC[2])
		# swap midpoints for edges that go horizontally to their destination
		swapVars = unique(edges$lhs)
		if (edges[whichRow]$lhs %in% swapVars | edges[whichRow]$rhs %in% swapVars) bezierMid = c(bezierMid[c(3,4)], bezierMid[c(1,2)])
		# no midpoints for straight lines
		if (fromC[1]==toC[1] | fromC[2]==toC[2]) bezierMid = c((fromC + toC) / 2, (fromC + toC) / 2)
	}
	
	# draw the path for the current edge
	if (curved %in% c(0,1)) {
		edge = data.frame(bezier(c(fromC[1], bezierMid[1], toC[1]),
							c(fromC[2], bezierMid[2], toC[2]), evaluation=len))
	}
	if(curved==2) {
		edge = data.frame(bezier(c(fromC[1], bezierMid[1], bezierMid[3], toC[1]),
							c(fromC[2], bezierMid[2], bezierMid[4], toC[2]), evaluation=len))	
	}
	
	# store an identifier for the sequence of points on the path
	edge$sequence = 1:len
	
	# identify the edge
	edge$group = paste(edges[whichRow, c('rhs', 'lhs'), with=FALSE], collapse = '>')
	return(edge)
}
