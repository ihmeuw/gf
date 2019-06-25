# -----------------------------------
# David Phillips
# 
# 3/26/2019
# Analyze explained variance along the full results chain, i.e. impact analysis
# -----------------------------------


# to do
# show % explained by 'data quality' (completeness)?

# -----------------------------------------------
# Load/prep data and functions

source('./impact_evaluation/drc/set_up_r.r')
library(RColorBrewer)

# load model results
load(outputFile5a)
means1 = copy(means)
data1 = copy(data)
load(outputFile5b)
means2 = copy(means)
data2 = copy(data)

# put together coefficient tables
means = rbind(means1, means2)

# standardize variable names (should be fixed earlier)
origVars = c('ITN','SSCACT','RDT','SP','severeMalariaTreated','mildMalariaTreated',
	'SSCACT_under5','severeMalariaTreated_under5','mildMalariaTreated_under5')
newVars = c('ITN_consumed_cumulative','ACTs_SSC_cumulative','RDT_completed_cumulative',
	'SP_cumulative','severeMalariaTreated_cumulative','totalPatientsTreated_cumulative',
	'ACTs_SSC_under5_cumulative','severeMalariaTreated_under5_cumulative',
	'totalPatientsTreated_under5_cumulative')
for(i in seq(length(origVars))) {
	means[lhs==origVars[i], lhs:=newVars[i]]
	means[rhs==origVars[i], rhs:=newVars[i]]
}	

# load nodeTable for graphing
nodeTable1 = fread(nodeTableFile1)
nodeTable2 = fread(nodeTableFile2)

# ensure there are no extra variables introducted from nodeTable
nodeTable1 = nodeTable1[variable %in% names(data1)]
nodeTable2 = nodeTable2[variable %in% names(data2)]
nodeTable = rbind(nodeTable1, nodeTable2)
nodeTable[, label:=gsub('Lead of','',label)]
# -----------------------------------------------


# -----------------------------------------------
# Compute explained variance

# establish the outcome variable
outcomeVar = 'lead_malariaDeaths_rate'
byVars = c('lhs', 'rhs', 'est.std', 'se.std')

# prep each level of coefficients
outcomeVars = outcomeVar
i=1
while(any(outcomeVars %in% means$lhs)) { 
	print(paste('Level:', i))
	currentLevel = means[lhs%in%outcomeVars & op=='~', byVars, with=FALSE]

	# get unexplained
	unexplained = means[lhs%in%outcomeVars & rhs%in%outcomeVars & lhs==rhs, byVars, with=FALSE]
	unexplained[!grepl('completeness',rhs) & !grepl('completeness',lhs), rhs:='unexplained']
	unexplained[, est.std:=est.std^2]

	# drop completeness controls
	currentLevel = rbind(currentLevel, unexplained)
	
	# drop completeness controls and rescale
	currentLevel = currentLevel[!grepl('completeness',rhs)]
	currentLevel = currentLevel[!grepl('completeness',lhs)]	
	
	# drop fixed variances and covariances
	currentLevel = currentLevel[se.std!=0]
	
	# compute explained variance
	currentLevel[, est.std:=abs(est.std)/sum(abs(est.std)), by=lhs]	
	
	# assign
	currentLevel = currentLevel[order(lhs, rhs)]
	currentLevel[, level:=i]
	assign(paste0('level',i), currentLevel)
	
	# update outcome vars to the next level down
	outcomeVars = currentLevel$rhs
	i=i+1
}
nLevels = i-1
# -----------------------------------------------


# -----------------------------------------------
# Function that sets up for a 2-level sunburst
# Inputs: 
# var (character) - name of a variable to center on
# pcts (logical) - whether to add percentages to slice labels
setup2LevelSB = function(var='ITN_consumed_cumulative', pcts=TRUE) {
	
	# find the most distal set of estimates that contain the given variable
	for(i in rev(seq(nLevels))) {
		if (var %in% get(paste0('level',i))[['lhs']]) tmplevel1 = copy(get(paste0('level',i)))
	}
		
	# subset to given variable
	tmplevel1 = tmplevel1[lhs==var]

	# find the most distal set of estimates that contain the child variables to the given variable	
	tmplevel2=copy(tmplevel1)
	for(i in rev(seq(nLevels))) {
		childVars = unique(tmplevel1$rhs)
		childVars = childVars[childVars!='unexplained']
		if (any(childVars %in% get(paste0('level',i))[['lhs']])) tmplevel2 = copy(get(paste0('level',i)))
	}
	
	# subset to given variable
	tmplevel2 = tmplevel2[lhs %in% childVars]
	
	# relabel levels
	tmplevel1[, level:=1]
	tmplevel2[, level:=2]
	
	# drop fixed covariances
	tmplevel1 = tmplevel1[!which(rhs=='unexplained' & se.std==0)]
	tmplevel2 = tmplevel2[!which(rhs=='unexplained' & se.std==0)]
	
	# make dummy level 0
	level0 = data.table(lhs='Parent', rhs=var, est.std=0, se.std=0, level=0)
	out = rbind(level0, tmplevel1)
	
	# sum level 2 to level 1 by outcome
	for(l in unique(tmplevel1$rhs)) { 
		tmplevel2[lhs==l, est.std:=(est.std/sum(est.std))*tmplevel1[rhs==l]$est.std]
	}
	
	# add row to continue higher-level unexplained
	tmplevel2 = rbind(tmplevel2, data.table(lhs='unexplained',rhs='unexplained',
		est.std=tmplevel1[rhs=='unexplained']$est.std, se.std=0, level=2))
		
	# add rows for "unsaturated" sunbursts
	for(v in tmplevel1$rhs) {
		if (!v %in% tmplevel2$lhs) {
			tmplevel2 = rbind(tmplevel2, data.table(lhs=v,rhs=v,
				est.std=tmplevel1[rhs==v]$est.std, se.std=0, level=2))			
		}
	}
	
	# add level 2 to estimates
	out = rbind(out, tmplevel2)

	# fill everything by its highest-level outcome
	out[level==1, fill:=rhs]
	out[level==2, fill:=lhs]

	# store as character for discrete levels
	out[, level:=as.character(level)]
	
	# bring in labels
	out = merge(out, nodeTable, by.x='rhs', by.y='variable', all.x=TRUE)
	out[rhs=='unexplained', label:='-Unexplained by Model-']
	out[is.na(label), label:='']
	if (pcts==TRUE) out[label!='', label:=paste(label, '-', round(est.std*100, 1), '%')]
	out[rhs==lhs & level==2, label:='']
	out[rhs=='population', label:=paste('Population Control -', round(est.std*100, 1), '%')]
	
	# return
	return(out)
}
# -----------------------------------------------


# --------------------------------------------------------------------------------------------
# Graph sunbursts

# colors
cols = brewer.pal(12, 'Paired')
cols = c('#969696', cols)

# store variables
outcomeVars = NULL
for(i in seq(nLevels-2)) outcomeVars = c(outcomeVars, unique(get(paste0('level',i))[['lhs']]))
outcomeVars = unique(outcomeVars)

# make one sunburst per variable
sunBursts = lapply(rev(outcomeVars), function(v) { 
	# prep data
	graphData = setup2LevelSB(v, TRUE)
	
	# count necessary colors
	c = length(unique(graphData[level==2]$lhs))
	
	# skip if no second-level variables
	if (c>1) {
	
	# get label
	l = nodeTable[variable==v]$label
	l = paste('Increasing\n', l)
	if (v %in% c('lead_malariaDeaths_rate', 'lead_case_fatality', 
		'lead_newCasesMalariaMild_rate', 'lead_newCasesMalariaSevere_rate')) {
		l = gsub('Increasing', 'Declining', l)
	}
	
	# label sizes based on the number of labels
	nLabels = length(graphData[label!='']$label)
	if (nLabels<5) s=3.5
	if (nLabels>=5) s=3
	if (nLabels>=10) s=2.5
	if (nLabels>=20) s=2
	
	# graph
	ggplot(graphData, aes(x=as.numeric(level), y=est.std, fill=fill, alpha=level)) +
		geom_col(width=1, color='gray80', size=0.3, position=position_stack()) +
		geom_text_repel(aes(label=label, x=as.numeric(level)+.5), size=s, position=position_stack(vjust=0.5), segment.color='black') +
		annotate('text', 0, 0, label=l, size=5, vjust=1.25) +
		coord_polar(theta='y') +
		scale_alpha_manual(values=c('0'=0, '1'=1, '2'=0.65), guide=F) +
		scale_fill_manual('', values=rev(cols[1:c])) +
		theme_void() + 
		theme(legend.position='none')
	}
})
# --------------------------------------------------------------------------------------------


# -----------------------------------------------
# Save
print(paste('Saving:', outputFile6c)) 
pdf(outputFile6c, height=5.5, width=9)
for(s in seq(length(sunBursts))) if (!is.null(sunBursts[[s]])) print(sunBursts[[s]])
dev.off()

archive(outputFile6c)
# -----------------------------------------------

