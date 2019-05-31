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
	
	# compute explained variance
	currentLevel[, est.std:=abs(est.std)/sum(abs(est.std)), by=lhs]	
	
	# assign
	currentLevel = currentLevel[order(lhs, rhs)]
	currentLevel[, level:=i]
	assign(paste0('level',i), currentLevel)
	currentLevel$level = NULL
	
	# update outcome vars to the next level down
	outcomeVars = currentLevel$rhs

	# rename
	var = paste0('level', i, '_var')
	estVar = paste0('level', i, '_est')
	setnames(currentLevel, c('rhs', 'est.std', 'se.std'), 
		c(var, estVar, paste0('level', i, '_se')))
	
	# merge levels (many-to-many hence the crazy lapply)
	if (i==1) estimates = copy(currentLevel) 
	if (i>1) {
		prevVar = paste0('level', i-1, '_var')
		estimates = lapply(1:nrow(estimates), function(l) {
			merge(estimates[l], currentLevel, 
				by.x=prevVar, by.y='lhs', all.x=TRUE)
		})
		estimates = rbindlist(estimates)
		estimates[is.na(get(var)), (var):='NA']
	}
	i=i+1
}
# -----------------------------------------------


# -----------------------------------------------
# Reassemble estimates from level1 and 2 in stacked format for graph

tmplevel1 = copy(level1)
tmplevel2 = copy(level2)

# make dummy level 0
level0 = data.table(lhs='Parent', rhs='lead_malariaDeaths_rate', 
	est.std=0, se.std=0, level=0)
estimates = rbind(level0, tmplevel1)

# sum level 2 to level 1 by outcome
for(l in unique(tmplevel1$rhs)) { 
	tmplevel2[lhs==l, est.std:=est.std*tmplevel1[rhs==l]$est.std]
}

# add row to continue higher-level unexplained
tmplevel2 = rbind(tmplevel2, data.table(lhs='unexplained',rhs='unexplained',
	est.std=tmplevel1[rhs=='unexplained']$est.std, se.std=0, level=2))
	
# add level 2 to estimates
estimates = rbind(estimates, tmplevel2)

# fill everything by its highest-level outcome
estimates[level==1, fill:=rhs]
estimates[level==2, fill:=lhs]

# store as character for discrete levels
estimates[, level:=as.character(level)]
# -----------------------------------------------


# -----------------------------------------------
# Reassemble estimates leading up to ITN coverage

estimates2 = means[grepl('ITN',lhs) & (op=='~' | lhs==rhs) & !grepl('completeness', rhs)]
estimates2$op=NULL
estimates2[lhs==rhs, rhs:='unexplained']
estimates2[, est.std:=abs(est.std)/sum(abs(est.std)), by=lhs]
estimates2[lhs=='ITN_rate_cumul', level:=1]
estimates2[lhs=='ITN_consumed_cumulative', level:=2]
estimates2[lhs=='ITN_received_cumulative', level:=3]
estimates2 = estimates2[!which(rhs=='unexplained' & se.std==0)]

estimates2 = estimates2[level!=1]

# make level 3 sum to level 2 explained variance
estimates2[, level2_sum:=sum(estimates2[level==2 & rhs!='unexplained']$est.std)]
estimates2[level==3, est.std:=est.std*level2_sum]
estimates2$level2_sum=NULL

# add higher-level unexplained
estimates2 = rbind(estimates2, data.table(lhs='unexplained',rhs='unexplained',
	est.std=estimates2[level==2][rhs=='unexplained']$est.std, 
	se.std=0, level=3), fill=TRUE)

# add level 0
estimates2 = rbind(estimates2, data.table(lhs='Parent',rhs='Parent',
	est.std=0, se.std=0, level=0), fill=TRUE)
	
estimates2[level==2, fill:=rhs]
estimates2[level==3, fill:=lhs]

estimates2[, level:=as.character(level)]
# -----------------------------------------------


# -----------------------------------------------
# Set up to graph

# bring in labels
level1Graph = merge(level1, nodeTable, by.x='rhs', by.y='variable', all.x=TRUE)
level1Graph[is.na(label), label:='-Unexplained by Model-']
level2Graph = merge(level2, nodeTable, by.x='rhs', by.y='variable', all.x=TRUE)
level2Graph[is.na(label), label:='-Unexplained by Model-']
estimatesGraph = merge(estimates, nodeTable, by.x='rhs', by.y='variable', all.x=TRUE)
estimatesGraph[rhs=='unexplained', label:='-Unexplained by Model-']
estimatesGraph[rhs=='unexplained' & lhs=='unexplained', label:='']
level3Graph = merge(level3, nodeTable, by.x='rhs', by.y='variable', all.x=TRUE)
level3Graph[is.na(label), label:='-Unexplained by Model-']
level4Graph = merge(level4, nodeTable, by.x='rhs', by.y='variable', all.x=TRUE)
level4Graph[is.na(label), label:='-Unexplained by Model-']
level4Graph[rhs=='date', label:='Time Trend']
level5Graph = merge(level5, nodeTable, by.x='rhs', by.y='variable', all.x=TRUE)
level5Graph[is.na(label), label:='-Unexplained by Model-']
level5Graph[rhs=='date', label:='Time Trend']
level6Graph = merge(level5, nodeTable, by.x='rhs', by.y='variable', all.x=TRUE)
level6Graph[is.na(label), label:='-Unexplained by Model-']
level6Graph[rhs=='date', label:='Time Trend']
estimatesGraph2 = merge(estimates2, nodeTable, by.x='rhs', by.y='variable', all.x=TRUE)
estimatesGraph2[rhs=='unexplained', label:='-Unexplained by Model-']
estimatesGraph2[rhs=='unexplained' & lhs=='unexplained', label:='']

# aggregate funders
level4Graph[grepl('exp_',rhs), label:='Global Fund']
level4Graph[grepl('ghe_',rhs), label:='Government']
level4Graph[grepl('other_dah_',rhs), label:='All Other Donors']
level4Graph = level4Graph[, .(est.std=sum(est.std)), by=c('lhs','label','level')]
level5Graph[grepl('exp_',rhs), label:='Global Fund']
level5Graph[grepl('ghe_',rhs), label:='Government']
level5Graph[grepl('other_dah_',rhs), label:='All Other Donors']
level5Graph = level5Graph[, .(est.std=sum(est.std)), by=c('lhs','label','level')]
level6Graph[grepl('exp_',rhs), label:='Global Fund']
level6Graph[grepl('ghe_',rhs), label:='Government']
level6Graph[grepl('other_dah_',rhs), label:='All Other Donors']
level6Graph = level6Graph[, .(est.std=sum(est.std)), by=c('lhs','label','level')]

# add empty holes
hole = level1Graph[1]
for(v in names(hole)) { 
	if (class(hole[[v]])=='character') hole[, (v):='']
	if (class(hole[[v]])=='numeric') hole[, (v):=0]
}
hole[, hole:=1]
for(i in 1:6) { 
	get(paste0('level',i,'Graph'))[,x:=1]
	assign(paste0('level',i,'Graph'), 
		rbind(get(paste0('level',i,'Graph')), hole, fill=TRUE))
}

# colors
cols = brewer.pal(12, 'Paired')
cols = c('#969696', cols)
# -----------------------------------------------


# -----------------------------------------------
# Graph

# pie chart of contributors to mortality
p1 = ggplot(level1Graph, aes(y=est.std, x=x, fill=label)) + 
	geom_bar(width=1, color='gray90', stat='identity', position='stack') + 
	geom_text(aes(label=label), size=3, position=position_stack(vjust=.5)) +
	annotate('text', label='Declining\nMortality\nRates', y=0, x=-0.5, size=5) +
	coord_polar(theta='y') + 
	scale_fill_manual('', values=cols) +
	labs(title='Impact on Mortality Rates') + 
	theme_void() + 
	theme(legend.position='none')

# pie chart of contributors to case fatality
p2 = ggplot(level2Graph[lhs=='lead_case_fatality' | hole==1], 
		aes(y=est.std, x=x, fill=label)) + 
	geom_bar(width=1, color='gray90', stat='identity', position='stack') + 
	geom_text(aes(label=label), size=3, position=position_stack(vjust=.5)) +
	annotate('text', label='Declining\nCase Fatality', y=0, x=-0.5, size=5) +
	coord_polar(theta='y') + 
	scale_fill_manual('', values=cols) +
	labs(title='Impact on Case Fatality') + 
	theme_void() + 
	theme(legend.position='none')

# pie chart of contributors to mild incidence
p3 = ggplot(level2Graph[lhs=='lead_newCasesMalariaMild_rate' | hole==1], 
		aes(y=est.std, x=x, fill=label)) + 
	geom_bar(width=1, color='gray90', stat='identity', position='stack') + 
	geom_text(aes(label=label), size=3, position=position_stack(vjust=.5)) +
	annotate('text', label='Declining\nIncidence Rates\n(mild)', y=0, x=-0.5, size=5) +
	coord_polar(theta='y') + 
	scale_fill_manual('', values=cols) +
	labs(title='Impact on Incidence Rates (mild)') + 
	theme_void() + 
	theme(legend.position='none')

# pie chart of contributors to severe incidence
p4 = ggplot(level2Graph[lhs=='lead_newCasesMalariaSevere_rate' | hole==1], 
		aes(y=est.std, x=x, fill=label)) + 
	geom_bar(width=1, color='gray90', stat='identity', position='stack') + 
	geom_text(aes(label=label), size=3, position=position_stack(vjust=.5)) +
	annotate('text', label='Declining\nIncidence Rates\n(severe)', y=0, x=-0.5, size=5) +
	coord_polar(theta='y') + 
	scale_fill_manual('', values=cols) +
	labs(title='Impact on Incidence Rates (severe)') + 
	theme_void() + 
	theme(legend.position='none')

# sunburst of last two levels	
p5 = ggplot(estimatesGraph, aes(x=level, y=est.std, fill=fill, alpha=level)) +
	geom_col(width=1, color='gray80', size=0.3, position=position_stack()) +
	geom_text_repel(aes(label=label), size=2.5, position=position_stack(vjust=0.5)) +
	annotate('text', 1.25, 0, label='Declining\nMortality\nRates', size=5, vjust=1.25) +
	coord_polar(theta='y') +
	scale_alpha_manual(values=c('0'=0, '1'=1, '2'=0.6), guide=F) +
	scale_fill_manual('', values=rev(cols[1:4])) +
	theme_void() + 
	theme(legend.position='none')

# pie chart of contributors to mild treatment
p6 = ggplot(level3Graph[lhs=='mildMalariaTreated_rate' | hole==1], 
		aes(y=est.std, x=x, fill=label)) + 
	geom_bar(width=1, color='gray90', stat='identity', position='stack') + 
	geom_text(aes(label=label), size=3, position=position_stack(vjust=.5)) +
	annotate('text', label='Increasing\nTreatment Coverage\n(mild)', y=0, x=-0.5, size=5) +
	coord_polar(theta='y') + 
	scale_fill_manual('', values=cols) +
	labs(title='Impact on Treatment Coverage (mild)') + 
	theme_void() + 
	theme(legend.position='none')

# pie chart of contributors to severe treatment
p7 = ggplot(level3Graph[lhs=='severeMalariaTreated_rate' | hole==1], 
		aes(y=est.std, x=1, fill=label)) + 
	geom_bar(width=1, color='gray90', stat='identity', position='stack') + 
	geom_text(aes(label=label), size=3, position=position_stack(vjust=.5)) +
	annotate('text', label='Increasing\nTreatment Coverage\n(severe)', y=0, x=-0.5, size=5) +
	coord_polar(theta='y') + 
	scale_fill_manual('', values=cols) +
	labs(title='Impact on Treatment Coverage (severe)') + 
	theme_void() + 
	theme(legend.position='none')

# pie chart of contributors to itn coverage
p8 = ggplot(level3Graph[lhs=='ITN_rate_cumul' | hole==1], 
		aes(y=est.std, x=x, fill=label)) + 
	geom_bar(width=1, color='gray90', stat='identity', position='stack') + 
	geom_text(aes(label=label), size=3, position=position_stack(vjust=.5)) +
	annotate('text', label='Increasing\nITN Coverage', y=0, x=-0.5, size=5) +
	coord_polar(theta='y') + 
	scale_fill_manual('', values=cols) +
	labs(title='Impact on ITN Coverage') + 
	theme_void() + 
	theme(legend.position='none')

# pie chart of contributors to act shipment
p9 = ggplot(level6Graph[(lhs=='ACT_received_cumulative' & label!='Time Trend') | 
		hole==1], aes(y=est.std, x=x, fill=label)) + 
	geom_bar(width=1, color='gray90', stat='identity', position='stack') + 
	geom_text(aes(label=label), size=3, position=position_stack(vjust=.5)) +
	annotate('text', label='Increasing\nACT Distribution', y=0, x=-0.5, size=5) +
	coord_polar(theta='y') + 
	scale_fill_manual('', values=cols) +
	labs(title='Impact on ACT Distribution') + 
	theme_void() + 
	theme(legend.position='none')

# pie chart of contributors to itn shipment
p10 = ggplot(level6Graph[(lhs=='ITN_received_cumulative' & label!='Time Trend') | 
		hole==1], aes(y=est.std, x=x, fill=label)) + 
	geom_bar(width=1, color='gray90', stat='identity', position='stack') + 
	geom_text(aes(label=label), size=3, position=position_stack(vjust=.5)) +
	annotate('text', label='Increasing\nITN Shipment', y=0, x=-0.5, size=5) +
	coord_polar(theta='y') + 
	scale_fill_manual('', values=cols) +
	labs(title='Impact on ITN Distribution') + 
	theme_void() + 
	theme(legend.position='none')

# sunburst of ITNs
p11 = ggplot(estimatesGraph2, aes(x=level, y=est.std, fill=fill, alpha=level)) +
	geom_col(width=1, color='gray80', size=0.3, position=position_stack()) +
	geom_text_repel(aes(label=label), size=2.5, position=position_stack(vjust=0.5)) +
	annotate('text', 1, 0, label='Increasing\nITN Distribution', size=5, vjust=1.25) +
	coord_polar(theta='y') +
	scale_alpha_manual(values=c('0'=0, '2'=1, '3'=0.65), guide=F) +
	scale_fill_manual('', values=rev(cols[1:2])) +
	theme_void() + 
	theme(legend.position='none')
sum(estimatesGraph2[level==3][2:3]$est.std)/sum(estimatesGraph2[level==3][2:7]$est.std) # PCT of p11 that is GF
# -----------------------------------------------


# -----------------------------------------------
# Save
print(paste('Saving:', outputFile6c)) 
pdf(outputFile6c, height=5.5, width=9)
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)
print(p7)
print(p8)
print(p9)
print(p10)
print(p11)
dev.off()

archive(outputFile6c)
# -----------------------------------------------

