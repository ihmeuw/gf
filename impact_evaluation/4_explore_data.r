# ------------------------------------------------
# David Phillips
# 
# 1/18/2019
# Exploratory visualizations to get a good sense of the impact evaluation data
# The current working directory should be the root of this repo (set manually by user)
# ------------------------------------------------

source('./impact_evaluation/_common/set_up_r.r')

# ----------------------------------------------------------------------------
# Load/prep data

# load
data = readRDS(outputFile3)

# test unique identifiers
test = nrow(data)==nrow(unique(data[,'date', with=F]))
if (test==FALSE) stop(paste('Something is wrong. date does not uniquely identify rows.'))

# last-minute prep that shouldn't be necessary after bugs are fixed
	# combine the two ITN budget categories since FGH can't distinguish
	data[, other_dah_M1_1:=other_dah_M1_1+other_dah_M1_2]
	data$other_dah_M1_2 = NULL
	
	# combine M2 (all case management) with M2_1 (facility tx) for GF budgets (one summary budget from 2015-2017 has it)
	data[, budget_M2_1:=budget_M2_1+budget_M2]
	data$budget_M2 = NULL
	
	# set other_dah to NA (not 0) after 2016
	for(v in names(data)[grepl('other_dah',names(data))]) data[date>=2017, (v):=NA]

# compute cumulative budgets
rtVars = names(data)
rtVars = rtVars[grepl('budget|other_dah', rtVars)]
for(v in rtVars) data[, (paste0(v,'_cumulative')):=cumsum(get(v))]
# ----------------------------------------------------------------------------


# ----------------------------------------------
# Set up to graph

# reshape long for graphs
long = melt(data, id.vars='date')

# parse variable names
long[, metric:=str_split_fixed(variable, '_', 2)[,1]]
long[, indicator:=str_split_fixed(variable, '_', 2)[,2]]
long[metric=='other', metric:='other_dah']
long[metric=='other_dah', indicator:=gsub('dah_','',indicator)]
long[, cumulative:=ifelse(grepl('cumulative',indicator), 'Cumulative', 'Not Cumulative')]
long[grepl('cumulative',indicator), indicator:=gsub('_cumulative', '', indicator)]

# identify modules based on codes
codes = data.table(read_excel(mfFile, sheet='Malaria Interventions'))
codes$Module = NULL
codes$Intervention = NULL
setnames(codes, c('Abbreviated Module','Abbreviated Intervention'), c('module','intervention'))
long = merge(long, codes, by.x='indicator',by.y='Code',all.x=TRUE)
long[intervention=='LLIN: Mass campaign' & metric=='other_dah', intervention:='LLIN: Cont. and Mass Distribution']

# label other indicators nicely
long[is.na(intervention), activity:=ifelse(grepl('received',indicator), 'Activity', 'Output')]

# subset dates
long = long[date>=2010 & date<2019]

# load "node table" for convenient labels FIX THIS FILE PATH
nodeTable = fread('C:/local/gf/impact_evaluation/visualizations/vartable.csv')
# ----------------------------------------------


# ----------------------------------------------
# Make time series graphs

# time series of inputs
p1a = ggplot(long[!is.na(intervention) & metric=='budget' & cumulative=='Not Cumulative'], 
		aes(y=value, x=date, color=intervention)) + 
	geom_line() + 
	geom_point() + 
	labs(title='Time Series - Global Fund', y='Budget', x='Quarter', color='Intervention') + 
	theme_bw(base_size=16)

# time series of cumulative inputs
p1b = ggplot(long[!is.na(intervention) & metric=='budget' & cumulative=='Cumulative'], 
		aes(y=value, x=date, color=intervention)) + 
	geom_line() + 
	geom_point() + 
	labs(title='Time Series - Global Fund', y='Cumulative Budget', x='Quarter', color='Intervention') + 
	theme_bw(base_size=16)


# time series of inputs
p1c = ggplot(long[!is.na(intervention) & metric=='other_dah' & cumulative=='Not Cumulative'], 
		aes(y=value, x=date, color=intervention)) + 
	geom_line() + 
	geom_point() + 
	labs(title='Time Series - Other Development Assistance for Malaria', 
		y='Disbursement', x='Quarter', color='Intervention') + 
	theme_bw(base_size=16)

# time series of cumulative inputs
p1d = ggplot(long[!is.na(intervention) & metric=='other_dah' & cumulative=='Cumulative'], 
		aes(y=value, x=date, color=intervention)) + 
	geom_line() + 
	geom_point() + 
	labs(title='Time Series - Other Development Assistance for Malaria', 
		y='Cumulative Disbursement', x='Quarter', color='Intervention') + 
	theme_bw(base_size=16)

# time series of activities
p2a = ggplot(long[activity=='Activity' & metric=='value'], 
		aes(y=value, x=date, color=indicator)) + 
	geom_line() + 
	geom_point() + 
	labs(title='Time Series - Activities', y='Quantity', x='Quarter', color='Activity') + 
	theme_bw(base_size=16)

# time series of activities' completeness
p2b = ggplot(long[activity=='Activity' & metric=='completeness'], 
		aes(y=value, x=date, color=indicator)) + 
	geom_line() + 
	geom_point() + 
	labs(title='Time Series - Completeness (Activities)', y='Completeness', x='Quarter', color='Activity') + 
	theme_bw(base_size=16)

# time series of outputs
p3a = ggplot(long[activity=='Output' & metric=='value'], 
		aes(y=value, x=date, color=indicator)) + 
	geom_line() + 
	geom_point() + 
	labs(title='Time Series - Outputs', y='Quantity', x='Quarter', color='Output') + 
	theme_bw(base_size=16)

# time series of outputs' completeness
p3b = ggplot(long[activity=='Output' & metric=='completeness'], 
		aes(y=value, x=date, color=indicator)) + 
	geom_line() + 
	geom_point() + 
	labs(title='Time Series - Completeness (Outputs)', y='Completeness', x='Quarter', color='Output') + 
	theme_bw(base_size=16)
# ----------------------------------------------


# ----------------------------------------------
# Make distribution graphs

# histograms of distributions for inputs
p4a = ggplot(long[!is.na(intervention) & cumulative=='Cumulative' & metric=='budget'], aes(x=value)) + 
	geom_histogram() + 
	facet_wrap(~intervention, scales='free') + 
	labs(title='Histograms - Global Fund', y='Frequency (Quarters)', x='Cumulative Budget') + 
	theme_bw(base_size=16)

# histograms of distributions for inputs
p4b = ggplot(long[!is.na(intervention) & cumulative=='Cumulative' & metric=='other_dah'], aes(x=value)) + 
	geom_histogram() + 
	facet_wrap(~intervention, scales='free') + 
	labs(title='Histograms - Other Development Assistance for Malaria', y='Frequency (Quarters)', x='Cumulative Disbursement') + 
	theme_bw(base_size=16)
	
# histograms of distributions for activities
p4c = ggplot(long[activity=='Activity' & metric=='value'], aes(x=value)) + 
	geom_histogram() + 
	facet_wrap(~indicator, scales='free') + 
	labs(title='Histograms - Activities', y='Frequency (Quarters)', x='Value') + 
	theme_bw(base_size=16)

# histograms of distributions for outputs
p4d = ggplot(long[activity=='Output' & metric=='value'], aes(x=value)) + 
	geom_histogram() + 
	facet_wrap(~indicator, scales='free') + 
	labs(title='Histograms - Outputs', y='Frequency (Quarters)', x='Value') + 
	theme_bw(base_size=16)
# ----------------------------------------------


# ----------------------------------------------
# Make correlation graphs
	
# scatterplot of ITN correlations
p5a = list()
i=1
for(v in c('budget_M1_1_cumulative', 'budget_M1_2_cumulative', 'other_dah_M1_1_cumulative')) { 
	l = nodeTable[variable==v]$label
	p5a[[i]] = ggplot(data[!is.na(value_ITN_received) & !is.na(get(v))], 
			aes_string(y='value_ITN_received', x=v)) + 
		geom_point() + 
		geom_smooth(method='lm', se=FALSE) + 
		labs(y='ITN Received', x=l) + 
		theme_bw(base_size=16)
	i=i+1
}
	
# scatterplot of RDT correlations
p5b = list()
i=1
for(v in c('budget_M2_1_cumulative', 'budget_M2_3_cumulative', 
	'other_dah_M2_cumulative', 'other_dah_M2_3_cumulative')) { 
	l = nodeTable[variable==v]$label
	p5b[[i]] = ggplot(data[!is.na(value_RDT_received) & !is.na(get(v))], 
			aes_string(y='value_RDT_received', x=v)) + 
		geom_point() + 
		geom_smooth(method='lm', se=FALSE) + 
		labs(y='RDT Received', x=l) + 
		theme_bw(base_size=16)
	i=i+1
} 

# scatterplot of ACT correlations
p5c = list()
i=1
for(v in c('budget_M2_1_cumulative','budget_M2_3_cumulative', 
	'other_dah_M2_cumulative', 'other_dah_M2_3_cumulative')) { 
	l = nodeTable[variable==v]$label
	p5c[[i]] = ggplot(data[!is.na(value_RDT_received) & !is.na(get(v))], 
			aes_string(y='value_ACT_received', x=v)) + 
		geom_point() + 
		geom_smooth(method='lm', se=FALSE) + 
		labs(y='ACT Received', x=l) + 
		theme_bw(base_size=16)
	i=i+1
}

# scatterplot of output correlations
p6 = list()
pairs = data.table(y=c('value_ITN_consumed','value_ACTs_CHWs','value_RDT_completed',
	'value_SP','value_severeMalariaTreated','value_totalPatientsTreated'), 
	x=c('value_ITN_received','value_ACT_received','value_RDT_received',
	'budget_M3_1_cumulative','budget_M2_6_cumulative','value_ACT_received'))
for(i in seq(nrow(pairs))) { 
	vy=pairs[i]$y
	vx=pairs[i]$x
	ly = nodeTable[variable==vy]$label
	lx = nodeTable[variable==vx]$label
	p6[[i]] = ggplot(data[!is.na(get(vy)) & !is.na(get(vx))], aes_string(y=vy, x=vx)) + 
		geom_point() + 
		geom_smooth(method='lm', se=FALSE) + 
		labs(y=ly, x=lx) + 
		theme_bw(base_size=12)
}
# ----------------------------------------------


# --------------------------------
# Save file
pdf(outputFile4, height=5.5, width=9)
# p1a
p1b
# p1c
p1d
p2a
# p2b
p3a
# p3b
p4a
p4b
p4c
p4d
do.call('grid.arrange',c(p5a,list(ncol=2, top=textGrob('Correlations - ITN Inputs and Activities', gp=gpar(fontsize=16)))))
do.call('grid.arrange',c(p5b, list(top=textGrob('Correlations - RDT Inputs and Activities', gp=gpar(fontsize=16)))))
do.call('grid.arrange',c(p5c, list(top=textGrob('Correlations - ACT Inputs and Activities', gp=gpar(fontsize=16)))))
do.call('grid.arrange',c(p6, list(top=textGrob('Correlations - All Activities and Outputs', gp=gpar(fontsize=16)))))
dev.off()
# --------------------------------
