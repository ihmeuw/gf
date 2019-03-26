# -----------------------------------
# David Phillips
# 
# 3/25/2019
# Analyze relative magnitude of coefficients, i.e. bottleneck analysis
# -----------------------------------


# -----------------------------------------------
# Load/prep data and functions

source('./impact_evaluation/_common/set_up_r.r')

# load the custom predict_lavaan.r function
source('./impact_evaluation/_common/predict_lavaan.r')

# load home-made sem graphing function
source('./impact_evaluation/visualizations/graphLavaan.r')

# load model results
load(outputFile5b)
data1=copy(data)
means1 = copy(means)
summaries1 = copy(summaries)
scaling_factors1=copy(scaling_factors)
load(outputFile5e)
data2=copy(data)
means2 = copy(means)
summaries2 = copy(summaries)
scaling_factors2=copy(scaling_factors)

# load nodeTable for graphing
nodeTable1 = fread('./impact_evaluation/visualizations/vartable.csv')
nodeTable2 = fread('./impact_evaluation/visualizations/vartable_second_half.csv')

# ensure there are no extra variables introducted from nodeTable
nodeTable1 = nodeTable1[variable %in% names(data1)]
nodeTable2 = nodeTable2[variable %in% names(data2)]
# -----------------------------------------------


# -----------------------------------------------
# Set up data

# unrescale
tmp = unique(melt(scaling_factors1, value.name='scaling_factor'))
new_summaries1 = merge(summaries1, tmp, by.x='rhs', by.y='variable', all.x=TRUE)
new_summaries1 = merge(new_summaries1, tmp, by.x='lhs', by.y='variable', all.x=TRUE)
new_summaries1[is.na(scaling_factor.x), scaling_factor.x:=1]
new_summaries1[is.na(scaling_factor.y), scaling_factor.y:=1]
new_summaries1[, est_unrescaled:=est/scaling_factor.x*scaling_factor.y]
new_summaries1[, se_unrescaled:=se/scaling_factor.x*scaling_factor.y]
new_means1 = new_summaries1[op=='~', .(est.std=mean(est.std), est=mean(est), est_unrescaled=mean(est_unrescaled), 
	se.std=mean(se.std), se=mean(se), se_unrescaled=mean(se_unrescaled)), 
	by=c('lhs','op','rhs')]

# handle mediation (stanard error needs improving...)
mediation_means = merge(new_means1, new_means1, by.x='rhs', by.y='lhs')
mediation_means[, est_unrescaled:=est.y*est.std.x]
mediation_means[, se_unrescaled:=se.y*se.std.x]
tmp = new_means1[!rhs %in% mediation_means$rhs.y]
tmp = tmp[,-c('est_unrescaled','se_unrescaled')] # FIX!
setnames(tmp, c('rhs','est.std','se.std'), c('rhs.y','est_unrescaled','se_unrescaled'))
mediation_means = rbind(mediation_means, tmp, fill=TRUE)
mediation_means[rhs=='ITN_received_cumulative']
mediation_means[lhs=='severeMalariaTreated_cumulative']

# pull in labels
new_means1 = merge(new_means1, nodeTable1, by.x='lhs', by.y='variable')
new_means1 = merge(new_means1, nodeTable1, by.x='rhs', by.y='variable')
setnames(new_means1, c('label.x','label.y'), c('label_lhs','label_rhs'))
mediation_means = merge(mediation_means, nodeTable1, by.x='lhs', by.y='variable')
mediation_means = merge(mediation_means, nodeTable1, by.x='rhs.y', by.y='variable')
setnames(mediation_means, c('label.x','label.y'), c('label_lhs','label_rhs'))
# -----------------------------------------------


# ----------------------------------------------
# Bottlenecks in efficiency and effectiveness

actVars = c('ITN_received_cumulative', 'ACT_received_cumulative', 'RDT_received_cumulative')
outVars1 = c('RDT_completed_cumulative', 'severeMalariaTreated_cumulative', 'totalPatientsTreated_cumulative')
outVars2 = c('ACTs_SSC_cumulative', 'ITN_consumed_cumulative', 'SP_cumulative')
	
# graph coefficients from inputs to activities
p1 = ggplot(new_means1[lhs %in% actVars & rhs!='date'], 
		aes(y=est_unrescaled, ymin=est_unrescaled-1.96*se_unrescaled, 
			ymax=est_unrescaled+1.96*se_unrescaled, x=label_rhs)) + 
	geom_bar(stat='identity') + 
	geom_errorbar(width=.25) + 
	facet_wrap(~label_lhs, scales='free', ncol=1) + 
	labs(y='Activities per Additional Dollar Invested',x='Input') + 
	theme_bw() + 
	coord_flip()
	
# graph coefficients from inputs to activities
p2 = ggplot(mediation_means[lhs %in% outVars1 & !rhs.y %in% actVars], 
		aes(y=est_unrescaled, ymin=est_unrescaled-1.96*se_unrescaled, 
			ymax=est_unrescaled+1.96*se_unrescaled, x=label_rhs)) + 
	geom_bar(stat='identity') + 
	geom_errorbar(width=.25) + 
	facet_wrap(~label_lhs, scales='free', ncol=1) + 
	labs(y='Activities per Additional Dollar Invested',x='Input') + 
	theme_bw() + 
	coord_flip()
	
# graph coefficients from inputs to activities
p3 = ggplot(mediation_means[lhs %in% outVars2 & !rhs.y %in% actVars], 
		aes(y=est_unrescaled, ymin=est_unrescaled-1.96*se_unrescaled, 
			ymax=est_unrescaled+1.96*se_unrescaled, x=label_rhs)) + 
	geom_bar(stat='identity') + 
	geom_errorbar(width=.25) + 
	facet_wrap(~label_lhs, scales='free', ncol=1) + 
	labs(y='Activities per Additional Dollar Invested',x='Input') + 
	theme_bw() + 
	coord_flip()
# ----------------------------------------------
