# -----------------------------------
# David Phillips
# 
# 3/25/2019
# Analyze relative magnitude of coefficients, i.e. bottleneck analysis
# -----------------------------------


# -----------------------------------------------
# Load/prep data and functions

source('./impact_evaluation/_common/set_up_r.r')

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
# Set up first half data

# unrescale
tmp = unique(melt(scaling_factors1, value.name='scaling_factor'))
new_summaries1 = merge(summaries1, tmp, by.x='rhs', by.y='variable', all.x=TRUE)
new_summaries1 = merge(new_summaries1, tmp, by.x='lhs', by.y='variable', all.x=TRUE)
new_summaries1[is.na(scaling_factor.x), scaling_factor.x:=1]
new_summaries1[is.na(scaling_factor.y), scaling_factor.y:=1]
new_summaries1[, est_unrescaled:=est/scaling_factor.x*scaling_factor.y]
new_summaries1[, se_unrescaled:=se/scaling_factor.x*scaling_factor.y]
new_summaries1[, se_ratio:=se/est]
new_summaries1[, se_unrescaled_ratio:=se_unrescaled/est]
new_summaries1[, se.std_ratio:=se.std/est.std]
new_means1 = new_summaries1[op=='~', .(est.std=mean(est.std), est=mean(est), est_unrescaled=mean(est_unrescaled), 
	se.std_ratio=mean(se.std_ratio), se_ratio=mean(se_ratio), se_unrescaled_ratio=mean(se_unrescaled_ratio), 
	se.std=mean(se.std), se=mean(se), se_unrescaled=mean(se_unrescaled)), 
	by=c('lhs','op','rhs')]
new_means1[se>abs(se_ratio*est), se:=abs(se_ratio*est)]
new_means1[se.std>abs(se.std_ratio*est.std), se.std:=abs(se.std_ratio*est.std)]
# new_means1[, se_unrescaled:=abs(se_unrescaled_ratio*est_unrescaled)]
new_means1[se_unrescaled>abs(se_unrescaled_ratio*est_unrescaled), se_unrescaled:=abs(se_unrescaled_ratio*est_unrescaled)] # FIX!
new_means1[, lower_unrescaled:=est_unrescaled-(1.96*se_unrescaled)]
new_means1[, upper_unrescaled:=est_unrescaled+(1.96*se_unrescaled)]

# handle mediation (stanard error needs improving...)
mediation_means = merge(new_means1, new_means1, by.x='rhs', by.y='lhs')
mediation_means[, est_unrescaled:=est.y*est.std.x]
mediation_means[, se_unrescaled:=se.y*se.std.x]
tmp = new_means1[!rhs %in% mediation_means$rhs.y]
tmp = tmp[,-c('est_unrescaled','se_unrescaled')] # FIX!
setnames(tmp, c('rhs','est.std','se.std'), c('rhs.y','est_unrescaled','se_unrescaled'))
mediation_means = rbind(mediation_means, tmp, fill=TRUE)
mediation_means[, lower_unrescaled:=est_unrescaled-(1.96*se_unrescaled)]
mediation_means[, upper_unrescaled:=est_unrescaled+(1.96*se_unrescaled)]

# pull in labels
new_means1 = merge(new_means1, nodeTable1, by.x='lhs', by.y='variable')
new_means1 = merge(new_means1, nodeTable1, by.x='rhs', by.y='variable')
setnames(new_means1, c('label.x','label.y'), c('label_lhs','label_rhs'))
mediation_means = merge(mediation_means, nodeTable1, by.x='lhs', by.y='variable')
mediation_means = merge(mediation_means, nodeTable1, by.x='rhs.y', by.y='variable')
setnames(mediation_means, c('label.x','label.y'), c('label_lhs','label_rhs'))
# -----------------------------------------------


# -----------------------------------------------
# Set up second half data

# unrescale
tmp = unique(melt(scaling_factors2, value.name='scaling_factor'))
new_summaries2 = merge(summaries2, tmp, by.x='rhs', by.y='variable', all.x=TRUE)
new_summaries2 = merge(new_summaries2, tmp, by.x='lhs', by.y='variable', all.x=TRUE)
new_summaries2[is.na(scaling_factor.x), scaling_factor.x:=1]
new_summaries2[is.na(scaling_factor.y), scaling_factor.y:=1]
new_summaries2[, est_unrescaled:=est/scaling_factor.x*scaling_factor.y]
new_summaries2[, se_unrescaled:=se/scaling_factor.x*scaling_factor.y]
new_summaries2[, se_ratio:=se/est]
new_summaries2[, se_unrescaled_ratio:=se_unrescaled/est_unrescaled]
new_summaries2[, se.std_ratio:=se.std/est.std]
new_means2 = new_summaries2[op=='~', .(est.std=mean(est.std), est=mean(est), est_unrescaled=mean(est_unrescaled), 
	se.std_ratio=mean(se.std_ratio), se_ratio=mean(se_ratio), se_unrescaled_ratio=mean(se_unrescaled_ratio)), 
	by=c('lhs','op','rhs')]
new_means2[, se:=abs(se_ratio*est)]
new_means2[, se.std:=abs(se.std_ratio*est.std)]
new_means2[, se_unrescaled:=abs(se_unrescaled_ratio*est_unrescaled)]
new_means2[, lower_unrescaled:=est_unrescaled-(1.96*se_unrescaled)]
new_means2[, upper_unrescaled:=est_unrescaled+(1.96*se_unrescaled)]

# exponentiate
new_means2[, est_unrescaled:=1.01^est_unrescaled] # 1.01 to make it "per 1% increase in x"
new_means2[, lower_unrescaled:=1.01^lower_unrescaled] # 1.01 to make it "per 1% increase in x"
new_means2[, upper_unrescaled:=1.01^upper_unrescaled] # 1.01 to make it "per 1% increase in x"

# pull in labels
new_means2 = merge(new_means2, nodeTable2, by.x='lhs', by.y='variable')
new_means2 = merge(new_means2, nodeTable2, by.x='rhs', by.y='variable')
setnames(new_means2, c('label.x','label.y'), c('label_lhs','label_rhs'))
# -----------------------------------------------


# -----------------------------------------------
# Display some statistics

# ITN, ACT and RDT shipment costs
for(c in c('ITN','ACT','RDT')) {
	output = paste0(c, '_received_cumulative')
	commodity_cost = new_means1[lhs==output,c('rhs','est_unrescaled','se_unrescaled'), with=F]
	commodity_cost[grepl('exp',rhs), funder:='Global Fund']
	commodity_cost[grepl('other_dah',rhs), funder:='All Other Donors']
	commodity_cost[grepl('ghe',rhs), funder:='Government']
	commodity_cost = commodity_cost[, .(est=sum(est_unrescaled), se=mean(se_unrescaled)), by=funder]
	commodity_cost[, lower:=est+(1.96*se)]
	commodity_cost[, upper:=est-(1.96*se)]
	commodity_cost$se = NULL
	commodity_cost[, est:=1/est]
	commodity_cost[, lower:=1/lower]
	commodity_cost[, upper:=1/upper]
	if(any(commodity_cost$upper<0)) {
		commodity_cost[, upper:=as.character(upper)]
		commodity_cost[grepl('-',upper), upper:='Negative']
	}
	print(paste0('Overall cost to ship one ', c, ':'))
	print(commodity_cost)
}
# -----------------------------------------------


# ----------------------------------------------
# Bottlenecks in efficiency and effectiveness

actVars = c('ITN_received_cumulative', 'ACT_received_cumulative', 'RDT_received_cumulative')
outVars1 = c('RDT_completed_cumulative', 'severeMalariaTreated_cumulative', 'totalPatientsTreated_cumulative')
outVars2 = c('ACTs_SSC_cumulative', 'ITN_consumed_cumulative', 'SP_cumulative')
incVars = c('lead_newCasesMalariaMild_rate', 'lead_newCasesMalariaSevere_rate')
mortVars = c('lead_malariaDeaths_rate', 'lead_case_fatality')

# graph coefficients from inputs to activities
p1 = ggplot(new_means1[lhs %in% actVars & rhs!='date'], 
		aes(y=est_unrescaled, ymin=lower_unrescaled, 
			ymax=upper_unrescaled, x=label_rhs)) + 
	geom_bar(stat='identity') + 
	geom_errorbar(width=.25) + 
	facet_wrap(~label_lhs, scales='free', ncol=1) + 
	labs(title='Efficiency', subtitle='Activities', 
		y='Activities per Additional Dollar Invested',x='Input') + 
	theme_bw() + 
	coord_flip()
	
# graph coefficients from inputs to outputs
p2 = ggplot(mediation_means[lhs %in% outVars1 & !rhs.y %in% actVars], 
		aes(y=est_unrescaled, ymin=lower_unrescaled, 
			ymax=upper_unrescaled, x=label_rhs)) + 
	geom_bar(stat='identity') + 
	geom_errorbar(width=.25) + 
	facet_wrap(~label_lhs, scales='free', ncol=1) + 
	labs(title='Efficiency', subtitle='Outputs', 
		y='Outputs per Additional Dollar Invested',x='Input') + 
	theme_bw() + 
	coord_flip()
	
# graph coefficients from inputs to outputs
p3 = ggplot(mediation_means[lhs %in% outVars2 & !rhs.y %in% actVars], 
		aes(y=est_unrescaled, ymin=lower_unrescaled, 
			ymax=upper_unrescaled, x=label_rhs)) + 
	geom_bar(stat='identity') + 
	geom_errorbar(width=.25) + 
	facet_wrap(~label_lhs, scales='free', ncol=1) + 
	labs(title='Efficiency', subtitle='Outputs', 
		y='Outputs per Additional Dollar Invested',x='Input') + 
	theme_bw() + 
	coord_flip()
	
# graph standardized coefficients from inputs to activities
p4 = ggplot(new_means1[lhs %in% actVars & rhs!='date'], 
		aes(y=est.std, ymin=est.std-(1.96*se.std), 
			ymax=est.std+(1.96*se.std), x=label_rhs)) + 
	geom_bar(stat='identity') + 
	geom_errorbar(width=.25) + 
	facet_wrap(~label_lhs, scales='free_y', ncol=1) + 
	labs(title='Standardized Efficiency', subtitle='Activities', 
		y='Activities per Additional Dollar Invested',x='Input') + 
	theme_bw() + 
	coord_flip()
	
# graph coefficients from outcomes to incidence
p5 = ggplot(new_means2[lhs %in% incVars], 
		aes(y=100-est_unrescaled*100, ymin=100-lower_unrescaled*100, 
			ymax=100-upper_unrescaled*100, x=label_rhs)) + 
	geom_bar(stat='identity') + 
	geom_errorbar(width=.25) + 
	geom_hline(yintercept=0) + 
	facet_wrap(~label_lhs, scales='free', ncol=1) + 
	labs(title='Effectiveness', subtitle='Incidence', 
		y='Percent Reduction in Incidence Rate per 1% Increase of Coverage',
		x='Outcome') + 
	theme_bw() + 
	coord_flip()

# graph coefficients from outcomes to incidence
p6 = ggplot(new_means2[lhs %in% mortVars], 
		aes(y=100-est_unrescaled*100, ymin=100-lower_unrescaled*100, 
			ymax=100-upper_unrescaled*100, x=label_rhs)) + 
	geom_bar(stat='identity') + 
	geom_errorbar(width=.25) + 
	geom_hline(yintercept=0) + 
	facet_wrap(~label_lhs, scales='free', ncol=1) + 
	labs(title='Effectiveness', subtitle='Mortality', 
		y='Percent Reduction in Mortality Rate or Case Fatality Ratio per 1% Increase of Coverage', 
		x='Outcome') + 
	theme_bw() + 
	coord_flip()
# ----------------------------------------------


# ----------------------------------------------
# Save
pdf(outputFile6b, height=5.5, width=9)
p1
p2
p3
p4
p5
p6
dev.off()

# save a time-stamped version for reproducibility
archive(outputFile6b)
# ----------------------------------------------
