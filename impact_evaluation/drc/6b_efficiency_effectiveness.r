# -----------------------------------
# David Phillips
# 
# 3/25/2019
# Analyze relative magnitude of coefficients, i.e. bottleneck analysis
# -----------------------------------


# -----------------------------------------------
# Load/prep data and functions

source('./impact_evaluation/drc/set_up_r.r')

# load model results
load(outputFile5a)
data1=copy(data)
means1 = copy(means)
summaries1 = copy(summaries)
load(outputFile5b)
data2=copy(data)
means2 = copy(means)
summaries2 = copy(summaries)

# load nodeTable for graphing
nodeTable1 = fread(nodeTableFile1)
nodeTable2 = fread(nodeTableFile2)

# ensure there are no extra variables introducted from nodeTable
nodeTable1 = nodeTable1[variable %in% names(data1)]
nodeTable2 = nodeTable2[variable %in% names(data2)]
# -----------------------------------------------


# -----------------------------------------------
# Set up first half estimates

# subset to coefficients of interest
means1 = means1[op=='~' & !grepl('completeness|date',rhs)]

# compute uncertainty intervals
means1[, lower:=est-(1.96*se)]
means1[, lower.std:=est.std-(1.96*se.std)]
means1[, upper:=est+(1.96*se)]
means1[, upper.std:=est.std+(1.96*se.std)]

# estimate the combination of coefficients and their next downstream coefficient (mediation)
# (uncertainty needs improving)
mediation_means = merge(means1, means1, by.x='rhs', by.y='lhs')
mediation_means[, est:=est.y*est.std.x]
mediation_means[, se:=se.y*est.std.x]
mediation_means[, lower:=est-(1.96*se)]
mediation_means[, upper:=est+(1.96*se)]

# pull in labels
means1 = merge(means1, nodeTable1, by.x='lhs', by.y='variable')
means1 = merge(means1, nodeTable1, by.x='rhs', by.y='variable')
setnames(means1, c('label.x','label.y'), c('label_lhs','label_rhs'))
mediation_means = merge(mediation_means, nodeTable1, by.x='lhs', by.y='variable')
mediation_means = merge(mediation_means, nodeTable1, by.x='rhs.y', by.y='variable')
setnames(mediation_means, c('label.x','label.y'), c('label_lhs','label_rhs'))
# -----------------------------------------------


# -----------------------------------------------
# Pools funders together, weighting by investment size

# reshape data long
long = melt(data1, id.vars=c('orig_health_zone','health_zone','date'))

# aggregate to total across whole time series (unrescaling not necessary)
long = long[, .(value=sum(value)), by=variable]

# merge to means
pooled_means1 = merge(means1, long, by.x='rhs', by.y='variable', all.x=TRUE)

# take the weighted average across funders
pooled_means1[grepl('\\$',label_rhs), label_rhs:='Pooled Investment']
byVars = c('lhs','label_lhs','label_rhs')
pooled_means1 = pooled_means1[, .(est=weighted.mean(est, value), 
	se=weighted.mean(se, value)), by=byVars]
	
# get uncertainty
pooled_means1[, lower:=est-(1.96*se)]
pooled_means1[, upper:=est+(1.96*se)]
# -----------------------------------------------


# -----------------------------------------------
# Set up second half estimates

# subset to coefficients of interest
means2 = means2[op=='~' & !grepl('completeness|date',rhs)]

# compute uncertainty intervals
means2[, lower:=est-(1.96*se)]
means2[, lower.std:=est.std-(1.96*se.std)]
means2[, upper:=est+(1.96*se)]
means2[, upper.std:=est.std+(1.96*se.std)]

# exponentiate
means2[, est:=1.01^est] # 1.01 to make it "per 1% increase in x"
means2[, lower:=1.01^lower] # 1.01 to make it "per 1% increase in x"
means2[, upper:=1.01^upper] # 1.01 to make it "per 1% increase in x"

# pull in labels
means2 = merge(means2, nodeTable2, by.x='lhs', by.y='variable')
means2 = merge(means2, nodeTable2, by.x='rhs', by.y='variable')
setnames(means2, c('label.x','label.y'), c('label_lhs','label_rhs'))
# -----------------------------------------------


# -----------------------------------------------
# Display some statistics

# ITN, ACT and RDT shipment costs
for(c in c('ITN','ACT','RDT')) {
	output = paste0(c, '_received_cumulative')
	if (!output %in% pooled_means1$lhs) output = gsub('_cumulative','_cumulative',output) 
	commodity_cost = pooled_means1[lhs==output,c('label_rhs','est','se'), with=F]
	commodity_cost = commodity_cost[, .(est=sum(est), se=mean(se))]
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

# pooled, mediated means comparing different types of treatment

# -----------------------------------------------


# ----------------------------------------------
# Bottlenecks in efficiency and effectiveness

actVars = c('ITN_received_cumulative', 'ACT_received_cumulative', 'RDT_received_cumulative')
outVars1 = c('RDT_completed_cumulative', 'severeMalariaTreated_cumulative', 'totalPatientsTreated_cumulative')
outVars2 = c('ACTs_SSC_cumulative', 'ITN_consumed_cumulative', 'SP_cumulative')
outVarsTx = c('severeMalariaTreated_cumulative', 'totalPatientsTreated_cumulative', 'ACTs_SSC_cumulative')
incVars = c('lead_newCasesMalariaMild_rate', 'lead_newCasesMalariaSevere_rate')
mortVars = c('lead_malariaDeaths_rate', 'lead_case_fatality')

# graph coefficients from inputs to activities
p1 = ggplot(means1[lhs %in% actVars & rhs!='date'], 
		aes(y=est, ymin=lower, 
			ymax=upper, x=label_rhs)) + 
	geom_bar(stat='identity') + 
	geom_errorbar(width=.25) + 
	facet_wrap(~label_lhs, scales='free', ncol=1) + 
	labs(title='Efficiency', subtitle='Activities', 
		y='Activities per Additional Dollar Invested',x='Input') + 
	theme_bw() + 
	coord_flip()
	
# graph coefficients from inputs to outputs
p2 = ggplot(mediation_means[lhs %in% outVars1 & !rhs.y %in% actVars], 
		aes(y=est, ymin=lower, 
			ymax=upper, x=label_rhs)) + 
	geom_bar(stat='identity') + 
	geom_errorbar(width=.25) + 
	facet_wrap(~label_lhs, scales='free', ncol=1) + 
	labs(title='Efficiency', subtitle='Outputs', 
		y='Outputs per Additional Dollar Invested',x='Input') + 
	theme_bw() + 
	coord_flip()
	
# graph coefficients from inputs to outputs
p3 = ggplot(mediation_means[lhs %in% outVars2 & !rhs.y %in% actVars], 
		aes(y=est, ymin=lower, 
			ymax=upper, x=label_rhs)) + 
	geom_bar(stat='identity') + 
	geom_errorbar(width=.25) + 
	facet_wrap(~label_lhs, scales='free', ncol=1) + 
	labs(title='Efficiency', subtitle='Outputs', 
		y='Outputs per Additional Dollar Invested',x='Input') + 
	theme_bw() + 
	coord_flip()
	
# graph standardized coefficients from inputs to activities
p4 = ggplot(means1[lhs %in% actVars & rhs!='date'], 
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
p5 = ggplot(means2[lhs %in% incVars], 
		aes(y=100-est*100, ymin=100-lower*100, 
			ymax=100-upper*100, x=label_rhs)) + 
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
p6 = ggplot(means2[lhs %in% mortVars], 
		aes(y=100-est*100, ymin=100-lower*100, 
			ymax=100-upper*100, x=label_rhs)) + 
	geom_bar(stat='identity') + 
	geom_errorbar(width=.25) + 
	geom_hline(yintercept=0) + 
	facet_wrap(~label_lhs, scales='free', ncol=1) + 
	labs(title='Effectiveness', subtitle='Mortality', 
		y='Percent Reduction in Mortality Rate or Case Fatality Ratio per 1% Increase of Coverage', 
		x='Outcome') + 
	theme_bw() + 
	coord_flip()
	
# graph pooled coefficients from inputs to activities
p7 = ggplot(pooled_means1[lhs %in% actVars], 
		aes(y=est, ymin=lower, 
			ymax=upper, x=label_lhs)) + 
	geom_bar(stat='identity') + 
	geom_errorbar(width=.25) + 
	labs(title='Efficiency', subtitle='Activities', 
		y='Activities per Additional Dollar Invested',x='Input') + 
	theme_bw() + 
	coord_flip()
	
# graph pooled coefficients from inputs to treatment outputs
# p8 = ggplot(pooled_means1[lhs %in% outVarsTx], 
		# aes(y=est, ymin=lower, 
			# ymax=upper, x=label_lhs)) + 
	# geom_bar(stat='identity') + 
	# geom_errorbar(width=.25) + 
	# labs(title='Efficiency', subtitle='Activities', 
		# y='Activities per Additional Dollar Invested',x='Input') + 
	# theme_bw() + 
	# coord_flip()
# ----------------------------------------------


# ----------------------------------------------
# Save
print(paste('Saving:', outputFile6b)) 
pdf(outputFile6b, height=5.5, width=9)
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)
print(p7)
dev.off()

# save a time-stamped version for reproducibility
archive(outputFile6b)
# ----------------------------------------------
