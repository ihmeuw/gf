# -----------------------------------
# David Phillips,  Francisco
# 
# 3/25/2019
# Analyze relative magnitude of coefficients, i.e. bottleneck analysis
# -----------------------------------


# -----------------------------------------------
# Load/prep data and functions

source('./impact_evaluation/sen/set_up_r.r')

# load model results
load(outputFile5a)
data1=copy(data)
paramVars = c('est.std','est','se_ratio.std', 'se_ratio', 'se.std', 'se')
urFits[, se_ratio.std:=se.std/est.std]
urFits[, se_ratio:=se/est]
means = urFits[, lapply(.SD, mean, na.rm=T), .SDcols=paramVars, by=c('lhs','op','rhs')]
means[se.std>abs(se_ratio.std*est.std), se.std:=abs(se_ratio.std*est.std)]
means[se>abs(se_ratio*est), se:=abs(se_ratio*est)]

urFits1 = copy(means)


# load nodeTable for graphing
nodeTable1 = fread(nodeTableFile1)

# ensure there are no extra variables introducted from nodeTable
nodeTable1 = nodeTable1[variable %in% names(data1)]
# -----------------------------------------------


# -----------------------------------------------
# Set up first half estimates

# subset to coefficients of interest
urFits1 = urFits1[op=='~' & !grepl('completeness|date',rhs)]

# compute uncertainty intervals
urFits1[, lower:=est-(1.96*se)]
urFits1[, lower.std:=est.std-(1.96*se.std)]
urFits1[, upper:=est+(1.96*se)]
urFits1[, upper.std:=est.std+(1.96*se.std)]

# estimate the combination of coefficients and their next downstream coefficient (mediation)
# (uncertainty needs improving)
mediation_means = merge(urFits1, urFits1, by.x='rhs', by.y='lhs', allow.cartesian=T)
mediation_means[, est:=est.y*est.std.x]
mediation_means[, se:=se.y*est.std.x]
mediation_means[, lower:=est-(1.96*se)]
mediation_means[, upper:=est+(1.96*se)]

# pull in labels
urFits1 = merge(urFits1, nodeTable1, by.x='lhs', by.y='variable')
urFits1 = merge(urFits1, nodeTable1, by.x='rhs', by.y='variable')
setnames(urFits1, c('label.x','label.y'), c('label_lhs','label_rhs'))
mediation_means = merge(mediation_means, nodeTable1, by.x='lhs', by.y='variable')
mediation_means = merge(mediation_means, nodeTable1, by.x='rhs.y', by.y='variable')
setnames(mediation_means, c('label.x','label.y'), c('label_lhs','label_rhs'))
# -----------------------------------------------


# -----------------------------------------------
# Pools funders together, weighting by investment size

# reshape data long
long = melt(data1, id.vars=c('region','date'))

# aggregate to total across whole time series (unrescaling not necessary)
long = long[, .(value=sum(value)), by=variable]

# merge to means
pooled_means1 = merge(urFits1, long, by.x='rhs', by.y='variable', all.x=TRUE)

# take the weighted average across funders
pooled_means1[grepl('Exp.',label_rhs), label_rhs:='Pooled Investment']
byVars = c('lhs','label_lhs','label_rhs')
pooled_means1 = pooled_means1[, .(est=weighted.mean(est, value, na.rm=T), 
	se=weighted.mean(se, value, na.rm=T)), by=byVars]
	
# get uncertainty
pooled_means1[, lower:=est-(1.96*se)]
pooled_means1[, upper:=est+(1.96*se)]
# -----------------------------------------------



# -----------------------------------------------
# Display some statistics

# activties
activityVars <- c('com_radio_cumulative', 'com_cause_cumulative', 
                  'com_mobsoc_cumulative', 'tot_genexpert_cumulative', 
                  'dx_count_cumulative', 'tb_vih_arv_cumulative',
                  'ntr_all_cumulative', 'tb_tfc_cumulative', 'perf_lab')
for(c in activityVars) {
	commodity_cost = pooled_means1[lhs==c,c('label_rhs','est','se'), with=F]
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
	print(paste0('Overall cost per one ', c, ':'))
	print(commodity_cost)
}

# pooled, mediated means comparing different types of treatment

# -----------------------------------------------


# ----------------------------------------------
# Bottlenecks in efficiency and effectiveness

actVars = names(data)[grep("act", names(data))]
outVars = names(data)[grep("out", names(data))]

# graph coefficients from inputs to activities
p1 = ggplot(urFits1[lhs %in% actVars & rhs!='date'], 
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
p2 = ggplot(mediation_means[lhs %in% outVars & !rhs.y %in% actVars], 
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
p3 = ggplot(urFits1[lhs %in% actVars & rhs!='date'], 
		aes(y=est.std, ymin=est.std-(1.96*se.std), 
			ymax=est.std+(1.96*se.std), x=label_rhs)) + 
	geom_bar(stat='identity') + 
	geom_errorbar(width=.25) + 
	facet_wrap(~label_lhs, scales='free_y', ncol=1) + 
	labs(title='Standardized Efficiency', subtitle='Activities', 
		y='Activities per Additional Dollar Invested',x='Input') + 
	theme_bw() + 
	coord_flip()
	
	
# graph pooled coefficients from inputs to activities
p4 = ggplot(pooled_means1[lhs %in% actVars], 
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
dev.off()

# save a time-stamped version for reproducibility
archive(outputFile6b)
# ----------------------------------------------
