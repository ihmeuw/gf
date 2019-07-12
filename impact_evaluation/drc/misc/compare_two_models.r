# compare two model runs
rm(list=ls())
library(data.table)
library(ggplot2)

# file paths
run1 = 'J:/Project/Evaluation/GF/impact_evaluation/cod/prepped_data/first_half_model_results.rdata'
run2 = 'J:/Project/Evaluation/GF/impact_evaluation/cod/prepped_data/first_half_model_resultstime_pc.rdata'

# load and rename
load(run1)
for(o in c('means','summaries','urFits')) {
	assign(paste0(o, '_run1'), copy(get(o)))
}
load(run2)
for(o in c('means','summaries','urFits')) {
	assign(paste0(o, '_run2'), copy(get(o)))
}

# merge
means = merge(means_run1, means_run2, by=c('lhs','op','rhs'), suffixes=c('_space_fe', '_time_fe'))

# compare
ggplot(means, aes(y=est.std_time_fe, x=est.std_space_fe)) + 
	geom_point() + 
	geom_abline(slope=1, intercept=0) + 
	theme_bw()

# look at a few coefficients from run2 compared to the mean from run1
tmpSummaries_run2 = summaries_run2[lhs %in% c('ACT_received_cumulative', 'ITN_received_cumulative', 'RDT_received_cumulative') & op=='~' & !grepl('completeness', rhs)]
tmpMeans_run1 = means_run1[lhs %in% c('ACT_received_cumulative', 'ITN_received_cumulative', 'RDT_received_cumulative') & op=='~' & !grepl('completeness|date', rhs)]
ggplot(tmpSummaries_run2[lhs=='ACT_received_cumulative'], aes(y=est.std, x=date, color=rhs)) + 
	geom_point() + 
	geom_line() + 
	geom_hline(data=tmpMeans_run1[lhs=='ACT_received_cumulative'], aes(yintercept=est.std, color=rhs)) + 
	theme_bw()
	
ggplot(tmpSummaries_run2[lhs=='ITN_received_cumulative'], aes(y=est.std, x=date, color=rhs)) + 
	geom_point() + 
	geom_line() + 
	geom_hline(data=tmpMeans_run1[lhs=='ITN_received_cumulative'], aes(yintercept=est.std, color=rhs)) + 
	theme_bw()
	
ggplot(tmpSummaries_run2[lhs=='RDT_received_cumulative'], aes(y=est.std, x=date, color=rhs)) + 
	geom_point() + 
	geom_line() + 
	geom_hline(data=tmpMeans_run1[lhs=='RDT_received_cumulative'], aes(yintercept=est.std, color=rhs)) + 
	theme_bw()
