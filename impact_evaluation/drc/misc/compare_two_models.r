# compare two model runs
rm(list=ls())
library(data.table)
library(ggplot2)

# file paths
run1 = 'J:/Project/Evaluation/GF/impact_evaluation/cod/prepped_data/first_half_model_results_best.rdata'
run2 = 'J:/Project/Evaluation/GF/impact_evaluation/cod/prepped_data/first_half_model_results_pc.rdata'
graphFile = 'J:/Project/Evaluation/GF/impact_evaluation/cod/visualizations/miscellaneous/sem_vs_sem_pc.pdf'

# load and rename
load(run1)
for(o in c('means','summaries','urFits')) {
	assign(paste0(o, '_run1'), copy(get(o)))
}
load(run2)
for(o in c('means','summaries','urFits')) {
	assign(paste0(o, '_run2'), copy(get(o)))
}

# rename 
means_run2 = means_run2[grepl('_pc', means_run2)]
summaries_run2 = summaries_run2[grepl('_pc', summaries_run2)]
means_run2[, lhs:=gsub('_pc', '', lhs)]
means_run2[, rhs:=gsub('_pc', '', rhs)]

# keep only primary coefficients
means_run1 = means_run1[op=='~']
means_run2 = means_run2[op=='~']

# merge
means = merge(means_run1, means_run2, by=c('lhs','op','rhs'), suffixes=c('_run1', '_run2'))

# compare
p1 = ggplot(means, aes(y=est.std_run2, x=est.std_run1)) + 
	geom_point() + 
	geom_abline(slope=1, intercept=0) + 
	labs(title='Mean SEM Coefficients', y='Per-Capita Variables', x='Numeric Variables') + 
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

pdf(graphFile, height=6, width=8)
p1
dev.off()

