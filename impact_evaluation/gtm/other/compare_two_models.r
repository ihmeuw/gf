# compare two model runs
rm(list=ls())
library(data.table)
library(ggplot2)

# file paths
# Run 1 is original TERG model, 
# Run 2 is TERG model run on quarter-level data, and 
# Run 3 is new model with RSSH improvements on quarter-level data. 
run1 = "J:/Project/Evaluation/GF/impact_evaluation/gtm/prepped_data/model_runs/first_half_model_results_2019_09_06_12_12_02.rdata"
run2 = "J:/Project/Evaluation/GF/impact_evaluation/gtm/prepped_data/model_runs/first_half_model_results_2019_10_28_10_06_09.rdata"
run3 = "J:/Project/Evaluation/GF/impact_evaluation/gtm/prepped_data/model_runs/first_half_model_results_2019_10_25_14_17_40.rdata"
graphFile = 'J:/Project/Evaluation/GF/impact_evaluation/gtm/visualizations/compare_glm.pdf'

# load and rename
load(run1)
for(o in c('data', 'urFits')) {
	assign(paste0(o, '_run1'), copy(get(o)))
}
load(run2)
for(o in c('data','urFits')) {
	assign(paste0(o, '_run2'), copy(get(o)))
}

# keep only primary coefficients
urFits_run1 = urFits_run1[op=='~']
urFits_run2 = urFits_run2[op=='~']

#Subset 
urFits_run1 = urFits_run1[, .(lhs, rhs, op, est, est.std, department, date)]
urFits_run2 = urFits_run2[, .(lhs, rhs, op, est, est.std, department, date)]

# merge
urFits = merge(urFits_run1, urFits_run2, by=c('lhs','op','rhs', 'department'), suffixes=c('_run1', '_run2'))

# compare
p1 = ggplot(urFits, aes(y=est.std_run2, x=est.std_run1)) + 
	geom_point() + 
	geom_abline(slope=1, intercept=0) + 
	labs(title='GLM Standardized Coefficients', y='New Model', x='Old Model') + 
	theme_bw(base_size=14)

# Run the same graph, only showing variables where the estimates have changed. 
differences = urFits[est.std_run1!=est.std_run2]
p1_a = ggplot(differences, aes(y=est.std_run2, x=est.std_run1, color=rhs)) + 
  geom_point() + 
  geom_abline(slope=1, intercept=0) + 
  labs(title='GLM standardized coefficients, for variables that changed between models', y='New Model', x='Old Model') + 
  theme_bw(base_size=14)

# look at a few coefficients from run2 compared to the mean from run1
# The differences are happening for each of the GF input variables. 
p2 = ggplot(urFits[rhs=="gf_tb_cumulative"], aes(y=est.std_run2, x=est.std_run1, color=lhs)) + 
  geom_point() + 
  geom_abline(slope=1, intercept=0) + 
  labs(title='Comparison of GF TB input', subtitle="*Each point represents one department-level estimate", y='New Model', x='Old Model') + 
  theme_bw(base_size=14)
	
p3 = ggplot(urFits[rhs=="gf_tbhiv_cumulative"], aes(y=est.std_run2, x=est.std_run1, color=lhs)) + 
  geom_point() + 
  geom_abline(slope=1, intercept=0) + 
  labs(title='Comparison of GF HIV/TB input', subtitle="*Each point represents one department-level estimate", y='New Model', x='Old Model') + 
  theme_bw(base_size=14)
	
p4 = ggplot(urFits[rhs=="gf_mdrtb_cumulative"], aes(y=est.std_run2, x=est.std_run1, color=lhs)) + 
  geom_point() + 
  geom_abline(slope=1, intercept=0) + 
  labs(title='Comparison of GF MDR-TB input', subtitle="*Each point represents one department-level estimate", y='New Model', x='Old Model') + 
  theme_bw(base_size=14)

pdf(graphFile, height=6, width=8)
p1
p1_a
p2
p3
p4
dev.off()

