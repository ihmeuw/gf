# -----------------------------------
# David Phillips
# 
# 2/4/2019
# This visualizes results of the SEM
# -----------------------------------


# -----------------------------------------------
# Load/prep data and functions

source('./impact_evaluation/drc/set_up_r.r')

# load home-made sem graphing function
source('./impact_evaluation/_common/graphLavaan.r')

# load model results
load(outputFile5a)
data1=copy(data)
means1 = copy(means)
summaries1 = copy(summaries)
urFits1 = copy(urFits)
load(outputFile5b)
data2=copy(data)
means2 = copy(means)
summaries2 = copy(summaries)
urFits2 = copy(urFits)

# load nodeTable for graphing
nodeTable1 = fread(nodeTableFile1)
nodeTable2 = fread(nodeTableFile2)

# ensure there are no extra variables introducted from nodeTable
nodeTable1 = nodeTable1[variable %in% names(data1)]
nodeTable2 = nodeTable2[variable %in% names(data2)]

# compute averages (approximation of standard error, would be better as Monte Carlo simulation)
paramVars = c('est.std','est','se_ratio.std', 'se_ratio', 'se.std', 'se')
urFits1[, se_ratio.std:=se.std/est.std]
urFits1[, se_ratio:=se/est]
urFit1 = urFits1[, lapply(.SD, mean), .SDcols=paramVars, by=c('lhs','op','rhs')]
urFit1[se.std>abs(se_ratio.std*est.std), se.std:=abs(se_ratio.std*est.std)]
urFit1[se>abs(se_ratio*est), se:=abs(se_ratio*est)]
urFits2[, se_ratio.std:=se.std/est.std]
urFits2[, se_ratio:=se/est]
urFit2 = urFits2[, lapply(.SD, mean), .SDcols=paramVars, by=c('lhs','op','rhs')]
urFit2[se.std>abs(se_ratio.std*est.std), se.std:=abs(se_ratio.std*est.std)]
urFit2[se>abs(se_ratio*est), se:=abs(se_ratio*est)]
# -----------------------------------------------


# -----------------------------------------------
# Compare SEM to GLM

# merge
comp = merge(summaries1, urFits1, by=c('lhs','op','rhs','health_zone'),suffixes=c('_sem','_glm'))

# -----------------------------------------------


# ----------------------------------------------
# Display results
ggplot(comp[op=='~' & lhs=='ITN_received_cumulative'], 
		aes(y=est_sem, x=est_glm)) + 
	geom_point() + 
	geom_abline(intercept=0, slope=1) + 
	facet_wrap(~rhs, scales='free') + 
	labs(title='Coefficients Relating to ITN Received', 
		y='SEM Estimate', x='GLM Estimate') + 
	theme_bw()
	
	
ggplot(comp[op=='~' & lhs=='ACT_received_cumulative'], 
		aes(y=est_sem, x=est_glm)) + 
	geom_point() + 
	geom_abline(intercept=0, slope=1) + 
	facet_wrap(~rhs, scales='free') + 
	labs(title='Coefficients Relating to ACT Received', 
		y='SEM Estimate', x='GLM Estimate') + 
	theme_bw()
# ----------------------------------------------


# # -----------------------------------
# # Save output
# print(paste('Saving:', outputFile6e)) 
# pdf(outputFile6e, height=6, width=9)
# print(p1)

# dev.off()

# # save a time-stamped version for reproducibility
# archive(outputFile6e)
# # -----------------------------------
