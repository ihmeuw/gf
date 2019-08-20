# -----------------------------------
# David Phillips
# 
# 2/4/2019
# This visualizes results of the SEM
# -----------------------------------


# -----------------------------------------------
# Load/prep data and functions

source('./impact_evaluation/gtm/set_up_r.r')

# load home-made sem graphing function
source('./impact_evaluation/_common/graphLavaan.r')

# load model results
load(outputFile5a)
data1=copy(data)
model1=copy(model)
# means1 = copy(means)
# summaries1 = copy(summaries)
urFits1 = copy(urFits)
load(outputFile5b)
data2=copy(data)
model2=copy(model)
# means2 = copy(means)
# summaries2 = copy(summaries)
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
urFit1 = urFits1[, lapply(.SD, mean, na.rm=TRUE), .SDcols=paramVars, by=c('lhs','op','rhs')]
urFit1[se.std>abs(se_ratio.std*est.std), se.std:=abs(se_ratio.std*est.std)]
urFit1[se>abs(se_ratio*est), se:=abs(se_ratio*est)]
urFits2[, se_ratio.std:=se.std/est.std]
urFits2[, se_ratio:=se/est]
urFit2 = urFits2[, lapply(.SD, mean, na.rm=TRUE), .SDcols=paramVars, by=c('lhs','op','rhs')]
urFit2[se.std>abs(se_ratio.std*est.std), se.std:=abs(se_ratio.std*est.std)]
urFit2[se>abs(se_ratio*est), se:=abs(se_ratio*est)]
# -----------------------------------------------


#Some visualization code for model run 8/15/19 
# View(urFits1[rhs=="MDR_Cases_Started_Treatment_out_cumulative" & lhs=="Cases_Notified_out_cumulative"])
# urFits1[rhs=="MDR_Cases_Started_Treatment_out_cumulative" & lhs=="Cases_Notified_out_cumulative", mean(est)] # A very high positive average, but also a lot of variation. 
# View(urFits1[rhs=="MDR_Cases_Started_Treatment_out_cumulative" & lhs=="Cases_Started_on_Treatment_out_cumulative"])
# urFits1[rhs=="MDR_Cases_Started_Treatment_out_cumulative" & lhs=="Cases_Started_on_Treatment_out_cumulative", mean(est)] # A very low negative average. 
# # Do these relationships make sense? 
# 
# # ----------------------------------------------
# Display results

# my sem graph function for first half model
# p1 = semGraph(parTable=means1, nodeTable=nodeTable1, 
# 	scaling_factors=NA, standardized=TRUE, edgeLabels=FALSE,
# 	lineWidth=1.5, curved=0, tapered=FALSE)
# 
# # my sem graph function for second half model
# p2 = semGraph(parTable=means2, nodeTable=nodeTable2,
# 	scaling_factors=NA, standardized=TRUE, edgeLabels=FALSE,
# 	lineWidth=1.5, curved=0, tapered=FALSE)
# 
# # my sem graph function for first half model with coefficients
# p3 = semGraph(parTable=means1, nodeTable=nodeTable1, 
# 	scaling_factors=NA, standardized=TRUE, 
# 	lineWidth=1.5, curved=0, tapered=FALSE)
# 
# # my sem graph function for second half model with coefficients
# p4 = semGraph(parTable=means2, nodeTable=nodeTable2,
# 	scaling_factors=NA, standardized=TRUE,
# 	lineWidth=1.5, curved=0, tapered=FALSE)

# my sem graph function for first half "unrelated regressions" model
p5 = semGraph(parTable=urFit1, nodeTable=nodeTable1, 
	scaling_factors=NA, standardized=TRUE, 
	lineWidth=1.5, curved=0, tapered=FALSE)

# my sem graph function for second half "unrelated regressions" model
p6 = semGraph(parTable=urFit2, nodeTable=nodeTable2,
	scaling_factors=NA, standardized=TRUE,
	lineWidth=1.5, curved=0, tapered=FALSE)
# ----------------------------------------------


# -----------------------------------
# Save output
print(paste('Saving:', outputFile6a)) 
pdf(outputFile6a, height=6, width=9)
# print(p1)
# print(p2)
# print(p3)
# print(p4)
print(p5)
print(p6)
dev.off()

# save a time-stamped version for reproducibility
archive(outputFile6a)
# -----------------------------------
# 
# #Save just the GLM diagrams with correlation coefficients as PNGs. 
ggsave("J:/Project/Evaluation/GF/impact_evaluation/gtm/visualizations/model_first_half.png", p5, height=10, width=13)
ggsave("J:/Project/Evaluation/GF/impact_evaluation/gtm/visualizations/model_second_half.png", p6, height=10, width=13)
