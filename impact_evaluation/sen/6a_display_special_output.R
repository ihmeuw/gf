# -----------------------------------
# Francisco Rios, David Phillips
# 
# 9/13/2019
# This visualizes results of the SEM
# -----------------------------------


# -----------------------------------------------
# Load/prep data and functions

source('./impact_evaluation/sen/set_up_r.r')

# load home-made sem graphing function
source('./impact_evaluation/_common/graphLavaan.r')

# load model results
load(outputFile5a)
data1=copy(data)
model1=copy(model)
# means1 = copy(means)
# summaries1 = copy(summaries)
urFits1 = copy(urFits)
# load(outputFile5b)
# data2=copy(data)
# model2=copy(model)
# means2 = copy(means)
# summaries2 = copy(summaries)
#urFits2 = copy(urFits)

# load nodeTable for graphing
nodeTable1 = fread(nodeTableFile1)
#nodeTable2 = fread(nodeTableFile2)

names(data1)[!names(data1)%in%nodeTable1$variable]

# ensure there are no extra variables introducted from nodeTable
nodeTable1 = nodeTable1[variable %in% names(data1)]
# nodeTable2 = nodeTable2[variable %in% names(data2)]

# compute averages (approximation of standard error, would be better as Monte Carlo simulation)
paramVars = c('est.std','est','se_ratio.std', 'se_ratio', 'se.std', 'se')
urFits1[, se_ratio.std:=se.std/est.std]
urFits1[, se_ratio:=se/est]
urFit1 = urFits1[, lapply(.SD, mean, na.rm=TRUE), .SDcols=paramVars, by=c('lhs','op','rhs')]
urFit1[se.std>abs(se_ratio.std*est.std), se.std:=abs(se_ratio.std*est.std)]
urFit1[se>abs(se_ratio*est), se:=abs(se_ratio*est)]

# -----------------------------------------------

# # ----------------------------------------------
# Display results

# my sem graph function for first half "unrelated regressions" model
p1 = semGraph(parTable=urFit1, nodeTable=nodeTable1, 
	scaling_factors=NA, standardized=TRUE, 
	lineWidth=1.5, curved=0, tapered=FALSE, colScaleMin= -0.5, colScaleMax=2, colScaleBreaks = 1)


# community activities pathways
p2 = semGraph(parTable=urFit1, nodeTable=nodeTable1,
         scaling_factors=NA, standardized=TRUE,
         lineWidth=1.5, curved=0, tapered=FALSE,
         dim=TRUE, highlight=c('com_mobsoc_cumulative', 
                               'com_cause_cumulative',
                               'com_radio_cumulative',
                               'com_vad_touss_cumulative',
                               'lead_gueris_taux',
                               'tb_tfc_cumulative',
                               'tb_vih_arv_cumulative'
         ))


# ----------------------------------------------
# -----------------------------------
# Save output
pdf(paste0('J:/Project/Evaluation/GF/impact_evaluation/sen/special_output/tbmodelv.pdf'), height=6, width=9)
print(p1)
print(p2)
dev.off()
