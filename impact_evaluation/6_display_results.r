# -----------------------------------
# David Phillips
# 
# 2/4/2019
# This visualizes results of the SEM
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
load(outputFile5d)
data2=copy(data)

# load nodeTable for graphing
nodeTable1 = fread('./impact_evaluation/visualizations/vartable.csv')
nodeTable2 = fread('./impact_evaluation/visualizations/vartable_second_half.csv')

# ensure there are no extra variables introducted from nodeTable
nodeTable1 = nodeTable1[variable %in% names(data1)]
nodeTable2 = nodeTable2[variable %in% names(data2)]
# -----------------------------------------------


# ----------------------------------------------
# Display results

# my sem graph function for first half model
p1 = semGraph(semFit, nodeTable=nodeTable1, 
	scaling_factors=NA, standardized=TRUE, 
	lineWidth=1.5, curved=0, tapered=FALSE)

# my sem graph function for second half model
p2 = semGraph(parTable=means, nodeTable=nodeTable2, 
	scaling_factors=NA, standardized=TRUE, 
	lineWidth=1.5, curved=0, tapered=FALSE, 
	boxWidth=2, boxHeight=.5)
# ----------------------------------------------


# -----------------------------------
# Save output
pdf(outputFile6, height=6, width=9)
p1
p2
dev.off()

# save a time-stamped version for reproducibility
date_time = gsub('-|:| ', '_', Sys.time())
outputFile6Archive = gsub('visualizations/', 'visualizations/archive/', outputFile6)
outputFile6Archive = gsub('.pdf', paste0('_', date_time, '.pdf'), outputFile6Archive)
file.copy(outputFile6, outputFile6Archive)
# -----------------------------------
