# -----------------------------------
# David Phillips
# 
# 2/4/2019
# This visualizes results of the SEM
# -----------------------------------


# -----------------------------------------------
# Load/prep data and functions

# load home-made sem graphing function FIX THIS FILE PATH
source('./impact_evaluation/visualizations/graphLavaan.r')

# load model results
load(outputFile5b)

# load nodeTable for graphing FIX THIS FILE PATH
nodeTable = fread('C:/local/gf/impact_evaluation/visualizations/vartable.csv')

# ensure there are no extra variables introducted from nodeTable
nodeTable = nodeTable[variable %in% names(data)]
# -----------------------------------------------


# ----------------------------------------------
# Display results

# pre-packed alternatives for sem graphics
# semPaths(semFit, 'std', intercepts=FALSE)
# lavaanPlot(model=semFit, coefs=TRUE)

# my sem graph function
semGraph(semFit, nodeTable=nodeTable)
# ----------------------------------------------
