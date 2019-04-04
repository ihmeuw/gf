# ------------------------------------------------
# David Phillips
# 
# 3/18/2019
# This runs the second-half SEM dose response model for one health zone
# Intended to be run in parallel by 5d
# The current working directory should be the root of this repo
# Command-Line Arguments:
# modelVersion - (character) name of the model script to loaded (not including the file extension)
# ------------------------------------------------

# ----------------------------------------------
# Store task ID and other args from command line
task_id <- as.integer(Sys.getenv("SGE_TASK_ID"))

# cut args down to non-system variables
args = commandArgs(trailingOnly=TRUE)

# the second argument should be the model version to use
modelVersion = args[1]

# print for log
print(paste('Task ID:', task_id))
print(paste('Command Args:', args))
print(paste('Model Version:', modelVersion))
# ----------------------------------------------

source('./impact_evaluation/_common/set_up_r.r')


# ---------------------------
# Load data
set.seed(1)
load(outputFile5d_scratch)

# subset to current health zone
h = unique(data$health_zone)[task_id]
subData = data[health_zone==h]
# ---------------------------


# ----------------------------------------------
# Define model object
source(paste0('./impact_evaluation/models/', modelVersion, '.r'))
# ----------------------------------------------


# --------------------------------------------------------------
# Run model
# jitter completeness variables to avoid perfect collinearity
for(v in names(subData)[grepl('completeness', names(subData))]) { 
	subData[, (v):=get(v)+rnorm(nrow(subData), 0, (sd(subData[[v]])+2)/10)]
}

semFit = bsem(model, subData, adapt=5000, burnin=10000, sample=1000, bcontrol=list(thin=3))

# store summary
standardizedSummary = data.table(standardizedSolution(semFit, se=TRUE))
setnames(standardizedSummary, c('se','ci.lower', 'ci.upper'), c('se.std','ci.lower.std','ci.upper.std'))
summary = data.table(parTable(semFit))
summary = merge(summary, standardizedSummary, by=c('lhs','op','rhs'))
summary[, health_zone:=h]
# --------------------------------------------------------------


# ------------------------------------------------------------------
# Save model output and clean up

# make unique file name
outputFile5ftmp1 = paste0(clustertmpDir2, 'second_half_model_results_', task_id, '.rdata')
outputFile5ftmp2 = paste0(clustertmpDir2, 'second_half_semFit_', task_id, '.rds')
outputFile5ftmp3 = paste0(clustertmpDir2, 'second_half_summary_', task_id, '.rds')

# save
print(paste('Saving:', outputFile5ftmp2))
# save(list=c('subData','model','semFit','summary'), file=outputFile5ftmp1)
saveRDS(semFit, file=outputFile5ftmp2)
saveRDS(summary, file=outputFile5ftmp3)
# ------------------------------------------------------------------
