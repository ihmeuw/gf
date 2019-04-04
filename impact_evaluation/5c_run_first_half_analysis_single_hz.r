# ------------------------------------------------
# David Phillips
# 
# 3/18/2019
# This runs the second-half SEM dose response model for one health zone
# Intended to be run in parallel by 5d
# The current working directory should be the root of this repo
# Command-Line Arguments:
# modelVersion - (character) name of the model script to loaded (not including the file extension)
# modelStage - (numeric) 1 for first half of results chain, 2 for second half (controls input and output file names, must make sense given modelVersion)
# testRun - (logical) TRUE will run the model with limited MCMC steps, FALSE will run the full thing
# ------------------------------------------------

source('./impact_evaluation/_common/set_up_r.r')


# ----------------------------------------------
# Store task ID and other args from command line
task_id <- as.integer(Sys.getenv("SGE_TASK_ID"))

# store non-system command arguments
args = commandArgs(trailingOnly=TRUE)
if(length(args)==0) stop('No commandArgs found!') 

# the first argument should be the model version to use
modelVersion = args[1]

# the second argument should be the "model stage" (1 or 2)
modelStage = as.numeric(args[2])

# the third argument should be whether to run a test run (TRUE) or full run (FALSE)
testRun = as.logical(args[3])

# print for log
print(paste('Task ID:', task_id))
print(paste('Command Args:', args))
print(paste('Model Version:', modelVersion))
print(paste('Model Stage:', modelStage))
print(paste('Test Run:', testRun))
# ----------------------------------------------


# ---------------------------------------------------------------------------------------------------
# Load data
set.seed(1)
if (Sys.info()[1]!='Windows' & modelStage==1) load(outputFile5a_scratch)
if (Sys.info()[1]=='Windows' & modelStage==1) load(outputFile5a)
if (Sys.info()[1]!='Windows' & modelStage==2) load(outputFile5d_scratch)
if (Sys.info()[1]=='Windows' & modelStage==2) load(outputFile5d)

# subset to current health zone
h = unique(data$health_zone)[task_id]
subData = data[health_zone==h]

# jitter to avoid perfect collinearity
for(v in names(subData)[!names(subData)%in%c('orig_health_zone','health_zone','date')]) { 
	if (all(subData[[v]]>0)) subData[, (v):=get(v)+rpois(nrow(subData), (sd(subData[[v]])+2)/10)]
	if (!all(subData[[v]]>0)) subData[, (v):=get(v)+rnorm(nrow(subData), 0, (sd(subData[[v]])+2)/10)]
}
# ---------------------------------------------------------------------------------------------------


# ----------------------------------------------------------------
# Run model

# define model object
source(paste0('./impact_evaluation/models/', modelVersion, '.r'))

# fit model
if (testRun==TRUE) semFit = bsem(model, subData, adapt=50, burnin=10, sample=10, bcontrol=list(thin=3))
if (testRun==FALSE) semFit = bsem(model, subData, adapt=5000, burnin=10000, sample=1000, bcontrol=list(thin=3))
# ----------------------------------------------------------------


# --------------------------------------------------------------
# Store coefficient table from model

# get standardized solution
standardizedSummary = data.table(standardizedSolution(semFit, se=TRUE))
setnames(standardizedSummary, c('se','ci.lower', 'ci.upper'), c('se.std','ci.lower.std','ci.upper.std'))

# get unstandardized parameter values
summary = data.table(parTable(semFit))
summary = summary[, c('lhs','op','rhs','est','se'), with=FALSE]

# compute uncertainty intervals
summary[, lower:=est-(1.95996*se)]
summary[, upper:=est+(1.95996*se)]

# unrescale coefficients to reflect actual units of x and y variables
tmp = unique(melt(scaling_factors, value.name='scaling_factor'))
summary = merge(summary, tmp, by.x='rhs', by.y='variable', all.x=TRUE)
summary = merge(summary, tmp, by.x='lhs', by.y='variable', all.x=TRUE, suffixes=c('.rhs','.lhs'))
summary[is.na(scaling_factor.rhs), scaling_factor.rhs:=1]
summary[is.na(scaling_factor.lhs), scaling_factor.lhs:=1]
summary[, est_unrescaled:=est/(scaling_factor.rhs/scaling_factor.lhs)]
summary[, lower_unrescaled:=lower/(scaling_factor.rhs/scaling_factor.lhs)]
summary[, upper_unrescaled:=upper/(scaling_factor.rhs/scaling_factor.lhs)]
summary$lower = NULL
summary$upper = NULL

# store summary
summary = merge(summary, standardizedSummary, by=c('lhs','op','rhs'))
summary[, health_zone:=h]
# --------------------------------------------------------------


# ------------------------------------------------------------------
# Save model output and clean up

# make unique file name
if(modelStage==1) outputFile5tmp1 = paste0(clustertmpDir2, 'first_half_semFit_', task_id, '.rds')
if(modelStage==1) outputFile5tmp2 = paste0(clustertmpDir2, 'first_half_summary_', task_id, '.rds')
if(modelStage==2) outputFile5tmp1 = paste0(clustertmpDir2, 'second_half_semFit_', task_id, '.rds')
if(modelStage==2) outputFile5tmp2 = paste0(clustertmpDir2, 'second_half_summary_', task_id, '.rds')

# save
print(paste('Saving:', outputFile5tmp1))
saveRDS(semFit, file=outputFile5tmp1)

print(paste('Saving:', outputFile5tmp2))
saveRDS(summary, file=outputFile5tmp2)
# ------------------------------------------------------------------
