# ------------------------------------------------
# David Phillips
# 
# 3/18/2019
# This runs the SEM dose response model for one health zone
# Intended to be run in parallel by 5d
# The current working directory should be the root of this repo
# Command-Line Arguments:
# modelVersion - (character) name of the model script to loaded (not including the file extension)
# modelStage - (numeric) 1 for first half of results chain, 2 for second half (controls input and output file names, must make sense given modelVersion)
# testRun - (logical) TRUE will run the model with limited MCMC steps, FALSE will run the full thing
# ------------------------------------------------

source('./impact_evaluation/drc/set_up_r.r')

# for testing purposes
# task_id = 12
# args = c('drc_malaria6', '1', 'TRUE')

# ----------------------------------------------
# Store task ID and other args from command line
if (!'task_id' %in% ls()) task_id <- as.integer(Sys.getenv("SGE_TASK_ID"))

# store non-system command arguments
print(commandArgs())
print(commandArgs(trailingOnly=TRUE))
if (!'args' %in% ls()) args = commandArgs(trailingOnly=TRUE)
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
if (Sys.info()[1]!='Windows' & modelStage==1) load(outputFile4a_scratch)
if (Sys.info()[1]=='Windows' & modelStage==1) load(outputFile4a)
if (Sys.info()[1]!='Windows' & modelStage==2) load(outputFile4b_scratch)
if (Sys.info()[1]=='Windows' & modelStage==2) load(outputFile4b)

# subset to current health zone
h = unique(data$health_zone)[task_id]
subData = data[health_zone==h]

# define model object
source(paste0('./impact_evaluation/drc/models/', modelVersion, '.r'))

# reduce the data down to only necessary variables
parsedModel = lavParseModelString(model)
modelVars = unique(c(parsedModel$lhs, parsedModel$rhs))
modelVars = c('health_zone','date',modelVars)
subData = subData[, unique(modelVars), with=FALSE]

# jitter to avoid perfect collinearity
for(v in names(subData)[!names(subData)%in%c('orig_health_zone','health_zone','date')]) { 
	if (all(subData[[v]]>0)) subData[, (v):=get(v)+rpois(nrow(subData), (sd(subData[[v]])+2)/10)]
	if (!all(subData[[v]]>0)) subData[, (v):=get(v)+rnorm(nrow(subData), 0, (sd(subData[[v]])+2)/10)]
}

# rescale variables to have similar variance
# see Kline Principles and Practice of SEM (2011) page 67
scaling_factors = data.table(date=1)
numVars = names(subData)[!names(subData)%in%c('orig_health_zone','health_zone','date')]
for(v in numVars) {
	s=1
	while(var(subData[[v]]/s)>1000) s=s*10
	while(var(subData[[v]]/s)<100) s=s/10
	scaling_factors[,(v):=s]
}
for(v in names(scaling_factors)) subData[, (v):=get(v)/scaling_factors[[v]]]
# ---------------------------------------------------------------------------------------------------


# ----------------------------------------------------------------
# Run model

# fit model
if (testRun==TRUE) semFit = bsem(model, subData, adapt=50, burnin=10, sample=10, bcontrol=list(thin=3))
if (testRun==FALSE) semFit = bsem(model, subData, adapt=5000, burnin=10000, sample=1000, bcontrol=list(thin=3))

# run series of unrelated linear models for comparison
urFit = lavaanUR(model, subData)
# ----------------------------------------------------------------


# --------------------------------------------------------------
# Store coefficient table from model

# get standardized solution
standardizedSummary = data.table(standardizedSolution(semFit, se=TRUE))
setnames(standardizedSummary, 'se', 'se.std')
standardizedSummary = standardizedSummary[, -c('ci.lower', 'ci.upper')]

# get unstandardized parameter values
summary = data.table(parTable(semFit))
summary = summary[, c('lhs','op','rhs','est','se'), with=FALSE]

# unrescale SEM coefficients to reflect actual units of x and y variables
tmp = unique(melt(scaling_factors, value.name='scaling_factor'))
summary = merge(summary, tmp, by.x='rhs', by.y='variable', all.x=TRUE)
summary = merge(summary, tmp, by.x='lhs', by.y='variable', all.x=TRUE, suffixes=c('.rhs','.lhs'))
summary[is.na(scaling_factor.rhs), scaling_factor.rhs:=1]
summary[is.na(scaling_factor.lhs), scaling_factor.lhs:=1]
summary[, est_raw:=est] # just to be able to see the rescaled value
summary[, est:=est/(scaling_factor.rhs/scaling_factor.lhs)]
summary[, se:=se/(scaling_factor.rhs/scaling_factor.lhs)]

# unrescale UR coefficients to reflect actual units of x and y variables
urFit = merge(urFit, tmp, by.x='rhs', by.y='variable', all.x=TRUE)
urFit = merge(urFit, tmp, by.x='lhs', by.y='variable', all.x=TRUE, suffixes=c('.rhs','.lhs'))
urFit[is.na(scaling_factor.rhs), scaling_factor.rhs:=1]
urFit[is.na(scaling_factor.lhs), scaling_factor.lhs:=1]
urFit[, est_raw:=est] # just to be able to see the rescaled value
urFit[, est:=est/(scaling_factor.rhs/scaling_factor.lhs)]
urFit[, se:=se/(scaling_factor.rhs/scaling_factor.lhs)]

# add standardized coefficients to summary
summary = merge(summary, standardizedSummary, by=c('lhs','op','rhs'))

# label health zone
urFit[, health_zone:=h]
summary[, health_zone:=h]
# --------------------------------------------------------------


# ------------------------------------------------------------------
# Save model output and clean up

# make unique file name
if(modelStage==1) outputFile5tmp1 = paste0(clustertmpDir2, 'first_half_semFit_', task_id, '.rds')
if(modelStage==1) outputFile5tmp2 = paste0(clustertmpDir2, 'first_half_summary_', task_id, '.rds')
if(modelStage==1) outputFile5tmp3 = paste0(clustertmpDir2, 'first_half_urFit_', task_id, '.rds')
if(modelStage==2) outputFile5tmp1 = paste0(clustertmpDir2, 'second_half_semFit_', task_id, '.rds')
if(modelStage==2) outputFile5tmp2 = paste0(clustertmpDir2, 'second_half_summary_', task_id, '.rds')
if(modelStage==2) outputFile5tmp3 = paste0(clustertmpDir2, 'second_half_urFit_', task_id, '.rds')

# save
print(paste('Saving:', outputFile5tmp1))
saveRDS(semFit, file=outputFile5tmp1)

print(paste('Saving:', outputFile5tmp2))
saveRDS(summary, file=outputFile5tmp2)

print(paste('Saving:', outputFile5tmp3))
saveRDS(urFit, file=outputFile5tmp3)
# ------------------------------------------------------------------
