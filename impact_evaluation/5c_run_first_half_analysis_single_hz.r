# ------------------------------------------------
# David Phillips
# 
# 3/18/2019
# This runs the second-half SEM dose response model for one health zone
# Intended to be run in parallel by 5d
# The current working directory should be the root of this repo
# ------------------------------------------------

# ----------------------------------------------
# Store task ID from command line
task_id <- as.integer(Sys.getenv("SGE_TASK_ID"))
# ----------------------------------------------

source('./impact_evaluation/_common/set_up_r.r')


# ---------------------------
# Load data
set.seed(1)
load(outputFile5a_scratch)

# subset to current health zone
h = unique(data$health_zone)[task_id]
subData = data[health_zone==h]
# ---------------------------


# ----------------------------------------------
# Define model object
source('./impact_evaluation/models/drc_malaria5.r')
# ----------------------------------------------


# --------------------------------------------------------------
# Run model
# jitter to avoid perfect collinearity
for(v in names(subData)[!names(subData)%in%c('health_zone','date')]) { 
	subData[, (v):=get(v)+rpois(nrow(subData), (sd(subData[[v]])+1)/10)]
}

# fit model
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
outputFile5ctmp1 = paste0(clustertmpDir2, 'first_half_model_results_', task_id, '.rdata')
outputFile5ctmp2 = paste0(clustertmpDir2, 'first_half_semFit_', task_id, '.rds')
outputFile5ctmp3 = paste0(clustertmpDir2, 'first_half_summary_', task_id, '.rds')

# save
print(paste('Saving:', outputFile5ctmp2))
# save(list=c('subData','model','semFit','summary'), file=outputFile5ctmp1)
saveRDS(semFit, file=outputFile5ctmp2)
saveRDS(summary, file=outputFile5ctmp3)
# ------------------------------------------------------------------
