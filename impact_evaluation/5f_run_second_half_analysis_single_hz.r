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
load(outputFile5c_scratch)

# subset to current health zone
h = unique(data$health_zone)[task_id]
subData = data[health_zone==h]
# ---------------------------


# ----------------------------------------------
# Define model object
source('./impact_evaluation/models/drc_malaria_impact3.r')
# ----------------------------------------------


# --------------------------------------------------------------
# Run model
semFit = bsem(model, subData, adapt=5000, burnin=10000, sample=1000, bcontrol=list(thin=3))

# store summary
summary = data.table(standardizedSolution(semFit, se=TRUE))
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
