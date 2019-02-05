# ------------------------------------------------
# David Phillips
# 
# 1/18/2019
# This runs the SEM dose-response model
# ------------------------------------------------


# ---------------------------
# Load data
data = readRDS(outputFile5a)
# ---------------------------


# ----------------------------------------------
# Define model object
# DECISIONS
# Should we include value_ACT_received in the value_severeMalariaTreated linkage 2 regression?
source('./impact_evaluation/models/drc_malaria2.r')
# ----------------------------------------------


# -------------------------
# Run model
semFit = bsem(model, data, adapt=500, burnin=1000, sample=1000)
summary(semFit)
# -------------------------


# ------------------------------------------------------
# Save model output
save(list=c('data','model','semFit'), file=outputFile5b)
# ------------------------------------------------------
