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
model = readLines('./impact_evaluation/models/drc_malaria1.r')
# ----------------------------------------------


# -------------------------
# Run model
semFit = sem(model, data)
summary(semFit)
# -------------------------


# ------------------------------------------------------
# Save model output
save(list=c('data','model','semFit'), file=outputFile5b)
# ------------------------------------------------------
