# ------------------------------------------------
# David Phillips
# 
# 1/18/2019
# This runs the SEM dose-response model
# ------------------------------------------------


# ---------------------------
# Load data
set.seed(1)
load(outputFile5a)
# ---------------------------


# -------------------------
# Run series of unrelated linear models

lmFit1 = lm(value_ITN_received ~ budget_M1_1_cumulative + other_dah_M1_1_cumulative, data)
lmFit2 = lm(value_RDT_received ~ budget_M2_1_cumulative + budget_M2_3_cumulative + other_dah_M2_cumulative + other_dah_M2_3_cumulative, data)
lmFit3 = lm(value_ACT_received ~ budget_M2_1_cumulative + budget_M2_3_cumulative + other_dah_M2_cumulative + other_dah_M2_3_cumulative, data)

# linkage 2 regressions
lmFit4 = lm(value_ITN_consumed ~ value_ITN_received, data)
lmFit5 = lm(value_ACTs_CHWs ~ value_ACT_received, data)
lmFit6 = lm(value_RDT_completed ~ value_RDT_received, data)
lmFit7 = lm(value_SP ~ budget_M3_1_cumulative, data)
lmFit8 = lm(value_severeMalariaTreated ~ budget_M2_6_cumulative + value_ACT_received, data)
lmFit9 = lm(value_totalPatientsTreated ~ value_ACT_received, data)

summary(lmFit1)
summary(lmFit2)
summary(lmFit3)
summary(lmFit4)
summary(lmFit5)
summary(lmFit6)
summary(lmFit7)
summary(lmFit8)
summary(lmFit9)
# -------------------------


# ----------------------------------------------
# Define model object
# DECISIONS
# Should we include value_ACT_received in the value_severeMalariaTreated linkage 2 regression?
# Currently combining M1_1 (mass campaigns) and M1_2 (continuous) because FGH can't distinguish
# including date as a control variable in linkage 1 regressions because otherwise all RT variables are positively correlated (when GF and other should be negative)
source('./impact_evaluation/models/drc_malaria2.r')
# ----------------------------------------------


# --------------------------------------------------------------
# Run model
if ('semFit' %in% ls()) rm('semFit')
# semFit = sem(model, data)
semFit = bsem(model, data, adapt=5000, burnin=1000, sample=1000)
summary(semFit)
# --------------------------------------------------------------


# ------------------------------------------------------------------
# Save model output and clean up

# save
save(list=c('data','model','semFit','scaling_factors'), file=outputFile5b)

# save a time-stamped version for reproducibility
date_time = gsub('-|:| ', '_', Sys.time())
outputFile5bArchive = gsub('prepped_data/', 'prepped_data/model_runs/', outputFile5b)
outputFile5bArchive = gsub('.rdata', paste0('_', date_time, '.rdata'), outputFile5bArchive)
save(list=c('data','model','semFit','scaling_factors'), file=outputFile5bArchive)

# clean up in case jags saved some output
if(dir.exists('./lavExport/')) unlink('./lavExport', recursive=TRUE)
# ------------------------------------------------------------------
