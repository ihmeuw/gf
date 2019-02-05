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
# Run series of unrelated linear models

lmFit1 = lm(value_ITN_received ~ budget_M1_1_cumulative + budget_M1_2_cumulative + other_dah_M1_1_cumulative + other_dah_M1_2_cumulative, data)
lmFit1 = lm(value_RDT_received ~ budget_M2_1_cumulative + budget_M2_3_cumulative + other_dah_M2_1_cumulative + other_dah_M2_3_cumulative, data)
lmFit1 = lm(value_ACT_received ~ budget_M2_1_cumulative + budget_M2_3_cumulative + other_dah_M2_1_cumulative + other_dah_M2_3_cumulative, data)

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


# -------------------------
# Run model
semFit = bsem(model, data, adapt=500, burnin=1000, sample=1000)
summary(semFit)
# -------------------------


# ------------------------------------------------------------------
# Save model output and clean up

# save
save(list=c('data','model','semFit'), file=outputFile5b)

# clean up in case jags saved some output
if(dir.exists('./lavExport/')) unlink('./lavExport', recursive=TRUE)
# ------------------------------------------------------------------
