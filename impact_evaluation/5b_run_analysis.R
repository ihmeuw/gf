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

lmFit1 = lm(value_ITN_received ~ exp_M1_1_cumulative + exp_M1_2_cumulative + other_dah_M1_1_cumulative, data)
lmFit2 = lm(value_RDT_received ~ exp_M2_1_cumulative + exp_M2_3_cumulative + other_dah_M2_cumulative + other_dah_M2_3_cumulative, data)
lmFit3 = lm(value_ACT_received ~ exp_M2_1_cumulative + exp_M2_3_cumulative + other_dah_M2_cumulative + other_dah_M2_3_cumulative, data)

# linkage 2 regressions
lmFit4 = lm(value_ITN_consumed ~ value_ITN_received, data)
lmFit5 = lm(value_ACTs_CHWs ~ value_ACT_received, data)
lmFit6 = lm(value_RDT_completed ~ value_RDT_received, data)
lmFit7 = lm(value_SP ~ exp_M3_1_cumulative, data)
lmFit8 = lm(value_severeMalariaTreated ~ exp_M2_6_cumulative + value_ACT_received, data)
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
source('./impact_evaluation/models/drc_malaria4.r')
# ----------------------------------------------


# --------------------------------------------------------------
# Run model
if ('semFit' %in% ls()) rm('semFit')
# semFit = sem(model, data)
semFit = bsem(model, data, adapt=5000, burnin=10000, sample=1000, bcontrol=list(thin=3))
summary(semFit)
# --------------------------------------------------------------


# ------------------------------------------------------------------
# Save model output and clean up

# save all sem fits just in case they're needed
print(paste('Saving', outputFile5b))
save(list=c('data','model','summaries','means','scaling_factors'), file=outputFile5b)

# save full output for archiving
print(paste('Saving', outputFile5b_big))
semFits = lapply(seq(T), function(i) {
	suppressWarnings(readRDS(paste0(clustertmpDir2, 'first_half_semFit_', i, '.rds')))
})
outputFile5b_big = gsub('.rdata','_all_semFits.rdata',outputFile5b)
save(list=c('data','model','semFits','summaries','means','scaling_factors'), file=outputFile5b_big)

# save a time-stamped version for reproducibility
print('Archiving files...')
date_time = gsub('-|:| ', '_', Sys.time())
outputFile5bArchive = gsub('prepped_data/', 'prepped_data/model_runs/', outputFile5b)
outputFile5bArchive = gsub('.rdata', paste0('_', date_time, '.rdata'), outputFile5bArchive)
file.copy(outputFile5b, outputFile5bArchive)
outputFile5bArchive_big = gsub('prepped_data/', 'prepped_data/model_runs/', outputFile5b_big)
outputFile5bArchive_big = gsub('.rdata', paste0('_', date_time, '.rdata'), outputFile5bArchive_big)
file.copy(outputFile5b_big, outputFile5bArchive_big)

# clean up in case jags saved some output
if(dir.exists('./lavExport/')) unlink('./lavExport', recursive=TRUE)

# clean up qsub files
print(paste('Cleaning up cluster temp files...'))
if (runAsQsub==TRUE) { 
	system(paste0('rm ', clustertmpDireo, '/*'))
	system(paste0('rm ', clustertmpDir1	, '/*'))
	system(paste0('rm ', clustertmpDir2	, '/*'))
}
# ------------------------------------------------------------------
