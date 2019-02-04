# ------------------------------------------------
# David Phillips
# 
# 1/18/2019
# This runs the SEM dose-response model
# ------------------------------------------------


# ----------------------------------------------
# Load data

# ----------------------------------------------


# ----------------------------------------------
# Define model object
# DECISIONS
# Should we include value_ACT_received in the value_severeMalariaTreated linkage 2 regression?

model = '
	# linkage 1 regressions
	value_ITN_received ~ 1*budget_M1_1_cumulative + budget_M1_2_cumulative + other_dah_M1_1_cumulative + other_dah_M1_2_cumulative
	value_RDT_received ~ 1*budget_M2_1_cumulative + budget_M2_3_cumulative + other_dah_M2_1_cumulative + other_dah_M2_3_cumulative
	value_ACT_received ~ 1*budget_M2_1_cumulative + budget_M2_3_cumulative + other_dah_M2_1_cumulative + other_dah_M2_3_cumulative
 
	# linkage 2 regressions
	value_ITN_consumed ~ value_ITN_received
	value_ACTs_CHWs ~ value_ACT_received
	value_RDT_completed ~ value_RDT_received
	value_SP ~ budget_M3_1_cumulative
	value_severeMalariaTreated ~ 1*budget_M2_6_cumulative + value_ACT_received
	value_totalPatientsTreated ~ value_ACT_received
	
	# latent variables
	
	# fixed variances
	# budget_M1_1_cumulative ~~1*budget_M1_1_cumulative
	# budget_M1_2_cumulative ~~1*budget_M1_2_cumulative
	# budget_M2_1_cumulative ~~1*budget_M2_1_cumulative
	# budget_M2_3_cumulative ~~1*budget_M2_3_cumulative
	# other_dah_M1_1_cumulative ~~1*other_dah_M1_1_cumulative
	# other_dah_M1_2_cumulative ~~1*other_dah_M1_2_cumulative
	# other_dah_M2_1_cumulative ~~1*other_dah_M2_1_cumulative
	# other_dah_M2_3_cumulative ~~1*other_dah_M2_3_cumulative
	
	# covariances
	budget_M1_1_cumulative ~~ other_dah_M1_1_cumulative
	budget_M1_2_cumulative ~~ other_dah_M1_2_cumulative
	budget_M2_1_cumulative ~~ other_dah_M2_1_cumulative
	budget_M2_3_cumulative ~~ other_dah_M2_3_cumulative
	
	# fixed covariances
	value_ITN_consumed ~~ 0*value_ACTs_CHWs
	value_ITN_consumed ~~ 0*value_RDT_completed
	value_ITN_consumed ~~ 0*value_SP
	value_ITN_consumed ~~ 0*value_severeMalariaTreated
	value_ITN_consumed ~~ 0*value_totalPatientsTreated
	
	value_ACTs_CHWs ~~ 0*value_RDT_completed
	value_ACTs_CHWs ~~ 0*value_SP
	value_ACTs_CHWs ~~ 0*value_severeMalariaTreated
	value_ACTs_CHWs ~~ 0*value_totalPatientsTreated
	
	value_RDT_completed ~~ 0*value_SP
	value_RDT_completed ~~ 0*value_severeMalariaTreated
	value_RDT_completed ~~ 0*value_totalPatientsTreated
	
	value_SP ~~ 0*value_severeMalariaTreated
	value_SP ~~ 0*value_totalPatientsTreated
	
	value_severeMalariaTreated ~~ 0*value_totalPatientsTreated
'
# ----------------------------------------------


# ----------------------------------------------
# Fix model
semFit = sem(model, wide)
summary(semFit)
# ----------------------------------------------


# ----------------------------------------------
# Display results
# semPaths(semFit, 'std', intercepts=FALSE)
# lavaanPlot(model=semFit, coefs=TRUE)
# ----------------------------------------------
