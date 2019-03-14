# model: drc_malaria4.r
# purpose: drc_malaria2 with GHE and controls for completeness

# TO DO
# Add arrow from RDT_completed to severeMalariaTreated, totalPatientsTreated and ACTs_CHWs

model = '

	# linkage 1 regressions
	
	# linkage 2 regressions
	newCasesMalariaMild_rate ~ lag_ITN_rate + lag_mildMalariaTreated_rate + date
	newCasesMalariaSevere_rate ~ lag_ITN_rate + lag_severeMalariaTreated_rate + date
	malariaDeaths_rate ~ newCasesMalariaMild_rate + newCasesMalariaSevere_rate + lag_mildMalariaTreated_rate + lag_severeMalariaTreated_rate + date
	
	# latent variables
	
	# fixed variances
	# value_RDT_received ~ 1*value_RDT_received
	
	# covariances
	# exp_M1_1_cumulative ~~ other_dah_M1_1_cumulative
	
	# fixed covariances
	# exp_M2_3_cumulative ~~ 0*exp_M3_1_cumulative
	
'
