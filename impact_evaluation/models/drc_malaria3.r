# model: drc_malaria3.r
# purpose: drc_malaria1, controlling for completeness
model = '

	# linkage 1 regressions
	value_ITN_received ~ 1*budget_M1_1_cumulative + budget_M1_2_cumulative + other_dah_M1_1_cumulative + other_dah_M1_2_cumulative
	value_RDT_received ~ 1*budget_M2_1_cumulative + budget_M2_3_cumulative + other_dah_M2_1_cumulative + other_dah_M2_3_cumulative
	value_ACT_received ~ 1*budget_M2_1_cumulative + budget_M2_3_cumulative + other_dah_M2_1_cumulative + other_dah_M2_3_cumulative
	
	# linkage 1 regressions with hotfixes for heywood cases (temporary)
	
	
	# linkage 2 regressions
	value_ITN_consumed ~ value_ITN_received + completeness_ITN_received
	value_ACTs_CHWs ~ value_ACT_received + completeness_ACT_received
	value_RDT_completed ~ value_RDT_received + completeness_RDT_received
	value_SP ~ budget_M3_1_cumulative
	value_severeMalariaTreated ~ 1*budget_M2_6_cumulative + value_ACT_received + completeness_ACT_received
	value_totalPatientsTreated ~ value_ACT_received + completeness_ACT_received
	
	# latent variables
	
	# fixed variances
	value_RDT_received ~ 1*value_RDT_received
	value_ACT_received ~ 1*value_ACT_received
	value_ITN_received ~ 1*value_ITN_received
	value_ITN_consumed ~ 1*value_ITN_consumed
	value_ACTs_CHWs ~ 1*value_ACTs_CHWs
	value_RDT_completed ~ 1*value_RDT_completed
	value_SP ~ 1*value_SP
	value_severeMalariaTreated ~ 1*value_severeMalariaTreated
	value_totalPatientsTreated ~ 1*value_totalPatientsTreated
	
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
