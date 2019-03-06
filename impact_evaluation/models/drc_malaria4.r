# model: drc_malaria4.r
# purpose: drc_malaria2 with GHE and controls for completeness

# TO DO
# Add arrow from RDT_completed to severeMalariaTreated, totalPatientsTreated and ACTs_CHWs

model = '

	# linkage 1 regressions
	value_ITN_received ~ 1*exp_M1_1_cumulative + prior("dgamma(1,1)")*exp_M1_2_cumulative + prior("dgamma(1,1)")*other_dah_M1_1_cumulative + date + prior("dgamma(1,1)")*ghe_cumulative
	value_RDT_received ~ 1*exp_M2_1_cumulative + prior("dgamma(1,1)")*exp_M2_3_cumulative + prior("dgamma(1,1)")*other_dah_M2_cumulative + prior("dgamma(1,1)")*other_dah_M2_3_cumulative + date + prior("dgamma(1,1)")*ghe_cumulative
	value_ACT_received ~ 1*exp_M2_1_cumulative + prior("dgamma(1,1)")*exp_M2_3_cumulative + prior("dgamma(1,1)")*other_dah_M2_cumulative + prior("dgamma(1,1)")*other_dah_M2_3_cumulative + date + prior("dgamma(1,1)")*ghe_cumulative
	
	# linkage 1 regressions with hotfixes for heywood cases (temporary)

	
	# linkage 2 regressions
	value_ITN_consumed ~ prior("dgamma(1,1)")*value_ITN_received
	value_ACTs_CHWs ~ prior("dgamma(1,1)")*value_ACT_received
	value_RDT_completed ~ prior("dgamma(1,1)")*value_RDT_received
	value_SP ~ prior("dgamma(1,1)")*exp_M3_1_cumulative + date + prior("dgamma(1,1)")*ghe_cumulative
	value_severeMalariaTreated ~ 1*exp_M2_6_cumulative + prior("dgamma(1,1)")*value_ACT_received + date + prior("dgamma(1,1)")*ghe_cumulative
	value_totalPatientsTreated ~ prior("dgamma(1,1)")*value_ACT_received
	
	# latent variables
	
	# fixed variances
	# value_RDT_received ~ 1*value_RDT_received
	# value_ACT_received ~ 1*value_ACT_received
	# value_ITN_received ~ 1*value_ITN_received
	# value_ITN_consumed ~ 1*value_ITN_consumed
	# value_ACTs_CHWs ~ 1*value_ACTs_CHWs
	# value_RDT_completed ~ 1*value_RDT_completed
	# value_SP ~ 1*value_SP
	# value_severeMalariaTreated ~ 1*value_severeMalariaTreated
	# value_totalPatientsTreated ~ 1*value_totalPatientsTreated
	
	# covariances
	exp_M1_1_cumulative ~~ other_dah_M1_1_cumulative
	exp_M1_2_cumulative ~~ other_dah_M1_1_cumulative
	exp_M2_1_cumulative ~~ other_dah_M2_cumulative
	exp_M2_1_cumulative ~~ other_dah_M2_cumulative
	exp_M2_6_cumulative ~~ other_dah_M2_cumulative
	exp_M2_3_cumulative ~~ other_dah_M2_3_cumulative
	
	value_ITN_consumed ~~ value_RDT_completed
	value_RDT_completed ~~ value_totalPatientsTreated
	
	# fixed covariances
	exp_M2_3_cumulative ~~ 0*exp_M3_1_cumulative
	exp_M2_3_cumulative ~~ 0*exp_M2_6_cumulative
	exp_M2_6_cumulative ~~ 0*exp_M3_1_cumulative
	
	value_ITN_consumed ~~ 0*value_ACTs_CHWs
	# value_ITN_consumed ~~ 0*value_RDT_completed
	value_ITN_consumed ~~ 0*value_SP
	value_ITN_consumed ~~ 0*value_severeMalariaTreated
	value_ITN_consumed ~~ 0*value_totalPatientsTreated
	
	value_ACTs_CHWs ~~ 0*value_RDT_completed
	value_ACTs_CHWs ~~ 0*value_SP
	value_ACTs_CHWs ~~ 0*value_severeMalariaTreated
	value_ACTs_CHWs ~~ 0*value_totalPatientsTreated
	
	value_RDT_completed ~~ 0*value_SP
	value_RDT_completed ~~ 0*value_severeMalariaTreated
	# value_RDT_completed ~~ 0*value_totalPatientsTreated
	
	value_SP ~~ 0*value_severeMalariaTreated
	value_SP ~~ 0*value_totalPatientsTreated
	
	value_severeMalariaTreated ~~ 0*value_totalPatientsTreated
'
