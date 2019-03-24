# model: drc_malaria4.r
# purpose: drc_malaria2 with GHE and controls for completeness

# TO DO
# Add arrow from RDT_completed to severeMalariaTreated, totalPatientsTreated and ACTs_SSC

model = '

	# linkage 1 regressions
	ITN_received ~ prior("dgamma(1,1)")*exp_M1_1_cumulative + prior("dgamma(1,1)")*exp_M1_2_cumulative + prior("dgamma(1,1)")*other_dah_M1_1_cumulative + date + prior("dgamma(1,1)")*ghe_cumulative
	RDT_received ~ prior("dgamma(1,1)")*exp_M2_1_cumulative + prior("dgamma(1,1)")*exp_M2_3_cumulative + prior("dgamma(1,1)")*other_dah_M2_cumulative + prior("dgamma(1,1)")*other_dah_M2_3_cumulative + date + prior("dgamma(1,1)")*ghe_cumulative
	ACT_received ~ prior("dgamma(1,1)")*exp_M2_1_cumulative + prior("dgamma(1,1)")*exp_M2_3_cumulative + prior("dgamma(1,1)")*other_dah_M2_cumulative + prior("dgamma(1,1)")*other_dah_M2_3_cumulative + date + prior("dgamma(1,1)")*ghe_cumulative
	
	# linkage 1 regressions with hotfixes for heywood cases (temporary)

	
	# linkage 2 regressions
	ITN_consumed ~ prior("dgamma(1,1)")*ITN_received
	ACTs_SSC ~ prior("dgamma(1,1)")*ACT_received
	RDT_completed ~ prior("dgamma(1,1)")*RDT_received
	SP ~ prior("dgamma(1,1)")*exp_M3_1_cumulative + date + prior("dgamma(1,1)")*ghe_cumulative
	severeMalariaTreated ~ prior("dgamma(1,1)")*exp_M2_6_cumulative + prior("dgamma(1,1)")*ACT_received + date + prior("dgamma(1,1)")*ghe_cumulative
	totalPatientsTreated ~ prior("dgamma(1,1)")*ACT_received
	
	# latent variables
	
	# fixed variances
	# RDT_received ~ 1*RDT_received
	# ACT_received ~ 1*ACT_received
	# ITN_received ~ 1*ITN_received
	# ITN_consumed ~ 1*ITN_consumed
	# ACTs_SSC ~ 1*ACTs_SSC
	# RDT_completed ~ 1*RDT_completed
	# SP ~ 1*SP
	# severeMalariaTreated ~ 1*severeMalariaTreated
	# totalPatientsTreated ~ 1*totalPatientsTreated
	
	# covariances
	exp_M1_1_cumulative ~~ other_dah_M1_1_cumulative
	exp_M1_2_cumulative ~~ other_dah_M1_1_cumulative
	exp_M2_1_cumulative ~~ other_dah_M2_cumulative
	exp_M2_1_cumulative ~~ other_dah_M2_cumulative
	exp_M2_6_cumulative ~~ other_dah_M2_cumulative
	exp_M2_3_cumulative ~~ other_dah_M2_3_cumulative
	
	ITN_consumed ~~ RDT_completed
	RDT_completed ~~ totalPatientsTreated
	
	# fixed covariances
	exp_M2_3_cumulative ~~ 0*exp_M3_1_cumulative
	exp_M2_3_cumulative ~~ 0*exp_M2_6_cumulative
	exp_M2_6_cumulative ~~ 0*exp_M3_1_cumulative
	
	ITN_consumed ~~ 0*ACTs_SSC
	# ITN_consumed ~~ 0*RDT_completed
	ITN_consumed ~~ 0*SP
	ITN_consumed ~~ 0*severeMalariaTreated
	ITN_consumed ~~ 0*totalPatientsTreated
	
	ACTs_SSC ~~ 0*RDT_completed
	ACTs_SSC ~~ 0*SP
	ACTs_SSC ~~ 0*severeMalariaTreated
	ACTs_SSC ~~ 0*totalPatientsTreated
	
	RDT_completed ~~ 0*SP
	RDT_completed ~~ 0*severeMalariaTreated
	# RDT_completed ~~ 0*totalPatientsTreated
	
	SP ~~ 0*severeMalariaTreated
	SP ~~ 0*totalPatientsTreated
	
	severeMalariaTreated ~~ 0*totalPatientsTreated
'
