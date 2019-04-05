# model: drc_malaria4.r
# purpose: drc_malaria2 with GHE

# TO DO
# Add arrow from RDT_completed to severeMalariaTreated, totalPatientsTreated and ACTs_SSC

model = '

	# linkage 1 regressions
	ITN_received_cumulative ~ prior("dgamma(1,1)")*exp_M1_1_cumulative + prior("dgamma(1,1)")*exp_M1_2_cumulative + prior("dgamma(1,1)")*other_dah_M1_1_cumulative + date + prior("dgamma(1,1)")*ghe_cumulative
	RDT_received_cumulative ~ prior("dgamma(1,1)")*exp_M2_1_cumulative + prior("dgamma(1,1)")*other_dah_M2_cumulative + date + prior("dgamma(1,1)")*ghe_cumulative
	ACT_received_cumulative ~ prior("dgamma(1,1)")*exp_M2_1_cumulative + prior("dgamma(1,1)")*other_dah_M2_cumulative + date + prior("dgamma(1,1)")*ghe_cumulative
	
	# linkage 1 regressions with hotfixes for heywood cases (temporary)

	
	# linkage 2 regressions
	ITN_consumed_cumulative ~ prior("dgamma(1,1)")*ITN_received_cumulative
	ACTs_SSC_cumulative ~  prior("dgamma(1,1)")*exp_M2_3_cumulative + prior("dgamma(1,1)")*other_dah_M2_3_cumulative + prior("dgamma(1,1)")*ghe_cumulative
	RDT_completed_cumulative ~ prior("dgamma(1,1)")*RDT_received_cumulative
	SP_cumulative ~ prior("dgamma(1,1)")*exp_M3_1_cumulative + date + prior("dgamma(1,1)")*ghe_cumulative
	severeMalariaTreated_cumulative ~ prior("dgamma(1,1)")*exp_M2_6_cumulative + prior("dgamma(1,1)")*ACT_received_cumulative + date + prior("dgamma(1,1)")*ghe_cumulative
	totalPatientsTreated_cumulative ~ prior("dgamma(1,1)")*ACT_received_cumulative
	
	# latent variables
	
	# fixed variances
	
	# covariances
	exp_M1_1_cumulative ~~ other_dah_M1_1_cumulative
	exp_M1_2_cumulative ~~ other_dah_M1_1_cumulative
	exp_M2_1_cumulative ~~ other_dah_M2_cumulative
	exp_M2_1_cumulative ~~ other_dah_M2_cumulative
	exp_M2_6_cumulative ~~ other_dah_M2_cumulative
	exp_M2_3_cumulative ~~ other_dah_M2_3_cumulative
	
	# fixed covariances
	exp_M2_3_cumulative ~~ 0*exp_M3_1_cumulative
	exp_M2_3_cumulative ~~ 0*exp_M2_6_cumulative
	exp_M2_6_cumulative ~~ 0*exp_M3_1_cumulative
	
	ITN_consumed_cumulative ~~ 0*ACTs_SSC_cumulative
	ITN_consumed_cumulative ~~ 0*RDT_completed_cumulative
	ITN_consumed_cumulative ~~ 0*SP_cumulative
	ITN_consumed_cumulative ~~ 0*severeMalariaTreated_cumulative
	ITN_consumed_cumulative ~~ 0*totalPatientsTreated_cumulative
	
	ACTs_SSC_cumulative ~~ 0*RDT_completed_cumulative
	ACTs_SSC_cumulative ~~ 0*SP_cumulative
	ACTs_SSC_cumulative ~~ 0*severeMalariaTreated_cumulative
	ACTs_SSC_cumulative ~~ 0*totalPatientsTreated_cumulative
	
	RDT_completed_cumulative ~~ 0*SP_cumulative
	RDT_completed_cumulative ~~ 0*severeMalariaTreated_cumulative
	
	SP_cumulative ~~ 0*severeMalariaTreated_cumulative
	SP_cumulative ~~ 0*totalPatientsTreated_cumulative
	
	severeMalariaTreated_cumulative ~~ 0*totalPatientsTreated_cumulative
'
