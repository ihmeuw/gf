# model: drc_malaria6.r
# purpose: drc_malaria6 with OOP expenditure in places

model = '

	# linkage 1 regressions
	ITN_received_cumulative ~ prior("dgamma(1,1)")*lag_exp_M1_1_cumulative + prior("dgamma(1,1)")*lag_exp_M1_2_cumulative + prior("dgamma(1,1)")*lag_other_dah_M1_1_cumulative + date + prior("dgamma(1,1)")*lag_ghe_cumulative + prior("dgamma(1,1)")*lag_oop_cumulative + completeness_ITN_received
	RDT_received_cumulative ~ prior("dgamma(1,1)")*lag_exp_M2_1_cumulative + prior("dgamma(1,1)")*lag_other_dah_M2_cumulative + date + prior("dgamma(1,1)")*lag_ghe_cumulative + prior("dgamma(1,1)")*lag_oop_cumulative + completeness_RDT_received
	ACT_received_cumulative ~ prior("dgamma(1,1)")*lag_exp_M2_1_cumulative + prior("dgamma(1,1)")*lag_other_dah_M2_cumulative + date + prior("dgamma(1,1)")*lag_ghe_cumulative + prior("dgamma(1,1)")*lag_oop_cumulative + completeness_ACT_received
	
	# linkage 1 regressions with hotfixes for heywood cases (temporary)

	
	# linkage 2 regressions
	ITN_consumed_cumulative ~ prior("dgamma(1,1)")*ITN_received_cumulative + completeness_ITN_consumed
	ACTs_SSC_cumulative ~  prior("dgamma(1,1)")*lag_exp_M2_3_cumulative + prior("dgamma(1,1)")*lag_other_dah_M2_3_cumulative + prior("dgamma(1,1)")*lag_ghe_cumulative + prior("dgamma(1,1)")*lag_oop_cumulative + completeness_ACTs_SSC
	RDT_completed_cumulative ~ prior("dgamma(1,1)")*RDT_received_cumulative + completeness_RDT_completed
	SP_cumulative ~ prior("dgamma(1,1)")*lag_exp_M3_1_cumulative + date + prior("dgamma(1,1)")*lag_ghe_cumulative + prior("dgamma(1,1)")*lag_oop_cumulative + completeness_SP
	severeMalariaTreated_cumulative ~ prior("dgamma(1,1)")*lag_exp_M2_6_cumulative + prior("dgamma(1,1)")*ACT_received_cumulative + date + prior("dgamma(1,1)")*lag_ghe_cumulative + prior("dgamma(1,1)")*lag_oop_cumulative + completeness_severeMalariaTreated
	totalPatientsTreated_cumulative ~ prior("dgamma(1,1)")*ACT_received_cumulative + completeness_totalPatientsTreated
	
	# latent variables
	
	# fixed variances
	
	# covariances
	lag_exp_M1_1_cumulative ~~ lag_other_dah_M1_1_cumulative
	lag_exp_M1_2_cumulative ~~ lag_other_dah_M1_1_cumulative
	lag_exp_M2_1_cumulative ~~ lag_other_dah_M2_cumulative
	lag_exp_M2_1_cumulative ~~ lag_other_dah_M2_cumulative
	lag_exp_M2_6_cumulative ~~ lag_other_dah_M2_cumulative
	lag_exp_M2_3_cumulative ~~ lag_other_dah_M2_3_cumulative
	
	# fixed covariances
	lag_exp_M2_3_cumulative ~~ 0*lag_exp_M3_1_cumulative
	lag_exp_M2_3_cumulative ~~ 0*lag_exp_M2_6_cumulative
	lag_exp_M2_6_cumulative ~~ 0*lag_exp_M3_1_cumulative
	
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
