# model: drc_malaria6.r
# purpose: drc_malaria5 with lags on inputs

model = '

	# linkage 1 regressions
	ITN_received_cumulative_pc ~ prior("dgamma(1,1)")*lag_exp_M1_1_cumulative_pc + prior("dgamma(1,1)")*lag_exp_M1_2_cumulative_pc + prior("dgamma(1,1)")*lag_other_dah_M1_1_cumulative_pc + completeness_ITN_received
	RDT_received_cumulative_pc ~ prior("dgamma(1,1)")*lag_exp_M2_1_cumulative_pc + prior("dgamma(1,1)")*lag_other_dah_M2_cumulative_pc + prior("dgamma(1,1)")*lag_ghe_cumulative_pc + completeness_RDT_received
	ACT_received_cumulative_pc ~ prior("dgamma(1,1)")*lag_exp_M2_1_cumulative_pc + prior("dgamma(1,1)")*lag_other_dah_M2_cumulative_pc + prior("dgamma(1,1)")*lag_ghe_cumulative_pc + completeness_ACT_received
	
	# linkage 1 regressions with hotfixes for heywood cases (temporary)

	
	# linkage 2 regressions
	ITN_consumed_cumulative_pc ~ prior("dgamma(1,1)")*ITN_received_cumulative_pc + completeness_ITN_consumed
	ACTs_SSC_cumulative_pc ~  prior("dgamma(1,1)")*lag_exp_M2_3_cumulative_pc + prior("dgamma(1,1)")*lag_other_dah_M2_3_cumulative_pc + prior("dgamma(1,1)")*lag_ghe_cumulative_pc + completeness_ACTs_SSC
	RDT_completed_cumulative_pc ~ prior("dgamma(1,1)")*RDT_received_cumulative_pc + completeness_RDT_completed
	SP_cumulative_pc ~ prior("dgamma(1,1)")*lag_exp_M3_1_cumulative_pc + prior("dgamma(1,1)")*lag_ghe_cumulative_pc + completeness_SP
	severeMalariaTreated_cumulative_pc ~ prior("dgamma(1,1)")*lag_exp_M2_6_cumulative_pc + prior("dgamma(1,1)")*ACT_received_cumulative_pc + prior("dgamma(1,1)")*lag_ghe_cumulative_pc + completeness_severeMalariaTreated
	totalPatientsTreated_cumulative_pc ~ prior("dgamma(1,1)")*ACT_received_cumulative_pc + completeness_totalPatientsTreated
	
	# latent variables
	
	# fixed variances
	
	# covariances
	lag_exp_M1_1_cumulative_pc ~~ lag_other_dah_M1_1_cumulative_pc
	lag_exp_M1_2_cumulative_pc ~~ lag_other_dah_M1_1_cumulative_pc
	lag_exp_M2_1_cumulative_pc ~~ lag_other_dah_M2_cumulative_pc
	lag_exp_M2_1_cumulative_pc ~~ lag_other_dah_M2_cumulative_pc
	lag_exp_M2_6_cumulative_pc ~~ lag_other_dah_M2_cumulative_pc
	lag_exp_M2_3_cumulative_pc ~~ lag_other_dah_M2_3_cumulative_pc
	
	# fixed covariances
	lag_exp_M2_3_cumulative_pc ~~ 0*lag_exp_M3_1_cumulative_pc
	lag_exp_M2_3_cumulative_pc ~~ 0*lag_exp_M2_6_cumulative_pc
	lag_exp_M2_6_cumulative_pc ~~ 0*lag_exp_M3_1_cumulative_pc
	
	ITN_consumed_cumulative_pc ~~ 0*ACTs_SSC_cumulative_pc
	ITN_consumed_cumulative_pc ~~ 0*RDT_completed_cumulative_pc
	ITN_consumed_cumulative_pc ~~ 0*SP_cumulative_pc
	ITN_consumed_cumulative_pc ~~ 0*severeMalariaTreated_cumulative_pc
	ITN_consumed_cumulative_pc ~~ 0*totalPatientsTreated_cumulative_pc
	
	ACTs_SSC_cumulative_pc ~~ 0*RDT_completed_cumulative_pc
	ACTs_SSC_cumulative_pc ~~ 0*SP_cumulative_pc
	ACTs_SSC_cumulative_pc ~~ 0*severeMalariaTreated_cumulative_pc
	ACTs_SSC_cumulative_pc ~~ 0*totalPatientsTreated_cumulative_pc
	
	RDT_completed_cumulative_pc ~~ 0*SP_cumulative_pc
	RDT_completed_cumulative_pc ~~ 0*severeMalariaTreated_cumulative_pc
	
	SP_cumulative_pc ~~ 0*severeMalariaTreated_cumulative_pc
	SP_cumulative_pc ~~ 0*totalPatientsTreated_cumulative_pc
	
	severeMalariaTreated_cumulative_pc ~~ 0*totalPatientsTreated_cumulative_pc
'
