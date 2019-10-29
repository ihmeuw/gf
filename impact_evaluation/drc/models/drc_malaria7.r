# model: drc_malaria7.r
# purpose: drc_malaria6 and drc_malaria_impact4 together without the impact section

model = '

	# linkage 1 regressions
	ITN_received_cumulative ~ prior("dgamma(1,1)")*lag_exp_M1_1_cumulative + prior("dgamma(1,1)")*lag_exp_M1_2_cumulative + prior("dgamma(1,1)")*lag_other_dah_M1_1_cumulative + date + completeness_ITN_received
	RDT_received_cumulative ~ prior("dgamma(1,1)")*lag_exp_M2_1_cumulative + prior("dgamma(1,1)")*lag_other_dah_M2_cumulative + date + prior("dgamma(1,1)")*lag_ghe_cumulative + completeness_RDT_received
	ACT_received_cumulative ~ prior("dgamma(1,1)")*lag_exp_M2_1_cumulative + prior("dgamma(1,1)")*lag_other_dah_M2_cumulative + date + prior("dgamma(1,1)")*lag_ghe_cumulative + completeness_ACT_received
	
	# linkage 2 regressions
	ITN_consumed_cumulative ~ prior("dgamma(1,1)")*ITN_received_cumulative + completeness_ITN_consumed
	ACTs_SSC_cumulative ~  prior("dgamma(1,1)")*lag_exp_M2_3_cumulative + prior("dgamma(1,1)")*lag_other_dah_M2_3_cumulative + prior("dgamma(1,1)")*lag_ghe_cumulative + completeness_ACTs_SSC
	RDT_completed_cumulative ~ prior("dgamma(1,1)")*RDT_received_cumulative + completeness_RDT_completed
	SP_cumulative ~ prior("dgamma(1,1)")*lag_exp_M3_1_cumulative + date + prior("dgamma(1,1)")*lag_ghe_cumulative + completeness_SP
	severeMalariaTreated_cumulative ~ prior("dgamma(1,1)")*lag_exp_M2_6_cumulative + prior("dgamma(1,1)")*ACT_received_cumulative + date + prior("dgamma(1,1)")*lag_ghe_cumulative + completeness_severeMalariaTreated
	totalPatientsTreated_cumulative ~ prior("dgamma(1,1)")*ACT_received_cumulative + completeness_totalPatientsTreated
	
	# linkage 3 regressions
	ITN_rate ~ prior("dgamma(1,1)")*ITN_consumed_cumulative + completeness_ITN_consumed + population
	mildMalariaTreated_rate ~ prior("dgamma(1,1)")*totalPatientsTreated_cumulative + RDT_rate + completeness_totalPatientsTreated + population
	severeMalariaTreated_rate ~ prior("dgamma(1,1)")*severeMalariaTreated_cumulative + RDT_rate + completeness_severeMalariaTreated + population
	ACTs_CHWs_rate ~ prior("dgamma(1,1)")*ACTs_SSC_cumulative + completeness_ACTs_SSC + population
	SP_rate ~ prior("dgamma(1,1)")*SP_cumulative + completeness_SP + population
	RDT_rate ~ prior("dgamma(1,1)")*RDT_completed_cumulative + completeness_RDT_completed + population	
	
	# latent variables
	
	# fixed variances
	
	# covariances
	lag_exp_M1_1_cumulative ~~ lag_other_dah_M1_1_cumulative
	lag_exp_M1_2_cumulative ~~ lag_other_dah_M1_1_cumulative
	lag_exp_M2_1_cumulative ~~ lag_other_dah_M2_cumulative
	lag_exp_M2_1_cumulative ~~ lag_other_dah_M2_cumulative
	lag_exp_M2_6_cumulative ~~ lag_other_dah_M2_cumulative
	lag_exp_M2_3_cumulative ~~ lag_other_dah_M2_3_cumulative
	
	severeMalariaTreated_rate ~~ mildMalariaTreated_rate
	ACTs_CHWs_rate ~~ mildMalariaTreated_rate
	severeMalariaTreated_rate ~~ ACTs_CHWs_rate
	
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
	

	ITN_consumed_cumulative ~~ 0*totalPatientsTreated_cumulative
	ITN_consumed_cumulative ~~ 0*severeMalariaTreated_cumulative
	ITN_consumed_cumulative ~~ 0*ACTs_SSC_cumulative
	ITN_consumed_cumulative ~~ 0*SP_cumulative
	ITN_consumed_cumulative ~~ 0*RDT_completed_cumulative
	totalPatientsTreated_cumulative ~~ 0*RDT_completed_cumulative
	totalPatientsTreated_cumulative ~~ 0*severeMalariaTreated_cumulative
	totalPatientsTreated_cumulative ~~ 0*ACTs_SSC_cumulative
	totalPatientsTreated_cumulative ~~ 0*SP_cumulative
	severeMalariaTreated_cumulative ~~ 0*RDT_completed_cumulative
	severeMalariaTreated_cumulative ~~ 0*ACTs_SSC_cumulative
	severeMalariaTreated_cumulative ~~ 0*SP_cumulative
	RDT_completed_cumulative ~~ 0*ACTs_SSC_cumulative
	RDT_completed_cumulative ~~ 0*SP_cumulative
	RDT_completed_cumulative ~~ 0*SP_cumulative
'
