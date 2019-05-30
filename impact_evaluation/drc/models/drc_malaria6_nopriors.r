# model: drc_malaria6_nopriors.r
# purpose: drc_malaria6 with all the priors and fixed covariances removed for readibility

model = '

	# linkage 1 regressions
	ITN_received_cumulative ~ lag_exp_M1_1_cumulative + lag_exp_M1_2_cumulative + lag_other_dah_M1_1_cumulative + date + lag_ghe_cumulative + completeness_ITN_received
	RDT_received_cumulative ~ lag_exp_M2_1_cumulative + lag_other_dah_M2_cumulative + date + lag_ghe_cumulative + completeness_RDT_received
	ACT_received_cumulative ~ lag_exp_M2_1_cumulative + lag_other_dah_M2_cumulative + date + lag_ghe_cumulative + completeness_ACT_received
	
	# linkage 2 regressions
	ITN_consumed_cumulative ~ ITN_received_cumulative + completeness_ITN_consumed
	ACTs_SSC_cumulative ~  lag_exp_M2_3_cumulative + lag_other_dah_M2_3_cumulative + lag_ghe_cumulative + completeness_ACTs_SSC
	RDT_completed_cumulative ~ RDT_received_cumulative + completeness_RDT_completed
	SP_cumulative ~ lag_exp_M3_1_cumulative + date + lag_ghe_cumulative + completeness_SP
	severeMalariaTreated_cumulative ~ lag_exp_M2_6_cumulative + ACT_received_cumulative + date + lag_ghe_cumulative + completeness_severeMalariaTreated
	totalPatientsTreated_cumulative ~ ACT_received_cumulative + completeness_totalPatientsTreated
	
	# covariances
	lag_exp_M1_1_cumulative ~~ lag_other_dah_M1_1_cumulative
	lag_exp_M1_2_cumulative ~~ lag_other_dah_M1_1_cumulative
	lag_exp_M2_1_cumulative ~~ lag_other_dah_M2_cumulative
	lag_exp_M2_1_cumulative ~~ lag_other_dah_M2_cumulative
	lag_exp_M2_6_cumulative ~~ lag_other_dah_M2_cumulative
	lag_exp_M2_3_cumulative ~~ lag_other_dah_M2_3_cumulative
'
