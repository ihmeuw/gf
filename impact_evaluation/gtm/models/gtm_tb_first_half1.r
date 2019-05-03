# model: gtm_tb_first_half1 adapted from drc_malaria1 by J Ross.
# This is a computationally viable model that adapts the DRC malaria SEM for the GTM TB context
# May 2019
#
# In-process regression equations are indicated with the #, which I remove once variable names are formalized


model = '

  # Linkage 1 = relationships between inputs and activities
	# linkage 1 regressions
	value_ITN_received ~ 1*budget_M1_1_cumulative + other_dah_M1_1_cumulative + date
	value_RDT_received ~ 1*budget_M2_1_cumulative + budget_M2_3_cumulative + other_dah_M2_cumulative + other_dah_M2_3_cumulative + date
	value_ACT_received ~ 1*budget_M2_1_cumulative + budget_M2_3_cumulative + other_dah_M2_cumulative + other_dah_M2_3_cumulative + date
	
	# linkage 1 regressions with hotfixes for heywood cases (temporary)
	
	# Linkage 2 = relationships between activities and outputs or inputs and outputs
	# linkage 2 regressions

  #routine surveillance and case notification ~ Cases Notified_value
  Active Case Finding Missions Conducted_value ~ Cases Notified_value
  Active Case Finding Missions Conducted_value ~ Children in Contact with TB Started IPT_value
  Number of Cases Screened for MDR_value ~ Persons with MDR-TB treated
  Number of Cases Screened for MDR_value ~ MDR Cases Notified_value
  #Prisoners Screened_value ~ prisoners found to have TB
  
	
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
  # This is where to specify the relationships between inputs and other inputs
	budget_M1_1_cumulative ~~ other_dah_M1_1_cumulative
	# budget_M1_2_cumulative ~~ other_dah_M1_2_cumulative
	budget_M2_1_cumulative ~~ other_dah_M2_cumulative
	budget_M2_3_cumulative ~~ other_dah_M2_3_cumulative
	
	# fixed covariances
	budget_M2_3_cumulative ~~ 0*budget_M3_1_cumulative
	budget_M2_3_cumulative ~~ 0*budget_M2_6_cumulative
	budget_M2_6_cumulative ~~ 0*budget_M3_1_cumulative

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
