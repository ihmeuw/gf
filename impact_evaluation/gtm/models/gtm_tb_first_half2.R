#-----------------------------------------------------------------------------------------------
# AUTHOR: Jen Ross, adapted from drc_malaria1 code by David Phillips. 
# UPDATE DATE: July 12, 2019, by Emily Linebarger 
#
# model: gtm_tb_first_half1 adapted by J Ross from drc_malaria1 code by D Phillips.
# model: gtm_tb_first_half2 modified by Emily Linebarger. All first half variables changed to cumulative
#   sums, and model linkages where data was unavailable were removed. 
# This is a computationally viable model that adapts the DRC malaria SEM for the GTM TB context
# May 2019
#
# In-process regression equations are indicated with the #, which I remove once variable names are formalized
# My current convention is to group funding by module ordered within module by GF, GHE, and DAH
#-----------------------------------------------------------------------------------------------


model = '

  # Linkage 1 = relationships between inputs and activities

  Isoniazid_Distributed_act_cumulative ~ exp_T1_2_cumulative + ghe_T1_2_cumulative + other_dah_T1_2_cumulative + exp_R1_ALL_cumulative + ghe_R1_ALL_cumulative + other_dah_R1_ALL_cumulative + date
  Total_Drugs_Distributed_act_cumulative ~ exp_T1_2_cumulative + exp_R1_ALL_cumulative + other_dah_T1_2_cumulative + ghe_T1_2_cumulative + ghe_R1_ALL_cumulative + other_dah_R1_ALL_cumulative + date
  Number_of_Cases_Screened_for_MDR_act_cumulative ~ exp_T3_1_cumulative + ghe_T3_1_cumulative + other_dah_T3_1_cumulative + date
  PLHIV_Screened_for_TB_act_cumulative ~ exp_T2_ALL_cumulative + ghe_T2_ALL_cumulative + other_dah_T2_ALL_cumulative + date
  TB_Patients_Tested_for_HIV_act_cumulative ~ exp_T2_ALL_cumulative + ghe_T2_ALL_cumulative + other_dah_T2_ALL_cumulative + date
	
	# Linkage 2 = relationships between activities and outputs or inputs and outputs
  
  Cases_Notified_out_cumulative ~ exp_T1_1_cumulative + ghe_T1_1_cumulative + other_dah_T1_1_cumulative + date #Can add outreach and training and microscopy tests if data come through
  HIV_TB_Cases_Notified_out_cumulative ~ PLHIV_Screened_for_TB_act_cumulative + TB_Patients_Tested_for_HIV_act_cumulative + date
  MDR_Cases_Notified_out_cumulative ~ Number_of_Cases_Screened_for_MDR_act_cumulative + date
  Cases_Started_on_Treatment_out_cumulative ~ exp_T1_1_cumulative + ghe_T1_1_cumulative + other_dah_T1_1_cumulative + Isoniazid_Distributed_act_cumulative + date #Can add outreach if data come through
  MDR_Cases_Started_Treatment_out_cumulative ~ exp_T3_2_cumulative + ghe_T3_2_cumulative + other_dah_T3_2_cumulative + Number_of_Cases_Screened_for_MDR_act_cumulative + date
  Additional_Cases_Detected_via_ACF_out_cumulative ~ exp_T1_5_cumulative + ghe_T1_5_cumulative + other_dah_T1_5_cumulative + date #Change this to Outreach_teams_formed when that is populated
  PLHIV_started_on_IPT_out_cumulative ~ PLHIV_Screened_for_TB_act_cumulative + Isoniazid_Distributed_act_cumulative + date
  Cases_Notified_in_Prisons_out_cumulative ~ exp_T1_6_cumulative + ghe_T1_6_cumulative + other_dah_T1_6_cumulative + date
	

  #These are all leftover examples from the malaria model that I have not updated yet for TB---------------------------------------------------------------------
	# latent variables
	
	# fixed variances
	# value_RDT_received ~ 1*value_RDT_received
	
	# covariances
  # This is where to specify the relationships between inputs and other inputs
	# budget_M1_1_cumulative ~~ other_dah_M1_1_cumulative

	# fixed covariances
	# budget_M2_3_cumulative ~~ 0*budget_M3_1_cumulative

'
