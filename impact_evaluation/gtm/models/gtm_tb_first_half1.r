# model: gtm_tb_first_half1 adapted by J Ross from drc_malaria1 code by D Phillips.
# This is a computationally viable model that adapts the DRC malaria SEM for the GTM TB context
# May 2019
#
# In-process regression equations are indicated with the #, which I remove once variable names are formalized
# My current convention is to group funding by module ordered within module by GF, GHE, and DAH


model = '

  # Linkage 1 = relationships between inputs and activities

  Isoniazid_Distributed_act ~ exp_T1_2 + ghe_T1_2 + other_dah_T1_2 + exp_R1_ALL + ghe_R1_ALL + other_dah_R1_ALL + date
  Total_Drugs_Distributed_act ~ exp_T1_2 + exp_R1_ALL + other_dah_T1_2 + ghe_T1_2 + ghe_R1_ALL + other_dah_R1_ALL + date
  #Outreach_teams_formed - No activities data yet + exp_T1_5 + ghe_T1_5 + other_dah_T1_5 + date
  #Trainings_conducted - No data yet
  Number_of_Cases_Screened_for_MDR_act ~ exp_T3_1 + ghe_T3_1 + other_dah_T3_1 + date
  PLHIV_Screened_for_TB_act ~ exp_T2_ALL + ghe_T2_ALL + other_dah_T2_ALL + exp_H11_ALL + ghe_H11_ALL + other_dah_H11_ALL + date
  TB_Patients_Tested_for_HIV_act ~ exp_T2_ALL + ghe_T2_ALL + other_dah_T2_ALL + exp_H11_ALL + ghe_H11_ALL + other_dah_H11_ALL + date
  #Microscopy_tests_done
  #Lab_supplies_distributed ~ #Lab supplies and reagents distributed
	
	# Linkage 2 = relationships between activities and outputs or inputs and outputs
  
  Cases_Notified_out ~ exp_T1_1 + ghe_T1_1 + other_dah_T1_1 + date #Can add outreach and training and microscopy tests if data come through
  HIV/TB_Cases_Notified_out ~ PLHIV_Screened_for_TB_act + TB_Patients_Tested_for_HIV_act + date
  MDR_Cases_Notified_out ~ Number_of_Cases_Screened_for_MDR_act + date
  Cases_Started_on_Treatment_out ~ exp_T1_1 + ghe_T1_1 + other_dah_T1_1 + Isoniazid_Distributed_act + date #Can add outreach if data come through
  MDR_Cases_Started_Treatment_out ~ exp_T3_2 + ghe_T3_2 + other_dah_T3_2 + Number_of_Cases_Screened_for_MDR_act + date
  Additional_Cases_Detected_via_ACF_out ~ exp_T1_5 + ghe_T1_5 + other_dah_T1_5 + date #Change this to Outreach_teams_formed when that is populated
  PLHIV_started_on_IPT_out ~ PLHIV_Screened_for_TB_act + Isoniazid_Distributed_act + date
  TB_HIV_receiving_ART ~ TB_Patients_Tested_for_HIV_act + date
  #Children_in_Contact_with_TB_detected ~ exp_T1_7 + ghe_T1_7 + other_dah_T1_7 #Confirm this is an output, Add outreach teams
  Cases_Notified_in_Prisons_out ~ exp_T1_6 + ghe_T1_6 + other_dah_T1_6 + date
	

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
