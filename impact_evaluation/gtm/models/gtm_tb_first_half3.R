#-----------------------------------------------------------------------------------------------
# AUTHOR: Jen Ross, adapted from drc_malaria1 code by David Phillips. 
# UPDATE DATE: July 12, 2019, by Emily Linebarger 
#
# model: gtm_tb_first_half1 adapted by J Ross from drc_malaria1 code by D Phillips.
# model: gtm_tb_first_half2 modified by Emily Linebarger. All first half variables changed to cumulative
#   sums, and model linkages where data was unavailable were removed. 
# model: gtm_tb_first_half3 modified by Emily Linebarger - dramatically paring down relationships to a simpler 
#   structure, agreed on by Jen Ross 8/13/19
# This is a computationally viable model that adapts the DRC malaria SEM for the GTM TB context
# May 2019
#
# In-process regression equations are indicated with the #, which I remove once variable names are formalized
# My current convention is to group funding by module ordered within module by GF, GHE, and DAH
#-----------------------------------------------------------------------------------------------


model = '

  # Linkage 1 = relationships between inputs and activities

  # Linkage 1 = relationships between inputs and activities
  Isoniazid_Distributed_act_cumulative ~ gf_tb_cumulative + ghe_tb_cumulative + odah_tb_cumulative + date
  Total_Drugs_Distributed_act_cumulative ~ gf_tb_cumulative + ghe_tb_cumulative + odah_tb_cumulative + date
  Number_of_Cases_Screened_for_MDR_act_cumulative ~ gf_mdrtb_cumulative + ghe_tb_cumulative + odah_tb_cumulative + date
  TB_Patients_Tested_for_HIV_act_cumulative ~ gf_tbhiv_cumulative + ghe_tb_cumulative + odah_tb_cumulative + date
   
  # Linkage 2 = relationships between activities and outputs or inputs and outputs
  Cases_Notified_out_cumulative  ~ gf_tb_cumulative + ghe_tb_cumulative + odah_tb_cumulative + date
  Cases_Started_on_Treatment_out_cumulative ~ gf_tb_cumulative + ghe_tb_cumulative + odah_tb_cumulative + Isoniazid_Distributed_act_cumulative + Total_Drugs_Distributed_act_cumulative + date
  MDR_Cases_Started_Treatment_out_cumulative ~ Number_of_Cases_Screened_for_MDR_act_cumulative + date
  HIV_TB_Cases_Notified_out_cumulative ~ TB_Patients_Tested_for_HIV_act_cumulative + date

	# latent variables
	
	# fixed variances - zero out the relationships between outputs variables, except where it makes theoretical sense. 
  Cases_Notified_out_cumulative~~0*MDR_Cases_Started_Treatment_out_cumulative
  Cases_Started_on_Treatment_out_cumulative~~0*MDR_Cases_Started_Treatment_out_cumulative
  Cases_Started_on_Treatment_out_cumulative~~0*HIV_TB_Cases_Notified_out_cumulative
  MDR_Cases_Started_Treatment_out_cumulative~~0*HIV_TB_Cases_Notified_out_cumulative

	# covariances
  Cases_Notified_out_cumulative ~~ HIV_TB_Cases_Notified_out_cumulative
  Isoniazid_Distributed_act_cumulative ~~ Total_Drugs_Distributed_act_cumulative
  Cases_Notified_out_cumulative~~Cases_Started_on_Treatment_out_cumulative

  
'
