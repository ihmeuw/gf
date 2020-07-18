#This code defines the model object for the Guatemala TB model described in the publication by Phillips, et al.
#J. Ross July 18, 2020


model = '

  # Linkage 1 = relationships between inputs and activities
  Firstline_Distributed_act_cumulative ~  ghe_tb_cumulative
  Secondline_Distributed_act_cumulative ~ gf_mdrtb_cumulative + gf_mdrtb_cumulative*gf_rssh_cumulative 
  Number_of_Cases_Screened_for_MDR_act_cumulative ~ gf_mdrtb_cumulative + gf_mdrtb_cumulative*gf_rssh_cumulative +  ghe_tb_cumulative + odah_tb_cumulative
  TB_Patients_Tested_for_HIV_act_cumulative ~ gf_tbhiv_cumulative + gf_tbhiv_cumulative*gf_rssh_cumulative +  ghe_tb_cumulative + odah_tb_cumulative 
  Additional_Cases_Detected_via_ACF_out_cumulative ~ gf_tb_cumulative + gf_tb_cumulative*gf_rssh_cumulative 

  # Linkage 2 = relationships between activities and outputs or inputs and outputs
  Cases_Notified_out_cumulative  ~ Additional_Cases_Detected_via_ACF_out_cumulative + gf_tb_cumulative + gf_tb_cumulative*gf_rssh_cumulative +  ghe_tb_cumulative + odah_tb_cumulative 
  Cases_Started_on_Treatment_out_cumulative ~ gf_tb_cumulative + gf_tb_cumulative*gf_rssh_cumulative +  ghe_tb_cumulative + odah_tb_cumulative + Firstline_Distributed_act_cumulative + Cases_Notified_out_cumulative
  MDR_Cases_Started_Treatment_out_cumulative ~ Number_of_Cases_Screened_for_MDR_act_cumulative + Secondline_Distributed_act_cumulative
  HIV_TB_Cases_Notified_out_cumulative ~ TB_Patients_Tested_for_HIV_act_cumulative 
  Cases_Notified_in_Prisons_out_cumulative ~ gf_tb_cumulative + gf_tb_cumulative*gf_rssh_cumulative + Number_of_Cases_Screened_for_MDR_act_cumulative
  Children_less5_referred_out_cumulative ~ Additional_Cases_Detected_via_ACF_out_cumulative

  #Linkage 3 = relationships between outputs and outcomes 
  Case_Notification_Rate_imp_log ~ Cases_Notified_out_cumulative 
  Proportion_of_HIV_TB_Cases_Treated_out_log ~ HIV_TB_Cases_Notified_out_cumulative 
  Treatment_Success_Rate_imp ~ Cases_Started_on_Treatment_out_cumulative
  HIV_TB_Treatment_Success_Rate_imp ~ HIV_TB_Cases_Notified_out_cumulative
  Proportion_of_MDR_Cases_Treated_out_log ~  MDR_Cases_Started_Treatment_out_cumulative
  Proportion_of_Cases_in_Prisons_Treated_out_log ~ Cases_Notified_in_Prisons_out_cumulative
  Proportion_of_Patients_Receiving_DST_out_log ~ Number_of_Cases_Screened_for_MDR_act_cumulative 
  Children_in_Contact_with_TB_Started_IPT_out_cumulative ~ Children_less5_referred_out_cumulative 

  
'
