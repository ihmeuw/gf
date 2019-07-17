#----------------------------------------------------------
# AUTHOR: Jen Ross
# PURPOSE: This is a computationally viable model that adapts the DRC malaria SEM for the GTM TB context
# This is a computationally viable model that adapts the DRC malaria SEM for the GTM TB context
# LAST UPDATED: July 16, 2019 by Emily Linebarger
#
# # MODEL VERSIONS: 
# gtm_tb_sec_half_1: 
# In-process regression equations are indicated with the #, which I remove once variable names are formalized
# 
# gtm_tb_sec_half2: 
# Created by Emily Linebarger. Using framework set up by Jen, fix equations to match variable names available in data. 

# DESIRED MODEL 
# model = '
# 
# # Linkage 3 = relationships between outputs and outcomes
# 
# #Proportion_ds_treated ~ Cases_Notified_out + Cases_Started_on_Treatment_out + date
# #Treatment_Success_Rate_value_out ~ Cases_Started_on_Treatment_out + date
# #Prop_plhiv_receive_ipt ~ PLHIV_started_on_IPT_out + date
# #Under5_getting_ipt ~ Children_in_Contact_with_TB_detected + date
# #Prop_mdr_treated ~ MDR_Cases_Started_Treatment_out + date #----------NOTE to sort out whether samples tested for suscept is activity or outcome
# #Prop_mdr_smear_convert ~ MDR_Cases_Started_Treatment_out + date
# 
# # Linkage 4 = relationships between outcomes and impact
# 
# #Incidence_rate_value ~ Proportion_ds_treated + Children_in_Contact_with_TB_detected + Cases_Notified_in_Prisons_out + Prop_mdr_treated + date #Add diagnoses in other vulnerable pops if this category survives
# #Mortality_rate_value_m ~ Proportion_ds_treated + Treatment_success_rate + Prop_plhiv_receive_ipt + TB_HIV_receiving_ART + Prop_mdr_treated + Prop_mdr_6_month_therapy + date
# 
# '

# MODEL WITH CONFIRMED AVAILABLE VARIABLES 
model = '

# Linkage 3 = relationships between outputs and outcomes

Proportion_of_Patients_Receiving_DST_value_outc ~ Cases_Notified_value_outp + Cases_Started_on_Treatment_value_outp + date
Treatment_Success_Rate_value_imp ~ Cases_Started_on_Treatment_value_outp + date
Proportion_of_HIV_TB_Cases_Treated_value_outc ~ PLHIV_started_on_IPT_value_outp + date
#Under5_getting_ipt ~ Children_in_Contact_with_TB_detected + date
# Proportion of MDR_Cases_Treated_value_outc ~ MDR_Cases_Started_Treatment_out + date #----------NOTE to sort out whether samples tested for suscept is activity or outcome
#Prop_mdr_smear_convert ~ MDR_Cases_Started_Treatment_out + date

# Linkage 4 = relationships between outcomes and impact

#Incidence_rate_value ~ Proportion_of_Patients_Receiving_DST_value_outc + Cases_Notified_in_Prisons_value_outp + Proportion_of_MDR_Cases_Treated_value_outc + date #Add diagnoses in other vulnerable pops if this category survives
Mortality_Rate_value_imp ~ Proportion_of_Patients_Receiving_DST_value_outc + Treatment_Success_Rate_value_imp + Proportion_of_HIV_TB_Cases_Treated_value_outc + Proportion_of_MDR_Cases_Treated_value_outc + date
'