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
#
# gtm_tb_sec_half3: 
# Created by Emily Linebarger 8/16/19. Setting up model as GLM instead of SEM - removing all covariances and fixed variances, as well as Linkage 4. 


# MODEL WITH CONFIRMED AVAILABLE VARIABLES 
model = '

# Linkage 3 = relationships between outputs and outcomes

Case_Notification_Rate_value_imp ~ Cases_Notified_value_outp + date
Proportion_of_HIV_TB_Cases_Treated_value_outc ~ HIV_TB_Cases_Notified_value_outp + date
Treatment_Success_Rate_value_imp ~ Cases_Started_on_Treatment_value_outp + date
HIV_TB_Treatment_Success_Rate_value_imp ~ HIV_TB_Cases_Notified_value_outp + date
Proportion_of_MDR_Cases_Treated_value_outc ~ Number_of_Cases_Screened_for_MDR_act + date
MDR_Probably_Cured_rate_imp ~ MDR_Cases_Started_Treatment_value_outp + date
Proportion_of_Cases_in_Prisons_Treated_value_outc ~ Cases_Started_on_Treatment_in_Prisons_value_outp + date
Proportion_of_Patients_Receiving_DST_value_outc ~ Number_of_Cases_Screened_for_MDR_act + date

'

