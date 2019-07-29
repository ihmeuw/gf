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


# MODEL WITH CONFIRMED AVAILABLE VARIABLES 
model = '

# Linkage 3 = relationships between outputs and outcomes

Case_Notification_Rate_value_imp ~ Cases_Notified_value_outp + date
#Case_Notification_Rate_value_imp ~ Proportion_of_persons_with_DS-TB_Treated + date #This linkage may become available later. EL 7/29/2019
Proportion_of_HIV_TB_Cases_Treated_value_outc ~ HIV_TB_Cases_Notified_value_outp + date
Treatment_Success_Rate_value_imp ~ Cases_Started_on_Treatment_value_outp + date
HIV_TB_Treatment_Success_Rate_value_imp ~ HIV_TB_Cases_Notified_value_outp + date
#Children_in_Contact_with_TB_Started_IPT_value_outp ~  #This linkage may become available later. EL 7/29/2019
Proportion_of_MDR_Cases_Treated_value_outc ~ Number_of_Cases_Screened_for_MDR_act + date
MDR_Probably_Cured_rate_imp ~ MDR_Cases_Started_Treatment_value_outp + date
Proportion_of_Cases_in_Prisons_Treated_value_outc ~ Cases_Started_on_Treatment_in_Prisons_value_outp + date
Proportion_of_Patients_Receiving_DST_value_outc ~ Number_of_Cases_Screened_for_MDR_act + date


# Linkage 4 = relationships between outcomes and impact - EL leaving this commented out for now, until we get some incidence data! 7/29/2019

#Incidence_rate_value ~ Proportion_of_Patients_Receiving_DST_value_outc + Cases_Notified_in_Prisons_value_outp + Proportion_of_MDR_Cases_Treated_value_outc + date #Add diagnoses in other vulnerable pops if this category survives
#Mortality_Rate_value_imp ~ Proportion_of_Patients_Receiving_DST_value_outc + Treatment_Success_Rate_value_imp + Proportion_of_HIV_TB_Cases_Treated_value_outc + Proportion_of_MDR_Cases_Treated_value_outc + date
'

#Documentation on how variable names were matched to pathways diagram - EL 7/29/2019
# load(outputFile4b)
# 
# dt = data.table(var_name = names(data))
# dt = dt[!var_name%in%c('department', 'date')]
# 
# dt[var_name=="HIV_TB_Mortality_Rate_value_imp", label:="HIV-TB Mortality"]
# dt[var_name=="Mortality_Rate_value_imp", label:="Mortality"]
# dt[var_name=="Cases_Notified_value_outp", label:="Case notifications"]
# dt[var_name=="MDR_Cases_Notified_value_outp", label:="MDR Case notifications"]
# dt[var_name=="HIV_TB_Cases_Notified_value_outp", label:="HIV/TB Case notifications"]
# dt[var_name=="PLHIV_started_on_IPT_value_outp", label:="PLHIV receiving IPT"]
# dt[var_name=="Additional_Cases_Detected_via_ACF_value_outp", label:="Additional cases via ACF"]
# dt[var_name=="MDR_Cases_Started_Treatment_value_outp", label:="Persons with MDR-TB treated"]
# dt[var_name=="Cases_Started_on_Treatment_value_outp", label:="Persons receiving first-line treatment"]
# dt[var_name=="Cases_Notified_in_Prisons_value_outp", label:="Prisoners diagnosed with TB"]
# dt[var_name=="Cases_Started_on_Treatment_in_Prisons_value_outp", label:="Prisoners started on treatment for TB"]
# dt[var_name=="Proportion_of_MDR_Cases_Treated_value_outc", label:="Proportion of persons with MDR-TB treated"]
# dt[var_name=="Proportion_of_Patients_Receiving_DST_value_outc", label:="Proportion_of_Patients_Receiving_DST"]
# dt[var_name=="Children_in_Contact_with_TB_Started_IPT_value_outp", label:="Household contacts <5 receiving IPT"]
# dt[var_name=="Proportion_of_Cases_in_Prisons_Treated_value_outc", label:="Proportion of prisoners treated"]
# dt[var_name=="Proportion_of_HIV_TB_Cases_Treated_value_outc", label:="Proportion of persons with HIV-TB treated"]
# dt[var_name=="HIV_TB_Treatment_Success_Rate_value_imp", label:="Treatment success rate- HIV/TB"]
# dt[var_name=="Treatment_Success_Rate_value_imp", label:="Treatment success rate - DS"]
# dt[var_name=="MDR_Probably_Cured_rate_imp", label:="MDR sputum sample conversion"]
# dt[var_name=="Case_Notification_Rate_value_imp", label:="Case notification rate"]
# dt[var_name=="Number_of_Cases_Screened_for_MDR_act", label:="# of cases screened for MDR-TB"]
# 
# #We aren't using these variables in the model right now.
# dt[var_name=="Proportion_of_Cases_Treated_value_outc", label:="MISSING"]
# dt[var_name=="Proportion_of_TB_Patients_who_Received_HIV_Test_value_outc", label:="MISSING"]
# 
# dt[is.na(label)]
# 


#-----------------------------------------------
# CURRENT UNUSED VARIABLES 
#-----------------------------------------------
#Proportion_of_Cases_Treated_value_outc
#Proportion_of_TB_Patients_who_Received_HIV_Test_value_outc - CHANGE TO AN ACTIVITY

# Treatment success rate and HIV/TB treatment success rate 








