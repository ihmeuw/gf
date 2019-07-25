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

#Documentation on how variable names were matched to pathways diagram - EL 7/25/2019
# load(outputFile4b)
# 
# dt = data.table(var_name = names(data))
# dt = dt[!var_name%in%c('department', 'date')]
# dt[var_name=="HIV_TB_Mortality_Rate_value_imp", label:="HIV-TB Mortality"]
# dt[var_name=="Mortality_Rate_value_imp", label:="Mortality"]
# dt[var_name=="Cases_Notified_value_outp", label:="#Case notifications"]
# dt[var_name=="MDR_Cases_Notified_value_outp", label:="#MDR Case notifications"]
# dt[var_name=="HIV_TB_Cases_Notified_value_outp", label:="HIV/TB Case notifications"]
# dt[var_name=="PLHIV_started_on_IPT_value_outp", label:="#PLHIV receiving IPT"]
# dt[var_name=="Additional_Cases_Detected_via_ACF_value_outp", label:="#Additional cases via ACF"]
# dt[var_name=="MDR_Cases_Started_Treatment_value_outp", label:="#Persons with MDR-TB treated"]
# dt[var_name=="Cases_Started_on_Treatment_value_outp", label:="#Persons receiving first-line treatment"]
# dt[var_name=="Cases_Notified_in_Prisons_value_outp", label:="Prisoners diagnosed with TB"]
# dt[var_name=="Cases_Started_on_Treatment_in_Prisons_value_outp", label:="Prisoners started on treatment for TB"]
# dt[var_name=="Proportion_of_MDR_Cases_Treated_value_outc", label:="Proportion of persons with DS-TB treated"]
# dt[var_name=="Proportion_of_Patients_Receiving_DST_value_outc", label:="Proportion of persons with MDR-TB treated"]
# 
# dt[is.na(label)]












