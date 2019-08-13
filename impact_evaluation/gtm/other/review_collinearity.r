source('./impact_evaluation/gtm/set_up_r.r')

# for testing purposes
task_id = 1
modelVersion = 'gtm_tb_first_half2'
modelStage = 1
testRun = TRUE


# ---------------------------------------------------------------------------------------------------
# Load data
set.seed(1)
if (Sys.info()[1]!='Windows' & modelStage==1) load(outputFile4a)
if (Sys.info()[1]=='Windows' & modelStage==1) load(outputFile4a)
if (Sys.info()[1]!='Windows' & modelStage==2) load(outputFile4b_scratch)
if (Sys.info()[1]=='Windows' & modelStage==2) load(outputFile4b)

# subset to current health zone
d = unique(data$department)[task_id]
subData = data[department==d]

# define model object
source(paste0('./impact_evaluation/gtm/models/', modelVersion, '.R'))

# reduce the data down to only necessary variables
parsedModel = lavParseModelString(model)
modelVars = unique(c(parsedModel$lhs, parsedModel$rhs))
modelVars = c('department','date',modelVars)
subData = subData[, unique(modelVars), with=FALSE]

#----------------------------------------------------------------------------------------
#Do simple linear models to examine variance inflation factors to check for collinearity. 
#----------------------------------------------------------------------------------------

lm1 = lm(Isoniazid_Distributed_act_cumulative ~ exp_T1_2_cumulative + ghe_T1_2_cumulative + other_dah_T1_2_cumulative + exp_R1_ALL_cumulative + date, data=subData) #Test commenting this out to see if it resolves linear dependence issue. EL 7/30/2019
ols_coll_diag(lm1) # PROBLEM - VIF>10 

lm2 = lm(Total_Drugs_Distributed_act_cumulative ~ exp_T1_2_cumulative + exp_R1_ALL_cumulative + other_dah_T1_2_cumulative + ghe_T1_2_cumulative + date, data=subData)
ols_coll_diag(lm2) # PROBLEM - VIF>10 

lm3 = lm(Number_of_Cases_Screened_for_MDR_act_cumulative ~ exp_T3_ALL + other_dah_T3_1_cumulative + date, data=subData)
ols_coll_diag(lm3)

lm4 = lm(PLHIV_Screened_for_TB_act_cumulative ~ exp_T2_ALL_cumulative + date, data=subData)
ols_coll_diag(lm4)

lm5 = lm(TB_Patients_Tested_for_HIV_act_cumulative ~ exp_T2_ALL_cumulative + date, data=subData)
ols_coll_diag(lm5)

# Linkage 2 = relationships between activities and outputs or inputs and outputs

lm6 = lm(Cases_Notified_out_cumulative ~ exp_T1_1_cumulative + ghe_T1_1_cumulative + other_dah_T1_1_cumulative + date, data=subData) #Can add outreach and training and microscopy tests if data come through
ols_coll_diag(lm6) # PROBLEM - VIF>10 

lm7 = lm(HIV_TB_Cases_Notified_out_cumulative ~ PLHIV_Screened_for_TB_act_cumulative + TB_Patients_Tested_for_HIV_act_cumulative + date, data=subData) 
ols_coll_diag(lm7) #ERROR HERE - INFINITE OR MISSING VALUES IN X

lm8 = lm(MDR_Cases_Notified_out_cumulative ~ Number_of_Cases_Screened_for_MDR_act_cumulative + date, data=subData) 
ols_coll_diag(lm8) #High values for VIF, should examine further. 

lm9 = lm(Cases_Started_on_Treatment_out_cumulative ~ exp_T1_1_cumulative + ghe_T1_1_cumulative + other_dah_T1_1_cumulative + Isoniazid_Distributed_act_cumulative + date, data=subData)  #Can add outreach if data come through
ols_coll_diag(lm9) #ERROR HERE - INFINITE OR MISSING VALUES IN X

lm10 = lm(MDR_Cases_Started_Treatment_out_cumulative ~ exp_T3_ALL + other_dah_T3_2_cumulative + Number_of_Cases_Screened_for_MDR_act_cumulative + date, data=subData)
ols_coll_diag(lm10)

lm11 = lm(Additional_Cases_Detected_via_ACF_out_cumulative ~ exp_T1_5_cumulative + date, data=subData)  #Change this to Outreach_teams_formed when that is populated
ols_coll_diag(lm11)

lm12 = lm(PLHIV_started_on_IPT_out_cumulative ~ PLHIV_Screened_for_TB_act_cumulative + Isoniazid_Distributed_act_cumulative + date, data=subData)
ols_coll_diag(lm12)#ERROR HERE - INFINITE OR MISSING VALUES IN X

lm13 = lm(Cases_Notified_in_Prisons_out_cumulative ~ exp_T1_6_cumulative + date, data=subData) 
ols_coll_diag(lm13)






