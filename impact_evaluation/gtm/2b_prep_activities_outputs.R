# Emily Linebarger, based on code by Audrey Batzel
# May/June 2019 
# Prep activities and outputs data for TB impact model in Guatemala. 
# The current working directory should be the root of this repo (set manually by user)
# -----------------------------------------------------------

# ---------------------------------------------------
# Read in data
# ---------------------------------------------------
drc = readRDS("J:/Project/Evaluation/GF/impact_evaluation/cod/prepped_data/outputs_activities_for_pilot_wide.RDS") #For reference 
activities = fread(actFile)
outputs = fread(outputsFile)

#Add _ to names of data. 
names(activities) = gsub(" ", "_", names(activities))
names(outputs) = gsub(" ", "_", names(outputs))

#----------------------------------------------------
# Validate files, and subset data. 
#----------------------------------------------------
#Make sure that merge below will work - dates.  
a_dates = unique(activities$date)
o_dates = unique(outputs$date)

a_dates[!a_dates%in%o_dates] #Don't have output data for 2018.  
o_dates[!o_dates%in%a_dates] #Nothing. 

#Departments
a_depts = unique(activities$department)
o_depts = unique(outputs$department)

a_depts[!a_depts%in%o_depts] #None. 
o_depts[!o_depts%in%a_depts] #None. 

#Municipalities
a_mun = unique(activities$municipality)
o_mun = unique(outputs$municipality)

a_mun[!a_mun%in%o_mun] #Some municipalities aren't matching here. 
o_mun[!o_mun%in%a_mun] #Some municipalities aren't matching here. 

#Subset data to only department-level, because municipalities aren't matching right now. 

# test for repeated muni codes across departments
unique = unique(activities[,c('department','municipality'),with=F])
unique = unique[, .N, by='municipality']
if (any(unique$N>1)) stop('Some municipalities appear in more than one department (in the activities data)!') 
unique = unique(outputs[,c('department','municipality'),with=F])
unique = unique[, .N, by='municipality']
if (any(unique$N>1)) stop('Some municipalities appear in more than one department (in the outputs data)!') 

# test for missing department or muni codes
if (any(is.na(activities$department))) stop('There are missing department codes in the activities data')
if (any(is.na(outputs$department))) stop('There are missing department codes in the outputs data')
if (any(is.na(activities$municipality))) stop('There are missing municipality codes in the activities data')
if (any(is.na(outputs$municipality))) stop('There are missing municipality codes in the outputs data')

#------------------------
# Collapse Activities
#------------------------

# identify variables at department-level and muni-level
deptVars = c('Total_Drugs_Distributed_value_d_x', 'Isoniazid_Distributed_value_d', 'Number_of_Cases_Screened_for_MDR_value_d', 'Total_Drugs_Distributed_value_d_y')
muniVars = c('TPLHIV_Screened_for_TB_value_m', 'TB_Patients_Tested_for_HIV_value_m')

# collapse to department level 
# (average variables that are already at department-level, sum variables that are muni-level)
byVars = c('date','department')
tmp1 = activities[, lapply(.SD, mean), .SDcols=deptVars, by=byVars]
tmp2 = activities[, lapply(.SD, sum), .SDcols=muniVars, by=byVars]
activities1 = merge(tmp1, tmp2, by=byVars)


#------------------------
# Collapse Outputs
#------------------------

# identify variables at department-level and muni-level
deptVars = c('Cases_Started_on_Treatment_value_d', 'Cases_Notified_in_Prisons_value', 'MDR_Cases_Notified_value_d', 'MDR_Cases_Started_Treatment_value_d')
muniVars = c('Cases_Notified_value_m', 'Additional_Cases_Detected_via_ACF_value_m', 'PLHIV_started_on_IPT_value_m', 
	'Children_in_Contact_with_TB_Started_IPT_value_m', 'HIV/TB_Cases_Notified_value_m', 
	'Cases_Started_on_Treatment_in_Prisons_value_m')

# collapse to department level 
# (average variables that are already at department-level, sum variables that are muni-level)
tmp1 = activities[, lapply(.SD, mean), .SDcols=deptVars, by=byVars]
tmp2 = activities[, lapply(.SD, sum), .SDcols=muniVars, by=byVars]
outputs1 = merge(tmp1, tmp2, by=byVars)

#Make sure you've accounted for all columns except municipality. 
stopifnot(ncol(outputs1) == ncol(outputs)-1)
new_names = names(outputs1)[3:ncol(outputs1)]  #Resest the names so they're distinguishable from activity variables. 
new_names = paste0(new_names, "_out")
names(outputs1)[3:ncol(outputs1)] <- new_names

#-----------------------------------------------------
# Merge data 
#-----------------------------------------------------

dt_final = merge(activities1, outputs1, by=c('date', 'department'))
#-----------------------------------------------------
# Save data 
#-----------------------------------------------------
saveRDS(dt_final, outputFile2b)
archive(outputFile2b)



