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
names(activities) = gsub("/", "_", names(activities))
names(outputs) = gsub("/", "_", names(outputs))

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

#Check that data is uniquely identified
activities[duplicated(activities, by=c('municipality', 'department','date')), dup:=TRUE]
if (nrow(activities[dup==TRUE])!=0){
  print(paste0("There are ", nrow(activities[dup==TRUE]), " duplicates in municipality and date in the activities data. Review."))
}

outputs[duplicated(outputs, by=c('municipality', 'department', 'date')), dup:=TRUE]
if (nrow(outputs[dup==TRUE])!=0){
  print(paste0("There are ", nrow(outputs[dup==TRUE]), " duplicates in municipality and date in the outputs data. Review."))
}

# Check to make sure that the first number of municipality is the department 
activities[, mun_start:=floor(municipality/100)]
activities[department!=mun_start, department_error:=TRUE]
activities[department==mun_start, department_error:=FALSE]
if (nrow(activities[department_error==TRUE])!=0){
  print(paste0("There are ", nrow(activities[department_error==TRUE]), " cases where the first numbers of municipality don't match department in activities data."))
}

outputs[, mun_start:=floor(municipality/100)]
outputs[department!=mun_start, department_error:=TRUE]
outputs[department==mun_start, department_error:=FALSE]
if (nrow(outputs[department_error==TRUE])!=0){
  print(paste0("There are ", nrow(outputs[department_error==TRUE]), " cases where the first numbers of municipality don't match department in outputs data."))
}

#See if there are any NAs in values for municipality, department, or date. 
vars = c('municipality', 'department', 'date')
for (var in vars){
  activities[is.na(get(var)), NA_ERROR:=TRUE]
  outputs[is.na(get(var)), NA_ERROR:=TRUE]
  if (var%in%c('municipality', 'department')){
    activities[get(var)==0, NA_ERROR:=TRUE]
    outputs[get(var)==0, NA_ERROR:=TRUE]
  }
}

if (nrow(activities[NA_ERROR==TRUE])!=0){
  print("There are NAs in key variables in activities data")
  print(unique(activities[NA_ERROR==TRUE, .(date, department, municipality)]))
}

if (nrow(outputs[NA_ERROR==TRUE])!=0){
  print("There are NAs in key variables in outputs data")
  print(unique(outputs[NA_ERROR==TRUE, .(date, department, municipality)]))
}

#Drop unneeded names 
activities = activities[, -c('dup', 'mun_start', 'department_error', 'NA_ERROR')]
outputs = outputs[, -c('dup', 'mun_start', 'department_error', 'NA_ERROR')]
#------------------------
# Activities
#------------------------
#Make a department-level dataset and a municipality-level dataset. 
dep_level_a = activities[, .(date, department, Total_Drugs_Distributed_value_d, Isoniazid_Distributed_value_d, Number_of_Cases_Screened_for_MDR_value_d, Second_Line_Drugs_Distributed_value_d)]
names(dep_level_a) = gsub("_d", "", names(dep_level_a))
dep_level_a = unique(dep_level_a)

mun_level_a = activities[, .(date, department, PLHIV_Screened_for_TB_value_m, TB_Patients_Tested_for_HIV_value_m)] #Don't need municipality here. 
mun_level_a = mun_level_a[, .(PLHIV_Screened_for_TB_value=sum(PLHIV_Screened_for_TB_value_m, na.rm=T), TB_Patients_Tested_for_HIV_value=sum(TB_Patients_Tested_for_HIV_value_m, na.rm=T)), 
          by=c('date', 'department')]

activities1 = merge(dep_level_a, mun_level_a, by=c('date', 'department'), all=T)
#Make sure you've accounted for all columns except municipality. 
stopifnot(ncol(activities1) == ncol(activities)-1)
new_names = names(activities1)[3:ncol(activities1)]  #Resest the names so they're distinguishable from activity variables. 
new_names = paste0(new_names, "_act")
names(activities1)[3:ncol(activities1)] <- new_names

#------------------------
# Outputs
#------------------------
dep_level_o = outputs[, .(date, department, Cases_Started_on_Treatment_value_d, Cases_Notified_in_Prisons_value, 
                             MDR_Cases_Notified_value_d, MDR_Cases_Started_Treatment_value_d)]
names(dep_level_o) = gsub("_d", "", names(dep_level_o))
dep_level_o = unique(dep_level_o)

mun_level_o = outputs[, .(date, department, Cases_Notified_value_m, Additional_Cases_Detected_via_ACF_value_m, PLHIV_started_on_IPT_value_m,
                             Children_in_Contact_with_TB_Started_IPT_value_m, `HIV_TB_Cases_Notified_value_m`, Cases_Started_on_Treatment_in_Prisons_value_m)] #Don't need municipality here. 
mun_vars = names(mun_level_o)[3:ncol(mun_level_o)]
for (v in mun_vars){
  mun_level_o[, (v):=sum(get(v), na.rm=T), by=c('date', 'department')]
}
mun_level_o = unique(mun_level_o)
names(mun_level_o) <- gsub("_m", "", names(mun_level_o))

outputs1 = merge(dep_level_o, mun_level_o, by=c('date', 'department'), all=T)
#Make sure you've accounted for all columns except municipality. 
stopifnot(ncol(outputs1) == ncol(outputs)-1)
new_names = names(outputs1)[3:ncol(outputs1)]  #Resest the names so they're distinguishable from activity variables. 
new_names = paste0(new_names, "_out")
names(outputs1)[3:ncol(outputs1)] <- new_names

#-----------------------------------------------------
# Check to make sure you're still uniquely identifying data 
#-----------------------------------------------------
activities[duplicated(activities, by=c('department','date')), dup:=TRUE]
if (nrow(activities[dup==TRUE])!=0){
  print(paste0("There are ", nrow(activities[dup==TRUE]), " duplicates in department and date in the activities data. Review."))
}

outputs[duplicated(outputs, by=c('department', 'date')), dup:=TRUE]
if (nrow(outputs[dup==TRUE])!=0){
  print(paste0("There are ", nrow(outputs[dup==TRUE]), " duplicates in department and date in the outputs data. Review."))
}

activities = activities[, -c('dup')]
outputs = outputs[, -c('dup')]

#-----------------------------------------------------
# Merge data 
#-----------------------------------------------------
dt_final = merge(activities1, outputs1, by=c('date', 'department'), all=T) #Save dates and departments from both, in case you have data in one and not the other. 
#-----------------------------------------------------
# Save data 
#-----------------------------------------------------
saveRDS(dt_final, outputFile2b)
archive(outputFile2b)



