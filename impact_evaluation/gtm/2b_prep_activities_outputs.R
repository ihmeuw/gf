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

#Change year and quarter to date 
activities[, quarter:=(quarter/4)-0.25]
activities[, date:=year+quarter]

outputs[, quarter:=(quarter/4)-0.25]
outputs[, date:=year+quarter]

#Add _ to names of data. 
names(activities) = gsub(" ", "_", names(activities))
names(outputs) = gsub(" ", "_", names(outputs))
names(activities) = gsub("/", "_", names(activities))
names(outputs) = gsub("/", "_", names(outputs))

#-------------------------------------------------------
# Before anything is changed, make general variable graphs. 
#-------------------------------------------------------
# activities_wide = melt(activities, id.vars = c('date', 'department', 'municipality'))
# pdf(paste0(visIeDir, "raw_activities_plots.pdf"), height=5.5, width=9)
# #Municipality level plots - only do where municipality is not NA
# act_muns = unique(activities_wide$municipality)
# for (m in act_muns){
#   plot = ggplot(activities_wide[municipality==m], aes(y=value, x=date)) +
#     geom_line() +
#     facet_wrap(~variable, scales='free') +
#     labs(title=paste('Time series of all activity vars for municipality ', m), y='Value', x='Date') +
#     theme_bw()
#   print(plot)
# }
# 
# #Department-level plots
# act_depts = unique(activities_wide$department)
# activities_wide_d = activities_wide[, .(value = sum(value)), by=c('date', 'department', 'variable')]
# for (d in act_depts){
#   plot = ggplot(activities_wide_d[department==d], aes(y=value, x=date)) +
#     geom_line() +
#     facet_wrap(~variable, scales='free') +
#     labs(title=paste('Time series of all activity vars for department ', d), y='Value', x='Date') +
#     theme_bw()
#   print(plot)
# }
# dev.off()
# 
# outputs_wide = melt(outputs, id.vars = c('date', 'department', 'municipality'))
# pdf(paste0(visIeDir, "raw_outputs_plots.pdf"), height=5.5, width=9)
# #Municipality level plots - only do where municipality is not NA
# out_muns = unique(outputs_wide$municipality)
# for (m in out_muns){
#   plot = ggplot(outputs_wide[municipality==m], aes(y=value, x=date)) +
#     geom_line() +
#     facet_wrap(~variable, scales='free') +
#     labs(title=paste('Time series of all output vars for municipality ', m), y='Value', x='Date') +
#     theme_bw()
#   print(plot)
# }
# 
# #Department-level plots
# out_depts = unique(outputs_wide$department)
# outputs_wide_d = outputs_wide[, .(value=sum(value)), by=c('date', 'department', 'variable')]
# for (d in out_depts){
#   plot = ggplot(outputs_wide_d[department==d], aes(y=value, x=date)) +
#     geom_line() +
#     facet_wrap(~variable, scales='free') +
#     labs(title=paste('Time series of all output vars for department ', d), y='Value', x='Date') +
#     theme_bw()
#   print(plot)
# }
# dev.off()

#----------------------------------------------------
# Validate files, and subset data. 
#----------------------------------------------------

#Replace unknown municipalities 
stopifnot(nrow(activities[is.na(municipality) | is.na(department)])==0)
stopifnot(nrow(outputs[is.na(municipality) | is.na(department)])==0)

#Drop all 0 departments and municipalities - these are national-level. 
# There are 0 of these cases in the 7.15.19 data - EL 
activities = activities[!(department==0|municipality==0)]
outputs = outputs[!(department==0 | municipality==0)]

#Make sure that merge below will work - dates.  
a_dates = unique(activities$date)
o_dates = unique(outputs$date)

a_dates[!a_dates%in%o_dates] #None. EL 8/7/19  
o_dates[!o_dates%in%a_dates] #2009 and 2012. EL 8/7/19 2009, EL 8/19/19

#Departments
a_depts = unique(activities$department)
o_depts = unique(outputs$department)

a_depts[!a_depts%in%o_depts] #None. 
o_depts[!o_depts%in%a_depts] #None. 

#Municipalities
a_mun = unique(activities$municipality)
o_mun = unique(outputs$municipality)

a_mun[!a_mun%in%o_mun] #None. 7.15.19 EL 
o_mun[!o_mun%in%a_mun] #None. 7.15.19 EL ==> Changed to several, EL 8/7/19 ==> Changed back to none 8/19/19. 

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
#Run a check to decide which variables are already at the dept. level and which need to be summed. 
vars = names(activities)[!names(activities)%in%c('date', 'department', 'municipality')]
dep_vars = c()
mun_vars = c()
for (var in vars){
  dt = unique(activities[, .(date, department, var=get(var))])
  dt[duplicated(dt, by=c('date', 'department')), dup:=TRUE]
  if (nrow(dt[dup==TRUE])!=0){
    mun_vars = c(mun_vars, var)
  } else {
    dep_vars = c(dep_vars, var)
  }
}

#Find out which variables are at the year and quarter level. 
year_vars = vars[grepl("yearly", vars)]
year_vars = c(year_vars, "Second_Line_Drugs_Distributed_value_d") #Hard-code second line drugs. EL 10.24.2019

quarter_vars = vars[grepl("quarterly", vars)]
stopifnot(length(year_vars)+length(quarter_vars)==length(vars)-2) #Subtract 2 because year and quarter are still in the data. 

# #Go ahead and hard code these variables to be department-level, because there is a data prep error. EL 7.8.19
#This should be removed once new data is sent! REMOVED 8/19/19 EL 
# dep_vars = c("Total_Drugs_Distributed_value_d", "Isoniazid_Distributed_value_d", dep_vars)
# mun_vars = mun_vars[!mun_vars%in%c("Total_Drugs_Distributed_value_d", "Isoniazid_Distributed_value_d")]

#Flag cases where variables end in _d but are in the mun-level dataset. 
dept_level_error = mun_vars[grepl("_d", mun_vars)]
if (length(dept_level_error)!=0){
  print("ERROR: Some department-level variables are not uniquely identified by department and date!")
  print(dept_level_error)
}

#-------------------------------------------------------------------------------------------------------------
# Handle 4 unique cases: department + year, department + quarter, municipality + year, municipality + quarter 

case1 = names(activities)[names(activities)%in%dep_vars & names(activities)%in%year_vars]
case2 = names(activities)[names(activities)%in%dep_vars & names(activities)%in%quarter_vars]
case3 = names(activities)[names(activities)%in%mun_vars & names(activities)%in%year_vars]
case4 = names(activities)[names(activities)%in%mun_vars & names(activities)%in%quarter_vars]

date_frame = data.table(expand.grid(year=seq(2009, 2018, by=1), quarter=seq(0.0, 0.75, by=0.25)))
date_frame[, date:=year+quarter]
date_frame$quarter = NULL

#-------------------------------
# Case 1 - department and year 

#Take the average of the department-level variables by date and department. 
dt_case1 = data.table(year=integer(), department=integer())
for (var in case1){
  var_subset = activities[, .(var=mean(get(var), na.rm=T)), by=c('year', 'department')]
  names(var_subset)[3] = var
  dt_case1 = merge(dt_case1, var_subset, by=c('year', 'department'), all=T)
}
#Check for uniqueness. 
dt_case1[duplicated(dt_case1, by=c('year', 'department')), dup:=TRUE]
stopifnot(nrow(dt_case1[dup==TRUE])==0)
dt_case1$dup<-NULL

# Divide into quarters. 
start_row = nrow(dt_case1)
dt_case1 = merge(dt_case1, date_frame, by='year', all.x=T, allow.cartesian=T)
for (var in case1){
  dt_case1[, (var):=get(var)/4] #Divide each variable to the quarter-level. This assumes everything is counts! 
}
stopifnot(nrow(dt_case1) == start_row*4)

#Fix names, and drop unneeded columns. 
names(dt_case1) = gsub("_m|_d|_yearly|_quarterly|_value", "", names(dt_case1))
dt_case1$year <- NULL

#-------------------------------
# Case 2 - department and quarter

#Take the average of the department-level variables by date and department. 
dt_case2 = data.table(date=integer(), department=integer())
for (var in case2){
  var_subset = activities[, .(var=mean(get(var), na.rm=T)), by=c('date', 'department')]
  names(var_subset)[3] = var
  dt_case2 = merge(dt_case2, var_subset, by=c('date', 'department'), all=T)
}
#Check for uniqueness. 
dt_case2[duplicated(dt_case2, by=c('date', 'department')), dup:=TRUE]
stopifnot(nrow(dt_case2[dup==TRUE])==0)
dt_case2$dup<-NULL

#Fix names. 
names(dt_case2) = gsub("_m|_d|_yearly|_quarterly|_value", "", names(dt_case2))

#-------------------------------
# Case 3 - municipality and year

#Take the sum of municipality-level variables by date and department. 
dt_case3 = data.table(year=integer(), department=integer())
for (var in case3){
  var_subset = activities[, .(var=sum(get(var), na.rm=T)), by=c('year', 'department')]
  names(var_subset)[3] = var
  dt_case3 = merge(dt_case3, var_subset, by=c('year', 'department'), all=T)
}
#Check for uniqueness. 
dt_case3[duplicated(dt_case3, by=c('year', 'department')), dup:=TRUE]
stopifnot(nrow(dt_case3[dup==TRUE])==0)
dt_case3$dup<-NULL

# Divide into quarters. 
start_row = nrow(dt_case3)
dt_case3 = merge(dt_case3, date_frame, by='year', all.x=T, allow.cartesian=T)
for (var in case3){
  dt_case3[, (var):=get(var)/4] #Divide each variable to the quarter-level. This assumes everything is counts! 
}
stopifnot(nrow(dt_case3) == start_row*4)

#Fix names, and drop unneeded columns. 
names(dt_case3) = gsub("_m|_d|_yearly|_quarterly|_value", "", names(dt_case3))
dt_case3$year <- NULL

#-------------------------------
# Case 4 - municipality and quarter

#Take the sum of municipality-level variables by date and department. 
dt_case4 = data.table(date=integer(), department=integer())
for (var in case4){
  var_subset = activities[, .(var=sum(get(var), na.rm=T)), by=c('date', 'department')]
  names(var_subset)[3] = var
  dt_case4 = merge(dt_case4, var_subset, by=c('date', 'department'), all=T)
}
#Check for uniqueness. 
dt_case4[duplicated(dt_case4, by=c('date', 'department')), dup:=TRUE]
stopifnot(nrow(dt_case4[dup==TRUE])==0)
dt_case4$dup<-NULL

#Fix names. 
names(dt_case4) = gsub("_m|_d|_yearly|_quarterly|_value", "", names(dt_case4))

#-----------------------------------------
# Merge all of this data together. 
activities1 = merge(dt_case1, dt_case2, by=c('date', 'department'), all=T)
activities1 = merge(activities1, dt_case3, by=c('date', 'department'), all=T)
activities1 = merge(activities1, dt_case4, by=c('date', 'department'), all=T)

#Make sure you've accounted for all columns except municipality, year, and quarter
stopifnot(ncol(activities1) == (ncol(activities)-3))
new_names = names(activities1)[3:ncol(activities1)]  #Resest the names so they're distinguishable from activity variables. 
new_names = paste0(new_names, "_act")
names(activities1)[3:ncol(activities1)] <- new_names

#------------------------
# Outputs
#------------------------
#Run a check to decide which variables are already at the dept. level and which need to be summed. 
vars = names(outputs)[!names(outputs)%in%c('date', 'department', 'municipality')]
dep_vars = c()
mun_vars = c()
for (var in vars){
  dt = unique(outputs[, .(date, department, var=get(var))])
  dt[duplicated(dt, by=c('date', 'department')), dup:=TRUE]
  if (nrow(dt[dup==TRUE])!=0){
    mun_vars = c(mun_vars, var)
  } else {
    dep_vars = c(dep_vars, var)
  }
}

#Find out which variables are at the year and quarter level. 
year_vars = vars[grepl("yearly", vars)]
quarter_vars = vars[grepl("quarterly", vars)]
stopifnot(length(year_vars)+length(quarter_vars)==length(vars)-2) #Subtract 2 because year and quarter are still in the data. 

# #Go ahead and hard code these variables to be department-level, because there is a data prep error. EL 7.8.19
#This should be removed once new data is sent! REMOVED 8/19/19 EL 
# dep_vars = c("Total_Drugs_Distributed_value_d", "Isoniazid_Distributed_value_d", dep_vars)
# mun_vars = mun_vars[!mun_vars%in%c("Total_Drugs_Distributed_value_d", "Isoniazid_Distributed_value_d")]

#Flag cases where variables end in _d but are in the mun-level dataset. 
dept_level_error = mun_vars[grepl("_d", mun_vars)]
if (length(dept_level_error)!=0){
  print("ERROR: Some department-level variables are not uniquely identified by department and date!")
  print(dept_level_error)
}

#-------------------------------------------------------------------------------------------------------------
# Handle 4 unique cases: department + year, department + quarter, municipality + year, municipality + quarter 

case1 = names(outputs)[names(outputs)%in%dep_vars & names(outputs)%in%year_vars]
case2 = names(outputs)[names(outputs)%in%dep_vars & names(outputs)%in%quarter_vars]
case3 = names(outputs)[names(outputs)%in%mun_vars & names(outputs)%in%year_vars]
case4 = names(outputs)[names(outputs)%in%mun_vars & names(outputs)%in%quarter_vars]

date_frame = data.table(expand.grid(year=seq(2009, 2018, by=1), quarter=seq(0.0, 0.75, by=0.25)))
date_frame[, date:=year+quarter]
date_frame$quarter = NULL

#-------------------------------
# Case 1 - department and year 

#Take the average of the department-level variables by date and department. 
dt_case1 = data.table(year=integer(), department=integer())
for (var in case1){
  var_subset = outputs[, .(var=mean(get(var), na.rm=T)), by=c('year', 'department')]
  names(var_subset)[3] = var
  dt_case1 = merge(dt_case1, var_subset, by=c('year', 'department'), all=T)
}
#Check for uniqueness. 
dt_case1[duplicated(dt_case1, by=c('year', 'department')), dup:=TRUE]
stopifnot(nrow(dt_case1[dup==TRUE])==0)
dt_case1$dup<-NULL

# Divide into quarters. 
start_row = nrow(dt_case1)
dt_case1 = merge(dt_case1, date_frame, by='year', all.x=T, allow.cartesian=T)
for (var in case1){
  dt_case1[, (var):=get(var)/4] #Divide each variable to the quarter-level. This assumes everything is counts! 
}
stopifnot(nrow(dt_case1) == start_row*4)

#Fix names, and drop unneeded columns. 
names(dt_case1) = gsub("_m|_d|_yearly|_quarterly|_value", "", names(dt_case1))
dt_case1$year <- NULL

#-------------------------------
# Case 2 - department and quarter

#Take the average of the department-level variables by date and department. 
dt_case2 = data.table(date=integer(), department=integer())
for (var in case2){
  var_subset = outputs[, .(var=mean(get(var), na.rm=T)), by=c('date', 'department')]
  names(var_subset)[3] = var
  dt_case2 = merge(dt_case2, var_subset, by=c('date', 'department'), all=T)
}
#Check for uniqueness. 
dt_case2[duplicated(dt_case2, by=c('date', 'department')), dup:=TRUE]
stopifnot(nrow(dt_case2[dup==TRUE])==0)
dt_case2$dup<-NULL

#Fix names. 
names(dt_case2) = gsub("_m|_d|_yearly|_quarterly|_value", "", names(dt_case2))

#-------------------------------
# Case 3 - municipality and year

#Take the sum of municipality-level variables by date and department. 
dt_case3 = data.table(year=integer(), department=integer())
for (var in case3){
  var_subset = outputs[, .(var=sum(get(var), na.rm=T)), by=c('year', 'department')]
  names(var_subset)[3] = var
  dt_case3 = merge(dt_case3, var_subset, by=c('year', 'department'), all=T)
}
#Check for uniqueness. 
dt_case3[duplicated(dt_case3, by=c('year', 'department')), dup:=TRUE]
stopifnot(nrow(dt_case3[dup==TRUE])==0)
dt_case3$dup<-NULL

# Divide into quarters. 
start_row = nrow(dt_case3)
dt_case3 = merge(dt_case3, date_frame, by='year', all.x=T, allow.cartesian=T)
for (var in case3){
  dt_case3[, (var):=get(var)/4] #Divide each variable to the quarter-level. This assumes everything is counts! 
}
stopifnot(nrow(dt_case3) == start_row*4)

#Fix names, and drop unneeded columns. 
names(dt_case3) = gsub("_m|_d|_yearly|_quarterly|_value", "", names(dt_case3))
dt_case3$year <- NULL

#-------------------------------
# Case 4 - municipality and quarter

#Take the sum of municipality-level variables by date and department. 
dt_case4 = data.table(date=integer(), department=integer())
for (var in case4){
  var_subset = outputs[, .(var=sum(get(var), na.rm=T)), by=c('date', 'department')]
  names(var_subset)[3] = var
  dt_case4 = merge(dt_case4, var_subset, by=c('date', 'department'), all=T)
}
#Check for uniqueness. 
dt_case4[duplicated(dt_case4, by=c('date', 'department')), dup:=TRUE]
stopifnot(nrow(dt_case4[dup==TRUE])==0)
dt_case4$dup<-NULL

#Fix names. 
names(dt_case4) = gsub("_m|_d|_yearly|_quarterly|_value", "", names(dt_case4))

#-----------------------------------------
# Merge all of this data together. 
outputs1 = merge(dt_case1, dt_case2, by=c('date', 'department'), all=T)
outputs1 = merge(outputs1, dt_case3, by=c('date', 'department'), all=T)
outputs1 = merge(outputs1, dt_case4, by=c('date', 'department'), all=T)

#Make sure you've accounted for all columns except municipality, year, and quarter
stopifnot(ncol(outputs1) == (ncol(outputs)-3))
new_names = names(outputs1)[3:ncol(outputs1)]  #Resest the names so they're distinguishable from activity variables. 
new_names = paste0(new_names, "_out")
names(outputs1)[3:ncol(outputs1)] <- new_names

#--------------------------------
# Add "Children <5 referred for tb evaluation" to ACF pathway EL 8/22/19 
# Using new extramuros data uploaded from Guillermo
extramuros = data.table(read_excel("J:/Project/Evaluation/GF/impact_evaluation/gtm/raw_data/TB-Extramuros-2017-2018.xlsx"))
names(extramuros) = tolower(names(extramuros))
extramuros = extramuros[variable=="Cantidad  de niños <de 5 años referidos para evaluación a través de ruta diagnostica para descartar tuberculosis", .(Children_less5_referred_out=sum(extramuros)), by=c('department', 'year')]
setnames(extramuros, 'year', 'date')

#Merge data together
outputs1 = merge(outputs1, extramuros, by=c('department', 'date'), all=T)
#-----------------------------------------------------
# Check to make sure you're still uniquely identifying data 
#-----------------------------------------------------
activities1[duplicated(activities1, by=c('department','date')), dup:=TRUE]
if (nrow(activities1[dup==TRUE])!=0){
  print(paste0("There are ", nrow(activities1[dup==TRUE]), " duplicates in department and date in the activities data. Review."))
}

outputs1[duplicated(outputs1, by=c('department', 'date')), dup:=TRUE]
if (nrow(outputs1[dup==TRUE])!=0){
  print(paste0("There are ", nrow(outputs1[dup==TRUE]), " duplicates in department and date in the outputs data. Review."))
}

activities1 = activities1[, -c('dup')]
outputs1 = outputs1[, -c('dup')]

#-----------------------------------------------------
# Merge data 
#-----------------------------------------------------
dt_final = merge(activities1, outputs1, by=c('date', 'department'), all=T) #Save dates and departments from both, in case you have data in one and not the other. 

#Replace NaN and NA with 0 - we can assume these actually mean 0. 
# cols = 3:ncol(dt_final) #Just don't do this for date and department, the first two columns. 
# for (col in cols){
#   dt_final[is.na(dt_final[[col]]), (col):=0]
# }

#Generate combined first- and second-line drug activity variables EL 8/22/19 
# EDIT FROM DAVID PHILLIPS 9/6/2019 - we need to impute the combination variables for this step right before creating first and second-line variables. 
# This imputation code is copied from step 4a. 

#--------------------------------------------------
# extrapolate where necessary using GLM (better would be to use multiple imputation)
i=1
for(v in drugComboVars) {
  for(h in unique(dt_final$department)) { 
    i=i+1
    #First, check whether all values for this department and this variable are zero. 
    # if they are, don't backcast. 
    values = unique(dt_final[department==h, as.vector(get(v))]) #Get a vector of the unique values of the variable.
    values[is.na(values)] = 0
    zero_compare = rep(0, length(values)) #Get an equal length vector of zeros.
    if (all(values==zero_compare)){
      print(paste0(v, " is completely zero for department", h, " - making 0 for the entire time series in this department"))
      dt_final[department==h, (v):=0]
    } else {
      #Backcast if it doesn't fall into this category. 
      if (!any(is.na(dt_final[department==h][[v]]))) next
      if (!any(!is.na(dt_final[department==h][[v]]))) next
      form = as.formula(paste0(v,'~date'))
      lmFit = glm(form, dt_final[department==h], family='poisson')
      dt_final[department==h, tmp:=exp(predict(lmFit, newdata=dt_final[department==h]))]
      lim = max(dt_final[department==h][[v]], na.rm=T)+sd(dt_final[department==h][[v]], na.rm=T)
      dt_final[department==h & tmp>lim, tmp:=lim]
      dt_final[department==h & is.na(get(v)), (v):=tmp]
    } 
    pct_complete = floor(i/(length(drugComboVars)*length(unique(dt_final$department)))*100)
    cat(paste0('\r', pct_complete, '% Complete'))
    flush.console() 
  }
}
dt_final$tmp = NULL


# Replace NAs with zeros after back-casting DP 8/16/19 
for (v in drugComboVars){
  dt_final[is.na(get(v)), (v):=0]
}
#-----------------------------------------------------------------------

dt_final[, Firstline_Distributed_act:=sum(Total_First_Line_Drugs_inusIsonizide__Distributed_act, Isoniazid_Distributed_act, na.rm=T), by=c('date', 'department')]
dt_final[, Secondline_Distributed_act:=sum(Second_Line_Drugs_Distributed_act, Total_MDR_Drugs_Distributed_act, na.rm=T), by=c('date', 'department')]

#-----------------------------------------------------
# Save data 
#-----------------------------------------------------
saveRDS(dt_final, outputFile2b)
archive(outputFile2b)

print("Step 2b: Prep activities outputs completed successfully.")



