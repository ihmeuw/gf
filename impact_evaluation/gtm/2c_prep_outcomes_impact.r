# ----------------------------------------------
# Emily Linebarger, based on code written by Audrey Batzel and David Phillips
# 
# June 2019
# Prep impacts, outcomes and impact indicators for 'second half' dose response model
# Intended to be run by 1_master_file.r
# ----------------------------------------------

# ----------------------------------------------
# to do


# ----------------------------------------------

source("./impact_evaluation/gtm/gen_proportion_denominators.r")

#Read in data 
outcomes = fread(outcomeFile)
impacts = fread(impactFile)

#Change year and quarter to date 
outcomes[, quarter:=(quarter/4)-0.25]
outcomes[, date:=year+quarter]

impacts[, quarter:=(quarter/4)-0.25]
impacts[, date:=year+quarter]

#Add _ to names of data. 
names(outcomes) = gsub(" ", "_", names(outcomes))
names(impacts) = gsub(" ", "_", names(impacts))
names(outcomes) = gsub("/", "_", names(outcomes))
names(impacts) = gsub("/", "_", names(impacts))

#----------------------------------------------------
# Validate files, and subset data. 
#----------------------------------------------------
#Make sure that merge below will work - dates.  
o_dates = unique(outcomes$date)
i_dates = unique(impacts$date)

o_dates[!o_dates%in%i_dates] #2018 
i_dates[!i_dates%in%o_dates] #2009-2011.  

#Departments
o_depts = unique(outcomes$department)
i_depts = unique(impacts$department)

o_depts[!o_depts%in%i_depts] #None. 
i_depts[!i_depts%in%o_depts] #None. 

#Municipalities
o_mun = unique(outcomes$municipality)
i_mun = unique(impacts$municipality)

o_mun[!o_mun%in%i_mun] #None.
i_mun[!i_mun%in%o_mun] #None. ==> Changed to municipalities 410 and 514, from none before. 8/7/19 EL

#Subset data to only department-level, because municipalities aren't matching right now. 

#Check that data is uniquely identified
outcomes[duplicated(outcomes, by=c('municipality', 'department','date')), dup:=TRUE]
if (nrow(outcomes[dup==TRUE])!=0){
  print(paste0("There are ", nrow(outcomes[dup==TRUE]), " duplicates in municipality and date in the outcomes data. Review."))
}

impacts[duplicated(impacts, by=c('municipality', 'department', 'date')), dup:=TRUE]
if (nrow(impacts[dup==TRUE])!=0){
  print(paste0("There are ", nrow(impacts[dup==TRUE]), " duplicates in municipality and date in the impacts data. Review."))
}

# Check to make sure that the first number of municipality is the department 
outcomes[, mun_start:=floor(municipality/100)]
outcomes[department!=mun_start, department_error:=TRUE]
outcomes[department==mun_start, department_error:=FALSE]
if (nrow(outcomes[department_error==TRUE])!=0){
  print(paste0("There are ", nrow(outcomes[department_error==TRUE]), " cases where the first numbers of municipality don't match department in outcomes data."))
}

impacts[, mun_start:=floor(municipality/100)]
impacts[department!=mun_start, department_error:=TRUE]
impacts[department==mun_start, department_error:=FALSE]
if (nrow(impacts[department_error==TRUE])!=0){
  print(paste0("There are ", nrow(impacts[department_error==TRUE]), " cases where the first numbers of municipality don't match department in impacts data."))
}

#See if there are any NAs in values for municipality, department, or date. 
vars = c('municipality', 'department', 'date')
for (var in vars){
  outcomes[is.na(get(var)), NA_ERROR:=TRUE]
  impacts[is.na(get(var)), NA_ERROR:=TRUE]
  if (var%in%c('municipality', 'department')){
    outcomes[get(var)==0, NA_ERROR:=TRUE]
    impacts[get(var)==0, NA_ERROR:=TRUE]
  }
}

if (nrow(outcomes[NA_ERROR==TRUE])!=0){
  print("There are NAs in key variables in outcomes data")
  print(unique(outcomes[NA_ERROR==TRUE, .(date, department, municipality)]))
}

if (nrow(impacts[NA_ERROR==TRUE])!=0){
  print("There are NAs in key variables in impacts data")
  print(unique(impacts[NA_ERROR==TRUE, .(date, department, municipality)]))
}

#Drop unneeded names 
outcomes = outcomes[, -c('dup', 'mun_start', 'department_error', 'NA_ERROR')]
impacts = impacts[, -c('dup', 'mun_start', 'department_error', 'NA_ERROR')]

#----------------------------------------------------------------------------------------
# Merge outcomes and impacts together, and then collapse to the department/quarter level. 
#----------------------------------------------------------------------------------------
dt = merge(outcomes, impacts, by=c('year', 'quarter', 'date', 'department', 'municipality'), all=T)

#Run a check to decide which variables are already at the dept. level and which need to be summed. 
vars = names(dt)[!names(dt)%in%c('date', 'quarter', 'year', 'department', 'municipality')]
dep_vars = c()
mun_vars = c()
for (var in vars){
  subset = unique(dt[, .(date, department, var=get(var))])
  subset = subset[!is.na(var)]#It's okay to ignore NAs. 
  subset[duplicated(subset, by=c('date', 'department')) & !is.na(var), dup:=TRUE]
  if (nrow(subset[dup==TRUE])!=0){
    mun_vars = c(mun_vars, var)
  } else {
    dep_vars = c(dep_vars, var)
  }
}

#Find out which variables are at the year and quarter level. 
year_vars = vars[grepl("yearly", vars)]
quarter_vars = vars[grepl("quarterly", vars)]
stopifnot(length(year_vars)+length(quarter_vars)==length(vars))

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

case1 = names(dt)[names(dt)%in%dep_vars & names(dt)%in%year_vars]
case2 = names(dt)[names(dt)%in%dep_vars & names(dt)%in%quarter_vars]
case3 = names(dt)[names(dt)%in%mun_vars & names(dt)%in%year_vars]
case4 = names(dt)[names(dt)%in%mun_vars & names(dt)%in%quarter_vars]

#make a date frame to expand to quarter-level. 
date_frame = data.table(expand.grid(date=seq(2009.0, 2018.75, by=0.25)))
date_frame[, year:=floor(date)]

#-------------------------------
# Case 1 - department and year. Take the average of the variable by department, and then use GLM to expand to quarter-level. 

#Take the average of the department-level variables by date and department. 
dt_case1 = data.table(year=integer(), department=integer())
for (var in case1){
  var_subset = dt[, .(var=mean(get(var), na.rm=T)), by=c('year', 'department')]
  names(var_subset)[3] = var
  dt_case1 = merge(dt_case1, var_subset, by=c('year', 'department'), all=T)
}
#Check for uniqueness. 
dt_case1[duplicated(dt_case1, by=c('year', 'department')), dup:=TRUE]
stopifnot(nrow(dt_case1[dup==TRUE])==0)
dt_case1$dup<-NULL

# Divide into quarters. 
# extrapolate where necessary using GLM (better would be to use multiple imputation)
dt_case1_final = data.table(date=double(), department=integer()) 
i=1
for (v in as.character(case1)){
  all_departments = data.table() 
  for(h in unique(dt_case1$department)) { 
    i=i+1
    #First, check whether all values for this department and this variable are zero. 
    # if they are, don't backcast. 
    subset = dt_case1[department==h, .(year, v=get(v))]
    subset[, date:=year+0.5] #make each of these data points the midpoint of the year. 
    subset = merge(subset, date_frame, all=T, by=c('date', 'year'))
    subset$department=h
    
    #We were replacing NAs with zeros before - I think it's better to leave them as-is here? EL
    if (!is.na(all(subset$v))){ #If you have at least one non-NA value, interpolate. 
      #Linearly interpolate all NAs
      form = as.formula('v~year')
      lmFit = glm(form, subset, family='poisson')
      subset[, tmp:=exp(predict(lmFit, newdata=subset))]
      #lim = max(dt_case1[department==h][[v]], na.rm=T)+sd(dt_case1[department==h][[v]], na.rm=T)
      # dt_case1[department==h & tmp>lim, tmp:=lim] #Remove limit for this interpolation EL 10/24/2019
      subset[is.na(v), v:=tmp]
      
    } 
    #Bind this together into a new data table 
    subset = subset[, .(date, department, v)]
    setnames(subset, 'v', v)
    
    all_departments = rbind(subset, all_departments)
    
    # Track progress
    pct_complete = floor(i/(length(case1)*length(unique(dt_case1$department)))*100)
    cat(paste0('\r', pct_complete, '% Complete'))
    flush.console() 
  }
  dt_case1_final = merge(dt_case1_final, all_departments, by=c('date', 'department'), all=T)
}

#Fix names, and drop unneeded columns. 
names(dt_case1_final) = gsub("_m|_d|_yearly|_quarterly|_value", "", names(dt_case1_final))
dt_case1_final$year <- NULL

#-------------------------------
# Case 2 - department and quarter

#Take the average of the department-level variables by date and department. 
dt_case2 = data.table(date=integer(), department=integer())
for (var in case2){
  var_subset = dt[, .(var=mean(get(var), na.rm=T)), by=c('date', 'department')]
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

# PART A: collapse municipality to the department-level, merge on denominator dataset and take a weighted average
dt_case3 = unique(dt[, c("Proportion_of_Cases_in_Prisons_Treated_value_d_yearly", "Mortality_Rate_value_m_yearly", "HIV_TB_Mortality_Rate_value_m_yearly",
                  'year', 'department', 'municipality')]) #Collapse out the quarter level. 

#Merge onto the municipality-level denominator dataset. 
dt_case3 = merge(dt_case3, municipality_denom_a, by=c('year',  'department', 'municipality'), all.x=T)

#Make a table of the numerators and their respective denominators. 
prop_and_denom = data.table(prop=c("Proportion_of_Cases_in_Prisons_Treated_value_d_yearly", "Mortality_Rate_value_m_yearly", "HIV_TB_Mortality_Rate_value_m_yearly"), 
                          denom=rep('population', 3))

#Using these denominators, take a weighted average to calculate the department level. 
dt_case3_step_a = data.table(year=integer(), department=integer()) 
for (i in 1:nrow(prop_and_denom)){
  proportion = prop_and_denom$prop[i]
  denominator = prop_and_denom$denom[i]
  
  #Just grab what you need. 
  cols = c('year', 'department', proportion, denominator)
  subset = dt_case3[, cols, with=FALSE]
  subset[, weighted_numerator_m:=get(proportion)*get(denominator)]
  
  subset[, numerator_d:=sum(weighted_numerator_m, na.rm=T), by=c('year', 'department')]
  subset[, total_d:=sum(get(denominator), na.rm=T), by=c('year', 'department')]
  subset[, (paste0(proportion)):=numerator_d/total_d, by=c('year', 'department')] #This reassigns the variable you originally started with! 
  
  #Merge onto aggregate dataset. 
  subset = subset[, c('year', 'department', proportion), with=FALSE]
  subset = unique(subset)
  dt_case3_step_a = merge(dt_case3_step_a, subset, by=c('year', 'department'), all=T)
}

# PART B: Divide into quarters. - run linear extrapolation. 
# extrapolate where necessary using GLM (better would be to use multiple imputation)
dt_case3_final = data.table(date=double(), department=integer()) 
i=1
for (v in as.character(case3)){
  all_departments = data.table() 
  for(h in unique(dt_case3_step_a$department)) { 
    i=i+1
    #First, check whether all values for this department and this variable are zero. 
    # if they are, don't backcast. 
    subset = unique(dt_case3_step_a[department==h, .(year, v=get(v))]) #These are all at the quarter-level, so just take the unique values.
    subset[, date:=year+0.5] #make each of these data points the midpoint of the date. 
    subset = merge(subset, date_frame, by=c('year', 'date'), all=T)
    subset$department=h
    
    #We were replacing NAs with zeros before - I think it's better to leave them as-is here? EL
    if (!is.na(all(subset$v))){ #If you have at least one non-NA value, interpolate. 
      #Linearly interpolate all NAs
      form = as.formula('v~year')
      lmFit = glm(form, subset, family='poisson')
      subset[, tmp:=exp(predict(lmFit, newdata=subset))]
      #lim = max(dt_case3[department==h][[v]], na.rm=T)+sd(dt_case3[department==h][[v]], na.rm=T)
      # dt_case3[department==h & tmp>lim, tmp:=lim] #Remove limit for this interpolation EL 10/24/2019
      subset[is.na(v), v:=tmp]
    } 
    #Bind this together into a new data table 
    subset = subset[, .(date, department, v)]
    setnames(subset, 'v', v)
    
    all_departments = rbind(subset, all_departments)
    
    # Track progress
    pct_complete = floor(i/(length(case3)*length(unique(dt_case3$department)))*100)
    cat(paste0('\r', pct_complete, '% Complete'))
    flush.console() 
  }
  dt_case3_final = merge(dt_case3_final, all_departments, by=c('date', 'department'), all=T)
}

#Fix names, and drop unneeded columns. 
names(dt_case3_final) = gsub("_m|_d|_yearly|_quarterly|_value", "", names(dt_case3_final))

#-------------------------------
# Case 4 - municipality and quarter

# PART A: collapse municipality to the department-level, merge on denominator dataset and take a weighted average
dt_case4 = dt[, c(case4, 'year', 'quarter', 'municipality', 'department'), with=FALSE] 

#Merge onto the municipality-level denominator dataset. 
dt_case4 = merge(dt_case4, municipality_denom_q, by=c('year', 'quarter', 'department', 'municipality'), all.x=T)

#Make a table of the numerators and their respective denominators. 
prop_and_denom = data.table(prop=c("Proportion_of_Cases_Treated_value_m_quarterly", "Proportion_of_HIV_TB_Cases_Treated_value_m_quarterly", 
                                   "Proportion_of_TB_Patients_who_Received_HIV_Test_value_m_quarterly", "Proportion_of_Patients_Receiving_DST_value_m_quarterly", 
                                   "Case_Notification_Rate_value_m_quarterly"), 
                            denom=c('cases_notified', 'hivtb_cases_notified', 'cases_notified', 'cases_notified', 'population'))

#Using these denominators, take a weighted average to calculate the department level. 
dt_case4_final = data.table(year=integer(), quarter=double(), department=integer()) 
for (i in 1:nrow(prop_and_denom)){
  proportion = prop_and_denom$prop[i]
  denominator = prop_and_denom$denom[i]
  
  #Just grab what you need. 
  cols = c('year', 'quarter', 'department', proportion, denominator)
  subset = dt_case4[, cols, with=FALSE]
  subset[, weighted_numerator_m:=get(proportion)*get(denominator)]
  
  subset[, numerator_d:=sum(weighted_numerator_m, na.rm=T), by=c('year', 'quarter', 'department')]
  subset[, total_d:=sum(get(denominator), na.rm=T), by=c('year', 'quarter', 'department')]
  subset[, (paste0(proportion)):=numerator_d/total_d, by=c('year', 'quarter', 'department')] #This reassigns the variable you originally started with! 
  
  #Merge onto aggregate dataset. 
  subset = subset[, c('year', 'quarter', 'department', proportion), with=FALSE]
  subset = unique(subset)
  dt_case4_final = merge(dt_case4_final, subset, by=c('year', 'quarter', 'department'), all=T)
}

#Generate date variable 
dt_case4_final[, date:=year+quarter]
dt_case4_final$year <- NULL
dt_case4_final$quarter <- NULL

#Fix names, and drop unneeded columns. 
names(dt_case4_final) = gsub("_m|_d|_yearly|_quarterly|_value", "", names(dt_case4_final))

#-----------------------------------------
# Merge all of this data together. 
dt1 = merge(dt_case1_final, dt_case2, by=c('date', 'department'), all=T)
dt1 = merge(dt1, dt_case3_final, by=c('date', 'department'), all=T)
dt1 = merge(dt1, dt_case4_final, by=c('date', 'department'), all=T)

#Make sure you've accounted for all columns except municipality, year, and quarter
stopifnot(ncol(dt1) == (ncol(dt)-3))

#Assign name suffixes - all variables in outcomes data are proportions, and all variables in impact data are rates. 
old_names1 = names(dt1)[grepl("proportion", tolower(names(dt1)))]
new_names1 = paste0(old_names1, "_out")
old_names2 = names(dt1)[grepl("rate", tolower(names(dt1)))]
new_names2 = paste0(old_names2, "_imp")

setnames(dt1, old_names1, new_names1)
setnames(dt1, old_names2, new_names2)
#-----------------------------------------------------
# Check to make sure you're still uniquely identifying data 
#-----------------------------------------------------
dt1[duplicated(dt1, by=c('department','date')), dup:=TRUE]
if (nrow(dt1[dup==TRUE])!=0){
  print(paste0("There are ", nrow(dt1[dup==TRUE]), " duplicates in department and date in the dt data. Review."))
}

#Drop any municipalities and departments that are 0 - this should represent the national-level data. 
dt1 = dt1[department!=0]

# #Pull one variable, # of cases screened for MDR-TB, out of the outputs dataset. 
# dt1 = readRDS(outputFile3)
# dt1 = dt1[, .(date, department, Number_of_Cases_Screened_for_MDR_act)]
# dt_final = merge(dt_final, dt1, by=c('date', 'department'), all=T)

#------------------------------------------------
# For model version 4, apply a lead of 6 months to all of the treatment success rate variables. 
# txSuccessVars is defined in 'set_up_r.r'. 
tx_success = dt1[, c(txSuccessVars, 'date', 'department'), with=F]
not_tx_success = dt1[, -txSuccessVars, with=F]

#Apply 6-month lead (data is at quarter-level)
tx_success[, date:=date+0.5]

#Merge back together. 
dt1 = merge(tx_success, not_tx_success, by=c('date', 'department'), all=T)

saveRDS(dt1, outputFile2c)
archive(outputFile2c)
# --------------------------------

print("Step 2c: Prep outcomes impacts completed successfully.")
