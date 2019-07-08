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

#Read in data 
outcomes = fread(outcomeFile)
impacts = fread(impactFile)


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

o_dates[!o_dates%in%i_dates] #Nothing. 
i_dates[!i_dates%in%o_dates] #Nothing. 

#Departments
o_depts = unique(outcomes$department)
i_depts = unique(impacts$department)

o_depts[!o_depts%in%i_depts] #None. 
i_depts[!i_depts%in%o_depts] #None. 

#Municipalities
o_mun = unique(outcomes$municipality)
i_mun = unique(impacts$municipality)

o_mun[!o_mun%in%i_mun] #None.
i_mun[!i_mun%in%o_mun] #None.

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
#------------------------
# outcomes
#------------------------
#Run a check to decide which variables are already at the dept. level and which need to be summed. 
vars = names(outcomes)[!names(outcomes)%in%c('date', 'department', 'municipality')]
dep_vars = c()
mun_vars = c()
for (var in vars){
  dt = unique(outcomes[, .(date, department, var=get(var))])
  dt[duplicated(dt, by=c('date', 'department')), dup:=TRUE]
  if (nrow(dt[dup==TRUE])!=0){
    mun_vars = c(mun_vars, var)
  } else {
    dep_vars = c(dep_vars, var)
  }
}

#Flag cases where variables end in _d but are in the mun-level dataset. 
dept_level_error = mun_vars[grepl("_d", mun_vars)]
if (length(dept_level_error)!=0){
  print("ERROR: Some department-level variables are not uniquely identified by department and date!")
  print(dept_level_error)
}

#Take the average of the department-level variables by date and department. 
dep_level_o = data.table(date=integer(), department=integer())
for (var in dep_vars){
  var_subset = outcomes[, .(var=mean(get(var), na.rm=T)), by=c('date', 'department')]
  names(var_subset)[3] = var
  dep_level_o = merge(dep_level_o, var_subset, by=c('date', 'department'), all=T)
}
#Check for uniqueness. 
dep_level_o[duplicated(dep_level_o, by=c('date', 'department')), dup:=TRUE]
stopifnot(nrow(dep_level_o[dup==TRUE])==0)
dep_level_o$dup<-NULL
names(dep_level_o) = gsub("_m|_d", "", names(dep_level_o))

#Take the sum of the municipality-level variables by date and department. 
mun_level_o = data.table(date=integer(), department=integer())
for (var in mun_vars){
  var_subset = outcomes[, .(var=sum(get(var), na.rm=T)), by=c('date', 'department')]
  names(var_subset)[3] = var
  mun_level_o = merge(mun_level_o, var_subset, by=c('date', 'department'), all=T)
}
#Check for uniqueness. 
mun_level_o[duplicated(mun_level_o, by=c('date', 'department')), dup:=TRUE]
stopifnot(nrow(mun_level_o[dup==TRUE])==0)
mun_level_o$dup<-NULL
names(mun_level_o) = gsub("_m|_d", "", names(mun_level_o))


outcomes1 = merge(dep_level_o, mun_level_o, by=c('date', 'department'), all=T)
#Make sure you've accounted for all columns except municipality. 
stopifnot(ncol(outcomes1) == ncol(outcomes)-1)
new_names = names(outcomes1)[3:ncol(outcomes1)]  #Resest the names so they're distinguishable from activity variables. 
new_names = paste0(new_names, "_out")
names(outcomes1)[3:ncol(outcomes1)] <- new_names

#------------------------
# impacts
#------------------------
#Run a check to decide which variables are already at the dept. level and which need to be summed. 
vars = names(impacts)[!names(impacts)%in%c('date', 'department', 'municipality')]
dep_vars = c()
mun_vars = c()
for (var in vars){
  dt = unique(impacts[, .(date, department, var=get(var))])
  dt[duplicated(dt, by=c('date', 'department')), dup:=TRUE]
  if (nrow(dt[dup==TRUE])!=0){
    mun_vars = c(mun_vars, var)
  } else {
    dep_vars = c(dep_vars, var)
  }
}

#Flag cases where variables end in _d but are in the mun-level dataset. 
dept_level_error = mun_vars[grepl("_d", mun_vars)]
if (length(dept_level_error)!=0){
  print("ERROR: Some department-level variables are not uniquely identified by department and date!")
  print(dept_level_error)
}

#Take the average of the department-level variables by date and department. 
dep_level_i = data.table(date=integer(), department=integer())
for (var in dep_vars){
  var_subset = impacts[, .(var=mean(get(var), na.rm=T)), by=c('date', 'department')]
  names(var_subset)[3] = var
  dep_level_i = merge(dep_level_i, var_subset, by=c('date', 'department'), all=T)
}
#Check for uniqueness. 
dep_level_i[duplicated(dep_level_i, by=c('date', 'department')), dup:=TRUE]
stopifnot(nrow(dep_level_i[dup==TRUE])==0)
dep_level_i$dup<-NULL
names(dep_level_i) = gsub("_m|_d", "", names(dep_level_i))

#Take the sum of the municipality-level variables by date and department. 
mun_level_i = data.table(date=integer(), department=integer())
for (var in mun_vars){
  var_subset = impacts[, .(var=sum(get(var), na.rm=T)), by=c('date', 'department')]
  names(var_subset)[3] = var
  mun_level_i = merge(mun_level_i, var_subset, by=c('date', 'department'), all=T)
}
#Check for uniqueness. 
mun_level_i[duplicated(mun_level_i, by=c('date', 'department')), dup:=TRUE]
stopifnot(nrow(mun_level_i[dup==TRUE])==0)
mun_level_i$dup<-NULL
names(mun_level_i) = gsub("_m|_d", "", names(mun_level_i))


impacts1 = merge(dep_level_i, mun_level_i, by=c('date', 'department'), all=T)
#Make sure you've accounted for all columns except municipality. 
stopifnot(ncol(impacts1) == ncol(impacts)-1)
new_names = names(impacts1)[3:ncol(impacts1)]  #Resest the names so they're distinguishable from activity variables. 
new_names = paste0(new_names, "_imp")
names(impacts1)[3:ncol(impacts1)] <- new_names
#-----------------------------------------------------
# Check to make sure you're still uniquely identifying data 
#-----------------------------------------------------
outcomes1[duplicated(outcomes1, by=c('department','date')), dup:=TRUE]
if (nrow(outcomes1[dup==TRUE])!=0){
  print(paste0("There are ", nrow(outcomes1[dup==TRUE]), " duplicates in department and date in the outcomes data. Review."))
}

impacts1[duplicated(impacts1, by=c('department', 'date')), dup:=TRUE]
if (nrow(impacts1[dup==TRUE])!=0){
  print(paste0("There are ", nrow(impacts1[dup==TRUE]), " duplicates in department and date in the impacts data. Review."))
}

outcomes1 = outcomes1[, -c('dup')]
impacts1 = impacts1[, -c('dup')]

#-----------------------------------------------------
# Merge data 
#-----------------------------------------------------
dt_final = merge(outcomes1, impacts1, by=c('date', 'department'), all=T) #Save dates and departments from both, in case you have data in one and not the other. 

#Replace NaN and NA with 0 - we can assume these actually mean 0. 
cols = 3:ncol(dt_final) #Just don't do this for date and department, the first two columns. 
for (col in cols){
  dt_final[is.na(dt_final[[col]]), (col):=0]
}

saveRDS(dt_final, outputFile2c)
archive(outputFile2c)
# --------------------------------
