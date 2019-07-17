# ---------------------------------------------------------
# AUTHOR: Audrey Batzel, Emily Linebarger and David Phillips
# PURPOSE: Merge resource tracking, activity, and outputs data. 
# DATE: Last updated January 2019. 
# INSTRUCTIONS: The current working directory should be the root of this repo (set manually by user)
# ----------------------------------------------------------

# ----------------------------------------------------------
# Load data

drc = readRDS("J:/Project/Evaluation/GF/impact_evaluation/cod/prepped_data/inputs_outputs.RDS") #For comparison

# Read in the previously saved files for resource tracking in 2a
resource_tracking <- readRDS(outputFile2a)
resource_tracking[, year:=floor(date)]
byVars = names(resource_tracking)[!names(resource_tracking)%in%c('date', 'year')]

# Read in the previously saved file for outputs/activities in 2b
outputs_activities <- readRDS(outputFile2b)
#Remove errors in names for outputs_activities here - but these need to be addressed earlier! 
names(outputs_activities) <- gsub("_value", "", names(outputs_activities))
# ----------------------------------------------------------


# -----------------------------------------------------------------------------------------------------
# Prep/merge data

# rectangularize before merge
departments = unique(outputs_activities$department)
departments = departments[!is.na(departments)]
frame = expand.grid(date = seq(1990, 2018, by=1), department = departments)

#Inputs 
resource_tracking_rect = merge(frame, resource_tracking, by='date') #Do I need to divide resource tracking dollars here? 
setDT(resource_tracking_rect)
stopifnot(nrow(resource_tracking_rect[is.na(date) | is.na(department), ])==0)

#Outputs/Activities
outputs_activities_rect = merge(frame, outputs_activities, by=c('department','date'), all=TRUE)
setDT(outputs_activities_rect)
stopifnot(nrow(outputs_activities_rect[is.na(date) | is.na(department)])==0)

# Merge rectangularized resource tracking and outputs/activites data 
merge_file <- merge(resource_tracking_rect, outputs_activities_rect, by=c('department','date'), all=TRUE)
setDT(merge_file)

#Check for uniqueness and NAs here. 
# -----------------------------------------------------------------------------------------------------


# --------------------------------------------------------------------------
# Distribute inputs by department proportionally to activities

# list variables that need redistribution
redistribution_mat = data.table(input_var = names(resource_tracking)[!names(resource_tracking)%in%c('date','department', 'year')])
redistribution_mat[grep("ALL", input_var), combo_input:=TRUE]
redistribution_mat[combo_input==TRUE, code:=substr(input_var, nchar(input_var)-5, nchar(input_var))]
redistribution_mat[is.na(combo_input), code:=substr(input_var, nchar(input_var)-3, nchar(input_var))]
	
# list corresponding variables to define distribution proportions
names(indicatorMap) = tolower(names(indicatorMap))
redistribution_mat = merge(redistribution_mat, indicatorMap[, .(indicator, type, code)], by='code', allow.cartesian=T, all=T)

# Fix names where they match with indicator map 
redistribution_mat[, indicator:=gsub(" ", "_", indicator)]
redistribution_mat[, indicator:=gsub("/", "_", indicator)]
redistribution_mat[, indicator:=paste0(indicator, "_", substr(type, 1, 3))]

output_vars = names(outputs_activities)[!names(outputs_activities)%in%c('department', 'date')]
stopifnot(output_vars%in%redistribution_mat$indicator) #Are you going to catch everything with the matrix above? 

#Once you've done the check above, go ahead and subset the matrix to only the indicators you have in the data. 
redistribution_mat = redistribution_mat[indicator%in%output_vars]

#Now, make sure that there is only one indicator for each input variable
unique(redistribution_mat[, .(code, indicator)]) #find the number of indicators mapping to each code

#For each row in codes_vars, create a 'total variable' which is the sum of all the indicators that map to that code
merge_file[, total_R1_ALL:= Total_Drugs_Distributed_act + Isoniazid_Distributed_act + Second_Line_Drugs_Distributed_act]
redistribution_mat[code=="R1_ALL", redist_var:="total_R1_ALL"]

merge_file[, total_R2_ALL:= Cases_Notified_out + MDR_Cases_Notified_out]
redistribution_mat[code=="R2_ALL", redist_var:="total_R2_ALL"]

merge_file[, total_T1_1:= Cases_Notified_out + Additional_Cases_Detected_via_ACF_out + Cases_Started_on_Treatment_out]
redistribution_mat[code=="T1_1", redist_var:="total_T1_1"]

merge_file[, total_T1_2:= Total_Drugs_Distributed_act + Isoniazid_Distributed_act + Second_Line_Drugs_Distributed_act + Cases_Started_on_Treatment_out]
redistribution_mat[code=="T1_2", redist_var:="total_T1_2"]

merge_file[, total_T1_6:=Cases_Notified_in_Prisons_out + Cases_Started_on_Treatment_in_Prisons_out]
redistribution_mat[code=="T1_6", redist_var:="total_T1_6"]

merge_file[, total_T2_ALL:=PLHIV_Screened_for_TB_act + TB_Patients_Tested_for_HIV_act + HIV_TB_Cases_Notified_out]
redistribution_mat[code=="T2_ALL", redist_var:="total_T2_ALL"]

merge_file[, total_T3_1:=Number_of_Cases_Screened_for_MDR_act + MDR_Cases_Notified_out + MDR_Cases_Started_Treatment_out]
redistribution_mat[code=="T3_1", redist_var:="total_T3_1"]

#These few codes don't have duplicate activity/output indicators!
redistribution_mat[code=="T1_7" | code=="T2_1" | code=="T3_2", redist_var:=indicator]

stopifnot(nrow(redistribution_mat[is.na(redist_var)])==0)

redistribution_mat = unique(redistribution_mat[, .(code, input_var, redist_var)])

#Make a variable for the number of departments for equal distribution if needed. 
n_depts = length(departments)
equal_division = 1/n_depts 
# loop over financial variables and redistribute subnationally
for(i in 1:nrow(redistribution_mat)) {
  print(i)
	v = redistribution_mat[i, input_var]
	a = redistribution_mat[i, redist_var] #Changed from 'indicator' in DRC code EL 7.8.19

	# disallow zeroes
	merge_file[get(a)>0, min:=min(get(a), na.rm=TRUE)]
	merge_file[, mean:=mean(get(a), na.rm=TRUE), by=department]
	merge_file[is.na(min), min:=0]
	merge_file[is.na(mean), mean:=min]
	
	# set up redistribution coefficients
	merge_file[, tmp:=get(a)+min]
	merge_file[is.na(tmp), tmp:=mean]
	merge_file[, prop:=tmp/sum(tmp), by=date]
	
	#If there is no data for 'a' for a given year, divide the data equally between departments. 
	a_zeros = merge_file[, .(total=sum(get(a))), by='date']
	a_zeros = a_zeros[total==0]
	if (nrow(a_zeros)!=0){
	  print("Some input variables are being scaled equally among all departments,\nbecause there is no data for the next activity/output indicator for the following years")
	  print(a)
	  print(unique(a_zeros$date))
	  merge_file[date%in%a_zeros$date, prop:=equal_division]
	}

	#Check to make sure these redistribution variables sum to 1 by date (over all departments)
	check = merge_file[, .(prop=sum(prop, na.rm=T)), by='date']
	stopifnot(unique(check$prop)==1)
	
	#Redistribute
	merge_file[, (v):=get(v)*prop]
	
	# test that it worked using original data
	orig = sum(resource_tracking[[v]], na.rm=TRUE)
	new = sum(merge_file[[v]], na.rm=TRUE)
	if (abs(orig-new)>.1) stop(paste0("Orig:", orig, " New:", new, 
	                                  " are not close for variable ", v, " (index ", i, ")")) 
}	
# --------------------------------------------------------------------------


# ----------------------------------------------------------
# Finish and save 

# drop unnecessary variables
merge_file = merge_file[, -c('mean','tmp','prop')]

# save
saveRDS(merge_file, outputFile3)

# save a time-stamped version for reproducibility
archive(outputFile3)
# ----------------------------------------------------------

print("Step 3: merge data completed successfully.")
