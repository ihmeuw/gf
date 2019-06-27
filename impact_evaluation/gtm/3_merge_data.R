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

resource_tracking_rect = merge(frame, resource_tracking, by='date', all=TRUE) #Do I need to divide resource tracking dollars here? 
setDT(resource_tracking_rect)
stopifnot(nrow(resource_tracking_rect[is.na(date) | is.na(department), ])==0)
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
redistribution_mat = data.table(input_var = names(resource_tracking)[!names(resource_tracking)%in%c('date','department')])
redistribution_mat[grep("ALL", input_var), combo_input:=TRUE]
redistribution_mat[combo_input==TRUE, code:=substr(input_var, nchar(input_var)-5, nchar(input_var))]
redistribution_mat[is.na(combo_input), code:=substr(input_var, nchar(input_var)-3, nchar(input_var))]
	
# list corresponding variables to define distribution proportions
names(indicatorMap) = tolower(names(indicatorMap))
redistribution_mat = merge(redistribution_mat, indicatorMap[, .(indicator, type, code)], by='code', allow.cartesian=T)

# Fix names where they match with indicator map 
redistribution_mat[, indicator:=gsub(" ", "_", indicator)]
redistribution_mat[, indicator:=gsub("/", "_", indicator)]
redistribution_mat[, indicator:=paste0(indicator, "_", substr(type, 1, 3))]

output_vars = names(outputs_activities)[!names(outputs_activities)%in%c('department', 'date')]
stopifnot(output_vars%in%redistribution_mat$indicator) #Are you going to catch everything with the matrix above? 

#Once you've done the check above, go ahead and subset the matrix to only the indicators you have in the data. 
redistribution_mat = redistribution_mat[indicator%in%output_vars]

# loop over financial variables and redistribute subnationally
for(i in 1:nrow(redistribution_mat)) {
  print(i)
	v = redistribution_mat[i, input_var]
	a = redistribution_mat[i, indicator]

	# disallow zeroes
	min = min(merge_file[[a]], na.rm=TRUE)
	merge_file[, mean:=mean(get(a), na.rm=TRUE), by=department]
	merge_file[is.na(mean), mean:=min]
	
	# distribute proportionally
	merge_file[, tmp:=get(a)+min]
	merge_file[is.na(tmp), tmp:=mean]
	merge_file[, prop:=tmp/sum(tmp), by=date]
	merge_file[, (v):=get(v)*prop]
	
	# test that it worked
	orig = sum(resource_tracking[[v]], na.rm=TRUE)
	new = sum(merge_file[[v]], na.rm=TRUE)
	# if (abs(orig-new)>.1) stop(i) 
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
