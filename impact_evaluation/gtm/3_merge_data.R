# ---------------------------------------------------------
# AUTHOR: Audrey Batzel, Emily Linebarger and David Phillips
# PURPOSE: Merge resource tracking, activity, and outputs data. 
# DATE: Last updated January 2019. 
# INSTRUCTIONS: The current working directory should be the root of this repo (set manually by user)
# ----------------------------------------------------------

source('./impact_evaluation/drc/set_up_r.r')

# ----------------------------------------------------------
# Load data

# Read in the previously saved files for resource tracking in 2a
resource_tracking <- readRDS(outputFile2a)

# Read in the previously saved file for outputs/activities in 2b
outputs_activities <- readRDS(outputFile2b_wide)
outputs_activities = outputs_activities[!is.na(health_zone)]
# ----------------------------------------------------------


# -----------------------------------------------------------------------------------------------------
# Prep/merge data

# rectangularize before merge
hzFrame = unique(outputs_activities[, c('dps','health_zone')])
i=1
for(d in unique(resource_tracking$date)) {
	hzFrame[, date:=d]
	if(i==1) frame = copy(hzFrame)
	if(i>1) frame = rbind(frame, hzFrame)
	i=i+1
}
resource_tracking_rect = merge(frame, resource_tracking, by='date', all.x=TRUE)
outputs_activities_rect = merge(frame, outputs_activities, by=c('dps','health_zone','date'), all.x=TRUE)

# Merge rectangularized resource tracking and outputs/activites data 
merge_file <- merge(resource_tracking_rect, outputs_activities_rect, by=c('dps','health_zone','date'))
# -----------------------------------------------------------------------------------------------------
	

# --------------------------------------------------------------------------
# Distribute inputs by health zone proportionally to activities

# list variables that need redistribution
# note: these vectors must be the same length and order matters
inVars = c('other_dah_M1_1', 'other_dah_M1_2', 'other_dah_M2', 'other_dah_M2_3', 'exp_M1_1', 
	'exp_M1_2', 'exp_M2', 'exp_M2_1', 'exp_M2_3', 'exp_M2_6', 'exp_M3_1', 'ghe', 'oop')
	
# list corresponding variables to define distribution proportions
actVars = c('value_ITN_received', 'value_ITN_received', 'value_total_tx', 'value_total_tx', 
	'value_ITN_received', 'value_ITN_received', 'value_total_tx', 'value_total_tx', 
	'value_total_tx', 'value_total_tx', 'value_ACT_received', 'value_total', 'value_total')
	
# create combined variables for redistribution where necessary
merge_file[, value_total_tx:= value_ACT_received + value_RDT_received]
merge_file[, value_total:= value_ACT_received + value_RDT_received + value_ITN_received]

# loop over financial variables and redistribute subnationally
for(i in seq_along(inVars)) {
	v = inVars[i]
	a = actVars[i]
	
	# disallow zeroes
	min = min(merge_file[get(a)>0][[a]], na.rm=TRUE)
	merge_file[, mean:=mean(get(a), na.rm=TRUE), by=health_zone]
	merge_file[is.na(mean), mean:=min]
	
	# distribute proportionally
	merge_file[, tmp:=get(a)+min]
	merge_file[is.na(tmp), tmp:=mean]
	merge_file[, prop:=tmp/sum(tmp), by=date]
	merge_file[, (v):=get(v)*prop]
	
	# test that it worked
	orig = sum(resource_tracking[[v]], na.rm=TRUE)
	new = sum(merge_file[[v]], na.rm=TRUE)
	if (abs(orig-new)>.1) stop(i) 
}	
# --------------------------------------------------------------------------


# ----------------------------------------------------------
# Finish and save 

# drop unnecessary variables
merge_file = merge_file[, -c('mean','tmp','prop','value_total','value_total_tx')]

# save
saveRDS(merge_file, outputFile3)

# save a time-stamped version for reproducibility
archive(outputFile3)
# ----------------------------------------------------------
