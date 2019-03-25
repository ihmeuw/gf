# ---------------------------------------------------------
# AUTHOR: Audrey Batzel and Emily Linebarger
# PURPOSE: Merge resource tracking, activity, and outputs data. 
# DATE: Last updated January 2019. 
# INSTRUCTIONS: The current working directory should be the root of this repo (set manually by user)
# ----------------------------------------------------------


#Read in the previously saved files for resource tracking in 2b 
resource_tracking <- readRDS(outputFile2a)

#Read in the previously saved file for outputs/activities in 2b
outputs_activities <- readRDS(outputFile2b_wide)
outputs_activities = outputs_activities[!is.na(health_zone)]
    # merge outputs_activities file with drc_mal_map
    # outputs_activities <- merge(drc_mal_map, outputs_activities, by = 'indicator', all = TRUE, allow.cartesian = TRUE)

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
	
#Merge rectangularized resource tracking and outputs/activites data 
merge_file <- merge(resource_tracking_rect, outputs_activities_rect, by=c('dps','health_zone','date'))

# distribute inputs by health zone proportionally to activities
inVars = c('other_dah_M1_1', 'other_dah_M1_2', 'other_dah_M2', 'other_dah_M2_3', 'exp_M1_1', 'exp_M1_2', 'exp_M2', 'exp_M2_1', 'exp_M2_3', 'exp_M2_6', 'exp_M3_1', 'ghe', 'oop')
actVars = c('ITN_received', 'ITN_received', 'total_tx', 'total_tx', 'ITN_received', 'ITN_received', 'total_tx', 'total_tx', 'total_tx', 'total_tx', 'ACT_received', 'total', 'total')
merge_file[, total_tx:=ACT_received+RDT_received]
merge_file[, total:=ACT_received+RDT_received+ITN_received]
for(i in seq_along(inVars)) {
	v = inVars[i]
	a = actVars[i]
	
	# disallow zeroes
	min = min(merge_file[get(a)>0][[a]], na.rm=TRUE)
	merge_file[, mean:=mean(get(a), na.rm=TRUE), by=health_zone]
	
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

# clean up 
merge_file = merge_file[, -c('mean','tmp','prop','total','total_tx')]


# merge_file$loc_name = 'cod'
# merge_file$disease = 'malaria'
# merge_file <- setorderv(merge_file, c('year', 'quarter', 'code', 'indicator', 'indicator_type'))
# merge_file <- merge_file[, .(year, quarter, code, module, intervention, indicator, indicator_type, value, completeness, budget, loc_name, disease, data_source)] #EKL are we missing some variables?
# merge_file <- merge_file[, .(year, quarter, code, module, intervention, indicator, indicator_type, value, completeness, budget, loc_name, disease, other_dah, data_source)]

saveRDS(merge_file, outputFile3)

# save a time-stamped version for reproducibility
date_time = gsub('-|:| ', '_', Sys.time())
outputFile3Archive = gsub('prepped_data/', 'prepped_data/archive/', outputFile3)
outputFile3Archive = gsub('.rdata', paste0('_', date_time, '.rdata'), outputFile3Archive)
file.copy(outputFile3, outputFile3Archive)
