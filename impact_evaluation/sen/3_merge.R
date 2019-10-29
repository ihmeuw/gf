# ----------------------------------------------------------
# AUTHOR: Francisco Rios Casas
# PURPOSE: 3. Merge prepped data for Sen TB Model
# DATE: Auguts 15, 2019
# INSTRUCTIONS: The current working directory should be the root of this repo (set manually by user)
# ----------------------------------------------------------

source('./impact_evaluation/sen/set_up_r.r')

# ----------------------------------------------------------
# Load data

# Read in the previously saved files for resource tracking in 2b
resource_tracking <- readRDS(outputFile2b)

# read in the previously saved files for outputs_outcomes in 2f
outputs_outcomes_1 <- readRDS(outputFile2f) # this datatable contains all of the data
outputs_outcomes_2 <- readRDS(outputFile2g) # this datatable just contains one variable

outputs_outcomes <- merge(outputs_outcomes_1, outputs_outcomes_2, by=c('region', 'date'), all.x=TRUE)

#first rectangualrize the data
 hzFrame = unique(outputs_outcomes[, c('region')])
 i=1
 for(d in unique(resource_tracking$date)) {
   hzFrame[, date:=d]
   if(i==1) frame = copy(hzFrame)
   if(i>1) frame = rbind(frame, hzFrame)
   i=i+1
 }
 
outcome_data = merge(frame, outputs_outcomes, by=c('region', 'date'), allow.cartesian=TRUE, all=TRUE)
 
# ----------------------------------------------------------------------
# Merge rectangularized  outputs/activites data and resource tracking 
merge_file <- merge(resource_tracking, outcome_data, by=c('date'), all=TRUE)

# --------------------------------------------------------------------------
# Distribute inputs by health zone proportionally to activities

# list variables that need redistribution
# note: these vectors must be the same length and order matters
inVars = c('exp_T2',
           'exp_T1_5',
           'exp_T1_1',
           'other_dah_T1_1',
           'exp_T3',
           'other_dah_T3')

# list corresponding variables to define distribution proportions
actVars = c('tb_vih',
            'value_com_act',
            'value_screen_act',
            'value_screen_act',
            'patients_prop_genexpert',
            'patients_prop_genexpert')

# create combined variables for redistribution where necessary
merge_file[, value_com_act:=com_mobsoc + com_cause + com_radio + com_vad_touss]
merge_file[, value_screen_act:=tot_genexpert + perf_lab + tb_cas_id]

# loop over financial variables and redistribute subnationally
for(i in seq_along(inVars)) {
  v = inVars[i]
  a = actVars[i]
  
  # disallow zeroes
  min = min(merge_file[get(a)>0][[a]], na.rm=TRUE)
  merge_file[, mean:=mean(get(a), na.rm=TRUE), by=region]
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
merge_file = merge_file[, -c('mean','tmp','prop','value_com_act', 'value_screen_act')]
# need to add merge variables back in

# save
saveRDS(merge_file, outputFile3)

# save a time-stamped version for reproducibility
archive(outputFile3)
# ----------------------------------------------------------
