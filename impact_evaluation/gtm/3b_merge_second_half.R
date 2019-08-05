# ---------------------------------------------------------
# AUTHOR: Audrey Batzel, Emily Linebarger and David Phillips
# PURPOSE: Merge outputs, outcomes, and impact data
# DATE: Last updated July 2019. 
# INSTRUCTIONS: The current working directory should be the root of this repo (set manually by user)
# ----------------------------------------------------------

# ----------------------------------------------------------
# Load data

drc = readRDS("J:/Project/Evaluation/GF/impact_evaluation/cod/prepped_data/inputs_outputs.RDS") #For comparison

#Read in the activity-output and outcomes-impact file. 
act_out = readRDS(outputFile2b)
out_imp = readRDS(outputFile2c)

#Only keep outputs from the first dataset
output_vars = names(act_out)[grep("_out", names(act_out))]
output_vars = c('department', 'date', output_vars)

act_out = act_out[, output_vars, with=FALSE]

#Make it easier to tell which are output vs. outcome variables 
names(act_out) = gsub("_out", "_outp", names(act_out))
names(out_imp) = gsub("_out", "_outc", names(out_imp))

# ----------------------------------------------------------


# -----------------------------------------------------------------------------------------------------
# Prep/merge data

#Are there any department/date differences between the two datasets? 
act_out_dates = unique(act_out$date)
out_imp_dates = unique(out_imp$date)

act_out_dates[!act_out_dates%in%out_imp_dates]
out_imp_dates[!out_imp_dates%in%act_out_dates] #2010 and 2011


act_out_departments = unique(act_out$department)
out_imp_departments = unique(out_imp$department)

act_out_departments[!act_out_departments%in%out_imp_departments] #None
out_imp_departments[!out_imp_departments%in%act_out_departments] #None

dt = merge(act_out, out_imp, by=c('department', 'date'), all=T)
#Check for uniqueness and NAs here. 
# -----------------------------------------------------------------------------------------------------



# ----------------------------------------------------------
# Finish and save 

#Restrict years to 2009 on (EL 8/5/2019) 
dt = dt[date>=2009]

# save
saveRDS(dt, outputFile3a)

# save a time-stamped version for reproducibility
archive(outputFile3a)
# ----------------------------------------------------------

print("Step 3b: merge second half data completed successfully.")
