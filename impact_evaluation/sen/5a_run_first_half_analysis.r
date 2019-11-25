# ------------------------------------------------
# Francisco Rios, adapted from code by David Phillips and Emily Linebarger
# 
# 08/23/2019
# This runs the SEM dose-response model (David presets)
# The current working directory should be the root of this repository
# qsub -l archive=TRUE -cwd -N ie_script_5a -l fthread=12 -l m_mem_free=12G -q all.q -P proj_pce -e /ihme/scratch/users/davidp6/impact_evaluation/errors_output/ -o /ihme/scratch/users/davidp6/impact_evaluation/errors_output/ ./core/r_shell_blavaan.sh ./impact_evaluation/sen/5a_run_first_half_analysis.r

# This runs the SEM dose-response model (Emily presets) 
# qsub -l archive=TRUE -cwd -N ie_script_5a -l fthread=12 -l m_mem_free=6G -q all.q 
# -P proj_pce -e /ihme/scratch/users/elineb/impact_evaluation/errors_output/ 
# -o /ihme/scratch/users/elineb/impact_evaluation/errors_output/ ./core/r_shell_blavaan.sh ./impact_evaluation/sen/5a_run_first_half_analysis.r

# This runs the SEM dose-response model (Francisco presets) 
# qsub -l archive=TRUE -cwd -N ie_script_5a -l fthread=12 -l m_mem_free=6G -q all.q 
# -P proj_pce -e /ihme/scratch/users/frc2/impact_evaluation/errors_output/ 
# -o /ihme/scratch/users/elineb/impact_evaluation/errors_output/ ./core/r_shell_blavaan.sh ./impact_evaluation/sen/5a_run_first_half_analysis.r

# qsub -l archive=TRUE -cwd -N ie_script_5a -l fthread=12 -l m_mem_free=12G -q all.q -P ihme_general -e /ihme/homes/users/frc2/impact_evaluation/errors_output/ -o /ihme/homes/users/frc2/impact_evaluation/errors_output/ ./core/r_shell_blavaan.sh ./impact_evaluation/sen/5a_run_first_half_analysis.r

# ------------------------------------------------

source('./impact_evaluation/sen/set_up_r.r')

# ---------------------------
# Settings

# whether to run each department in parallel or not
runInParallel = FALSE

# ---------------------------


# ---------------------------
# Load data
set.seed(1)
load(outputFile4a)

# store T (number of jobs)
hzs = unique(data$region)
T = length(hzs)
# ---------------------------


# ----------------------------------------------
# Define model object
# DECISIONS
# including date as a control variable in linkage 1 regressions because otherwise all RT variables are positively correlated (when GF and other should be negative)
source(paste0('./impact_evaluation/sen/models/', modelVersion1, '.r'))

# reduce the data down to only necessary variables
parsedModel = lavParseModelString(model)
modelVars = unique(c(parsedModel$lhs, parsedModel$rhs))
modelVars = c('region','date',modelVars)
data = data[, unique(modelVars), with=FALSE]
# ----------------------------------------------


# --------------------------------------------------------------
# Run model with each region in parallel (if specified by runInParallel)
if (runInParallel==TRUE) {

	# save copy of input file for jobs
	file.copy(outputFile4a, outputFile4a_scratch, overwrite=TRUE)

	# store cluster command to submit array of jobs
	qsubCommand = paste0('qsub -cwd -N ie1_job_array -t 1:', T, 
		' -l fthread=1 -l m_mem_free=2G -q long.q -P proj_pce -e ', 
		clustertmpDireo, ' -o ', clustertmpDireo, 
		' ./core/r_shell_blavaan.sh ./impact_evaluation/sen/5c_run_single_model.r ', 
		modelVersion1, ' 1 FALSE FALSE') #There is a final argument here that doesn't do anything - it's a hack to get around UNIX vs. DOS EOL characters. EL 7.16.19
			
	# submit array job to the cluster if we're running this in parallel
	system(qsubCommand)

	# # wait for jobs to finish (2 files per job)
	# while(length(list.files(clustertmpDir2, pattern='first_half_summary_'))<(T)) { 
	# 	Sys.sleep(5)
	# 	print(paste(length(list.files(clustertmpDir2, pattern='first_half_summary_')), 'of', T, 'files found...'))
	# }
	# 
	#While trying to get GLM model running - changing to look for GLM files. EL 8/15/19
	while(length(list.files(clustertmpDir2, pattern='first_half_urFit_'))<(T)) { 
	  Sys.sleep(5)
	  print(paste(length(list.files(clustertmpDir2, pattern='first_half_urFit_')), 'of', T, 'files found...'))
	}
	print("All GLM files found - all departments were run successfully.")
}
# --------------------------------------------------------------


# --------------------------------------------------------------
# Run model with each department serially (if specified by runInParallel)
if (runInParallel==FALSE) {

	# reassign the temporary output location
	clustertmpDir2 = tempIeDir

	# store arguments needed to run 5c_run_single_model.r
	# arguments 1-4 are meaningless system variables,
	# 5 is the model object name,
	# 6 is whether to run the first or second half,
	# 7 is whether to do a test run
	args = c('a', 'b', 'c', 'd', modelVersion1, '1', 'FALSE')
	
	# run each iteration sequentially
	for(task_id in seq(T)) {
		source('./impact_evaluation/sen/5c_run_single_model.r')
	}
}
# --------------------------------------------------------------


# --------------------------------------------------------------
# Organize results

# collect output (summary and urFit)
print('Collecting output...')
for(i in seq(T)) {
	# summary = readRDS(paste0(clustertmpDir2, 'first_half_summary_', i, '.rds'))
	urFit = readRDS(paste0(clustertmpDir2, 'first_half_urFit_', i, '.rds'))
	# if (i==1) summaries = copy(summary)
	# if (i>1) summaries = rbind(summaries, summary)
	if (i==1) urFits = copy(urFit)
	if (i>1) urFits = rbind(urFits, urFit)
}

# compute averages (approximation of standard error, would be better as Monte Carlo simulation)
# paramVars = c('est.std','est','se_ratio.std', 'se_ratio', 'se.std', 'se')
# summaries[, se_ratio.std:=se.std/est.std]
# summaries[, se_ratio:=se/est]
# means = summaries[, lapply(.SD, mean), .SDcols=paramVars, by=c('lhs','op','rhs')]
# means[se.std>abs(se_ratio.std*est.std), se.std:=abs(se_ratio.std*est.std)]
# means[se>abs(se_ratio*est), se:=abs(se_ratio*est)]
# --------------------------------------------------------------


# ------------------------------------------------------------------
# Save model output and clean up

# save all sem fits just in case they're needed
print(paste('Saving', outputFile5a))
save(list=c('data','model','summaries','means','urFits'), file=outputFile5a)
save(list=c('data','model','urFits'), file=outputFile5a)

# save full output for archiving
# outputFile5a_big = gsub('.rdata','_all_semFits.rdata',outputFile5a)
# print(paste('Saving', outputFile5a_big))
# semFits = lapply(seq(T), function(i) {
# 	suppressWarnings(readRDS(paste0(clustertmpDir2, 'first_half_semFit_', i, '.rds')))
# })
# save(list=c('data','model','semFits','summaries','means','urFits'), file=outputFile5a_big)
#save(list=c('data','model','urFits'), file=outputFile5a_big)

# save a time-stamped version for reproducibility
print('Archiving files...')
archive(outputFile5a, 'model_runs')
#archive(outputFile5a_big, 'model_runs')

# clean up in case jags saved some output
if(dir.exists('./lavExport/')) unlink('./lavExport', recursive=TRUE)

# clean up qsub files
print(paste('Cleaning up cluster temp files...'))
system(paste0('rm ', clustertmpDireo, '/ie1_job_array*'))
system(paste0('rm ', clustertmpDir1	, '/first_half_*'))
system(paste0('rm ', clustertmpDir2	, '/first_half_*'))
# ------------------------------------------------------------------
