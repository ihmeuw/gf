# ------------------------------------------------
# David Phillips
# 
# 1/18/2019
# This runs the SEM dose-response model
# qsub -l archive=TRUE -cwd -N ie_script_5a -l fthread=12 -l m_mem_free=12G -q all.q -P proj_pce -e /ihme/scratch/users/davidp6/impact_evaluation/errors_output/ -o /ihme/scratch/users/davidp6/impact_evaluation/errors_output/ ./core/r_shell_blavaan.sh ./impact_evaluation/drc/5a_run_first_half_analysis.r
# ------------------------------------------------

source('./impact_evaluation/drc/set_up_r.r')

# ---------------------------
# Settings

# whether to run in parallel using qsub or mclapply
runAsQsub = TRUE
if(Sys.info()[1]=='Windows') runAsQsub = FALSE

# model version to use
modelVersion = 'drc_malaria6'

# load function that runs a SEM as unrelated regressions
source('./impact_evaluation/_common/run_lavaan_as_glm.r')
# ---------------------------


# ---------------------------
# Load data
set.seed(1)
load(outputFile4a)
# ---------------------------


# ----------------------------------------------
# Define model object
# DECISIONS
# including date as a control variable in linkage 1 regressions because otherwise all RT variables are positively correlated (when GF and other should be negative)
source(paste0('./impact_evaluation/drc/models/', modelVersion, '.r'))

# reduce the data down to only necessary variables
parsedModel = lavParseModelString(model)
modelVars = unique(c(parsedModel$lhs, parsedModel$rhs))
modelVars = c('orig_health_zone','health_zone','date',modelVars)
data = data[, modelVars, with=FALSE]
# ----------------------------------------------


# ------------------------------------------------------
# Run series of unrelated linear models for comparison
urFit = lavaanUR(model, data)
# ------------------------------------------------------


# --------------------------------------------------------------
# Run model
if ('semFit' %in% ls()) rm('semFit')

# no health zone fixed effects (warning: slow)
# semFit = bsem(model, data, adapt=5000, burnin=10000, sample=1000, bcontrol=list(thin=3))

# run locally if specified
if(runAsQsub==FALSE) { 
	# run all sems
	semFits = mclapply(unique(data$health_zone), function(h) { 
		print(h)
		suppressWarnings(
			bsem(model, data[health_zone==h], adapt=5000, burnin=10000, sample=1000, bcontrol=list(thin=3))
		)
		
	}, mc.cores=ifelse(Sys.info()[1]=='Windows',1,24))
		
	# store summaries of each sem
	print('Summarizing results...')
	for(i in seq(length(semFits))) { 
		tmp = data.table(standardizedSolution(semFits[[i]], se=TRUE))
		tmp[, health_zone:=unique(data$health_zone)[i]]
		if (i==1) summaries = copy(tmp)
		if (i>1) summaries = rbind(summaries, tmp)
	}
}

# run fully in parallel if specified
if (runAsQsub==TRUE) { 
	# save copy of input file for jobs
	file.copy(outputFile4a, outputFile4a_scratch, overwrite=TRUE)
	# store T (length of array)
	hzs = unique(data$health_zone)
	T = length(hzs)
	# submit array job
	system(paste0('qsub -cwd -N ie1_job_array -t 1:', T, 
		' -l fthread=1 -l m_mem_free=2G -q all.q -P proj_pce -e ', 
		clustertmpDireo, ' -o ', clustertmpDireo, 
		' ./core/r_shell_blavaan.sh ./impact_evaluation/drc/5c_run_single_model.r ', 
		modelVersion, ' 1 FALSE'))
	# wait for jobs to finish (2 files per job)
	while(length(list.files(clustertmpDir2, pattern='first_half_summary_'))<(T)) { 
		Sys.sleep(5)
		print(paste(length(list.files(clustertmpDir2, pattern='first_half_summary_')), 'of', T, 'files found...'))
	}
	# collect output
	print('Collecting output...')
	summaries = NULL
	for(i in seq(T)) { 
		file = paste0(clustertmpDir2, 'first_half_summary_', i, '.rds')
		if (!file.exists(file)) next 
		summary = readRDS(file)
		if (is.null(summaries)) summaries = copy(summary)
		if (!is.null(summaries)) summaries = rbind(summaries, summary)
	}
}

# compute averages (approximation of standard error, would be better as Monte Carlo simulation)
paramVars = c('est.std','est','se_ratio.std', 'se_ratio', 'se.std', 'se')
summaries[, se_ratio.std:=se.std/est.std]
summaries[, se_ratio:=se/est]
means = summaries[, lapply(.SD, mean), .SDcols=paramVars, by=c('lhs','op','rhs')]
means[se.std>abs(se_ratio.std*est.std), se.std:=abs(se_ratio.std*est.std)]
means[se>abs(se_ratio*est), se:=abs(se_ratio*est)]
# --------------------------------------------------------------


# ------------------------------------------------------------------
# Save model output and clean up

# save all sem fits just in case they're needed
print(paste('Saving', outputFile5a))
save(list=c('data','untransformed','model','summaries','means','urFit'), file=outputFile5a)

# save full output for archiving
outputFile5a_big = gsub('.rdata','_all_semFits.rdata',outputFile5a)
print(paste('Saving', outputFile5a_big))
semFits = lapply(seq(T), function(i) {
	suppressWarnings(readRDS(paste0(clustertmpDir2, 'first_half_semFit_', i, '.rds')))
})
save(list=c('data','untransformed','model','semFits','summaries','means','urFit'), file=outputFile5a_big)

# save a time-stamped version for reproducibility
print('Archiving files...')
archive(outputFile5a, 'model_runs')
archive(outputFile5a_big, 'model_runs')

# clean up in case jags saved some output
if(dir.exists('./lavExport/')) unlink('./lavExport', recursive=TRUE)

# clean up qsub files
print(paste('Cleaning up cluster temp files...'))
if (runAsQsub==TRUE) { 
	system(paste0('rm ', clustertmpDireo, '/ie1_job_array*'))
	system(paste0('rm ', clustertmpDir1	, '/first_half_*'))
	system(paste0('rm ', clustertmpDir2	, '/first_half_*'))
}
# ------------------------------------------------------------------
