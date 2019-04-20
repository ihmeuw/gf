# ------------------------------------------------
# David Phillips
# 
# 1/18/2019
# This runs the SEM dose-response model
# qsub -l archive=TRUE -cwd -N ie_script_5e -l fthread=4 -l m_mem_free=4G -q all.q -P ihme_general -e /ihme/scratch/users/davidp6/impact_evaluation/errors_output/ -o /ihme/scratch/users/davidp6/impact_evaluation/errors_output/ ./core/r_shell_blavaan.sh ./impact_evaluation/5e_run_second_half_analysis.r
# ------------------------------------------------

source('./impact_evaluation/drc/set_up_r.r')


# ---------------------------
# Settings

# whether to run in parallel using qsub or mclapply
runAsQsub = TRUE
if(Sys.info()[1]=='Windows') runAsQsub = FALSE

# model version to use
modelVersion = 'drc_malaria_impact4'

# load function that runs a SEM as unrelated regressions
source('./impact_evaluation/_common/run_lavaan_as_glm.r')
# ---------------------------


# ---------------------------
# Load data
set.seed(1)
load(outputFile4b)
# ---------------------------


# ----------------------------------------------
# Define model object
# DECISIONS
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
	file.copy(outputFile4b, outputFile4b_scratch, overwrite=TRUE)
	# store T (length of array)
	hzs = unique(data$health_zone)
	T = length(hzs)
	# submit array job
	system(paste0('qsub -cwd -N ie2_job_array -t 1:', T, 
		' -l fthread=1 -l m_mem_free=2G -q all.q -P ihme_general -e ', 
		clustertmpDireo, ' -o ', clustertmpDireo, 
		' ./core/r_shell_blavaan.sh ./impact_evaluation/drc/5c_run_single_model.r ', 
		modelVersion, ' 2 FALSE'))
	# wait for jobs to finish (2 files per job)
	while(length(list.files(clustertmpDir2, pattern='second_half_summary_'))<(T)) { 
		Sys.sleep(5)
		print(paste(length(list.files(clustertmpDir2, pattern='second_half_summary_')), 'of', T, 'files found...'))
	}
	# collect output
	print('Collecting output...')
	for(i in seq(T)) { 
		summary = readRDS(paste0(clustertmpDir2, 'second_half_summary_', i, '.rds'))
		if (i==1) summaries = copy(summary)
		if (i>1) summaries = rbind(summaries, summary)
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
print(paste('Saving', outputFile5b))
save(list=c('data','model','summaries','means','scaling_factors','urFit'), file=outputFile5b)

# save full output for archiving
outputFile5b_big = gsub('.rdata','_all_semFits.rdata',outputFile5b)
print(paste('Saving', outputFile5b_big))
semFits = lapply(seq(T), function(i) {
	suppressWarnings(readRDS(paste0(clustertmpDir2, 'second_half_semFit_', i, '.rds')))
})
save(list=c('data','model','semFits','summaries','means','scaling_factors','urFit'), file=outputFile5b_big)

# save a time-stamped version for reproducibility
print('Archiving files...')
archive(outputFile5b, 'model_runs')
archive(outputFile5b_big, 'model_runs')

# clean up in case jags saved some output
if(dir.exists('./lavExport/')) unlink('./lavExport', recursive=TRUE)

# clean up qsub files
print(paste('Cleaning up cluster temp files...'))
if (runAsQsub==TRUE) { 
	system(paste0('rm ', clustertmpDireo, '/ie2_job_array*'))
	system(paste0('rm ', clustertmpDir1	, '/second_half_*'))
	system(paste0('rm ', clustertmpDir2	, '/second_half_*'))
}
# ------------------------------------------------------------------
