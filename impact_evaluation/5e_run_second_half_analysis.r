# ------------------------------------------------
# David Phillips
# 
# 1/18/2019
# This runs the SEM dose-response model
# qsub -l archive=TRUE -cwd -N ie_script_5e -l fthread=4 -l m_mem_free=4G -q all.q -P ihme_general -e /ihme/scratch/users/davidp6/impact_evaluation/errors_output/ -o /ihme/scratch/users/davidp6/impact_evaluation/errors_output/ ./core/r_shell_blavaan.sh ./impact_evaluation/5e_run_second_half_analysis.r
# ------------------------------------------------

source('./impact_evaluation/_common/set_up_r.r')


# ---------------------------
# Settings

# whether to run in parallel using qsub or mclapply
runAsQsub = TRUE
if(Sys.info()[1]=='Windows') runAsQsub = FALSE
# ---------------------------


# ---------------------------
# Load data
set.seed(1)
load(outputFile5d)
# ---------------------------


# -------------------------
# Run series of unrelated linear models

# linkage 1 regressions
lmFit1 = lm(ITN_rate_cumul ~ ITN, data)
lmFit2 = lm(mildMalariaTreated_rate ~ mildMalariaTreated + RDT_rate, data)
lmFit3 = lm(severeMalariaTreated_rate ~ severeMalariaTreated + RDT_rate, data)
lmFit4 = lm(ACTs_CHWs_rate ~ SSCACT, data)
lmFit5 = lm(SP_rate ~ SP, data)
lmFit6 = lm(RDT_rate ~ RDT, data)

# linkage 2 regressions
lmFit7 = lm(lead_newCasesMalariaMild_rate ~ ITN_rate_cumul + mildMalariaTreated_rate + ACTs_CHWs_rate + SP_rate + date, data)
lmFit8 = lm(lead_newCasesMalariaSevere_rate ~ ITN_rate_cumul + severeMalariaTreated_rate + ACTs_CHWs_rate + SP_rate + date, data)
lmFit9 = lm(lead_case_fatality ~ mildMalariaTreated_rate + severeMalariaTreated_rate + ACTs_CHWs_rate, data)
lmFit10 = lm(lead_malariaDeaths_rate ~ lead_newCasesMalariaMild_rate + lead_newCasesMalariaSevere_rate + lead_case_fatality, data)

# summarize
summary(lmFit7)
summary(lmFit8)
summary(lmFit9)
summary(lmFit10)
# -------------------------


# ----------------------------------------------
# Define model object
# DECISIONS
source('./impact_evaluation/models/drc_malaria_impact4.r')

# swap in health zone dummies where health_zone is specified (for convenience)
# model = gsub('health_zone', paste(unique(data$health_zone)[-1],collapse='+'), model)
# ----------------------------------------------


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
	file.copy(outputFile5d, outputFile5d_scratch, overwrite=TRUE)
	# store T (length of array)
	hzs = unique(data$health_zone)
	T = length(hzs)
	# submit array job
	system(paste0('qsub -cwd -N ie2_job_array -t 1:', T, 
		' -l fthread=2 -l m_mem_free=2G -q all.q -P ihme_general -e ', 
		clustertmpDireo, ' -o ', clustertmpDireo, 
		' ./core/r_shell_blavaan.sh ./impact_evaluation/5f_run_second_half_analysis_single_hz.r'))
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
print(paste('Saving', outputFile5e))
save(list=c('data','model','summaries','means','scaling_factors'), file=outputFile5e)

# save full output for archiving
outputFile5e_big = gsub('.rdata','_all_semFits.rdata',outputFile5e)
print(paste('Saving', outputFile5e_big))
semFits = lapply(seq(T), function(i) {
	suppressWarnings(readRDS(paste0(clustertmpDir2, 'second_half_semFit_', i, '.rds')))
})
save(list=c('data','model','semFits','summaries','means','scaling_factors'), file=outputFile5e_big)

# save a time-stamped version for reproducibility
print('Archiving files...')
archive(outputFile5e, 'model_runs')
archive(outputFile5e_big, 'model_runs')

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
