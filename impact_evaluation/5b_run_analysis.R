# ------------------------------------------------
# David Phillips
# 
# 1/18/2019
# This runs the SEM dose-response model
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
load(outputFile5a)
# ---------------------------


# -------------------------
# Run series of unrelated linear models

lmFit1 = lm(ITN_received_cumulative ~ exp_M1_1_cumulative + exp_M1_2_cumulative + other_dah_M1_1_cumulative, data)
lmFit2 = lm(RDT_received_cumulative ~ exp_M2_1_cumulative + exp_M2_3_cumulative + other_dah_M2_cumulative + other_dah_M2_3_cumulative, data)
lmFit3 = lm(ACT_received_cumulative ~ exp_M2_1_cumulative + exp_M2_3_cumulative + other_dah_M2_cumulative + other_dah_M2_3_cumulative, data)

# linkage 2 regressions
lmFit4 = lm(ITN_consumed_cumulative ~ ITN_received_cumulative, data)
lmFit5 = lm(ACTs_SSC_cumulative ~ ACT_received_cumulative, data)
lmFit6 = lm(RDT_completed_cumulative ~ RDT_received_cumulative, data)
lmFit7 = lm(SP_cumulative ~ exp_M3_1_cumulative, data)
lmFit8 = lm(severeMalariaTreated_cumulative ~ exp_M2_6_cumulative + ACT_received_cumulative, data)
lmFit9 = lm(totalPatientsTreated_cumulative ~ ACT_received_cumulative, data)

summary(lmFit1)
summary(lmFit2)
summary(lmFit3)
summary(lmFit4)
summary(lmFit5)
summary(lmFit6)
summary(lmFit7)
summary(lmFit8)
summary(lmFit9)
# -------------------------


# ----------------------------------------------
# Define model object
# DECISIONS
# including date as a control variable in linkage 1 regressions because otherwise all RT variables are positively correlated (when GF and other should be negative)
source('./impact_evaluation/models/drc_malaria4.r')

# test run
# tmp = data[health_zone==unique(data$health_zone)[1]]
# for(v in names(tmp)[!names(tmp)%in%c('health_zone','date')]) tmp[, (v):=get(v)+rpois(nrow(tmp), sd(tmp[[v]])/10)]
# semFit = bsem(model, tmp, adapt=500, burnin=100, sample=100, bcontrol=list(thin=3))
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
	file.copy(outputFile5a, outputFile5a_scratch, overwrite=TRUE)
	# store T (length of array)
	hzs = unique(data$health_zone)
	T = length(hzs)
	# submit array job
	system(paste0('qsub -cwd -N ie_job_array -t 1:', T, 
		' -l fthread=1 -l m_mem_free=1G -q all.q -P ihme_general -e ', 
		clustertmpDireo, ' -o ', clustertmpDireo, 
		' ./core/r_shell_blavaan.sh ./impact_evaluation/5c_run_first_half_analysis_single_hz.r'))
	# wait for jobs to finish (2 files per job)
	while(length(list.files(clustertmpDir2, pattern='_summary_'))<(T)) { 
		Sys.sleep(5)
		print(paste(length(list.files(clustertmpDir2, pattern='_summary_')), 'of', T, 'files found...'))
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
print(paste('Saving', outputFile5b))
save(list=c('data','untransformed','model','summaries','means','scaling_factors'), file=outputFile5b)

# save full output for archiving
print(paste('Saving', outputFile5b_big))
semFits = lapply(seq(T), function(i) {
	suppressWarnings(readRDS(paste0(clustertmpDir2, 'first_half_semFit_', i, '.rds')))
})
outputFile5b_big = gsub('.rdata','_all_semFits.rdata',outputFile5b)
save(list=c('data','untransformed','model','semFits','summaries','means','scaling_factors'), file=outputFile5b_big)

# save a time-stamped version for reproducibility
print('Archiving files...')
archive(outputFile5b)
archive(outputFile5b_big)

# clean up in case jags saved some output
if(dir.exists('./lavExport/')) unlink('./lavExport', recursive=TRUE)

# clean up qsub files
print(paste('Cleaning up cluster temp files...'))
if (runAsQsub==TRUE) { 
	system(paste0('rm ', clustertmpDireo, '/*'))
	system(paste0('rm ', clustertmpDir1	, '/*'))
	system(paste0('rm ', clustertmpDir2	, '/*'))
}
# ------------------------------------------------------------------
