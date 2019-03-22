# ------------------------------------------------
# David Phillips
# 
# 1/18/2019
# This runs the SEM dose-response model
# qsub -l archive=TRUE -cwd -N ie_script_5d -l fthread=4 -l m_mem_free=4G -q all.q -P ihme_general -e /ihme/scratch/users/davidp6/impact_evaluation/errors_output/ -o /ihme/scratch/users/davidp6/impact_evaluation/errors_output/ ./core/r_shell_blavaan.sh ./impact_evaluation/5d_run_second_half_analysis.r
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
load(outputFile5c)
# ---------------------------


# -------------------------
# Run series of unrelated linear models

# linkage 1 regressions


# linkage 2 regressions
# lmFit4 = lm(newCasesMalariaMild_rate ~ ITN_rate + lag_mildMalariaTreated_rate + health_zone + date, data)
# lmFit5 = lm(newCasesMalariaSevere_rate ~ ITN_rate + lag_severeMalariaTreated_rate + health_zone + date, data)
# lmFit6 = lm(malariaDeaths_rate ~ newCasesMalariaMild_rate + newCasesMalariaSevere_rate + lag_mildMalariaTreated_rate + lag_severeMalariaTreated_rate + health_zone + date, data)

# tmp = summary(lmFit4)$coefficients
# tmp[!grepl('health_zone',rownames(tmp)),]
# tmp = summary(lmFit5)$coefficients
# tmp[!grepl('health_zone',rownames(tmp)),]
# tmp = summary(lmFit6)$coefficients
# tmp[!grepl('health_zone',rownames(tmp)),]
# -------------------------


# ----------------------------------------------
# Define model object
# DECISIONS
source('./impact_evaluation/models/drc_malaria_impact3.r')

# swap in health zone dummies where health_zone is specified (for convenience)
# model = gsub('health_zone', paste(unique(data$health_zone)[-1],collapse='+'), model)
# ----------------------------------------------


# --------------------------------------------------------------
# Run model
if ('semFit' %in% ls()) rm('semFit')

# run locally if specified
if(runAsQsub==FALSE) { 
	# run all sems
	semFits = mclapply(unique(data$health_zone), function(h) { 
		print(h)
		suppressWarnings(
			bsem(model, data[health_zone==h], adapt=5000, burnin=10000, sample=1000, bcontrol=list(thin=3))
		)
	}, mc.cores=ifelse(Sys.info()[1]=='Windows',1,24))
}

# run fully in parallel if specified
if (runAsQsub==TRUE) { 
	# save copy of input file for jobs
	file.copy(outputFile5c, outputFile5c_scratch, overwrite=TRUE)
	# store T (length of array)
	hzs = unique(data$health_zone)
	T = length(hzs)
	# submit array job
	system(paste0('qsub -cwd -N ie_job_array -t 1:', T, 
		' -l fthread=1 -l m_mem_free=1G -q all.q -P ihme_general -e ', 
		clustertmpDireo, ' -o ', clustertmpDireo, 
		' ./core/r_shell_blavaan.sh ./impact_evaluation/5e_run_second_half_analysis_single_hz.r'))
	# wait for jobs to finish
	while(length(list.files(clustertmpDir2))<T) { 
		Sys.sleep(5)
		print(paste(length(list.files(clustertmpDir2)), 'of', T, 'files found...'))
	}
	# collect output
	print('Collecting output...')
	semFits = lapply(seq(T), function(i) {
		suppressWarnings(readRDS(paste0(clustertmpDir2, 'second_half_semFit_', i, '.rds')))
	})
}

# store summaries of each sem
print('Summarizing results...')
for(i in seq(length(semFits))) { 
	tmp = data.table(standardizedSolution(semFits[[i]], se=TRUE))
	tmp[, health_zone:=unique(data$health_zone)[i]]
	if (i==1) summaries = copy(tmp)
	if (i>1) summaries = rbind(summaries, tmp)
}

# compute averages
means = summaries[,.(est.std=mean(est.std), se=mean(se)), by=c('lhs','op','rhs')]
means
# --------------------------------------------------------------

# nodeTable = fread('./impact_evaluation/visualizations/vartable_second_half.csv')
# source('./impact_evaluation/visualizations/graphLavaan.r')
# semGraph(parTable=means, nodeTable=nodeTable, 
	# scaling_factors=NA, standardized=TRUE, 
	# lineWidth=1.5, curved=0, tapered=FALSE, 
	# boxWidth=2, boxHeight=.5)


# ------------------------------------------------------------------
# Save model output and clean up

# save all sem fits just in case they're needed
print(paste('Saving', outputFile5d))
save(list=c('data','model','summaries','means','scaling_factors'), file=outputFile5d)
outputFile5d_big = gsub('.rdata','_all_semFits.rdata',outputFile5d)
save(list=c('data','model','semFits','summaries','means','scaling_factors'), file=outputFile5d_big)

# save a time-stamped version for reproducibility
date_time = gsub('-|:| ', '_', Sys.time())
outputFile5dArchive = gsub('prepped_data/', 'prepped_data/model_runs/', outputFile5d)
outputFile5dArchive = gsub('.rdata', paste0('_', date_time, '.rdata'), outputFile5dArchive)
file.copy(outputFile5d, outputFile5dArchive)
outputFile5dArchive_big = gsub('prepped_data/', 'prepped_data/model_runs/', outputFile5d_big)
outputFile5dArchive_big = gsub('.rdata', paste0('_', date_time, '.rdata'), outputFile5dArchive_big)
file.copy(outputFile5d_big, outputFile5dArchive_big)

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
