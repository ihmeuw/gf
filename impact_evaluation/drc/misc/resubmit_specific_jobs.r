# scratch code for when the new cluster makes my life a little harder...
source('./impact_evaluation/_common/set_up_r.r')

# FIRST HALF

# manually if you know which jobs failed
for(i in c(83,84,197)) {
	system(paste0('qsub -cwd -N ie_job_array -t ', i, 
		' -l fthread=2 -l m_mem_free=2G -q all.q -P ihme_general -e ', 
		clustertmpDireo, ' -o ', clustertmpDireo, 
		' ./core/r_shell_blavaan.sh ./impact_evaluation/5c_run_first_half_analysis_single_hz.r'))
}


# automated after all jobs are finished
load(outputFile4a)
hzs = unique(data$health_zone)
T = length(hzs)
for(i in seq(T)) { 
	if(file.exists(paste0(clustertmpDir2, 'first_half_summary_', i, '.rds'))) next
	system(paste0('qsub -cwd -N ie1_job_array -t 1:', T, 
		' -l fthread=1 -l m_mem_free=2G -q all.q -P ihme_general -e ', 
		clustertmpDireo, ' -o ', clustertmpDireo, 
		' ./core/r_shell_blavaan.sh ./impact_evaluation/drc/5c_run_single_model.r ', 
		modelVersion, ' 1 FALSE'))
}

# SECOND HALF

# manually if you know which jobs failed
for(i in c(83,84,197)) {
	system(paste0('qsub -cwd -N ie_job_array -t ', i, 
		' -l fthread=2 -l m_mem_free=2G -q all.q -P ihme_general -e ', 
		clustertmpDireo, ' -o ', clustertmpDireo, 
		' ./core/r_shell_blavaan.sh ./impact_evaluation/5f_run_second_half_analysis_single_hz.r'))
}


# automated after all jobs are finished
load(outputFile4b)
hzs = unique(data$health_zone)
T = length(hzs)
for(i in seq(T)) { 
	if(file.exists(paste0(clustertmpDir2, 'second_half_summary_', i, '.rds'))) next
	system(paste0('qsub -cwd -N ie_job_array -t ', i, 
		' -l fthread=2 -l m_mem_free=2G -q all.q -P ihme_general -e ', 
		clustertmpDireo, ' -o ', clustertmpDireo, 
		' ./core/r_shell_blavaan.sh ./impact_evaluation/5f_run_second_half_analysis_single_hz.r'))
}
