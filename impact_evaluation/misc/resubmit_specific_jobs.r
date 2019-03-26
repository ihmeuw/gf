# scratch code for when the new cluster makes my life a little harder...
source('./impact_evaluation/_common/set_up_r.r')
for(i in c(61,90,128:132)) {
	system(paste0('qsub -cwd -N ie_job_array -t ', i, 
		' -l fthread=1 -l m_mem_free=1G -q all.q -P ihme_general -e ', 
		clustertmpDireo, ' -o ', clustertmpDireo, 
		' ./core/r_shell_blavaan.sh ./impact_evaluation/5f_run_second_half_analysis_single_hz.r'))
}
