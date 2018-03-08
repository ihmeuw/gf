# ----------------------------------------------
# David Phillips, Caitlin O'Brien-Carelli
#
# 3/5/2018
# Functions that runs extract_vl.r in parallel on the cluster
# The current working directory should be the root of this repo
# Inputs:
# allAges - (logical) whether or not to aggregate together all age groups in the extract
# allTbs - (logical) whether or not to aggregate together all TB groups in the extract
# bothSexes - (logical) whether or not to aggregate together both sex groups in the extract
# ----------------------------------------------


# --------------------
# Make function
webscrapeVL = function(allAges=FALSE, allTbs=FALSE, bothSexes=FALSE) {
  # --------------------
  
  
  # ----------------------------------------------
  # Load/prep data
  
  #loop over years - can be altered to run years separately
  for(y in c('14', '15', '16', '17', '18')) {
    
    # loop over months
    for(m in c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')){ 
  
  		# store qsub command
  		qsub <- paste0('qsub -e . -o . -cwd -N job', y, m, ' ./core/r_shell.sh ./outcome_measurement/hiv/extract_vl.r ', y, ' ', m, ' ', allAges, ' ', allTbs, ' ', bothSexes)
  
  		# submit job
  		system(qsub)
    }
  }
  # ----------------------------------------------
}
