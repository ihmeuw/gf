# submit dps level amelia runs
# ----------------------------------------------
library(data.table)
# ----------------------------------------------
# data directory
# when run on Unix, data directory needs to be set to /home/j (to run on the cluster), so set this here:
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/')
# ----------------------------------------------
# input file:
input <- "final_data_for_impuation.csv"

# load data
dt <- fread(paste0(dir, input)) 
dt$dps <- gsub(" ", "-", dt$dps)

# dps list
dps_list <- unique(dt$dps)
# ----------------------------------------------
# loop over dps to submit jobs
for (d in dps_list) { 
    
    # store qsub command
    qsub = paste0('qsub -e . -o . -cwd -pe multi_slot 10 -N amelia_run_dpsLevelPriors_', d, ' ./core/r_shell.sh ./outcome_measurement/malaria/cod/run_amelia.R .1 _dpsLevelPriors_', d, " ", d)
    
    # submit job
    system(qsub)
  }
# ----------------------------------------------

# submit a job that aggregates output

# store qsub command
qsub2 = paste0('qsub -e . -o . -cwd -pe multi_slot 50 -N amelia_run_', d, ' ./core/r_shell.sh ./outcome_measurement/malaria/cod/condense_imputed_data.R', '_dpsLevelPriors')

# submit job
system(qsub2)
