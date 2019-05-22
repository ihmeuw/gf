# ----------------------------------------------
# Audrey Batzel
#
# 5/21/2019
# post-processing of duplicates - compare indices in dup matrix to figure out how many that are the same are 0s (if it is all of them, we will want to remove those as "duplicates" for example)

# qsub to run this script on the cluster:
# qsub -e /ihme/scratch/users/abatzel/duplicate_fix_output/ -o /ihme/scratch/users/abatzel/duplicate_fix_output/ -cwd -l fthread=50 -l m_mem_free=20G -l h_rt=24:00:00 -P proj_pce -q all.q -l archive=TRUE ./core/r_shell.sh ./outcome_measurement/malaria/cod/update_duplicates.R


## RUN ON THE CLUSTER: 

library(data.table)
library(parallel)

root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(root, "/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/")
dt_for_dup_removal = "ameliaDT_with_index_for_dup_removal.rds"
dups =  "pnlp_matrix_of_duplicate_rows.rds"

# output files
outFile = "number_of_zeroes_for_dup_matrix.rds"
# ----------------------------------------------   

# ----------------------------------------------     
# read in data
# ---------------------------------------------- 
dt = readRDS( paste0(dir, dt_for_dup_removal) ) 
dups = readRDS( paste0(dir, dups))

id_vars <- c("dps", "health_zone", "date", "donor", "operational_support_partner", "population", "id")
inds = names(dt)[!names(dt) %in% id_vars]
# ---------------------------------------------- 

# ---------------------------------------------- 
# loop through rows and count the number of duplicates that are because both rows' columns are 0
# ---------------------------------------------- 
dups_zeroes = data.table()
current_dup_matrix = data.table()

dup_list = mclapply( seq(nrow(dups)), function(x) {
  
    # use the row of dups to index what the "duplicate" rows in dt are
    i = dups[ x, i ]
    j = dups[ x, j ]
    
    # compare the rows in dt to get the number of columns that are identical AND 0:
    # subset dt to the rows indexed in first step
    check = dt[ id %in% c(i, j)]
    # find which ones are BOTH 0 by summing the columns (note: important to NOT use na.rm = TRUE here)
    check = check[ , lapply(.SD, sum), .SDcols =  inds]
    # count the number of columns that are 0 after summing - if result is 0, both were 0; if result is NA, then at least one was NA and we don't want to count those.
    n = sum( colSums(check) == 0, na.rm = TRUE)
    
    tmp = data.table(i=i, j=j, num_identical_0s=n)
    current_dup_matrix = rbind(current_dup_matrix, tmp)
    print(current_dup_matrix)
    
    print(paste0("Progress status: ", x))
    
    }, mc.cores=ifelse(Sys.info()[1]=='Windows', 1, 50 ))

dups_zeroes = rbindlist(dup_list)

saveRDS(dups_zeroes, paste0(dir, outFile))
# ---------------------------------------------- 