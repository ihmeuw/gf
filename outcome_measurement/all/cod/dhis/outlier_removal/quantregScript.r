#------------------------------------
# This script is run on the cluster by run_quantreg_parallel.r
#------------------------------------

#------------------------------------
# handle arguments
#------------------------------------
# commented out for doing array job
# # handle incoming arguments
# e = commandArgs()[4]
# o = commandArgs()[5]
# i = commandArgs()[6]
# fileName = commandArgs()[7]
# impute = commandArgs()[8]
# v = commandArgs()[9]

# fileName = # commandArgs()[4]
# impute = "TRUE" # commandArgs()[5]

# print(e)
# print(o)
# print(i)
# print(v)
# print(fileName)
# print(impute) 

# get the task_id to index the array table
i = as.integer(Sys.getenv("SGE_TASK_ID"))
# read in the array table 
array_table = read.csv('/ihme/scratch/users/abatzel/array_table_for_qr.csv')

# read org unit from the array table
o = array_table[i]$org_unit_id # unique facility id
#------------------------------------

#------------------------------------
# set up
#------------------------------------
library(data.table)
library(quantreg)
library(fst)

# # detect if operating on windows or on the cluster 
# root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
# 
# # set the directory for output
# dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')
#------------------------------------

#------------------------------------
# load the data & subset to task_id/org_unit
#------------------------------------
# make a table of values to index rows by unique org_unit for retrieving rows while reading in the data
index_table <- data.table(index = seq(1:17104), f = seq(from= 1, to = 11339952, by = 663), t = seq(from= 663, to = 11339952, by = 663))
from_row = index_table[ i, f ]
to_row = index_table[ i, f ]

# dt <- readRDS(paste0(dir, 'prepped/', fileName))
# dt <- readRDS('/ihme/scratch/users/abatzel/data_for_qr.rds')
# dt = data.table(dt)
dt <- read.fst('/ihme/scratch/users/abatzel/data_for_qr.fst', from = from_row , to = to_row, as.data.table = TRUE) 

# **** NO LONGER NEED THIS since reading the data in from /ihme/scratch/ after it is prepped in run_quantreg_parallel
# # make variable ids
# dt[, variable_id:=.GRP, by='drug']
# dt[, element_id:=.GRP, by='variable']

# # remove new cases (not of interest for outlier detection)
# if (fileName=='viral_load_pnls_interim.rds') { 
#   dt = dt[case=='Old']
#   dt[ , case:=NULL]
# }

if ( length(unique(dt$org_unit_id) == 1)& nrow(dt)==663 ) {
  print("Indexing for read.rst() worked correctly!")
  subset = copy(dt)
} else {
  print("Indexing for read.fst() did not work correctly! Retrying by reading in full dt!")
  dt <- read.fst('/ihme/scratch/users/abatzel/data_for_qr.fst', as.data.table = TRUE)
  subset = dt[org_unit_id==o, ] 
}

# # subset the data by facility - to the current org_unit_id we want
# subset = dt[org_unit_id==o, ] 
#------------------------------------

#------------------------------------
# loop through drug and variable, run quant reg, and then combine results
#------------------------------------
combined_qr_results <- data.table()

for (e in unique(subset$element_id)) {
  for (v in unique(subset$variable_id)) {
    # subset the data further based on loop parameters for qr
    subset_further = subset[element_id == e & variable_id == v, ] 
    
    # skip cases that will fail
    n = nrow(subset_further[!is.na(value), ])
    # print(n)
    var = var(subset_further$value, na.rm=T)
    # print(var)
    nx = length(unique(subset_further$date))
    # print(nx)
    
    # skip if less than 3 data points or variance is 0
    if(n>=3 & var!=0 & nx>=2) {  
        # create formula
        form = 'value~date'
        # # add fixed effect on group if more than one group exists
        # if (length(unique(subset$group))>1) form = paste0(form, '+factor(group)')
        form = as.formula(form)
        
        # run quantreg
        quantFit <- rq(form, data=subset_further, tau=0.5)
        summary(quantFit)
        
        # run quantreg - no fixed effect on group
        # quantFit <- rq(value~date, data=subset, tau=0.5)
        # summary(quantFit)
        
        # list the residuals and add them to the out file
        r <- resid(quantFit)
        subset_further[, fitted_value:=predict(quantFit)]
        
        # if (impute=='TRUE') {
        subset_further[is.na(value), value:=fitted_value]
        subset_further[is.na(value), got_imputed:="yes"]
        # }
        subset_further[, resid:=r]
        subset_further[, skipped_qr := "no"]
    } else { # if the data doesn't pass the conditions necessary for qr, then skip qr & add these vars
      subset_further[, fitted_value:=NA]
      subset_further[, got_imputed:=NA]
      subset_further[, resid:=NA]
      subset_further[, skipped_qr := "yes"]
    }
    # for each iteration of the loop, add the subset of data to a combined results data table.
      if(nrow(combined_qr_results)==0) combined_qr_results = subset_further # first time through, just set combined results to be = the data
      if(nrow(combined_qr_results)>0) combined_qr_results = rbind(combined_qr_results, subset_further) # subsequent times through, add in combined results
  }
}

# at the end of this loop, the number of rows should be equal to what we started with (663), so check that!
if (nrow(combined_qr_results) != 663) {print("something went wrong with internal loop - number of rows is not the right number!")}
#------------------------------------

#------------------------------------
# Then, save the combined data set which has quant reg imputation results for all combinations of drug/variable for a specific org_unit
# (i will be = number of org units) / this will be done for each org_unit!
#------------------------------------
print(paste0('Saving: ', paste0('/ihme/scratch/users/abatzel/qr_results/quantreg_output', i, '.fst')))
write.fst(combined_qr_results, paste0('/ihme/scratch/users/abatzel/qr_results/quantreg_output', i, '.fst'))
#------------------------------------