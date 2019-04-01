#------------------------------------
# This script is run on the cluster by base_quantreg_parallel.r
#------------------------------------

library(data.table)
library(quantreg)
library(fst) 

user_name = 'ccarelli'

#------------------------------------
# handle arguments
#------------------------------------

# get the task_id to index the array table
i = as.integer(Sys.getenv("SGE_TASK_ID"))
print(i)

# read in the array table 
array_table = read.csv('/ihme/scratch/users/ccarelli/array_table_for_qr.csv')
array_table = as.data.table(array_table)

# read org unit from the array table
o = array_table[i]$org_unit_id # unique facility id
print(o)

#------------------------------------
#-----------------------------------
# load the data & subset to task_id/org_unit
#------------------------------------
# make a table of values to index rows by unique org_unit for retrieving rows while reading in the data
# index_table <- data.table(index = seq(1:N, f = seq(from= 1, to = 11339952, by = 663), t = seq(from= 663, to = 11339952), by = 663)
# from_row = index_table[ i, f ]
# to_row = index_table[ i, t ]

# dt <- readRDS(paste0(dir, 'prepped/', fileName))
# dt <- readRDS('/ihme/scratch/users/abatzel/data_for_qr.rds')
# dt = data.table(dt)
# dt = read.fst('/ihme/scratch/users/', username, '/data_for_qr.fst', from = from_row , to = to_row, as.data.table = TRUE) 
# 
# if ( length(unique(dt$org_unit_id) == 1) & nrow(dt)==663 & unique(dt$org_unit_id) == o ) {
#   print("Indexing for read.fst() worked correctly!")
#   subset = copy(dt)
# } else {
#   print("Indexing for read.fst() did not work correctly! Retrying by reading in full dt!")
#   dt <- read.fst('/ihme/scratch/users/abatzel/data_for_qr.fst', as.data.table = TRUE)
#   subset = dt[org_unit_id==o, ] 
# }

dt <- read.fst('/ihme/scratch/users/abatzel/data_for_qr.fst', as.data.table = TRUE)
subset = dt[org_unit_id==o, ] 

# # subset the data by facility - to the current org_unit_id we want
# subset = dt[org_unit_id==o, ] 
#------------------------------------

#------------------------------------
# loop through drug and variable, run quant reg, and then combine results
#------------------------------------
combined_qr_results <- data.table()

for (e in unique(subset$element_id)) {
  # for (v in unique(subset$variable_id)) {
    # subset the data further based on loop parameters for qr
    subset_further = subset[element_id == e & variable_id == v, ] 
    
    # skip cases that will fail
    n = nrow(subset_further[!is.na(value), ])
    print(n)
    var = var(subset_further$value, na.rm=T)
    print(var)
    nx = length(unique(subset_further$date))
    print(nx)
    
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
      # r <- resid(quantFit)
      subset_further[, fitted_value:=predict(quantFit, newdata = subset_further)]
      
      subset_further[, resid:=(fitted_value - value)]
      subset_further[, skipped_qr := "no"]
      
      # if (impute=='TRUE') {
      subset_further[is.na(value), got_imputed:="yes"]
      subset_further[is.na(value), value:=fitted_value]
      # }
      
    } else { # if the data doesn't pass the conditions necessary for qr, then skip qr & add these vars
      subset_further[, fitted_value:=NA]
      subset_further[, got_imputed:=NA]
      subset_further[, resid:=NA]
      subset_further[, skipped_qr := "yes"]
    }
    # for each iteration of the loop, add the subset of data to a combined results data table.
    if(nrow(combined_qr_results)==0){
      combined_qr_results = subset_further # first time through, just set combined results to be = the data 
    } else if (nrow(combined_qr_results)>0){
      combined_qr_results = rbindlist( list(combined_qr_results, subset_further), use.names=TRUE, fill = TRUE) # subsequent times through, add in combined results
    }
    print(paste0("completed loop with variable_id = ", v, " and element_id =", e))
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