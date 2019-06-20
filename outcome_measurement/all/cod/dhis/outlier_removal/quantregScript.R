#------------------------------------
# This script is run on the cluster by run_quantreg_parallel.r
# source from the cluster
# runs quantile regression on all data sets from DHIS
#------------------------------------
rm(list=ls())
library(data.table)
library(quantreg)
library(fst) 

user = Sys.info()[['user']]

#------------------------------------
# handle arguments
#------------------------------------

# get the task_id to index the array table
i = as.integer(Sys.getenv("SGE_TASK_ID"))
print(i)

# file paths
scratchDir = paste0('/ihme/scratch/users/', user, '/quantreg/')
scratchInFile = paste0(scratchDir, 'data_for_qr.fst')
arrayFile = paste0(scratchDir, 'array_table_for_qr.fst')
parallelDir = paste0(scratchDir, 'parallel_files/')
outFile = paste0(parallelDir, 'quantreg_output', i, '.fst')

# read in the array table 
array_table = read.fst(arrayFile)
array_table = data.table(array_table)

# read org unit from the array table
# o = array_table[i]$org_unit_id # unique facility id
e = array_table[i]$element_id # unique facility id
print(e)
#------------------------------------

#-----------------------------------
# load the data & subset to task_id/org_unit
#------------------------------------

dt = read.fst(scratchInFile)
dt = data.table(dt)
subset = dt[element_id==e, ] 
#------------------------------------

#------------------------------------
# loop through drug and variable, run quant reg, and then combine results
#------------------------------------
combined_qr_results = data.table()

for (o in unique(subset$org_unit_id)) {
  # subset the data further based on loop parameters for qr
    subset_further = subset[org_unit_id == o,] 
    
    # skip cases that will fail
    n = nrow(subset_further[!is.na(value), ])
    #print(n)
    var = var(subset_further$value, na.rm=T)
    #print(var)
    nx = length(unique(subset_further$date))
    #print(nx)
    
    # skip if less than 3 data points or variance is 0
    if(n>=3 & var!=0 & nx>=2) {  
      # create formula
      form = 'value~date'
      
      # # add fixed effect on group if more than one group exist
      form = as.formula(form)
      
      # run quantreg
      quantFit = rq(form, data=subset_further, tau=0.5)
      summary(quantFit) 
      
      # list the residuals and add them to the out file
      subset_further[, fitted_value:=predict(quantFit, newdata = subset_further)]
      subset_further[, resid:=(fitted_value - value)]
      subset_further[, skipped_qr := "no"]
      
    } else { 
      subset_further[, fitted_value:=NA]
      subset_further[, resid:=NA]
      subset_further[, skipped_qr := "yes"]
    }
    
    if (nrow(combined_qr_results)==0) {
      combined_qr_results = subset_further 
      } else { combined_qr_results = rbindlist(list(combined_qr_results, subset_further), use.names=TRUE, fill = TRUE) }
  }


#------------------------------------
# Then, save the combined data set which has quant reg imputation results for all combinations of drug/variable for a specific org_unit
# (i will be = number of org units) / this will be done for each org_unit!
#------------------------------------
print(paste0('Saving: ', outFile))
write.fst(combined_qr_results, outFile)
#------------------------------------
