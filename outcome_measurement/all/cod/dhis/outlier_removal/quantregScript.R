#------------------------------------
# This script is run on the cluster by run_quantreg_parallel.r
# source from the cluster
# runs quantile regression on all data sets from DHIS
#------------------------------------

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
arrayFile = paste0(scratchDir, 'array_table_for_qr.csv')
parallelDir = paste0(scratchDir, 'parallel_files/')
outFile = paste0(parallelDir, '/quantreg_output', i, '.fst')

# read in the array table 
array_table = fread(arrayFile)

# read org unit from the array table
o = array_table[i]$org_unit_id # unique facility id
print(o)
#------------------------------------

#-----------------------------------
# load the data & subset to task_id/org_unit
#------------------------------------

dt = read.fst(scratchInFile)
dt = data.table(dt)
subset = dt[org_unit_id==o] 

#------------------------------------

#------------------------------------
# loop through drug and variable, run quant reg, and then combine results
#------------------------------------
combined_qr_results = data.table()

for (e in unique(subset$element_id)) {
    
  # subset the data further based on loop parameters for qr
    subset_further = subset[element_id == e] 
    
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
      
      # # add fixed effect on group if more than one group exist
      form = as.formula(form)
      
      # run quantreg
      quantFit = rq(form, data=subset_further, tau=0.5)
      summary(quantFit) 
      
      # list the residuals and add them to the out file
      r = resid(quantFit)
      subset_further[, fitted_value:=predict(quantFit)]
      subset_further[, resid:=r]
      head(subset_further)
    } else { 
      subset_further[, fitted_value:=NA]
      subset_further[, resid:=NA]
    }
    
    # for each iteration of the loop, add the subset of data to a combined results data table.
    if(nrow(combined_qr_results)==0){
      combined_qr_results = subset_further # first time through, just set combined results to be = the data 
    } else (nrow(combined_qr_results)>0) {
      combined_qr_results = rbindlist(list(combined_qr_results, subset_further), use.names=TRUE, fill = TRUE) # subsequent times through, add in combined results
    }
    print(paste0("completed loop with element_id =", e))
  }


#------------------------------------
# Then, save the combined data set which has quant reg imputation results for all combinations of drug/variable for a specific org_unit
# (i will be = number of org units) / this will be done for each org_unit!
#------------------------------------
print(paste0('Saving: ', outFile))
write.fst(combined_qr_results, outFile)
#------------------------------------
