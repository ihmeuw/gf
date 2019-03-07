
# commented out for doing array job
# # handle incoming arguments
# e = commandArgs()[4]
# o = commandArgs()[5]
# i = commandArgs()[6]
# fileName = commandArgs()[7]
# impute = commandArgs()[8]
# v = commandArgs()[9]
# 
# print(e)
# print(o)
# print(i)
# print(v)
# print(fileName)
# print(impute) 

i = as.integer(Sys.getenv("SGE_TASK_ID"))
array_table = fread('/ihme/scratch/users/abatzel/array_table_for_qr.csv')
e = array_table[i]$element_id # available/consumed/lost
o = array_table[i]$org_unit_id # unique facility id
v = array_table[i]$variable_id # drug 

# detect if operating on windows or on the cluster 
library(data.table)
library(quantreg)
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

# load the data
# dt <- readRDS(paste0(dir, 'prepped/', fileName))
dt <- readRDS('/ihme/scratch/users/abatzel/data_for_qr.rds')
dt = data.table(dt)

# **** NO LONGER NEED THIS since reading the data in from /ihme/scratch/ after it is prepped
# # make variable ids
# dt[, variable_id:=.GRP, by='drug']
# dt[, element_id:=.GRP, by='variable']

# remove new cases (not of interest for outlier detection)
if (fileName=='viral_load_pnls_interim.rds') { 
  dt = dt[case=='Old']
  dt[ , case:=NULL]
}

# subset the data
subset = dt[element_id==e & org_unit_id==o & variable_id == v, ] 
head(subset)

# skip cases that will fail
n = nrow(subset[!is.na(value), ])
print(n)
var = var(subset$value, na.rm=T)
print(var)
nx = length(unique(subset$date))
print(nx)

# skip if less than 3 data points
if(n>=3 & var!=0 & nx>=2) {  
  # create formula
  form = 'value~date'
  # # add fixed effect on group if more than one group exists
  # if (length(unique(subset$group))>1) form = paste0(form, '+factor(group)')
  form = as.formula(form)

  # run quantreg
  quantFit <- rq(form, data=subset, tau=0.5)
  summary(quantFit)
  
  # run quantreg - no fixed effect on group
  # quantFit <- rq(value~date, data=subset, tau=0.5)
  # summary(quantFit)
  
  # list the residuals and add them to the out file
  r <- resid(quantFit)
  subset[, fitted_value:=predict(quantFit)]
  
  if (impute=='TRUE') {
    subset[is.na(value), got_imputed:=1]
    subset[is.na(value), value:=fitted_value]
  }
  
  subset[, resid:=r]
  head(subset)
  
} else { 
  subset[, fitted_value:=NA]
  subset[, resid:=NA]
}

# save
print(paste0('Saving: ', paste0('/ihme/scratch/users/abatzel/qr_results/quantreg_output', i, '.rds')))
saveRDS(subset, paste0('/ihme/scratch/users/abatzel/qr_results/quantreg_output', i, '.rds'))
