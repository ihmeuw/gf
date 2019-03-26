# handle incoming arguments
e = commandArgs()[4]
o = commandArgs()[5]
i = commandArgs()[6]
print(e)
print(o)
print(i)

# detect if operating on windows or on the cluster 
library(data.table)
library(quantreg)
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for output
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

# load the data from scratch
dt = readRDS('/ihme/scratch/users/ccarelli/base_to_screen.rds')
dt = data.table(dt)

# subset the data
subset = dt[element_id==e & org_unit_id==o] 
head(subset)

# skip cases that will fail
n = nrow(subset)
var = var(subset$value)
nx = length(unique(subset$date))

# skip if less than 3 data points
if(n>=3 & var!=0 & nx>=2) {  
  
  # add fixed effect on group if more than one group exists
  form = 'value~date'
  if (length(unique(subset$group))>1) form = paste0(form, '+factor(group)')
  form = as.formula(form)
  
  # run quantreg
  quantFit = rq(form, data=subset, tau=0.5)
  summary(quantFit)
  
  # list the residuals and add them to the out file
  r = resid(quantFit)
  subset[, fitted_value:=predict(quantFit)]
  subset[, resid:=r]
  head(subset)
} else { 
  subset[, fitted_value:=NA]
  subset[, resid:=NA]
}

# save
print(paste0('Saving: ', paste0('/ihme/scratch/users/ccarelli/qr_results/quantreg_output', i, '.rds')))
saveRDS(subset, paste0('/ihme/scratch/users/ccarelli/qr_results/quantreg_output', i, '.rds'))
