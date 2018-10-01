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
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

# load the data
vl <- readRDS(paste0(dir, 'prepped/viral_load_pnls_interim.rds'))

# make variable ids
vl[, element_id:=.GRP, by='variable']

# subset the data
subset = vl[element_id==e & org_unit_id==o] 
head(subset)

# skip cases that will fail
n = nrow(subset)
var = var(subset$value)
nx = length(unique(subset$date))

# skip if less than 3 data points
if(n>=3 & var!=0 & nx>=2) {  
  
  # run quantreg
  quantFit <- rq(value~date+factor(group), data=subset, tau=0.5)
  summary(quantFit)
  
  # list the residuals and add them to the out file
  r <- resid(quantFit)
  subset[, fitted_value:=predict(quantFit)]
  subset[, resid:=r]
  head(subset)
} else { 
  subset[, fitted_value:=NA]
  subset[, resid:=NA]
}

# save
print(paste0('Saving: ', paste0('/ihme/scratch/users/ccarelli/quantreg_output', i)))
saveRDS(subset, paste0('/ihme/scratch/users/ccarelli/quantreg_output', i))
