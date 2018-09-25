# handle incoming arguments
e = commandArgs()[3]
o = commandArgs()[4]
i = commandArgs()[5]
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

# run quantreg
quantFit <- rq(value~date, data=subset, tau=0.5)
summary(quantFit)

# list the residuals and add them to the out file
r <- resid(quantFit)
subset[, fitted_value:=predict(quantFit)]
subset[, resid:=r]
head(subset)

# save
print(paste0('Saving: ', paste0('/ihme/scratch/users/ccarelli/quantreg_output', i)))
saveRDS(subset, paste0('/ihme/scratch/users/ccarelli/quantreg_output', i))
