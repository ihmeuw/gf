# handle incoming arguments
v = commandArgs()[3]
o = commandArgs()[4]
i = commandArgs()[5]

# detect if operating on windows or on the cluster 
library(data.table)
library(quantreg)
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

# load the data
vl <- readRDS(paste0(dir, 'prepped/viral_load_pnls_interim.rds'))

# subset the data
subset = vl[variable==v & org_unit==o] 

# run quantreg
quantFit <- rq(value~date, data=subset, tau=0.5)

# list the residuals and add them to the out file
r <- resid(quantFit)
subset[, fitted_value:=predict(quantFit)]
subset[, resid:=r]

# save
saveRDS(subset, paste0('/ihme/scratch/users/ccarelli/quantreg_output', i))
