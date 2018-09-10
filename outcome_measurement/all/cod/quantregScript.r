# handle incoming arguments
e = commandArgs()[3]
p = commandArgs()[4]

# load the data
readRDS(paste0(dir, 'prepped/', file, '_prepped.rds'))

# subset the data
subset = dt[element==e & dps==p] 

# run quantreg
quantFit <- rq(value~date+factor(org_unit), data=subset, tau=0.5)

# list the residuals and add them to the out file
r <- resid(quantFit)
out[ , resid:=r]

# save

