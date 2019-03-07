# handle incoming arguments
e = commandArgs()[4]
o = commandArgs()[5]
i = commandArgs()[6]
fileName = commandArgs()[7]
impute = commandArgs()[8]
print(e)
print(o)
print(i)

# detect if operating on windows or on the cluster 
library(data.table)
library(quantreg)
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

# load the data
vl <- readRDS(paste0(dir, 'prepped/', fileName))
vl = data.table(vl)

# remove new cases (not of interest for outlier detection)
if (fileName=='viral_load_pnls_interim.rds') { 
  vl = vl[case=='Old']
  vl[ , case:=NULL]
}

# make variable ids
vl[, element_id:=.GRP, by='variable']

# subset the data
subset = vl[element_id==e & org_unit_id==o] 
head(subset)

# skip cases that will fail
n = nrow(subset[!is.na(value), ])
var = var(subset$value)
nx = length(unique(subset$date))

# skip if less than 3 data points
if(n>=3 & var!=0 & nx>=2) {  
  
  # add fixed effect on group if more than one group exists
  form = 'value~date'
  if (length(unique(subset$group))>1) form = paste0(form, '+factor(group)')
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
