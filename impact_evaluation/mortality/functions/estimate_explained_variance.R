# Function that takes a data table and estimates explained variance
# Inputs:
  # data: an oject of class data.table containing standardized variables:
    # - mortality_rate_std
    # - log_cases_var_std
    # - logit_mi_ratio_std

estEV = function(data) { 
  fitObject = lm(mortality_rate_std ~ log_cases_var_std + logit_mi_ratio_std, data)
  tmp = data.table(variable=names(coef(fitObject))[-1])
  values = sapply(tmp$variable, function(v) {
    # test for standardization
    if (round(mean(data[[v]]),5)!=0 | round(sd(data[[v]]),5)!=1) stop(paste('Variable', v, 'is not z-standardized'))
    # compute explained variance using pseudo decomposition of r squared
    # (see Anusar Farooqui 2016. A Natural Decomposition of R2 in Multiple Linear Regression)
    coef(fitObject)[[v]] * cov(data[[v]], fitObject$fitted.values)
  })
  tmp[, explained_variance := values]
  tmp = rbind(tmp, data.table(variable='Residuals', explained_variance=1-sum(values)))
  return(tmp)
}

# Function that takes a data table and estimates explained variance at a subnational level
# Inputs:
  # data: an oject of class data.table containing standardized variables:
    # - mortality_rate_std
    # - log_cases_var_std
    # - logit_mi_ratio_std
  # subnat_column: the column that corresponds to the level

estEV_subnational = function(data, subnat_column) { 
  subnat_regions = unique(data[, get(subnat_column)])
  
  evs = NULL
  for( i in 1:length(subnat_regions) ){
    subnat_data = data[get(subnat_column) == subnat_regions[i],]
    fitObject = lm(mortality_rate_std ~ log_cases_var_std + logit_mi_ratio_std, subnat_data)
    
    tmp = data.table(variable=names(coef(fitObject))[-1])
    values = sapply(tmp$variable, function(v) {
      # test for standardization
      if (round(mean(subnat_data[[v]]),5)!=0 | round(sd(subnat_data[[v]]),5)!=1) stop(paste('Variable', v, 'is not z-standardized'))
      # compute explained variance using pseudo decomposition of r squared
      # (see Anusar Farooqui 2016. A Natural Decomposition of R2 in Multiple Linear Regression)
      coef(fitObject)[[v]] * cov(subnat_data[[v]], fitObject$fitted.values)
    })
    tmp[, explained_variance := values]
    tmp = rbind(tmp, data.table(variable='Residuals', explained_variance=1-sum(values)))
    tmp[, subnat := subnat_regions[i]]
    
    if (i==1) evs = copy(tmp)
    if (i>1) evs = rbind(evs, tmp)
  }
  return(evs)
}

