# ----------------------------------------------
# Caitlin O'Brien-Carelli / Audrey Batzel
# Prep the QR outlier screened data for use
# Examine different thresholds
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(quantreg)
library(ggplot2)
library(RColorBrewer)
library(stringr)
# --------------------

#------------------------------------
# choose the data set to run the code on - pnls, base, or sigl

set = 'pnlp'

# user name for sourcing functions
user_name = 'abatzel'

#------------------------------------
# set directories

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

#-----------------------------------
# output files

if (set=='pnls') outFile = 'pnls_outliers/pnls_outputs/arv_outliers.pdf'
if (set=='base') outFile = 'outliers/base/base_outliers_replaced.pdf'
if (set=='sigl') {outFile = 'outliers/sigl/final_sigl_drugs_qr_outliers_04_24_19_updated_rules.pdf'
outData = 'prepped/sigl_drugs_prepped_outliers_labeled.rds' }
if (set=='pnlp') {outFile = '../prepped_data/PNLP/outliers/figures/pnlp_outliers_figures (correspond to DPS level outliers).pdf'
outFile2 = '../prepped_data/PNLP/outliers/figures/pnlp_outliers_figures (do not correspond to DPS level outliers)'
outFile_dps = '../prepped_data/PNLP/outliers/figures/pnlp_outliers_figures_dpsLevel'
outData = '../prepped_data/PNLP/outliers/figures/pnlp_outliers_labeled.rds' }
#------------------------------------
# read in the file

if (set=='pnls') {dt = readRDS(paste0(dir, 'pnls_outliers/base/qr_results_full.rds'))}
if (set=='base') {dt = readRDS(paste0(dir, 'outliers/base/base_quantreg_results.rds'))}
if (set=='sigl') dt = readRDS(paste0(dir, 'prepped/sigl/prepped_sigl_quantreg_imputation_results.rds'))
if (set=='pnlp') { dt = readRDS(paste0(dir, '../prepped_data/PNLP/outliers/pnlp_quantreg_results.rds'))
dt_dps = readRDS(paste0(dir, '../prepped_data/PNLP/outliers/pnlp_quantreg_results_dpsLevel.rds'))}
#------------------------------------

#-----------------------------------
# hacky base function - i will get rid of this
if (set=='base') {
  
  # keep the french element
  setnames(dt, 'element', 'element_fr')
  
  # translate the elements to english
  dt[element_id==1, element:='Severe malaria' ]
  dt[element_id==2, element:='Severe malaria treated' ]
  dt[element_id==3, element:='RDT performed' ]
  dt[element_id==4, element:='Presumed malaria treated' ]
  dt[element_id==5, element:='Positive RDT' ]
  dt[element_id==6, element:='Presumed malaria' ]
  dt[element_id==7, element:='Simple confirmed malaria treated' ]
  dt[element_id==8 , element:='Simple confirmed malaria' ]
  dt[element_id==9 , element:='SP 4th dose' ]
  dt[element_id==10 , element:='SP 2nd dose' ]
  
  dt[element_id==11 , element:='Simple confirmed malaria - pregnant woman' ]
  dt[element_id==12 , element:='LLIN distribued at ANC 2+' ]
  dt[element_id==13 , element:='SP 1st dose' ]
  dt[element_id==14 , element:='SP 3rd dose' ]
  dt[element_id==15 , element:='Severe malaria - pregnant women' ]
  dt[element_id==16 , element:='LLIN distributed at ANC 1' ]
  dt[element_id==17 , element:='Severe malaria treated - pregnant woman' ]
  dt[element_id==18 , element:='Simple malaria treated - pregnant woman' ]
}

#------------------------------------
# fix the date 
if (set != 'pnlp') dt[ , date:=as.Date(date, origin='1970-01-01')]

#------------------------------------
# merge in the facility names to label the graphs 
if (set %in% c('pnls', 'base', 'sigl')) {
  facilities = readRDS(paste0(dir, 'meta_data/master_facilities.rds'))
  facilities = facilities[ ,.(org_unit_id, org_unit)]
  dt = merge(dt, facilities, by='org_unit_id', all.x=TRUE) }

# fix merge issue
if (set == 'sigl') {
  dt[, org_unit.x := NULL]
  setnames(dt, "org_unit.y", "org_unit") }

#------------------------------------
# identify outliers at various levels/thresholds
if (set=='pnls') idVars = c('org_unit_id', 'element')
if (set=='base') idVars = c('org_unit_id', 'element')
if (set=='sigl') idVars = c('org_unit_id', 'drug', 'variable') 
if (set=='pnlp') idVars = c('org_unit_id', 'variable')

#------------------------------------
# identify outliers where the residuals are larger than +/- 10 MADS of the fitted values
# set threshold for different data sets:

if (set=='pnls' | set == 'base') {
  t1 = 5
  t2 = 10  } else if (set=='sigl'){
    t1 = 10
    t2 = 20 } else if (set == 'pnlp'){
      t1 = 10
      t2 = 15
      t3 = 20
      t4 = 25}

# threshold for outlier removal
# not sure if you need NA removal here
dt[!all(is.na(resid)) , mad_resid := mad(resid, na.rm=TRUE), by = idVars]
dt[!all(is.na(resid)) , sd_resid := sd(resid, na.rm=TRUE), by = idVars]
dt[ , thresh_var := mad_resid]
dt[ , stat_used := "mad"] # I want to keep track of which stat is used so we can assess if SD is working okay in place of mad
# if mad of residuals is less than one, use SD 
dt[ mad_resid < 1, thresh_var := sd_resid]
dt[ mad_resid < 1, stat_used := "sd"]
dt[ , c('sd_resid', 'mad_resid') := NULL]

# set lower and upper bounds
dt[ , t1_upper := fitted_value + (t1 * thresh_var)]
dt[ , t1_lower := fitted_value - (t1 * thresh_var)]
dt[ , t2_upper := fitted_value + (t2 * thresh_var)]
dt[ , t2_lower := fitted_value - (t2 * thresh_var)]

if (set == 'pnlp'){
  dt[ , t3_upper := fitted_value + (t3 * thresh_var)]
  dt[ , t3_lower := fitted_value - (t3 * thresh_var)]
  dt[ , t4_upper := fitted_value + (t4 * thresh_var)]
  dt[ , t4_lower := fitted_value - (t4 * thresh_var)]
}

# select outliers
# set minimum value to be considered an outlier based on the 99.5 percentile of the variable 
if (set=='pnls' | set == 'base'){
  limit = 100}
if (set=='sigl'){
  quantiles = dt[ , .( limit = quantile(value, 0.995, na.rm = TRUE), quantile = rep( 0.995)), by = c("variable","level", "drug")]
  dt = merge(dt, quantiles, by = c("variable", "level", "drug"))
}

# the value is greater than the limit set above and greater than 10 times the mad of residuals 
# or less than 10 times the negative mad of the residuals
if (set %in% c('pnls', 'sigl', 'base')){
  dt[, outlier := ifelse( (value > limit & ( value > t2_upper )), TRUE, FALSE) ]
  dt[ (value < t2_lower ), outlier :=TRUE ]}
if (set == 'pnlp') {
  dt[, outlier := ifelse( value > t3_upper, TRUE, FALSE) ]
  dt[ (value < t3_lower ), outlier :=TRUE ]
}
# number of outliers
dt[ outlier==TRUE, .N ]  # 9,220 at fitted_value +/- 20 MADs 
# ( dt[outlier==TRUE, .N]  / dt[!is.na(value), .N] ) * 100 # for sigl = 811; 0.017% of non-missing data; for PNLP, 0.55% of non-missing data

# for pnlp - identify outliers in dps level qr results, and use that to identify hz level outliers
if (set == 'pnlp') {
  dt_dps[!all(is.na(resid)) , mad_resid := mad(resid, na.rm=TRUE), by = idVars] # not sure if you need NA removal here
  dt_dps[!all(is.na(resid)) , sd_resid := sd(resid, na.rm=TRUE), by = idVars] # not sure if you need NA removal here
  dt_dps[ , thresh_var := mad_resid]
  dt_dps[ , stat_used := "mad"] # I want to keep track of which stat is used so we can assess if SD is working okay in place of mad
  dt_dps[ mad_resid < 1, thresh_var := sd_resid] # if mad of residuals is less than one, use SD 
  dt_dps[ mad_resid < 1, stat_used := "sd"]
  dt_dps[ is.na(thresh_var), stat_used := NA]
  dt_dps[ , c('sd_resid', 'mad_resid') := NULL]
  
  # set lower and upper bounds of different thresholds
  # does this need to be for only !all(is.na) as well? not sure
  dt_dps[ , t1_upper := fitted_value + (t1 * thresh_var)]
  dt_dps[ , t1_lower := fitted_value - (t1 * thresh_var)]
  dt_dps[ , t2_upper := fitted_value + (t2 * thresh_var)]
  dt_dps[ , t2_lower := fitted_value - (t2 * thresh_var)]
  dt_dps[ , t3_upper := fitted_value + (t3 * thresh_var)]
  dt_dps[ , t3_lower := fitted_value - (t3 * thresh_var)]
  
  dt_dps[, outlier_dpsLevel3 := ifelse( value > t3_upper, TRUE, FALSE) ]
  dt_dps[ (value < t3_lower ), outlier_dpsLevel3 :=TRUE ]
  dt_dps[, outlier_dpsLevel2 := ifelse( value > t2_upper, TRUE, FALSE) ]
  dt_dps[ (value < t2_lower ), outlier_dpsLevel2 :=TRUE ]
  dt_dps[, outlier_dpsLevel1 := ifelse( value > t1_upper, TRUE, FALSE) ]
  dt_dps[ (value < t1_lower ), outlier_dpsLevel1 :=TRUE ]
  # dt_dps[ outlier_dpsLevel1==TRUE, .N ] # 945 at fitted_value +/- 10 MADs 
  
  dt = merge(dt, dt_dps[, .(org_unit_id, date, variable, element_id, outlier_dpsLevel1, outlier_dpsLevel2, outlier_dpsLevel3)], all = TRUE, 
             by.x=c('dps', 'date', 'variable', 'element_id'), by.y=c('org_unit_id', 'date', 'variable', 'element_id'))
  
  dt[ outlier == TRUE & outlier_dpsLevel1 == TRUE, outlier_in_both_wdps1 := TRUE ]
  dt[ outlier == TRUE & outlier_dpsLevel2 == TRUE, outlier_in_both_wdps2 := TRUE ]
  dt[ outlier == TRUE & outlier_dpsLevel3 == TRUE, outlier_in_both_wdps3 := TRUE ]
  # dt[ outlier_in_both_wdps3 == TRUE, .N] 
}
#---------------------------------------------

#---------------------------------------------
# for PNLP only - example of outlier in dps level but NOT in hz level
#---------------------------------------------
if (set == "pnlp") {
  check_hz = unique(dt[ outlier == TRUE, .(dps, date, variable) ] )
  check_dps = unique(dt_dps[ outlier_dpsLevel3 == TRUE, .(org_unit_id, date, variable)])
  setnames(check_dps, "org_unit_id", "dps")
  
  # Are there any in check_dps that are NOT in check_hz? as in, are there any that are dps level outliers that don't have 
  # any corresponding health zone level outliers? 
  check_hz[, hz_level := TRUE ]
  check_dps[, dps_level := TRUE ]
  
  check = merge(check_hz, check_dps, by = c('dps', 'date', 'variable'), all = TRUE)
  
  check = check[is.na(hz_level),]
  
  for (j in 11:100){
    d = check[ j, dps ]
    d = "lualaba"
    v = check[ j, variable ]
    v = "ANC_1st"
    outlier_dates = check[ j, date ]
    
    dt_hz = dt[ dps == d & variable == v, ]
    dt_dps_subset = dt_dps[ org_unit_id == d & variable == v, ]
    
    greys = brewer.pal(9, 'Greys')
    
    list_of_plots = NULL
    i=1
    
    list_of_plots[[i]] = ggplot(dt_dps_subset, aes(x=date, y=value)) +
      geom_line(alpha = 0.5) +
      geom_point(alpha = 0.5) +
      geom_line(data = dt_dps_subset[], aes(x=date, y=fitted_value), color='black', alpha=0.9) +
      geom_point(data = dt_dps_subset[outlier_dpsLevel3==TRUE, ], color='#d73027', size=3) +
      geom_point(data = dt_dps_subset[outlier_dpsLevel3==TRUE, ], aes(x=date, y=fitted_value),
                 color='#4575b4', size=2, alpha=0.9) +
      scale_color_manual(values=greys) +
      geom_ribbon(data = dt_dps_subset[], aes(ymin=t1_lower, ymax=t1_upper),
                  alpha=0.2, fill='#feb24c', color=NA) +
      geom_ribbon(data = dt_dps_subset[], aes(ymin=t2_lower, ymax=t2_upper),
                  alpha=0.2, fill='#feb24c', color=NA) +
      geom_ribbon(data = dt_dps_subset[], aes(ymin=t3_lower, ymax=t3_upper),
                  alpha=0.2, fill='#feb24c', color=NA) +
      geom_ribbon(data = dt_dps_subset[], aes(ymin=t4_lower, ymax=t4_upper),
                  alpha=0.2, fill='#feb24c', color=NA) +
      labs(title=paste0(d, " - ", v), x='Date', y='Count') +
      theme_bw()
    
    i = 2
    # loop through the graphs
    for (hz in unique(dt_hz$health_zone)) {
      # title states variable, sex, facility
      title = paste0(hz, " (DPS = ", d, ") - ", unique(dt_hz$variable))
      
      # create the plot
      list_of_plots[[i]] = ggplot(dt_hz[health_zone == hz,], aes(x=date, y=value)) +
        geom_line(alpha = 0.5) +
        geom_point(alpha = 0.5) +
        geom_line(data = dt_hz[health_zone == hz,], aes(x=date, y=fitted_value), color='black', alpha=0.9) +
        geom_point(data = dt_hz[health_zone == hz & date %in% outlier_dates,], color='blue', size=3) +
        geom_point(data = dt_hz[health_zone == hz & outlier==TRUE], color='#d73027', size=2, alpha=0.9) +
        geom_point(data = dt_hz[health_zone == hz & outlier==TRUE], aes(x=date, y=fitted_value),
                   color='#4575b4', size=2, alpha=0.9) +
        scale_color_manual(values=greys) +
        geom_ribbon(data = dt_hz[health_zone == hz,], aes(ymin=t1_lower, ymax=t1_upper),
                    alpha=0.2, fill='#feb24c', color=NA) +
        geom_ribbon(data = dt_hz[health_zone == hz,], aes(ymin=t2_lower, ymax=t2_upper),
                    alpha=0.2, fill='#feb24c', color=NA) +
        geom_ribbon(data = dt_hz[health_zone == hz,], aes(ymin=t3_lower, ymax=t3_upper),
                    alpha=0.2, fill='#feb24c', color=NA) +
        geom_ribbon(data = dt_hz[health_zone == hz,], aes(ymin=t4_lower, ymax=t4_upper),
                    alpha=0.2, fill='#feb24c', color=NA) +
        labs(title=title, x='Date', y='Count') +
        theme_bw()
      
      i=i+1
    }
    
    pdf( paste0(dir, "../prepped_data/PNLP/outliers/problem_examples/dpsLevel_example_", j, ".pdf"), height = 10, width = 12 )
    for(i in seq(length(list_of_plots))) {
      print(list_of_plots[[i]])
    }
    dev.off()
  }
}
#---------------------------------------------

#---------------------------------------------
# remove the dps code from the facility name for the graph titles

# dt[ , facility:=word(org_unit, 2, -1)]

#----------------------------------------------
# subset to the health facilities and elements that contain outliers
#----------------------------
if (set=='pnls') dt[ , combine:=paste0(org_unit_id, sex, element)]
if (set=='base') dt[ , combine:=paste0(org_unit_id, element)]
if (set=='sigl') dt[ , combine := paste0(org_unit_id, drug)]
if (set=='pnlp') { dt[ , combine := paste0(org_unit_id, variable)]
  dt_dps[ , combine := paste0(org_unit_id, variable)] 
}

out_orgs = dt[outlier == TRUE, unique(combine)]
out = dt[combine %in% out_orgs]

# drop the unique identifier
out[ , combine := NULL]
dt[ , combine := NULL]
#----------------------------

#----------------------------
# EMERGING TRENDS RULE:
# eliminate outliers that are part of an emerging trend
# do not demarcate any two or more consecutive outliers as outliers
#----------------------------
if (set != 'pnlp'){
  # create a unique identifier to drop out emerging trends
  if (set=='pnls') out[ , combine2:=paste0(org_unit_id, sex, element, subpop, age)]
  if (set=='base') out[ , combine2:=paste0(org_unit_id, element, category)]
  if (set=='sigl') out[ , combine2:=paste0(org_unit_id, drug, variable)]
  
  # subset to only the age categories, subpops with more than one outlier
  out[ , count := sum(outlier), by = combine2]
  drop = out[ 1 < count & outlier == TRUE]
  
  # order by the unique identifier and then by date
  drop = drop[order(combine2, date)]
  
  # the subsequent or previous outlier is within 50 of the past data point
  drop[ , value_lag:=shift(value, type='lag')]
  drop[ , value_lead:=shift(value, type='lead')]
  
  # select outliers within 50 of each other
  drop[outlier==T & (abs(value_lead - value) <= 50), dif:=T]
  drop[outlier==T & (abs(value_lag - value) <= 50), dif:=T]
  
  drop[is.na(dif), dif:=F]
  drop = drop[dif==T]
  
  # convert outliers that are part of an emerging trend to false
  drop[ , combine3:=paste0(as.character(date), combine2)]
  out[ , combine3:=paste0(as.character(date), combine2)]
  emerging_trends = drop$combine3
  out[combine3 %in% emerging_trends, outlier:=F]
  
  # drop the unecessary variables
  out[ ,c('count', 'combine2', 'combine3'):=NULL]
  
  # subset again to only the sexes, facilities, variables with outliers
  # as some outliers have now been changed
  if (set=='pnls') out[ , combine:=paste0(org_unit_id, sex, element)]
  if (set=='base') out[ , combine:=paste0(org_unit_id, element)]
  if (set=='sigl') out[ , combine:=paste0(org_unit_id, drug)]
  
  out_new = out[outlier==T, unique(combine)]
  out = out[combine %in% out_new]
  out[ , combine:=NULL]
}
# # view distribution of outliers by variable
# if (set == 'sigl') dist = out[outlier == TRUE, .N, by = c('drug', 'variable')]
# if (set == 'sigl') dist2 = out[outlier == TRUE, .N, by = c('drug')]
#----------------------------

#----------------------------
# save a version of the data set with final outliers (after threshold set and emerging trends outliers dropped) labelled
#----------------------------
# need to set the values in "drop" to NOT be outliers:
if (set == "sigl"){
  drop[ , outlier:= FALSE]
  merge_vars = names(dt)[!names(dt) %in% "outlier"]
  drop = drop[, c(merge_vars, "outlier"), with= FALSE]
  check = merge(dt, drop, by = merge_vars, all.x = TRUE)
  check[ outlier.y == FALSE, outlier.x := FALSE]
  check[, outlier.y := NULL]
  setnames(check, "outlier.x", "outlier")
  
  saveRDS(check, paste0(dir, outData))
}
#----------------------------

#----------------------------
# create the graphs
#----------------------------
# create a palette
greys = brewer.pal(9, 'Greys')

# create a list of plots
list_of_plots = NULL
i=1
#----------------------------
# loop through the graphs 
if (set == 'pnls'){
  for (e in unique(out$element)) {
    for (o in unique(out[element==e]$org_unit_id)) {
      for (s in unique(out[element==e & org_unit_id==o]$sex)) {
        
        # title states variable, sex, facility
        name = out[org_unit_id==o, unique(facility)]
        title = paste0(e,' (', s, '): ', name)
        
        # create a subtitle with the outlier and the fitted value to impute
        out_point = out[element==e & org_unit_id==o & sex==s & outlier==TRUE, unique(value)]
        fit_point = out[element==e & org_unit_id==o & sex==s & outlier==TRUE, unique(fitted_value)]
        subtitle = paste0('Outlier value=', out_point, '; Fitted value=', round(fit_point, 1))
        
        # create the plot
        list_of_plots[[i]] = ggplot(out[element==e & org_unit_id==o & sex==s], aes(x=date, y=value, color=age)) +
          geom_line() +
          geom_point(data = out[element==e & org_unit_id==o & sex==s & outlier==TRUE], color='#d73027', size=3, alpha=0.8) +
          geom_point(data = out[element==e & org_unit_id==o & sex==s & outlier==TRUE], aes(x=date, y=fitted_value), 
                     color='#4575b4', size=3, alpha=0.8) +
          facet_wrap(~subpop) +
          scale_color_manual(values=greys)+
          geom_ribbon(data = out[element==e & org_unit_id==o & sex==s], aes(ymin=t1_lower, ymax=t1_upper), 
                      alpha=0.2, fill='#feb24c', color=NA) +
          geom_ribbon(data = out[element==e & org_unit_id==o & sex==s], aes(ymin=t2_lower, ymax=t2_upper), 
                      alpha=0.2, fill='#feb24c', color=NA) +
          labs(title=title, subtitle=subtitle, x='Date', y='Count',
               color='Age') +
          theme_bw()
        
        i=i+1
        
      }}}
}
#----------------------------
if (set == 'sigl'){
  for (d in unique(out$drug)) {
    for (o in unique(out[drug==d]$org_unit_id)) {
      # title states drug variable, facility
      title = paste0(d, ' in ', out[org_unit_id==o, unique(org_unit)])
      
      # create a subtitle with the outlier and the fitted value to impute
      out_point = out[drug==d & org_unit_id==o & outlier==TRUE, unique(value)]
      fit_point = out[drug==d & org_unit_id==o & outlier==TRUE, unique(fitted_value)]
      subtitle = paste0('Outlier value=', out_point, '; Fitted value=', round(fit_point, 1))
      
      
      # create the plot
      list_of_plots[[i]] = ggplot(out[drug==d & org_unit_id==o], aes(x=date, y=value)) +
        geom_point() +
        geom_line(aes(x=date, y=fitted_value), alpha = 0.3) +
        geom_point(data = out[drug==d & org_unit_id==o & outlier==TRUE], color='#d73027', size=3, alpha=0.8) +
        geom_point(data = out[drug==d & org_unit_id==o & outlier==TRUE], aes(x=date, y=fitted_value), 
                   color='#4575b4', size=3, alpha=0.8) +
        facet_wrap(~variable, scales = "free") +
        scale_color_manual(values=greys) +
        geom_ribbon(data = out[drug==d & org_unit_id==o], aes(ymin=t1_lower, ymax=t1_upper), 
                    alpha=0.2, fill='#feb24c', color=NA) +
        geom_ribbon(data = out[drug==d & org_unit_id==o], aes(ymin=t2_lower, ymax=t2_upper), 
                    alpha=0.2, fill='#feb24c', color=NA) +
        labs(title=title, subtitle=subtitle, x='Date', y='Count',
             color='Age') +
        theme_bw()
      
      i=i+1
    }}
}

#----------------------------
if (set=='base') {
  
  # loop through the graphs 
  for (e in unique(out$element)) {
    for (o in unique(out[element==e]$org_unit_id)) {
      
      # title states variable, sex, facility
      name = out[org_unit_id==o, unique(facility)]
      title = paste0(e,': ', name)
      
      # create a subtitle with the outlier and the fitted value to impute
      out_points = out[element==e & org_unit_id==o & outlier==TRUE, .(value=unique(value)), by=category]
      fit_points = out[element==e & org_unit_id==o & outlier==TRUE, .(fitted_value=unique(fitted_value)), by=category]
      x = length(unique(out_points$value))
      
      if (x==1) { subtitle = paste0('Outlier value for ', out_points$category[1],' = ',
                                    out_points$value[1],'; fitted value = ', round((fit_points$fitted_value[1]), 1)) 
      } else {subtitle = paste0('Outlier value for ', 
                                out_points$category[1],' = ', out_points$value[1],', fitted value = ', 
                                round(fit_points$fitted_value[1], 1), '; Outlier value for ', out_points$category[2],' = ',
                                out_points$value[2],', fitted value = ', round(fit_points$fitted_value[2],1)) }
      
      # create the plot
      list_of_plots[[i]] = ggplot(out[element==e & org_unit_id==o], aes(x=date, y=value)) +
        geom_line() +
        geom_point(alpha=0.2) +
        geom_line(data = out[element==e & org_unit_id==o], aes(x=date, y=fitted_value), color='#9ebcda', alpha=0.4) +
        geom_point(data = out[element==e & org_unit_id==o & outlier==TRUE], color='#d73027', size=2, alpha=0.8) +
        geom_point(data = out[element==e & org_unit_id==o & outlier==TRUE], aes(x=date, y=fitted_value), 
                   color='#4575b4', size=2, alpha=0.8) +
        facet_wrap(~category) +
        scale_color_manual(values=greys) +
        geom_ribbon(data = out[element==e & org_unit_id==o], aes(ymin=t1_lower, ymax=t1_upper),
                    alpha=0.2, fill='#feb24c', color=NA) +
        geom_ribbon(data = out[element==e & org_unit_id==o], aes(ymin=t2_lower, ymax=t2_upper), 
                    alpha=0.2, fill='#feb24c', color=NA) +
        labs(title=title, subtitle=subtitle, x='Date', y='Count') +
        theme_bw()
      
      i=i+1
      
    }}
}
#----------------------------
if (set=='pnlp') {
  setnames(out, "variable", "element")
  
  # loop through the graphs 
  for (e in unique(out$element)) {
    for (o in unique(out[element==e]$org_unit_id)) {
      
      # title states variable, sex, facility
      title = paste0(e,': ', o)
      
      # create a subtitle with the outlier and the fitted value to impute
      out_points = out[element==e & org_unit_id==o & outlier==TRUE, .(value=unique(value))]
      fit_points = out[element==e & org_unit_id==o & outlier==TRUE, .(fitted_value=unique(fitted_value))]
      
      # create the plot
      list_of_plots[[i]] = ggplot(out[element==e & org_unit_id==o], aes(x=date, y=value)) +
        geom_line() +
        geom_point(alpha=0.2) +
        geom_line(data = out[element==e & org_unit_id==o], aes(x=date, y=fitted_value), color='#9ebcda', alpha=0.4) +
        geom_point(data = out[element==e & org_unit_id==o & outlier==TRUE], color='#d73027', size=2, alpha=0.8) +
        geom_point(data = out[element==e & org_unit_id==o & outlier==TRUE], aes(x=date, y=fitted_value), 
                   color='#4575b4', size=2, alpha=0.8) +
        scale_color_manual(values=greys) +
        geom_ribbon(data = out[element==e & org_unit_id==o], aes(ymin=t1_lower, ymax=t1_upper),
                    alpha=0.2, fill='#feb24c', color=NA) +
        geom_ribbon(data = out[element==e & org_unit_id==o], aes(ymin=t2_lower, ymax=t2_upper),
                    alpha=0.2, fill='#feb24c', color=NA) +
        geom_ribbon(data = out[element==e & org_unit_id==o], aes(ymin=threshold3_lower, ymax=threshold3_upper),
                    alpha=0.2, fill='#feb24c', color=NA) +
        geom_ribbon(data = out[element==e & org_unit_id==o], aes(ymin=threshold4_lower, ymax=threshold4_upper),
                    alpha=0.2, fill='#feb24c', color=NA) +
        labs(title=title, x='Date', y='Count') +
        theme_bw()
      
      i=i+1
    }}
}
#--------------------------------

#--------------------------------
# print out the list of plots into a pdf
#--------------------------------
pdf(paste0(dir, outFile), height=6, width=10)

for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 

dev.off()
#--------------------------------


#------------------------------------------------------------
# remove outliers from the data set and perform final prep

# function to remove outliers from base and save as a prepped file
base_remove = function(x) {
  
  out = out[outlier==T]
  
  # create a unique identifier
  dt[ ,combine:=paste0(org_unit_id, element_id, category, date, value)]
  out[ , combine:=paste0(org_unit_id, element_id, category, date, value)]
  
  # subset to the outliers identified
  list_of_outliers = out$combine 
  dt[combine %in% list_of_outliers, outlier_new:=TRUE]
  dt[is.na(outlier_new), outlier_new:=FALSE]
  dt[ , combine:=NULL]
  
  # eliminate only the outliers that do not violate the emerging trends rule
  dt = dt[outlier_new==FALSE]
  dt[  , c('outlier', 'outlier_new'):=NULL]
  
  # look at structure of original prepped data 
  og = readRDS(paste0( dir, '/prepped/base_services_prepped.rds'))
  head(og)        
  
  #---------------------- ----------------------------------------       
  # format to to look the same as prepped data 
  
  #----------------------
  # subset to the necessary elements and rename
  
  dt[ ,c('fitted_value', 'resid', 'thresh_var', 't2_upper', 't2_lower', 't1_upper', 't1_lower',
         'facility', 'org_unit', 'element_fr'):=NULL]
  
  # rename the elements to the english elements and label the data set
  setnames(dt, 'element', 'element_eng')
  dt[ , data_set:='A- Services de Base']
  
  #----------------------
  # merge in facilities meta data 
  
  meta = readRDS(paste0(dir, 'meta_data/master_facilities.rds'))
  dt = merge(dt, meta, by='org_unit_id', all.x=T)
  
  #----------------------
  # merge in original element ids based on the names
  elements = readRDS(paste0(dir, 'meta_data/elements_fix.rds'))
  dt = merge(dt, elements, by='element_id', all.x=T)
  
  # fix to include correct element ids 
  dt[ ,element_id:=NULL]
  setnames(dt, 'old_element_id', 'element_id')
  
  # format appropriately
  dt = dt[ ,.(org_unit_id, element_id, org_unit, element_eng, date, category, value, 
              org_unit_type, level, country, dps, health_zone, health_area, element, data_set, coordinates)]
  
  
  #----------------------
  saveRDS(dt, paste0(dir, '/prepped/base_services_prepped_outliers_removed.rds'))
  return(dt)
  
}

# runs outlier removal on base and formats as prepped data 
if (set=='base') dt = base_remove(dt)

#--------------------------------








