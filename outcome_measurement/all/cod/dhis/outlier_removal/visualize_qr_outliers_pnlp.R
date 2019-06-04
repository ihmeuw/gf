# ----------------------------------------------
# Audrey Batzel
# 
# Running this only on PNLP because there are so many differences with it also at the DPS level
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
outFile = '../prepped_data/PNLP/outliers/figures/pnlp_outliers_figures (correspond to DPS level outliers).pdf'
outFile2 = '../prepped_data/PNLP/outliers/figures/pnlp_outliers_figures (do not correspond to DPS level outliers)'
outFile_dps = '../prepped_data/PNLP/outliers/figures/pnlp_outliers_figures_dpsLevel'
outData = '../prepped_data/PNLP/outliers/figures/pnlp_outliers_labeled.rds' 
#------------------------------------
# read in the file
dt = readRDS(paste0(dir, '../prepped_data/PNLP/outliers/pnlp_quantreg_results.rds'))
dt_dps = readRDS(paste0(dir, '../prepped_data/PNLP/outliers/pnlp_quantreg_results_dpsLevel.rds'))
#------------------------------------

#-----------------------------------

#------------------------------------
# identify outliers at various levels/thresholds
idVars = c('org_unit_id', 'variable')

#------------------------------------
# identify outliers where the residuals are larger than +/- 10 MADS of the fitted values
# set threshold for different data sets:
t1 = 10
t2 = 15
t3 = 20
t4 = 25

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
dt[ , t3_upper := fitted_value + (t3 * thresh_var)]
dt[ , t3_lower := fitted_value - (t3 * thresh_var)]
dt[ , t4_upper := fitted_value + (t4 * thresh_var)]
dt[ , t4_lower := fitted_value - (t4 * thresh_var)]

# set outliers
dt[, outlier := ifelse( value > t3_upper, TRUE, FALSE) ]
dt[ (value < t3_lower ), outlier :=TRUE ]

# number of outliers
dt[ outlier==TRUE, .N ]  # 9,220 at fitted_value +/- 20 MADs 

# identify outliers in dps level qr results, and use that to identify hz level outliers
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
#---------------------------------------------

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
# create the graphs
#----------------------------
# create a palette
greys = brewer.pal(9, 'Greys')

# create a list of plots
list_of_plots = NULL
i=1
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








