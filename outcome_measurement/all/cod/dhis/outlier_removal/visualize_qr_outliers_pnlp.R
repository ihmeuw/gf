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
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/outliers/')

#-----------------------------------
# output files
outFile = 'figures/pnlp_outliers_figures (correspond to DPS level outliers).pdf'
outFile2 = 'figures/pnlp_outliers_figures (do not correspond to DPS level outliers).pdf'
outFile_dps = 'figures/pnlp_outliers_figures_dpsLevel.pdf'
outFile_rdts = 'figures/outliers_in_RDTs.pdf'
outData = 'pnlp_outliers_labeled.rds' 
#------------------------------------
# read in the file
dt = readRDS(paste0(dir, 'pnlp_quantreg_results.rds'))
dt_dps = readRDS(paste0(dir, 'pnlp_quantreg_results_dpsLevel.rds'))
#------------------------------------

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
  # dt_dps[ outlier_dpsLevel2==TRUE, .N ] # 945 at fitted_value +/- 10 MADs; 533 at fitted_value +/- 15 MADs
  
dt = merge(dt, dt_dps[, .(org_unit_id, date, variable, element_id, outlier_dpsLevel1, outlier_dpsLevel2, outlier_dpsLevel3)], all = TRUE, 
           by.x=c('dps', 'date', 'variable', 'element_id'), by.y=c('org_unit_id', 'date', 'variable', 'element_id'))

#dt[ , outlier_in_both_wdps1 := ifelse(outlier == TRUE & outlier_dpsLevel1 == TRUE, TRUE, FALSE) ]
dt[ , outlier_in_both_wdps2 := ifelse(outlier == TRUE & outlier_dpsLevel2 == TRUE, TRUE, FALSE) ]
#dt[ , outlier_in_both_wdps3 := ifelse(outlier == TRUE & outlier_dpsLevel3 == TRUE, TRUE, FALSE) ]
# dt[ outlier_in_both_wdps2 == TRUE, .N] # 648 outliers at health zone level (+/- 20 MADs) with corresponding outlier at DPS level (+/- 15 MADs)
#---------------------------------------------

#----------------------------------------------
# save a copy of the data with outliers identified
#----------------------------------------------
dt = dt[, .(dps, health_zone, date, variable, donor, operational_support_partner, population, value, outlier, outlier_in_both_wdps2)]
dt = dt[ , outlier:= outlier_in_both_wdps2]
dt[, outlier_in_both_wdps2:=NULL]

dt[outlier==TRUE, .N]

saveRDS(dt, paste0(dir, outData))
#----------------------------------------------

#----------------------------------------------
# for graphing: subset to the health facilities and elements that contain outliers
#----------------------------------------------
dt[ , combine := paste0(org_unit_id, variable)]

out_orgs_w_dps = dt[outlier_in_both_wdps2 == TRUE, unique(combine)]
out_hz_w_dps = dt[combine %in% out_orgs_w_dps]
out_hz_w_dps[ outlier_in_both_wdps2 != TRUE, outlier := FALSE ]

out_orgs_wo_dps = dt[outlier == TRUE & outlier_dpsLevel2 != TRUE, unique(combine)]
out_hz_wo_dps = dt[combine %in% out_orgs_wo_dps]
out_hz_wo_dps[ outlier_in_both_wdps2 == TRUE, outlier := FALSE ] # setting outlier to be FALSE here for graphing purposes (we don't want these points to show )
#----------------------------
dt_dps[ , combine := paste0(org_unit_id, variable)] 

dt_dps[, outlier:= ifelse(outlier_dpsLevel2 == TRUE, TRUE, FALSE)]
out_orgs_dps = dt_dps[outlier == TRUE, unique(combine)]
out_dps = dt_dps[combine %in% out_orgs_dps]
#----------------------------------------------

#----------------------------------------------
# create the graphs
#----------------------------------------------
# create a palette
greys = brewer.pal(9, 'Greys')

# create a list of plots
list_of_plots = NULL
i=1
#----------------------------

# out <- copy(out_dps)
# subtitle = "Red points show DPS level outliers"

out <- copy(out_hz_w_dps)
subtitle = "Red points show HZ-level outliers also identified as DPS-level outliers"
# RDTs subset:
out = out[grepl(variable, pattern = "RDT"), ]

# out <- copy(out_hz_wo_dps)
# subtitle = "Red points show HZ-level outliers NOT identified as DPS-level outliers,\nand therefore not counted as outliers in final data"

setnames(out, "variable", "element")

# loop through the graphs 
for (e in unique(out$element)) {
  for (o in unique(out[element==e]$org_unit_id)) {
    
    # create the plot
    list_of_plots[[i]] = ggplot(out[element==e & org_unit_id==o], aes(x=date, y=value)) +
      geom_line(alpha = 0.5) +
      geom_point(alpha = 0.5) +
      geom_line(data = out[element==e & org_unit_id==o], aes(x=date, y=fitted_value), color='black') +
      geom_point(data = out[element==e & org_unit_id==o & outlier==TRUE], color='#d73027', size=3) +
      geom_point(data = out[element==e & org_unit_id==o & outlier==TRUE], aes(x=date, y=fitted_value), 
                 color='#4575b4', size=2) +
      scale_color_manual(values=greys) +
      geom_ribbon(data = out[element==e & org_unit_id==o], aes(ymin=t1_lower, ymax=t1_upper),
                  alpha=0.2, fill='#feb24c', color=NA) +
      geom_ribbon(data = out[element==e & org_unit_id==o], aes(ymin=t2_lower, ymax=t2_upper),
                  alpha=0.2, fill='#feb24c', color=NA) +
      geom_ribbon(data = out[element==e & org_unit_id==o], aes(ymin=t3_lower, ymax=t3_upper),
                  alpha=0.2, fill='#feb24c', color=NA) +
      geom_ribbon(data = out[element==e & org_unit_id==o], aes(ymin=t4_lower, ymax=t4_upper),
                  alpha=0.2, fill='#feb24c', color=NA) +
      labs(title=paste0(e,': ', o), x='Date', y='Count', subtitle = subtitle) +
      theme_bw()
    i = i + 1
}}

#--------------------------------

#--------------------------------
# print out the list of plots into a pdf
#--------------------------------
# pdf(paste0(dir, outFile_dps), height=6, width=10)
# pdf(paste0(dir, outFile), height=6, width=10)
# pdf(paste0(dir, outFile2), height=6, width=10)
pdf(paste0(dir, outFile_rdts), height=6, width=10)

for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 

dev.off()
#--------------------------------



#---------------------------------------------
# outliers in dps level but NOT in hz level ??
#---------------------------------------------
check_hz = unique(dt[ outlier == TRUE, .(dps, date, variable) ] )
check_dps = unique(dt_dps[ outlier_dpsLevel2 == TRUE, .(org_unit_id, date, variable)])
setnames(check_dps, "org_unit_id", "dps")

# Are there any in check_dps that are NOT in check_hz? as in, are there any that are dps level outliers that don't have 
# any corresponding health zone level outliers? 
check_hz[, hz_level := TRUE ]
check_dps[, dps_level := TRUE ]

check = merge(check_hz, check_dps, by = c('dps', 'date', 'variable'), all = TRUE)

check = check[is.na(hz_level),]

unique(check$variable) 
# look into variables that are included in the RC model
check = check[ variable %in% c('ANC_2nd', 'ANC_3rd', 'SP_1st', 'ArtLum_received', 'ArtLum_used', 'RDT_received', 'RDT_completed', 'ITN_received', 'ITN_distAtPreschool',
                               'ASAQreceived_14yrsAndOlder', 'ASAQreceived_2to11mos', 'SSCACT_5andOlder')]
check_vars = unique(check[, .(dps, variable)])

for (j in 1:nrow(check_vars)){
  d = check_vars[ j, dps ]
  v = check_vars[ j, variable ]
  outlier_dates = check[ dps == d & variable == v, unique(date) ]
  
  dt_hz = dt[ dps == d & variable == v, ]
  dt_dps_subset = dt_dps[ org_unit_id == d & variable == v, ]
  
  greys = brewer.pal(9, 'Greys')
  
  list_of_plots = NULL
  i=1
  
  list_of_plots[[i]] = ggplot(dt_dps_subset, aes(x=date, y=value)) +
    geom_line(alpha = 0.5) +
    geom_point(alpha = 0.5) +
    geom_line(data = dt_dps_subset[], aes(x=date, y=fitted_value), color='black', alpha=0.9) +
    geom_point(data = dt_dps_subset[outlier_dpsLevel2==TRUE & date %in% outlier_dates, ], color='#d73027', size=3) +
    geom_point(data = dt_dps_subset[outlier_dpsLevel2==TRUE & date %in% outlier_dates, ], aes(x=date, y=fitted_value),
               color='#4575b4', size=2, alpha=0.9) +
    scale_color_manual(values=greys) +
    geom_ribbon(data = dt_dps_subset[], aes(ymin=t1_lower, ymax=t1_upper),
                alpha=0.2, fill='#feb24c', color=NA) +
    geom_ribbon(data = dt_dps_subset[], aes(ymin=t2_lower, ymax=t2_upper),
                alpha=0.2, fill='#feb24c', color=NA) +
    geom_ribbon(data = dt_dps_subset[], aes(ymin=t3_lower, ymax=t3_upper),
                alpha=0.2, fill='#feb24c', color=NA) +
    labs(title=paste0(d, " - ", v), x='Date', y='Count', subtitle= "Red points show dates where there were no HZ-level outliers") +
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
      labs(title=title, x='Date', y='Count', subtitle = "Blue points show dates where there was an outlier identified at the DPS level") +
      theme_bw()
    
    i=i+1
  }
  
  pdf( paste0(dir, "examples_outlierDPSlevel_noneHZlevel/dpsLevel_example_", j, ".pdf"), height = 10, width = 12 )
  for(i in seq(length(list_of_plots))) {
    print(list_of_plots[[i]])
  }
  dev.off()
}
#---------------------------------------------








