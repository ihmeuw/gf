# ----------------------------------------------
# Caitlin O'Brien-Carelli
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
#------------------------------------
# set directories

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

# functions
source('./core/standardizeHZNames.R')

# choose the data set to run the code on - pnls, base, or sigl
set = 'base'

# read in the file
if (set=='pnls') dt = readRDS(paste0(dir, 'pnls_outliers/qr_results_full.rds'))
if (set=='base') dt = readRDS(paste0(dir, 'outliers/base_quantreg_results.rds'))
if (set=='sigl')dt = readRDS(paste0(dir, 'prepped/sigl_quantreg_imputation_results.rds')) 
#------------------------------------

#------------------------------------
# fix problem in sigl where when health zone is the org unit type, the health_zone variable was missing and 
# got set incorrectly to bena-tshadi.
#------------------------------------
if (set = 'sigl') {
  dt[org_unit_type == "health_zone", health_zone := NA]
  dt[is.na(health_zone) & org_unit_type == "health_zone", health_zone1 := unlist(lapply(strsplit(org_unit, " "), "[", 2))]
  dt[is.na(health_zone) & org_unit_type == "health_zone", health_zone2 := unlist(lapply(strsplit(org_unit, " "), "[", 3))]
  dt[is.na(health_zone) & org_unit_type == "health_zone", health_zone3 := unlist(lapply(strsplit(org_unit, " "), "[", 4))]
  dt[ health_zone3 != 'Zone' & health_zone2 != 'Zone', health_zone := paste(health_zone1, health_zone2, health_zone3) ]
  dt[ health_zone3=='Zone', health_zone := paste(health_zone1, health_zone2)]
  dt[ health_zone2=='Zone', health_zone := health_zone1]
  dt[, c('health_zone1', 'health_zone2', 'health_zone3'):=NULL]
  
  dt$health_zone <- standardizeHZNames(dt$health_zone)
}
#------------------------------------

#------------------------------------
# merge in the facility names to label the graphs 

facilities = readRDS(paste0(dir, 'meta_data/master_facilities.rds'))
facilities = facilities[ ,.(org_unit_id, org_unit)]
dt = merge(dt, facilities, by='org_unit_id', all.x=TRUE)

if (set = 'sigl') {
  dt[, org_unit.x := NULL]
  setnames(dt, "org_unit.y", "org_unit")
}
#------------------------------------
# identify outliers at various levels/thresholds
if (set=='pnls') idVars = c('org_unit_id', 'element')
if (set=='base') idVars = c('org_unit_id', 'element')
if (set=='sigl') idVars = c('org_unit_id', 'drug', 'variable') 

# threshold for outlier removal
dt[ , mad_resid:=mad(resid), by = idVars]
dt[ , sd_resid:=sd(resid, by=idVars)]
dt[mad_resid < 1, thresh_var:=sd_resid]
dt[(mad_resid >= 1), thresh_var:=mad_resid]
dt[ ,sd_resid:=NULL]

# identify the thresholds based on the SD of the residuals
dt[ , thresh5 :=  median(resid) + (5*thresh_var), by = idVars]
dt[ , thresh10 := median(resid) + (10*thresh_var), by = idVars]
dt[ , thresh20 := median(resid) + (20*thresh_var), by = idVars]

# select outliers 
# the value is 100 or more and greater than 10 times the SD of the residuals 
dt[thresh10 < value, outlier:=TRUE] 
dt[value < 100, outlier:=FALSE]
dt[is.na(outlier), outlier:=FALSE]

# test outliers correctly identified (should spit out true and false )
dt[100 < value & thresh10 < value, unique(outlier)]
dt[value <=100 & thresh10 >= value, unique(outlier)]

# set lower and upper bounds
dt[ , upper:= ( median(resid) + (10*thresh_var) ), by = idVars]
dt[ , lower:= ( median(resid) - (10*thresh_var) ), by = idVars]

# add a 5 SD bound just to be sure
dt[ , upper_mid := (median(resid) + (5*thresh_var) ), by = idVars]
dt[ , lower_mid := (median(resid) - (5*thresh_var) ), by = idVars]
#----------------------------
# remove the dps code from the facility name for the graph titles

dt[ ,facility:=word(org_unit, 2, -1)]

#------------------------------------
# subset to the health facilities, sexes, and variables with outliers

if (set=='pnls') {dt[ , combine:=paste0(org_unit_id, sex, element)]}
if (set=='base')  {dt[ , combine:=paste0(org_unit_id, category, element)]}

out_orgs = dt[outlier==T, unique(combine)]
out = dt[combine %in% out_orgs]

# drop the unique identifier
out[ , combine:=NULL]
dt[ , combine:=NULL]
#----------------------------
# eliminate outliers that are part of an emerging trend
# do not demarcate any two or more consecutive outliers as outliers

# define unique identifiers
if (set=='pnls') { subset_vars = c('org_unit_id', 'sex', 'element', 'subpop', 'age')}
if (set=='base') { subset_vars = c(org_unit_id,  element, subpop, age)}
#----------------------------
# eliminate outliers that are part of an emerging trend
# do not demarcate any two or more consecutive outliers as outliers

# create a unique identifier to drop out emerging trends
out[ , combine2:=paste0(org_unit_id, sex, element, subpop, age)]

# subset to only the age categories, subpops with more than one outlier
out[ , count:=sum(outlier), by=combine2]
drop = out[1 < count]

# order by the unique identifier and then by date 
drop[order(combine2, date)]

# the subsequent or previous outlier is within 50 of the past data point
drop[ , value_lag:=shift(value, type='lag')]
drop[ , value_lead:=shift(value, type='lead')]
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
out[ ,c('combine2', 'combine3'):=NULL]

# subset again to only the sexes, facilities, variables with outliers
out[ , combine:=paste0(org_unit_id, sex, element)]
out_new = out[outlier==T, unique(combine)]
out = out[combine %in% out_new]

#----------------------------
# create the graphs

# create a palette
greys = brewer.pal(9, 'Greys')

# create a list of plots
list_of_plots = NULL
i=1

# loop through the graphs 
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
        geom_ribbon(data = out[element==e & org_unit_id==o & sex==s], aes(ymin=lower_mid, ymax=upper_mid), 
                    alpha=0.2, fill='#feb24c', color=NA) +
        geom_ribbon(data = out[element==e & org_unit_id==o & sex==s], aes(ymin=lower, ymax=upper), 
                    alpha=0.2, fill='#feb24c', color=NA) +
        labs(title=title, subtitle=subtitle, x='Date', y='Count',
             color='Age') +
        theme_bw()
      
      i=i+1
      
    }}}

#--------------------------------
# print out the list of plots into a pdf
pdf(paste0(dir, 'pnls_outliers/pnls_outputs/arv_outliers.pdf'), height=6, width=10)

for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 

dev.off()

#--------------------------------
# create a data set exclusively of the outliers to remove
# save it to remove from the full data 

outliers = out[outlier==T]
saveRDS(outliers, paste0(dir, 'pnls_outliers/list_of_arv_outliers.rds'))

#--------------------------------





