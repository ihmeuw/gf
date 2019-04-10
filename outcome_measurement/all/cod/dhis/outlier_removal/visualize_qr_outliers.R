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
# --------------------

#------------------------------------
# choose the data set to run the code on - pnls, base, or sigl

set = 'base'

# user name for sourcing functions
user_name = 'ccarelli'

#------------------------------------
# set directories

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

#------------------------------------
# output files

if (set=='pnls') outFile = 'pnls_outliers/pnls_outputs/arv_outliers.pdf'
if (set=='base') outFile = 'outliers/base_outliers_replaced.pdf'
if (set=='sigl') outFile = 'outliers/sigl_drugs_qr_outliers.pdf'

#------------------------------------
# source function for health zone names

# source function - locally or on the cluster
# reset working directory using user_name to specify path
# setwd()
# source('./core/standardizeHZNames.R')
#------------------------------------
# read in the file

if (set=='pnls') {dt = readRDS(paste0(dir, 'pnls_outliers/qr_results_full.rds'))}
if (set=='base') {dt = readRDS(paste0(dir, 'outliers/base_quantreg_results.rds'))}
if (set=='sigl') dt = readRDS(paste0(dir, 'prepped/sigl_quantreg_imputation_results.rds'))
#------------------------------------

#------------------------------------
# fix problem in sigl where when health zone is the org unit type, the health_zone variable was missing and 
# got set incorrectly to bena-tshadi.

# ALSO - set value back to missing where it was imputed (with fitted value, so we can always go back and add it later)
#------------------------------------
if (set == 'sigl') {
  dt[org_unit_type == "health_zone", health_zone := NA]
  dt[is.na(health_zone) & org_unit_type == "health_zone", health_zone1 := unlist(lapply(strsplit(org_unit, " "), "[", 2))]
  dt[is.na(health_zone) & org_unit_type == "health_zone", health_zone2 := unlist(lapply(strsplit(org_unit, " "), "[", 3))]
  dt[is.na(health_zone) & org_unit_type == "health_zone", health_zone3 := unlist(lapply(strsplit(org_unit, " "), "[", 4))]
  dt[ health_zone3 != 'Zone' & health_zone2 != 'Zone', health_zone := paste(health_zone1, health_zone2, health_zone3) ]
  dt[ health_zone3=='Zone', health_zone := paste(health_zone1, health_zone2)]
  dt[ health_zone2=='Zone', health_zone := health_zone1]
  dt[, c('health_zone1', 'health_zone2', 'health_zone3'):=NULL]
  
  dt$health_zone <- standardizeHZNames(dt$health_zone)
  
  dt[ got_imputed== "yes", value := NA ]
}

#-----------------------------------
# hacky bae function - i will get rid of this
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

dt[ ,date:=as.Date(date, origin='1970-01-01')]
#------------------------------------
# merge in the facility names to label the graphs 

facilities = readRDS(paste0(dir, 'meta_data/master_facilities.rds'))
facilities = facilities[ ,.(org_unit_id, org_unit)]
dt = merge(dt, facilities, by='org_unit_id', all.x=TRUE)

# fix merge issue
if (set == 'sigl') {
  dt[, org_unit.x := NULL]
  setnames(dt, "org_unit.y", "org_unit") }

#------------------------------------
# identify outliers at various levels/thresholds
if (set=='pnls') idVars = c('org_unit_id', 'element')
if (set=='base') idVars = c('org_unit_id', 'element')
if (set=='sigl') idVars = c('org_unit_id', 'drug', 'variable') 

#------------------------------------
# identify outliers using the median of the fitted values + 10 MADS of the residuals

# threshold for outlier removal
# not sure if you need NA removal here
dt[ , mad_resid:=mad(resid), by = idVars]
dt[ , sd_resid:=sd(resid), by=idVars]
dt[ , thresh_var:=mad_resid]
dt[mad_resid < 1, thresh_var:=sd_resid]
dt[ , c('sd_resid', 'mad_resid'):=NULL]

# identify the thresholds based on the SD of the fitted_valueuals
dt[!all(is.na(resid)), thresh5:=median(fitted_value, na.rm=T) + (5*thresh_var), by = idVars]
dt[!all(is.na(resid)), thresh10:=median(fitted_value, na.rm=T) + (10*thresh_var), by = idVars]

# set lower and upper bounds
# does this need to be for only !all(is.na) as well? not sure
dt[ , upper:=thresh10]
dt[!all(is.na(resid)), lower:=(median(fitted_value, na.rm=T) - (10*thresh_var)), by = idVars]

# add a 5 SD bound to investigate on the graphs
dt[!all(is.na(resid)), upper_mid:=median(fitted_value, na.rm=T) + (5*thresh_var), by = idVars]
dt[!all(is.na(resid)), lower_mid:=median(fitted_value, na.rm=T) - (5*thresh_var), by = idVars]

# select outliers
# the value is 100 or more and greater than 10 times the SD of the fitted_values
dt[(value < lower | upper < value) & 100 < value, outlier:=TRUE]
dt[is.na(outlier), outlier:=FALSE]

#---------------------------------------------
# remove the dps code from the facility name for the graph titles

# dt[ , facility:=word(org_unit, 2, -1)]
#----------------------------------------------
# subset to the health facilities and elements that contain outliers

if (set=='pnls') dt[ , combine:=paste0(org_unit_id, sex, element)]
if (set=='base')  dt[ , combine:=paste0(org_unit_id, element)]
if (set=='sigl') dt[ , combine := paste0(org_unit_id, drug)]

out_orgs = dt[outlier==T, unique(combine)]
out = dt[combine %in% out_orgs]

# drop the unique identifier
out[ , combine:=NULL]
dt[ , combine:=NULL]

#----------------------------
# eliminate outliers that are part of an emerging trend
# do not demarcate any two or more consecutive outliers as outliers

# create a unique identifier to drop out emerging trends
if (set=='pnls') out[ , combine2:=paste0(org_unit_id, sex, element, subpop, age)]
if (set=='base') out[ , combine2:=paste0(org_unit_id, element, category)]
if (set=='sigl') out[ , combine2:=paste0(org_unit_id, drug, variable)]

# subset to only the age categories, subpops with more than one outlier
out[ , count:=sum(outlier), by=combine2]
drop = out[1 < count & outlier==T]

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
if (set=='sigl') out[, combine:=paste0(org_unit_id, drug)]

out_new = out[outlier==T, unique(combine)]
out = out[combine %in% out_new]
out[ , combine:=NULL]

#----------------------------
# create the graphs

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
        geom_ribbon(data = out[element==e & org_unit_id==o & sex==s], aes(ymin=lower_mid, ymax=upper_mid), 
                    alpha=0.2, fill='#feb24c', color=NA) +
        geom_ribbon(data = out[element==e & org_unit_id==o & sex==s], aes(ymin=lower, ymax=upper), 
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
          geom_line(aes(x=date, y=fitted_value), alpha = 0.5) +
          geom_point(data = out[drug==d & org_unit_id==o & outlier==TRUE], color='#d73027', size=3, alpha=0.8) +
          geom_point(data = out[drug==d & org_unit_id==o & outlier==TRUE], aes(x=date, y=fitted_value), 
                     color='#4575b4', size=3, alpha=0.8) +
          facet_wrap(~variable, scales = "free") +
          scale_color_manual(values=greys) +
          geom_ribbon(data = out[drug==d & org_unit_id==o], aes(ymin=lower_mid, ymax=upper_mid), 
                      alpha=0.2, fill='#feb24c', color=NA) +
          geom_ribbon(data = out[drug==d & org_unit_id==o], aes(ymin=lower, ymax=upper), 
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
        geom_ribbon(data = out[element==e & org_unit_id==o], aes(ymin=lower_mid, ymax=upper_mid),
                    alpha=0.2, fill='#feb24c', color=NA) +
        geom_ribbon(data = out[element==e & org_unit_id==o], aes(ymin=lower, ymax=upper), 
                    alpha=0.2, fill='#feb24c', color=NA) +
        labs(title=title, subtitle=subtitle, x='Date', y='Count') +
        theme_bw()
      
      i=i+1
      
    }}
}
#--------------------------------

#--------------------------------
# print out the list of plots into a pdf
pdf(paste0(dir, outFile), height=6, width=10)

for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 

dev.off()

#--------------------------------




