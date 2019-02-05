# ----------------------------------------------
# Caitlin O'Brien-Carelli
# Prep the qr screened data for use
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

# read in the file
dt = readRDS(paste0(dir, 'pnls_outliers/arv_quantreg_results_test.rds'))

#------------------------------------
# identify outliers at various levels/thresholds
dt[ ,thresh4:=median(resid)+(5*sd(resid)), by=org_unit_id]
dt[ ,thresh10:=median(resid)+(10*sd(resid)), by=org_unit_id]
dt[ ,thresh20:=median(resid)+(20*sd(resid)), by=org_unit_id]

# select outliers 
# the value is 100 or more and greater than 10 times the SD of the residuals 
dt[thresh10 < value & 100 <=value, outlier:=TRUE]
dt[value <= thresh10, outlier:=FALSE]

# set lower and upper bounds
dt[ ,upper:=median(resid)+(10*sd(resid)), by=org_unit_id]
dt[ ,lower:=(median(resid)-(10*sd(resid))), by=org_unit_id]

# add a 5 SD bound just to be sure
dt[ ,upper_mid:=median(resid)+(5*sd(resid)), by=org_unit_id]
dt[ ,lower_mid:=(median(resid)-(5*sd(resid))), by=org_unit_id]

# typically no values are below lower, but check
dt[value < lower, outlier:=TRUE]

#----------------------------
# create an alternate org_unit name for the graphs
dt[ ,facility:=word(org_unit, 2, -1)]

#------------------------------------
# subset to the health facilities with outliers and visualize 

# subset to the health facilities with 
out_orgs = dt[outlier==T, unique(org_unit_id)]
out = dt[org_unit_id %in% out_orgs]

# subset to only the sexes within facilities that have outliers
out[ , combine:=paste0(org_unit_id, sex)]
out_sex = out[outlier==T, unique(combine)]
out = out[combine %in% out_sex]
out[ , combine:=NULL]

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
pdf(paste0(dir, 'outliers/pnls_outputs/test_outliers_ipt.pdf'), height=6, width=10)

for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 

dev.off()

#--------------------------------








