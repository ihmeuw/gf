# ----------------------------------------------
# Caitlin O'Brien-Carelli / Audrey Batzel
# Prep the QR outlier screened data for use
# Examine different thresholds
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(stringr)
# --------------------

#------------------------------------
# choose the data set to run the code on - pnls, base, or sigl
set = 'pnls'

# output set 
pnlsSet = 'VCT'

# user name for sourcing functions
user_name = Sys.info()[['user']]

#------------------------------------
# set directories

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

#-----------------------------------
# output files

outFile = paste0(dir, 'outlier_screened/pnls_subset_2017_01_01_2019_04_01_outliers_replaced.rds')
outPDF = paste0(dir, 'pnls_outliers/pnls_', pnlsSet,  '_outliers.pdf')
  
#------------------------------------
# read in the file
dt = readRDS(paste0(dir, 'outlier_screened/pnls_subset_2017_01_01_2019_04_01_screened.rds'))

#------------------------------------
# identify outliers at various levels/thresholds
idVars = c('org_unit_id', 'element')

#------------------------------------
# identify outliers where the residuals are larger than +/- 10 MADS of the fitted values

# set the thresholds for the MADs  
t1 = 5
t2 = 10
t3 = 20

# threshold for outlier removal
# not sure if you need NA removal here
dt[ , mad_resid := mad(resid, na.rm=TRUE), by = idVars]
dt[ , thresh_var := mad_resid]
# 
# dt[ mad_resid < 1, thresh_var := sd_resid]

# set lower and upper bounds for distinct thresholds: 5, 10, 15 MADs
dt[ , t1_upper := fitted_value + (t1 * thresh_var)]
dt[ , t1_lower := fitted_value - (t1 * thresh_var)]
dt[ , t2_upper := fitted_value + (t2 * thresh_var)]
dt[ , t2_lower := fitted_value - (t2 * thresh_var)]
dt[ , t3_upper := fitted_value + (t3 * thresh_var)]
dt[ , t3_lower := fitted_value - (t3 * thresh_var)]
#------------------------------------
# select outliers

# set a limit under which no value can be an outlier (small values are possible)
limit = 100

# the value is greater than the limit set above and greater than 10 times the mad of residuals 
# or less than 10 times the negative mad of the residuals
dt[(t2_upper < value & limit < value), outlier:=TRUE]
dt[(value < t2_lower & limit < value), outlier:=TRUE ]
dt[value < limit, outlier:=FALSE]

#------------------------------------
# count the outliers and determine what percentage of the data set they constitute

# number of outliers
dt[ outlier==TRUE, .N ]  

# percentage of values identified as outliers
dt[ outlier==TRUE, .N]/nrow(dt)  
#---------------------------------------------

#---------------------------------------------
# remove the dps code from the facility name for the graph titles
# 
# dt[ , facility:=word(org_unit, 2, -1)]

#----------------------------------------------
# subset to the health facilities and elements that contain outliers for visualization 

dt[ , combine:=paste0(org_unit_id, element)]
out_orgs = dt[outlier == TRUE, unique(combine)]
out = dt[combine %in% out_orgs]

# drop the unique identifier from both 
out[ , combine:= NULL]
dt[ , combine:= NULL]
#----------------------------

#----------------------------------------------
# create the graphs
#----------------------------------------------
# create a palette
greys = brewer.pal(9, 'Greys')

# choose the pnls set you want to visualize
out = out[pnls_set==pnlsSet]

#----------------------------------------------
# create a list of plots
list_of_plots = NULL
i=1

#-----------------------------------------------------------------------------
# loop through the graphs 
  for (e in unique(out$element)) {
    for (o in unique(out[element==e]$org_unit_id)) { 
        
        # title states variable, sex, facility
        name = out[org_unit_id==o, unique(org_unit)]
        title = as.character(e)
        
         # create a subtitle with the outlier and the fitted value to impute
          # accomodate a maximum of two outliers
          out_point = out[element==e & org_unit_id==o & outlier==T, unique(value)]
        
          if (length(out_point)==1) { out_point = out_point
          fit_point = out[element==e & org_unit_id==o & value==out_point & outlier==T, unique(fitted_value)]
          subtitle = paste0('Outlier value=', out_point, '; Fitted value=', round(fit_point, 1))
               } else { out_point1 = out_point[1]
                fit_point1 = out[element==e & org_unit_id==o & value==out_point1 & outlier==T, unique(fitted_value)]
                out_point2 = out_point[2]
                fit_point2 = out[element==e & org_unit_id==o & value==out_point2 & outlier==T, unique(fitted_value)]
                subtitle = paste0('Outlier value 1=', out_point1, '; Fitted value 1=', round(fit_point1, 1),
                                  "; Outlier value 2=", out_point2, '; Fitted value 2=', round(fit_point2, 1)) }
      
        # create the plot
        list_of_plots[[i]] = ggplot(out[element==e & org_unit_id==o], aes(x=date, y=value)) +
          geom_line() +
          geom_point(data = out[element==e & org_unit_id==o & outlier==TRUE], color='#d73027', size=3, alpha=0.8) +
          geom_point(data = out[element==e & org_unit_id==o & outlier==TRUE], aes(x=date, y=fitted_value), 
                     color='#4575b4', size=3, alpha=0.8) +
          facet_wrap(~category) +
          scale_color_manual(values=greys) +
          geom_ribbon(data = out[element==e & org_unit_id==o], aes(ymin=t1_lower, ymax=t1_upper), 
                      alpha=0.2, fill='#feb24c', color=NA) +
          geom_ribbon(data = out[element==e & org_unit_id==o], aes(ymin=t2_lower, ymax=t2_upper), 
                      alpha=0.2, fill='#feb24c', color=NA) +
          geom_ribbon(data = out[element==e & org_unit_id==o], aes(ymin=t3_lower, ymax=t3_upper), 
                      alpha=0.2, fill='#feb24c', color=NA) +
          labs(title=paste0(title, ': ', name), subtitle=subtitle, x='Date', y='Count') +
          theme_bw()
        
        i=i+1
        
      }}
#-----------------------------------------------------------------------------


#--------------------------------
# print out the list of plots into a pdf
#--------------------------------
pdf(outPDF, height=9, width=12)

for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 

dev.off()
#--------------------------------

#-----------------------------------------------------------------------------
# drop the outliers and save

# drop the outliers 
dt[outlier==TRUE, value:=fitted_value]

# drop unecessary variables
dt[ , c('thresh_var', 't1_upper', 't2_upper', 't3_upper', 't1_lower', 't2_lower', 't3_lower',
        'fitted_value', 'resid', 'outlier', 'skipped_qr', 'element_id', 'mad_resid'):=NULL]
setnames(dt, 'id', 'element_id')

# save the output files with outliers replaced with fitted value
saveRDS(dt, outFile)

#-----------------------------------------------------------------------------
#----------------------------
# EMERGING TRENDS RULE:
# eliminate outliers that are part of an emerging trend
# do not demarcate any two or more consecutive outliers as outliers
# not currently reflected in outlier rule - can be changed
#----------------------------

out = out[(order(org_unit_id, element, category, date))]

for (e in unique(out$element)) {
  for (o in unique(out[element==e]$org_unit_id)) { 
    for (c in unique(out[element==e & org_unit_id==o]$category)) {   
      
      out[outlier==T & element==e & org_unit_id==o & category==c, emerging_trend:=(shift(outlier, type='lag')==TRUE)]
      out[outlier==T & element==e & org_unit_id==o & category==c, emerging_trend:=(shift(outlier, type='lead')==TRUE)]
    }}}


trends = copy(out)
trends[ , combine:=paste0(org_unit_id, element_id, category)]
emerge = trends[emerging_trend==TRUE, unique(combine)]
trends = trends[combine  %in% emerge]
