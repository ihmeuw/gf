# ----------------------------------------------
# Caitlin O'Brien-Carelli

# 5/15/2018
# Run descriptive statistics on imputed data sets for the Uganda Viral Load Dashboard
# ----------------------------------------------
# Set up R

rm(list=ls())
library(data.table)
library(rgeos)
library(raster)
library(ggplot2)
library(rgdal)
library(tibble)
library(dplyr)
library(RColorBrewer)
library(Amelia)
library(MASS)
library(gtools)

#-----------------------

# set input/output directory
#-----------------------
#import the imputed data set (+1 offset)
dir <- 'J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard'
avl <- readRDS(paste0(dir,"/imputed_offset.rds"))


#-----------------------
# check to make sure the data set ran accurately
avl[ , .(mean(patients_received, na.rm=TRUE))]
avl[ , .(mean(ratio, na.rm=TRUE))]
avl[is.na(patients_received)] # should be empty data table
avl[is.na(imputed)] # should be empty data table

#-------------------------------------------------------------
# prepare the imputed data  for analysis

# COUNTS
# exponentiate the counts
expVars <- c("patients_received", "samples_received", "rejected_samples", 
             "dbs_samples", "samples_tested", "valid_results")

avl <- avl[, lapply(.SD, exp), by=c('facility_id', 'facility_name', 'dhis2name', 'sex', 'date', 'district_id', 'dist_name', 'level', 'imputed', 'ratio',
                                    'missing_males', 'missing_females', 'ratio1', 'patients_received0', 'samples_received0', 'rejected_samples0', 
                                    'dbs_samples0', 'samples_tested0', 'valid_results0', 'suppressed0'), .SDcols=expVars]

#--------------------
# transform the identified 0s back to 0; all 0 values should be 1 before transformation
avl[patients_received0==T, unique(patients_received)] 
avl[patients_received0==T, unique(samples_received)] 

avl[patients_received0==T, patients_received:=0] 
avl[samples_received0==T, samples_received:=0] 
avl[rejected_samples0==T, rejected_samples:=0]
avl[dbs_samples0==T, dbs_samples:=0]
avl[samples_tested0==T, samples_tested:=0]
avl[valid_results0==T, valid_results:=0]
avl[suppressed0==T, suppressed:=0]


#-------------------------------------
# transform the viral suppression ratio 

# inverse logit the viral suppression ratio
avl[ ,ratio:=inv.logit(ratio)]

# if a ratio was originally 1, change it back to 1
avl[ratio1==1, unique(ratio)]
avl[ratio1==1, ratio:=1]
avl[ ,summary(ratio)]

# district ratio - ratios that were = 0/0 should be 0
avl[valid_results0==1 & suppressed0==1, ratio:=0]

# drop the identifiers
avl[ ,patients_received0:=NULL] 
avl[ ,samples_received0:=NULL] 
avl[ ,rejected_samples0:=NULL]
avl[ ,dbs_samples0:=NULL]
avl[ ,samples_tested0:=NULL]
avl[ ,valid_results0:=NULL]
avl[ ,suppressed0:=NULL]
avl[,ratio1:=NULL]

#-------------------------------------

# confirm these are rational values
table_ratio <- avl[ ,.(ratio=mean(ratio, na.rm=T)), by=.(date, imputed)]
ggplot(table_ratio, aes(x=date, y=ratio, color=factor(imputed))) + geom_point() + geom_line()

table_ratio_sex <- avl[ ,.(ratio=mean(ratio, na.rm=T)), by=.(date, sex)]
ggplot(table_ratio_sex, aes(x=date, y=ratio, color=factor(sex))) + geom_point() + geom_line()

table_1 <- avl[ ,.(pts=mean(patients_received, na.rm=T)), by=.(date, imputed)]
ggplot(table_1, aes(x=date, y=pts, color=factor(imputed))) + geom_point() + geom_line()

#-------------------------------------
# calculate the number suppressed using the ratio and the number of valid results

avl[ , suppressed:=(valid_results*ratio)]

# make the ratio out of 100 to make it intuitive on graphs
avl[ ,ratio:=(ratio*100)]

#--------------------------------
# calculate the 2.5/5th percentile and the mean for every variable by facility, month

byVars <- c('facility_id', 'facility_name', 'dhis2name', 'level', 'date', 
            'district_id', 'dist_name', 'imputed')

avl_mean <- avl[, .(patients_received=mean(patients_received),
                    samples_received=mean(samples_received),
                    rejected_samples=mean(rejected_samples),
                    dbs_samples =mean(dbs_samples),
                    samples_tested=mean(samples_tested),
                    valid_results=mean(valid_results),
                    suppressed=mean(suppressed),
                    ratio=mean(ratio),
  
      patients_received_lower=quantile(patients_received, 0.025),
      samples_received_lower=quantile(samples_received, 0.025),
      rejected_samples_lower=quantile(rejected_samples, 0.025),
      dbs_samples_lower=quantile(dbs_samples, 0.025),
      samples_tested_lower=quantile(samples_tested, 0.025),
      valid_results_lower=quantile(valid_results, 0.025),
      suppressed_lower=quantile(suppressed, 0.025),
      ratio_lower=quantile(ratio, 0.025),
      
      patients_received_upper=quantile(patients_received, 0.975),
      samples_received_upper=quantile(samples_received, 0.975),
      rejected_samples_upper=quantile(rejected_samples, 0.975),
      dbs_samples_upper=quantile(dbs_samples, 0.975),
      samples_tested_upper=quantile(samples_tested, 0.975),
      valid_results_upper=quantile(valid_results, 0.975),
      suppressed_upper=quantile(suppressed, 0.975),
      ratio_upper=quantile(ratio, 0.975)),
      
      by=byVars]

#--------------------------------
# FACILITY LOOP
# Loop over samples received, valid results, and suppression ratio by facility, month (no sex)

# store identifiers
idVars <- c("facility_id", "facility_name", "date", "imputed")

#create a data set that contains only the relevant variables
ratio_loop <- avl_mean[  , .(samples_received, samples_received_upper, samples_received_lower,
                             valid_results, valid_results_upper, valid_results_lower, ratio, 
                             ratio_upper, ratio_lower),
                            by=idVars]

# reshape indicators long
setnames(ratio_loop, c("samples_received", "valid_results", "ratio"), 
         c("samples_received_mean", "valid_results_mean", "ratio_mean"))
ratio_loop <- melt(ratio_loop, id.vars=idVars)
ratio_loop[, valvar := gsub("samples_received_|valid_results_|ratio_", "", variable)]
ratio_loop[, variable := gsub("_mean|_lower|_upper", "", variable)]
ratio_loop <- dcast(ratio_loop,
                    facility_id + facility_name + date + imputed + variable ~ valvar)

# drop out the lower and upper bound for the original values (same as the mean)
ratio_loop[imputed==0, lower:=NA]
ratio_loop[imputed==0, upper:=NA]

# label the variables for graph titles and put the graphs in an intuitive order
ratio_loop$variable <- factor(ratio_loop$variable, 
                              levels=c("samples_received", "valid_results", "ratio"), 
                              labels=c("Number of samples received", "Number of valid test results", "Percent virally suppressed"))


# single test graph
f=8392
name2 <- unique(ratio_loop[facility_id==f]$facility_name)

ggplot(ratio_loop[facility_id==f], aes(y=mean, ymin=lower, ymax=upper, x=date)) + 
  geom_point() +
  geom_line(alpha=0.5) + 
  facet_wrap(~variable, scales='free_y') +
  labs(title=name2, x="Date", y="") +
  theme_bw() +
  geom_errorbar(color='red', alpha=0.6)


# ---------------------------------
# loop over all facilities printing valid test results and the suppression ratio

list_of_plots = NULL
i=1

for(f in unique(ratio_loop$facility_id)) {
  
  # set the title to the facility name
  name2 <- unique(ratio_loop[facility_id==f]$facility_name)
  
  # create a graph of the monthly data stratified by sex
  list_of_plots[[i]] <- ggplot(ratio_loop[facility_id==f], aes(y=mean, x=date, ymin=lower, ymax=upper)) + 
    geom_point() +
    geom_line(alpha=0.5) + 
    facet_wrap(~variable, scales='free_y') +
    labs(title=name2, x="Date", y="") +
    theme_bw() +
    geom_errorbar(color='red', alpha=0.6)
  
  i=i+1
  
}

pdf(paste0(dir,'/webscrape_agg/outputs/imputed/offset_facility.pdf'), height=6, width=9)

for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 

dev.off()

# ---------------------------------
# FACILITY-SEX LOOP
# Loop over samples received, valid results, and suppression ratio by facility, month, sex

byVars2 <- c('facility_id', 'facility_name', 'dhis2name', 'level', 'date', 
            'district_id', 'dist_name', 'sex', 'imputed')

avl_mean_sex <- avl[, .(patients_received=mean(patients_received),
                    samples_received=mean(samples_received),
                    rejected_samples=mean(rejected_samples),
                    dbs_samples =mean(dbs_samples),
                    samples_tested=mean(samples_tested),
                    valid_results=mean(valid_results),
                    suppressed=mean(suppressed),
                    ratio=mean(ratio),
                    
                    patients_received_lower=quantile(patients_received, 0.025),
                    samples_received_lower=quantile(samples_received, 0.025),
                    rejected_samples_lower=quantile(rejected_samples, 0.025),
                    dbs_samples_lower=quantile(dbs_samples, 0.025),
                    samples_tested_lower=quantile(samples_tested, 0.025),
                    valid_results_lower=quantile(valid_results, 0.025),
                    suppressed_lower=quantile(suppressed, 0.025),
                    ratio_lower=quantile(ratio, 0.025),
                    
                    patients_received_upper=quantile(patients_received, 0.975),
                    samples_received_upper=quantile(samples_received, 0.975),
                    rejected_samples_upper=quantile(rejected_samples, 0.975),
                    dbs_samples_upper=quantile(dbs_samples, 0.975),
                    samples_tested_upper=quantile(samples_tested, 0.975),
                    valid_results_upper=quantile(valid_results, 0.975),
                    suppressed_upper=quantile(suppressed, 0.975),
                    ratio_upper=quantile(ratio, 0.975)),
                
                by=byVars2]

#--------------------------------
# Loop over samples received, valid results, and suppression ratio by facility, month (no sex)

# store identifiers
idVars2 <- c("facility_id", "facility_name", "date", "sex", "imputed")

#create a data set that contains only the relevant variables
ratio_loop2 <- avl_mean_sex[  , .(samples_received, samples_received_upper, samples_received_lower,
                             valid_results, valid_results_upper, valid_results_lower, ratio, 
                             ratio_upper, ratio_lower),
                         by=idVars2]

# reshape indicators long
setnames(ratio_loop2, c("samples_received", "valid_results", "ratio"), 
         c("samples_received_mean", "valid_results_mean", "ratio_mean"))
ratio_loop2 <- melt(ratio_loop2, id.vars=idVars)
ratio_loop2[, valvar := gsub("samples_received_|valid_results_|ratio_", "", variable)]
ratio_loop2[, variable := gsub("_mean|_lower|_upper", "", variable)]
ratio_loop2 <- dcast(ratio_loop2,
                    facility_id + facility_name + date + imputed + variable ~ valvar)

# drop out the lower and upper bound for the original values (same as the mean)
ratio_loop2[imputed==0, lower:=NA]
ratio_loop2[imputed==0, upper:=NA]

# label the variables for graph titles and put the graphs in an intuitive order
ratio_loop2$variable <- factor(ratio_loop2$variable, 
                              levels=c("samples_received", "valid_results", "ratio"), 
                              labels=c("Number of samples received", "Number of valid test results", "Percent virally suppressed"))


# single test graph
f=8392
name2 <- unique(ratio_loop2[facility_id==f]$facility_name)

ggplot(ratio_loop2[facility_id==f], aes(y=mean, ymin=lower, ymax=upper, x=date, color=sex, group=sex)) + 
  geom_point() +
  geom_line(alpha=0.5) + 
  facet_wrap(~variable, scales='free_y') +
  labs(title=name2, x="Date", y="", color="Sex") +
  theme_bw() +
  geom_errorbar()

#----------------------------


list_of_plots = NULL
i=1

for(f in unique(ratio_loop2$facility_id)) {
  
  # set the title to the facility name
  name2 <- unique(ratio_loop2[facility_id==f]$facility_name)
  
  # create a graph of the monthly data stratified by sex
  list_of_plots[[i]] <- ggplot(ratio_loop2[facility_id==f], aes(y=mean, x=date, ymin=lower, ymax=upper, color=sex, group=sex)) + 
    geom_point() +
    geom_line(alpha=0.5) + 
    facet_wrap(~variable, scales='free_y') +
    labs(title=name2, x="Date", y="") +
    theme_bw() +
    geom_errorbar( alpha=0.5)
  
  i=i+1
  
}

pdf(paste0(dir,'/webscrape_agg/outputs/imputed/offset_facility_sex.pdf'), height=6, width=9)

for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 

dev.off()



#------------------------------------------------
# district level loops









# Compare imputed and original data by month, facility id, sex using valid results and the viral suppression ratio

