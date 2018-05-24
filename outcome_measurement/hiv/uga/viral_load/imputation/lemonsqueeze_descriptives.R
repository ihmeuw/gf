# ----------------------------------------------
# Caitlin O'Brien-Carelli

# 5/15/2018
# Lemon squeeze descriptives - prep and loop after imputation
# ----------------------------------------------
# Set up R

rm(list=ls())
library(data.table)
library(rgeos)
library(raster)
library(ggplot2)
library(rgdal)
library(dplyr)
library(RColorBrewer)
library(Amelia)
library(MASS)
library(gtools)

# set input/output directory
# ----------------------------------------------
#dir <- 'J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/imputed'

j <- ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/imputed')

# upload the data with month, year, sex
uvl1 <- readRDS(paste0(dir, "/imputed_ls.rds"))
avl <- uvl1

#--------------------------------------
# transform samples received (exponentiate, substract 1)
avl[ , samples_received:=exp(samples_received)]
avl[ , samples_received:=(samples_received-1)]

#--------------------------------------
# back transform the ratios (revert back to before the lemon squeeze)

n <- avl[imputation_number==1 & imputed==0,.N]

avl[ , dbs:=inv.logit(dbs)]
avl[ , dbs:=(dbs*n)]
avl[ , dbs:=(dbs-0.5)]
avl[ , dbs:=(dbs/(n - 1))]

avl[ , tested:=inv.logit(tested)]
avl[ , tested:=(tested*n)]
avl[ , tested:=(tested-0.5)]
avl[ , tested:=(tested/(n - 1))]

avl[ , valid:=inv.logit(valid)]
avl[ , valid:=(valid*n)]
avl[ , valid:=(valid-0.5)]
avl[ , valid:=(valid/(n - 1))]

avl[ , sup:=inv.logit(sup)]
avl[ , sup:=(sup*n)]
avl[ , sup:=(sup-0.5)]
avl[ , sup:=(sup/(n - 1))]

#--------------------------------------

#--------------------------------------
# create counts from the ratios
avl[ ,dbs_samples:=(samples_received*dbs)]
avl[ ,samples_tested:=(samples_received*tested)]
avl[ ,valid_results:=(samples_tested*valid)]
avl[ ,suppressed:=(valid_results*sup)]

#--------------------------------------

# counts that were 0 should be transformed back to 0 (only true of 0/0)
avl[samples_received0==T, samples_received:=0]
avl[samples_tested0==T, samples_tested:=0]
avl[valid_results0==T, valid_results:=0]
avl[suppressed0==T, suppressed:=0]

#---------------------------------------
# save the back-transformed data to use in other analyses

#saveRDS(avl, paste0(dir,"/imputed_ls_prepped.rds"))

#--------------------------------
# calculate the 2.5/5th percentile and the mean for every variable by facility, month

byVars <- c('facility_id', 'facility_name', 'dhis2name', 'level', 'date', 
            'district_id', 'dist_name', 'imputed', 'sex')

avl_mean <- avl[, .(samples_received=mean(samples_received),
                    dbs_samples=mean(dbs_samples),
                    samples_tested=mean(samples_tested),
                    valid_results=mean(valid_results),
                    suppressed=mean(suppressed),
                    
                    samples_received_lower=quantile(samples_received, 0.025),
                    dbs_samples_lower=quantile(dbs_samples, 0.025),
                    samples_tested_lower=quantile(samples_tested, 0.025),
                    valid_results_lower=quantile(valid_results, 0.025),
                    suppressed_lower=quantile(suppressed, 0.025),
                   
                    samples_received_upper=quantile(samples_received, 0.975),
                    dbs_samples_upper=quantile(dbs_samples, 0.975),
                    samples_tested_upper=quantile(samples_tested, 0.975),
                    valid_results_upper=quantile(valid_results, 0.975),
                    suppressed_upper=quantile(suppressed, 0.975)), by=byVars]


#--------------------------------


#--------------------------------
# FACILITY LOOP
# Loop over samples received, valid results, and suppression ratio by facility, month (no sex)

# store identifiers
idVars <- c("facility_id", "facility_name", "date", "imputed", 'sex')

#create a data set that contains only the relevant variables
ratio_loop <- avl_mean[  , .(samples_received, samples_received_upper, samples_received_lower,
                             valid_results, valid_results_upper, valid_results_lower, suppressed, 
                             suppressed_upper, suppressed_lower),
                         by=idVars]

# reshape indicators long
setnames(ratio_loop, c("samples_received", "valid_results", "suppressed"), 
         c("samples_received_mean", "valid_results_mean", "suppressed_mean"))
ratio_loop <- melt(ratio_loop, id.vars=idVars)
ratio_loop[, valvar := gsub("samples_received_|valid_results_|suppressed_", "", variable)]
ratio_loop[, variable := gsub("_mean|_lower|_upper", "", variable)]
ratio_loop <- dcast(ratio_loop,
                    facility_id + facility_name + date + sex+ imputed + variable ~ valvar)

# drop out the lower and upper bound for the original values (same as the mean)
ratio_loop[imputed==0, lower:=NA]
ratio_loop[imputed==0, upper:=NA]

# label the variables for graph titles and put the graphs in an intuitive order
ratio_loop$variable <- factor(ratio_loop$variable, 
                              levels=c("samples_received", "valid_results", "suppressed"), 
                              labels=c("Number of samples received", "Number of valid test results", "Virally suppressed"))


# single test graph
f=2
name2 <- unique(ratio_loop[facility_id==f]$facility_name)

ggplot(ratio_loop[facility_id==f], aes(y=mean, ymin=lower, ymax=upper, x=date, color=factor(sex))) + 
  geom_point() +
  geom_line(alpha=0.5) + 
  facet_wrap(~variable, scales='free_y') +
  labs(title=name2, x="Date", y="", color="Sex") +
  theme_bw() +
  geom_errorbar(alpha=0.6)


# ---------------------------------
# loop over all facilities printing samples received, valid test results, and the suppression ratio
dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/')

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
    theme_bw() 
    if (!all(is.na(ratio_loop[facility_id==f]$lower))) { 
      list_of_plots[[i]] = list_of_plots[[i]] + geom_errorbar(alpha=0.6)
    }
  
  i=i+1
  
}

pdf(paste0(dir,'/webscrape_agg/outputs/imputed/lemon_squeeze_facility_sex.pdf'), height=6, width=9)

for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 

dev.off()


# ----------------------------------------------------------------------------



# ----------------------------------------------------------------------------





