# ----------------------------------------------
# Caitlin O'Brien-Carelli

# 5/16/2018
# Multiple imputation for the Uganda Viral Load Dashboard
# Run MI (linear and polytime models) on the cluster
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
j <- ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir <- paste0(j,'/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/imputed')

# upload the data with month, year, sex
offset <- readRDS(paste0(dir, "/imputed_offset_prepped.rds"))
ls <- readRDS(paste0(dir, "/imputed_ls_prepped.rds"))

#------------------------------------
# prep offset data

# only imputed data
offset <- offset[imputed==1]

# keep only the relevant variables
byVars <- c('facility_id', 'facility_name', 'dhis2name', 'level', 'date', 
              'district_id', 'dist_name', 'sex', 'imputation_number')

offset <- offset[ ,.(samples_received, dbs_samples, samples_tested, valid_results, suppressed), by=byVars]

#------------------------------------
# prep the ls data set

# only imputed data
ls <- ls[imputed==1]

# keep only the relevant variables
byVars <- c('facility_id', 'facility_name', 'dhis2name', 'level', 'date', 
            'district_id', 'dist_name', 'sex', 'imputation_number')

ls <- ls[ ,.(samples_received, dbs_samples, samples_tested, valid_results, suppressed), by=byVars]

#------------------------------------
# merge the two data sets

byVars2 <-  c('facility_id', 'facility_name', 'dhis2name', 'level', 'date', 
              'district_id', 'dist_name', 'sex', 'imputation_number')

ls_off <- merge(offset, ls, by=byVars2, all.x=T, all.y=T)

#------------------------------------


# single test graph
f=4
name <- unique(ls_off[district_id==f]$dist_name)

ggplot(ls_off[district_id==f], aes(x=suppressed.x, y=suppressed.y, color=sex)) + 
  geom_point() +
  facet_wrap(~sex, scales='free_y') +
  labs(title=paste(name, 'District'), x="Suppressed - Offset Model", y="Suppressed - Lemon Squeeze", color="Sex") +
  theme_bw() 

#----------------------------

dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/')

list_of_plots = NULL
i=1

for(f in unique(ls_off$district_id)) {
  
  # set the title to the facility name
  name <- unique(ls_off[district_id==f]$dist_name)
  
  # create a graph of the monthly data stratified by sex
  list_of_plots[[i]] <- ggplot(ls_off[district_id==f], aes(x=suppressed.x, y=suppressed.y, color=sex)) + 
    geom_point() +
    facet_wrap(~sex, scales='free_y') +
    labs(title=paste(name, 'District'), x="Suppressed - Offset Model", y="Suppressed - Lemon Squeeze", color="Sex") +
    theme_bw() 
  
  i=i+1
  
}

pdf(paste0(dir,'/webscrape_agg/outputs/imputed/ls_offset_compare_scatter.pdf'), height=6, width=9)

for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 

dev.off()


# -------------------------------------------------




