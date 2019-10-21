# K-means cluster analysis
# Final model for use in analysis
# Plots to determine number of clusters and variables to use

# Caitlin O'Brien-Carelli
# 10/21/2019

# ----------------------
# Set up R
rm(list=ls())
library(ggplot2)
library(rgdal)
library(dplyr)
library(RColorBrewer)
library(plyr)
library(data.table)
library(dendextend)
library(purrr)
library(cluster)
library(gridExtra)
library(plotly)

# turn off scientific notation
options(scipen=999)
# ----------------------
# home drive 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# data directory
dir = paste0(j,  '/Project/Evaluation/GF/outcome_measurement/uga/arv_stockouts/')

# import the data 
dt = readRDS(paste0(dir, 'prepped_data/arv_stockouts_2013_2019.rds'))

# set the working directory to the code repo to source functions
setwd('C:/Users/ccarelli/local/gf/outcome_measurement/hiv/uga/arvs/cluster_analysis/')

# ----------------------
# monthly stock outs

# sum stock outs ot the monthly level
dt_new = dt[ ,.(test_kits = sum(test_kits, na.rm=T), 
            arvs = sum(arvs, na.rm=T)), by=.(facility, year)]

# shape long
dt_long = melt(dt_new, id.vars=c('year', 'facility'))

# print the monthly plots
list_of_plots = NULL
i = 1

dt[art_site==T, site :='ART site']
dt[art_site==F, site :='Not an ART site']

 for (f in unique(dt_long$facility)) {
   
   facility_name = f
   facility_level = dt[facility==f, unique(level)]
   f_site = dt[facility==f, unique(site)]
   
  list_of_plots[[i]] = ggplot(dt_long[facility==f], aes(x=year, y=value, color=variable))+
  geom_point()+
  geom_line()+ 
  facet_wrap(~variable)+
  labs(title=paste0(f, ': ', facility_level, "; ", f_site))+
  theme_bw()
  
  i = i+1
  }

i = 1
pdf(paste0(dir, 'k_means_outputs/annual_stockouts_all_facilities_loop_labeled.pdf'),height=9, width=18)

for(i in seq(length(list_of_plots))) {
  print(list_of_plots[[i]])
  i = i+1
}

dev.off()

#--------------------------



