# K-means cluster analysis
# Final model for use in analysis

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

# drop 2013 - reporting is very low and short time series 
dt = dt[year!=2013]

# ----------------------
# source the functions for elbow plots and silhouette widths

source('cluster_functions.R')

#------------------------
# sum the main data table to a single value 

# calculate if the facility reported
dt[!is.na(test_kits), reported_tests:=TRUE]
dt[is.na(reported_tests), reported_tests:=FALSE]
dt[!is.na(arvs), reported_arvs:=TRUE]
dt[is.na(reported_arvs), reported_arvs:=FALSE]

# total test kits and total arvs
dt = dt[ ,.(test_kits=sum(test_kits, na.rm=T),
            arvs=sum(arvs, na.rm=T), reported_tests=sum(reported_tests),
            reported_arvs=sum(reported_arvs)),
         by=.(facility, level, region)] # do not include year

# calculate percent of time out of both commodities
# no NANs in data set - otherwise replace with 0s
dt[ , percent_tests:=round(100*(test_kits/reported_tests), 1)]
dt[ , percent_arvs:=round(100*(arvs/reported_arvs), 1)]

#------------------------

#----------------------------------------
# create a matrix for cluster analysis

dt_k = dt[ ,.(percent_tests, percent_arvs)]

#----------------------------------------
# calculate elbow plots and silhouette widths and plot

# calculate using sourced functions
elbow = elbow_fun(dt_k, 2, 10)
sil = sil_fun(dt_k, 2, 10)

# ----------------------
# plot the elbow plot

elbow_df = ggplot(elbow, aes(x=k, y=tot_withinss))+
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(y = "Total within-cluster sum of squares", x = "K Clusters",
       title='Elbow plot to empirically determine k clusters',
       subtitle='Percent of weeks out of ARVs, test kits* (2014 - 2019)',
       caption = '*% = total weeks out/total weeks reporting per facility')+
  theme(text=element_text(size=18))

# ----------------------
# plot the silhouette plot

sil_df = ggplot(sil, aes(x=k, y=sil_width))+
  geom_point()+
  geom_line()+
  theme_bw() +
  labs(x='K Clusters', y='Average silhouette width',
       title='Silhouette Width to determine k clusters')+
  theme(text=element_text(size=18))

# ----------------------

#----------------------------------------
# plot the clusters

list_of_plots = NULL
i = 1

# function to run the calculations for every cluster
for (x in c(2:10)) {
  # run a test cluster
  k_clust = kmeans(dt_k, centers = x)
  dt[ , kcluster:=k_clust$cluster]
  
  # mark the centroids for labeling
  dt[ ,centroid_x:=mean(percent_tests, na.rm=T), by=kcluster]
  dt[ ,centroid_y:=mean(percent_arvs, na.rm=T), by=kcluster]
  dt[ ,label:=paste0(round(centroid_x), ", ", round(centroid_y)), by=kcluster]
  
  # rbind the data 
  interim_data = copy(dt)
  interim_data[ , total_clusters:=x]
  if (i ==1) full_data = interim_data
  if (1 < i) full_data = rbind(full_data, interim_data)
  
  #----------------------------
  # create the plots of the percent of time out
  list_of_plots[[i]] = ggplot(full_data[total_clusters==x],
                              aes(x=percent_tests, y=percent_arvs, color=factor(kcluster)))+
    geom_jitter(alpha=0.6)+
    theme_bw()+
    annotate("text", x=full_data[total_clusters==x]$centroid_x,
             y=full_data[total_clusters==x]$centroid_y,
             label=full_data[total_clusters==x]$label)+
    labs(x = "Percent of weeks out of test kits (%)",
         y = "Percent of weeks out of ARVs (%)", color='Clusters',
         title="Percent of reporting weeks out of test kits and ARVs, 2014 - 2019",
         subtitle=paste0('Number of clusters = ', x))+
    theme(text=element_text(size=18))
  
  i = i+1 }

#----------------------------
# print a pdt of plots

pdf(paste0(dir, 'k_means_outputs/final/final_model_percent_arv_tests_2014_2019.pdf'),height=9, width=18)

grid.arrange(elbow_df, sil_df, nrow=1)
for(i in seq(length(list_of_plots))) {
  p = list_of_plots[[i]]
  grid.arrange(p, sil_df, nrow=1)
}

dev.off()

#----------------------------

#----------------------------------------
# export data for the regression

# subset to just the facility and the cluster
merge_dt = full_data[total_clusters==5,.(facility, kcluster)]

# import the data 
orig = readRDS(paste0(dir, 'prepped_data/arv_stockouts_2013_2019.rds'))

# merge the clusters into the original data 
dt = merge(orig, merge_dt, by='facility', all=T)

# save the data with associated clusters
saveRDS(dt, paste0(dir, 'prepped_data/cluster_assignments.rds'))

#----------------------------------------


