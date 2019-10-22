# K-means cluster analysis
# Final three potential models for use in analysis
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

# drop 2013 - reporting is very low and short time series 
dt = dt[year!=2013]

# ----------------------
# source the functions for elbow plots and silhouette widths

source('cluster_functions.R')

#----------------------------
# create a data frame on which to run linear regressions

# create a new data table
df = copy(dt)

# calculate if the facility reported on test kit or arv stock outs
df[!is.na(test_kits), reported_tests:=TRUE]
df[is.na(reported_tests), reported_tests:=FALSE]
df[!is.na(arvs), reported_arvs:=TRUE]
df[is.na(reported_arvs), reported_arvs:=FALSE]

#-----------
# sum to the annual level - weeks out and stock outs 
df = df[ ,.(test_kits=sum(test_kits, na.rm=T),
            arvs=sum(arvs, na.rm=T), reported_tests=sum(reported_tests),
            reported_arvs=sum(reported_arvs)),
         by=.(facility, level,region, year)]

# calculate percent of time out of both commodities
df[ , percent_tests:=round(100*(test_kits/reported_tests), 1)]
df[ , percent_arvs:=round(100*(arvs/reported_arvs), 1)]

# if they were never out of stock, set percent of time out to 0
df[is.na(percent_tests), percent_tests:=0]
df[is.na(percent_arvs), percent_arvs:=0]

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
         by=.(facility, level,region)] # do not include year

# calculate percent of time out of both commodities
# no NANs in data set - otherwise replace with 0s
dt[ , percent_tests:=round(100*(test_kits/reported_tests), 1)]
dt[ , percent_arvs:=round(100*(arvs/reported_arvs), 1)]

#------------------------
# calculate the slopes per facility
# calculate using the annual data, but append to the full time series data 

# slope of change in percent tests
for (f in unique(df$facility)) {
  model = lm(percent_tests~year, data=df[facility==f])
  dt[facility==f, test_slope:=coef(model)[[2]]]
}

# slope of change in percent arvs 
for (f in unique(df$facility)) {
  model = lm(percent_arvs~year, data=df[facility==f])
  dt[facility==f, arv_slope:=coef(model)[[2]]]
}
#----------------------------------------
# scale the variables

# scale the percentages
dt[ ,per_tests_scaled:=((percent_tests - mean(percent_tests))/sd(percent_tests))]
dt[ ,per_arvs_scaled:=((percent_arvs - mean(percent_arvs))/sd(percent_arvs))]

# scale the slopes
dt[ , tests_slope_scaled:=((test_slope - mean(test_slope))/sd(test_slope))]
dt[ , arvs_slope_scaled:=((arv_slope - mean(arv_slope))/sd(arv_slope))]

# scale the totals
dt[ , tests_scaled:=((test_kits - mean(test_kits))/sd(test_kits))]
dt[ , arvs_scaled:=((arvs - mean(arvs))/sd(arvs))]

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

pdf(paste0(dir, 'k_means_outputs/final/percent_arv_tests_2014_2019.pdf'),height=9, width=18)

grid.arrange(elbow_df, sil_df, nrow=1)
for(i in seq(length(list_of_plots))) {
  p = list_of_plots[[i]]
  grid.arrange(p, sil_df, nrow=1)
}

dev.off()

#----------------------------

#---------------------------------------------
# one alternate model

#----------------------------------------
# create a matrix for cluster analysis

dt_k_tot = dt[ ,.(percent_tests, percent_arvs, test_kits, arvs)]

#----------------------------------------
# calculate elbow plots and silhouette widths and plot

# calculate using sourced functions
elbow = elbow_fun(dt_k_tot, 2, 10)
sil = sil_fun(dt_k_tot, 2, 10)

# ----------------------
# plot the elbow plot

elbow_df = ggplot(elbow, aes(x=k, y=tot_withinss))+
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(y = "Total within-cluster sum of squares", x = "K Clusters",
       title='Elbow plot to empirically determine k clusters',
       subtitle='Total and percent of weeks out of ARVs, test kits* (2014 - 2019)',
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

list_of_plots_perc = NULL
list_of_plots_tot = NULL
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
  
  # mark the centroids for labeling
  dt[ ,centroid_x_tot:=mean(test_kits, na.rm=T), by=kcluster]
  dt[ ,centroid_y_tot:=mean(arvs, na.rm=T), by=kcluster]
  dt[ ,label_tot:=paste0(round(centroid_x_tot), ", ", round(centroid_y_tot)), by=kcluster]
  
  # rbind the data 
  interim_data = copy(dt)
  interim_data[ , total_clusters:=x]
  if (i ==1) full_data = interim_data
  if (1 < i) full_data = rbind(full_data, interim_data)
  
  #----------------------------
  # create the plots of the percent of time out
  list_of_plots_perc[[i]] = ggplot(full_data[total_clusters==x],
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
  
  list_of_plots_tot[[i]] = ggplot(full_data[total_clusters==x],
                              aes(x=test_kits, y=arvs, color=factor(kcluster)))+
    geom_jitter(alpha=0.6)+
    theme_bw()+
    annotate("text", x=full_data[total_clusters==x]$centroid_x_tot,
             y=full_data[total_clusters==x]$centroid_y_tot,
             label=full_data[total_clusters==x]$label_tot)+
    labs(x = "Total weeks out of test kits",
         y = "Total weeks out of ARVs", color='Clusters',
         title="Total weeks out of test kits and ARVs, 2014 - 2019",
         subtitle=paste0('Number of clusters = ', x))+
    theme(text=element_text(size=18))
  
  i = i+1 }

#----------------------------
# print a pdt of plots

pdf(paste0(dir, 'k_means_outputs/final/total_percent_arv_tests_2014_2019.pdf'),height=9, width=18)

grid.arrange(elbow_df, sil_df, nrow=1)
for(i in seq(length(list_of_plots))) {
  p = list_of_plots_perc[[i]]
  p2 = list_of_plots_tot[[i]]
  grid.arrange(p, p2, sil_df, nrow=1)
}

dev.off()

#----------------------------
# create a 3d graph for visualization 

plot_ly(full_data[total_clusters==4],
        x = ~percent_tests, y = ~percent_arvs, z = ~test_kits, color = ~factor(kcluster),
        colors = brewer.pal(9, 'RdYlBu')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = '% Test kits'),
                      yaxis = list(title = '% ARVs'),
                      zaxis = list(title = 'Weeks out of tests')))

#----------------------------
# SCALED MODEL

#----------------------------------------
# create a matrix for cluster analysis

dt_k_scale = dt[ ,.(tests_scaled, arvs_scaled, per_tests_scaled, 
                    per_arvs_scaled, tests_slope_scaled, 
                  arvs_slope_scaled)]

#----------------------------------------
# calculate elbow plots and silhouette widths and plot

# calculate using sourced functions
elbow = elbow_fun(dt_k_scale, 2, 10)
sil = sil_fun(dt_k_scale, 2, 10)

# ----------------------
# plot the elbow plot

elbow_df = ggplot(elbow, aes(x=k, y=tot_withinss))+
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(y = "Total within-cluster sum of squares", x = "K Clusters",
       title='Elbow plot to empirically determine k clusters',
       subtitle='Scaled total, percent, and slope: ARVs and test kits (2014 - 2019)')+
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

list_of_plots_scaled_slope = NULL
list_of_plots_scaled_tot = NULL
i = 1

# function to run the calculations for every cluster
for (x in c(2:10)) {
  # run a test cluster
  k_clust = kmeans(dt_k, centers = x)
  dt[ , kcluster:=k_clust$cluster]
  
  # mark the centroids for labeling
  dt[ ,centroid_x_tot_s:=mean(tests_scaled, na.rm=T), by=kcluster]
  dt[ ,centroid_y_tot_s:=mean(arvs_scaled, na.rm=T), by=kcluster]
  dt[ ,label_tot_s:=paste0(round(centroid_x_tot_s), ", ", round(centroid_y_tot_s)), by=kcluster]
  
  # mark the centroids for labeling
  dt[ ,centroid_x_s:=mean(tests_slope_scaled, na.rm=T), by=kcluster]
  dt[ ,centroid_y_s:=mean(arvs_slope_scaled, na.rm=T), by=kcluster]
  dt[ ,label_slope_s:=paste0(round(centroid_x_s), ", ", round(centroid_y_s)), by=kcluster]
  
  # rbind the data 
  interim_data = copy(dt)
  interim_data[ , total_clusters:=x]
  if (i ==1) full_data = interim_data
  if (1 < i) full_data = rbind(full_data, interim_data)
  
  #----------------------------
  # create the plots of the percent of time out
  list_of_plots_scaled_tot[[i]] = ggplot(full_data[total_clusters==x],
                              aes(x=tests_scaled, y=arvs_scaled, color=factor(kcluster)))+
    geom_jitter(alpha=0.6)+
    theme_bw()+
    annotate("text", x=full_data[total_clusters==x]$centroid_x_s,
             y=full_data[total_clusters==x]$centroid_y_s,
             label=full_data[total_clusters==x]$label_tot_s)+
    labs(x = "Weeks out of test kits, scaled",
         y = "Weeks out of ARVs, scaled", color='Clusters',
         subtitle=paste0('Number of clusters = ', x))+
    theme(text=element_text(size=18))
  
  list_of_plots_scaled_slope[[i]] = ggplot(full_data[total_clusters==x],
                                  aes(x=tests_slope_scaled, y=arvs_slope_scaled,
                                      color=factor(kcluster)))+
    geom_jitter(alpha=0.6)+
    theme_bw()+
    annotate("text", x=full_data[total_clusters==x]$centroid_x_s,
             y=full_data[total_clusters==x]$centroid_y_s,
             label=full_data[total_clusters==x]$label_slope_s)+
    labs(x = "Slope of test kits",
         y = "Slope of ARVs", color='Clusters',
         subtitle=paste0('Number of clusters = ', x))+
    theme(text=element_text(size=18))
  
  i = i+1 }

#----------------------------
# print a pdt of plots

pdf(paste0(dir, 'k_means_outputs/final/total_percent_slope_arv_tests_all_scaled_2014_2019.pdf'),height=9, width=18)

grid.arrange(elbow_df, sil_df, nrow=1)
for(i in seq(length(list_of_plots))) {
  p = list_of_plots_scaled_slope[[i]]
  p2 = list_of_plots_scaled_tot[[i]]
  grid.arrange(p, p2, sil_df, nrow=1)
}

dev.off()

#----------------------------
# create a 3d graph for visualization - scaled

plot_ly(full_data[total_clusters==4],
        x = ~arvs_scaled, y = ~tests_scaled, z = ~tests_slope_scaled, color = ~factor(kcluster),
        colors = brewer.pal(9, 'RdYlBu')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Test kits, scaled'),
                      yaxis = list(title = 'ARVs, scaled'),
                      zaxis = list(title = 'Test kit slope, scaled')))

#----------------------------
# create a 3d graph for visualization - percentages and slops

plot_ly(full_data[total_clusters==4],
        x = ~per_arvs_scaled, y = ~per_tests_scaled, z = ~tests_slope_scaled, color = ~factor(kcluster),
        colors = brewer.pal(9, 'RdYlBu')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = '% out of test kits, scaled'),
                      yaxis = list(title = '% out of ARVs, scaled'),
                      zaxis = list(title = 'Test kit slope, scaled')))

#----------------------------
