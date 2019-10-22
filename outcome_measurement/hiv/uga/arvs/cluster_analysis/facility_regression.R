# to create slopes

# K-means cluster analysis
# Plots to determine number of clusters and variables to use

# Caitlin O'Brien-Carelli
# 10/9/2019

# ----------------------
# Set up R
rm(list=ls())
library(ggplot2)
library(rgdal)
library(dplyr)
library(RColorBrewer)
library(plyr)
library(data.table)
library(cluster)
library(gridExtra)
library(purrr)
# ----------------------
# home drive 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# data directory
dir = paste0(j,  '/Project/Evaluation/GF/outcome_measurement/uga/arv_stockouts/')

# working directory to aggregate
dt = readRDS(paste0(dir, 'prepped_data/arv_stockouts_2013_2019.rds'))
# ----------------------
# source the functions for elbow plots and silhouette width
source('C:/Users/ccarelli/local/gf/outcome_measurement/hiv/uga/arvs/cluster_analysis/cluster_functions.R')

# ----------------------
# subset to annual totals and percents

# calculate if the facility reported 
dt[!is.na(test_kits), reported_tests:=TRUE]
dt[is.na(reported_tests), reported_tests:=FALSE]
dt[!is.na(arvs), reported_arvs:=TRUE]
dt[is.na(reported_arvs), reported_arvs:=FALSE]

# total test kits and total arvs 
dt = dt[ ,.(test_kits=sum(test_kits, na.rm=T), 
            arvs=sum(arvs, na.rm=T), reported_tests=sum(reported_tests),
            reported_arvs=sum(reported_arvs)), 
         by=.(facility, level,region, year)]


# -------------------------
# calculate the slopes per facility

for (f in unique(dt$facility)) {
  model = lm(test_kits~year, data=dt[facility==f])
  dt[facility==f, test_slope:=coef(model)[[2]]]
}

for (f in unique(dt$facility)) {
  model = lm(arvs~year, data=dt[facility==f])
  dt[facility==f, arv_slope:=coef(model)[[2]]]
}

# -------------------------

# create a matrix for cluster analysis
df_k = dt[ ,.(test_kits, arvs, test_slope, arv_slope)]

# ----------------------
# create elbow plots and silhouette widths

df_elbow = elbow_fun(df_k, 2, 10)
df_sil = sil_fun(df_k, 2, 10)

# ----------------------
# plot the elbow plot 

elbow_df = ggplot(df_elbow, aes(x=k, y=tot_withinss))+
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(y = "Total within-cluster sum of squares", x = "K Clusters",
       title='Elbow plot to empirically determine k clusters',
       subtitle='Variables: % of reporting weeks out of ARVs,
       % of reporting weeks out of tests (2014 - 2018)')+
  theme(text=element_text(size=18))

# ----------------------
# plot the silhouette plot

sil_df = ggplot(df_sil, aes(x=k, y=sil_width))+
  geom_point()+
  geom_line()+
  theme_bw() +
  labs(x='K Clusters', y='Average silhouette width', 
       title='Silhouette Width to determine k clusters')+
  theme(text=element_text(size=18))

#------------------------------------
# determine the number of ideal clusters and plot
# in this case, elbow plot indicates three
# or the sw indicated 2, 4, 3, 9, 10, 8 (order of widths)

# sequence of clusters to evaluate
clusts_df = c(2, 3, 4, 5, 9, 10)

#----------------------
# create plots for each 
list_of_plots_perc = NULL
i = 1

# function to run the calculations for every cluster 
for (x in clusts_df) {
  # run a test cluster
  k_clust = kmeans(df_k, centers = x)
  df[ , kcluster:=k_clust$cluster]
  
  # mark the centroids for labeling 
  df[ ,centroid_x:=mean(test_kits, na.rm=T), by=kcluster]
  df[ ,centroid_y:=mean(arvs, na.rm=T), by=kcluster]
  df[ ,label:=paste0(round(centroid_x), ", ", round(centroid_y)), by=kcluster]
  
  interim_data = copy(df)
  interim_data[ , total_clusters:=x]
  if (i ==1) full_data = interim_data
  if (1 < i) full_data = rbind(full_data, interim_data)
  
  #----------------------------
  # create the plots
  list_of_plots_perc[[i]] = ggplot(full_data[total_clusters==x], 
                                   aes(x=percent_tests, y=percent_arvs, color=factor(kcluster)))+
    geom_jitter(alpha=0.6)+
    theme_bw()+
    annotate("text", x=full_data[total_clusters==x]$centroid_x,
             y=full_data[total_clusters==x]$centroid_y, 
             label=full_data[total_clusters==x]$label)+
    labs(x = "Weeks out of test kits", 
         y = "Weeks out of ARVs", color='Clusters',
         subtitle=paste0('Number of clusters = ', x))+
    theme(text=element_text(size=18))
  
  i = i+1 }
#----------------------------
# print a pdf of plots 

pdf(paste0(dir, 'k_means_outputs/final/total_arvs_tests_slope_2013_2019.pdf'),height=9, width=18)

grid.arrange(elbow_df, sil_df, nrow=1)
for(i in seq(length(list_of_plots_perc))) { 
  p = list_of_plots_perc[[i]]
  grid.arrange(p, sil_df, nrow=1)
} 

dev.off()

#----------------------------



#-------------------------------------
# calculate percent of time 
dt[ , c('test_slope', 'arv_slope'):=NULL]
dt[ , percent_tests:=round(100*(test_kits/reported_tests), 1)]
dt[ , percent_arvs:=round(100*(arvs/reported_arvs), 1)]

# -------------------------
# calculate the slopes per facility

for (f in unique(dt$facility)) {
  model = lm(percent_tests~year, data=dt[facility==f])
  dt[facility==f, test_slope:=coef(model)[[2]]]
}

for (f in unique(dt$facility)) {
  model = lm(percent_arvs~year, data=dt[facility==f])
  dt[facility==f, arv_slope:=coef(model)[[2]]]
}

# create a matrix for cluster analysis
df_k = dt[ ,.(percent_tests, percent_arvs, test_slope, arv_slope)]

#----------------------------------------

#----------------------
# create plots for each 
list_of_plots_perc = NULL
i = 1

# function to run the calculations for every cluster 
for (x in clusts_df) {
  # run a test cluster
  k_clust = kmeans(df_k, centers = x)
  df[ , kcluster:=k_clust$cluster]
  
  # mark the centroids for labeling 
  df[ ,centroid_x:=mean(percent_tests, na.rm=T), by=kcluster]
  df[ ,centroid_y:=mean(percent_arvs, na.rm=T), by=kcluster]
  df[ ,label:=paste0(round(centroid_x), ", ", round(centroid_y)), by=kcluster]
  
  interim_data = copy(df)
  interim_data[ , total_clusters:=x]
  if (i ==1) full_data = interim_data
  if (1 < i) full_data = rbind(full_data, interim_data)
  
  #----------------------------
  # create the plots
  list_of_plots_perc[[i]] = ggplot(full_data[total_clusters==x], 
                                   aes(x=percent_tests, y=percent_arvs, color=factor(kcluster)))+
    geom_jitter(alpha=0.6)+
    theme_bw()+
    annotate("text", x=full_data[total_clusters==x]$centroid_x,
             y=full_data[total_clusters==x]$centroid_y, 
             label=full_data[total_clusters==x]$label)+
    labs(x = "Percent of weeks out of test kits", 
         y = "Percent of weeks out of ARVs", color='Clusters',
         title="Percent of reporting weeks out of test kits and ARVs, 2013 - 2019",
         caption = "Percentage is equal to total weeks out/total weeks reported per facility",
         subtitle=paste0('Number of clusters = ', x))+
    theme(text=element_text(size=18))
  
  i = i+1 }
#----------------------------
# print a pdf of plots 

pdf(paste0(dir, 'k_means_outputs/percent_arv_tests_slope_2013_2019.pdf'),height=9, width=18)

grid.arrange(elbow_df, sil_df, nrow=1)
for(i in seq(length(list_of_plots_perc))) { 
  p = list_of_plots_perc[[i]]
  grid.arrange(p, sil_df, nrow=1)
} 

dev.off()

#----------------------------









