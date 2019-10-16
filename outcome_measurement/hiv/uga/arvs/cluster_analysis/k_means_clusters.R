# K-means cluster analysis
# Plots to determine number of clusters and variables to use

# Caitlin O'Brien-Carelli
# 10/9/2019

# ----------------------
# Set up R
rm(list=ls())
library(ggplot2)
library(rgdal)
library(tibble)
library(dplyr)
library(RColorBrewer)
library(plyr)
library(data.table)
library(dummies)
library(dendextend)
library(purrr)
library(cluster)
library(gridExtra)
# ----------------------
# home drive 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# data directory
dir = paste0(j,  '/Project/Evaluation/GF/outcome_measurement/uga/arv_stockouts/')

# working directory to aggregate
dt = readRDS(paste0(dir, 'prepped_data/arv_stockouts_2013_2019.rds'))

# subset dates to before November 2018
dt = dt[year!=2013] 

# turn off scientific notation
options(scipen=999)

# ----------------------
# source the functions for elbow plots and silhouette width

source('C:/Users/ccarelli/local/gf/outcome_measurement/hiv/uga/arvs/cluster_analysis/cluster_functions.R')

# ---------------------------------------------------
# CLUSTERING TOTAL TEST KIT AND TOTAL ARV STOCK OUTS

# total arv and total test kit stock outs among all health facilitird
# 2014 - 2018 (max 2018 weeks out)
both = dt[ ,.(test_kits=sum(test_kits, na.rm=T), 
              arvs=sum(arvs, na.rm=T)), 
              by=.(facility, level, district, region)]

# create a matrix for cluster analysis
both_k = both[ ,.(test_kits, arvs)]

# ----------------------
# create elbow plots and silhouette widths

both_elbow = elbow_fun(both_k, 2, 10)
both_sil = sil_fun(both_k, 2, 10)

# ----------------------
# plot the elbow plot 

elbow_both = ggplot(both_elbow, aes(x=k, y=tot_withinss))+
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(y = "Total within-cluster sum of squares", x = "K Clusters",
       title='Elbow plot to empirically determine k clusters',
       subtitle='Variables: total time out of ARVs, total time of of tests (2014 - 2018)')+
  theme(text=element_text(size=18))

# ----------------------
# plot the silhouette plot

sil_both = ggplot(both_sil, aes(x=k, y=sil_width))+
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
clusts = c(2, 4, 3, 9, 10, 8)

#----------------------
# create plots for each 
list_of_plots = NULL
i = 1

# function to run the calculations for every cluster 
for (x in clusts) {
# run a test cluster
k_clust = kmeans(both_k, centers = x)
both[ , kcluster:=k_clust$cluster]

# mark the centroids for labeling 
both[ ,centroid_x:=mean(test_kits, na.rm=T), by=kcluster]
both[ ,centroid_y:=mean(arvs, na.rm=T), by=kcluster]
both[ ,label:=paste0(round(centroid_x), ", ", round(centroid_y)), by=kcluster]

interim_data = copy(both)
interim_data[ , total_clusters:=x]
if (i ==1) full_data = interim_data
if (1 < i) full_data = rbind(full_data, interim_data)

# create the plots

list_of_plots[[i]] = ggplot(full_data[total_clusters==x], 
                  aes(x=test_kits, y=arvs, color=factor(kcluster)))+
                  geom_jitter(alpha=0.6)+
                  theme_bw()+
                  annotate("text", x=full_data[total_clusters==x]$centroid_x,
                           y=full_data[total_clusters==x]$centroid_y, 
                           label=full_data[total_clusters==x]$label)+
                  labs(x = "Number of weeks out of test kits", 
                  y = "Number of weeks out of ARVs", color='Clusters',
         title="Total weeks out of test kits and ARVs, 2014 - 2018",
         subtitle =paste0('Number of clusters = ', x))+
    theme(text=element_text(size=18))
  
i = i+1 }

#----------------------------------
#----------------------------
# print a pdf of plots 

pdf(paste0(dir, 'k_means_outputs/k_total_arv_total_test_2014_2018.pdf'),height=9, width=18)

grid.arrange(elbow_both, sil_both, nrow=1)
for(i in seq(length(list_of_plots))) { 
  p = list_of_plots[[i]]
  grid.arrange(p, sil_both, nrow=1)
} 

dev.off()

#----------------------------

#--------------------------------------------
# time stocked out and mean duration - TEST KITS

# mean annual number of stock outs
# take the maximum value of sequentially counted stock outs per facility
test = dt[!is.na(tstock), .(total_stockouts = max(tstock, na.rm=T)), 
           by=.(facility, year, region)] # max can be 0 if no stock outs

# mean duration of stock outs
dur = dt[!is.na(tdur), .(dur_of_each = max(tdur, na.rm=T)),
         by=.(tstock, facility, year)]

# mean duration per region
dur = dur[ ,.(mean_duration=round(mean(dur_of_each), 1)),
           by=.(facility, year)]
test = merge(test, dur, by=c('facility', 'year'), all=T)
test[is.na(mean_duration), mean_duration:=0]

#----------
# diagnostic scatter plot to see the distribution

ggplot(test, aes(x=total_stockouts, y=mean_duration))+
  geom_jitter(alpha=0.2)+
  theme_bw()
#----------
# create a matrix for cluster analysis
test_k = test[ ,.(total_stockouts, mean_duration)]

#----------------------
# calculate elbow plots and silhouette widths 

test_elbow = elbow_fun(test_k, 2, 10)
test_df = sil_fun(test_k, 2, 10)
# ----------------------
# create an elbow plot comparing the clusters

elbow_test = ggplot(test_elbow, aes(x=k, y=tot_withinss))+
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(y = "Total within-cluster sum of squares", x = "K Clusters",
       title='Elbow plot to empirically determine k clusters')+
  theme(text=element_text(size=18))
# ----------------------
# plot the silhouette widths

sil_test = ggplot(test_df, aes(x=k, y=sil_width))+
  geom_point()+
  geom_line()+
  theme_bw() +
  labs(x='K Clusters', y='Average silhouette width', 
       title='Silhouette Width to determine k clusters')+
  theme(text=element_text(size=18))

# ----------------------
i = 1
list_of_plots = NULL
for (x in seq(2:12)) {

# run a test cluster
k_clust = kmeans(test_k, centers = x)
test[ , kcluster:=k_clust$cluster]

# mark the centroids for labeling 
test[ ,centroid_x:=mean(total_stockouts, na.rm=T), by=kcluster]
test[ ,centroid_y:=mean(mean_duration, na.rm=T), by=kcluster]
test[ ,label:=paste0(round(centroid_x), ", ", round(centroid_y)), by=kcluster]

# create a large data table with everything 
interim_data = copy(test)
interim_data[ , total_clusters:=x]
if (i ==1) full_data = interim_data
if (1 < i) full_data = rbind(full_data, interim_data)

list_of_plots[[i]] = ggplot(full_data[total_clusters==x], aes(x=total_stockouts, y=mean_duration, 
                    color=factor(kcluster)))+
    geom_jitter(alpha=0.6)+
    theme_bw()+
    annotate("text", x=full_data[total_clusters==x]$centroid_x, y=full_data[total_clusters==x]$centroid_y, 
             label=full_data[total_clusters==x]$label)+
    labs(x = "Total weeks out of test kits by year", 
       y = "Mean duration of each stockout by year", color='Clusters',
       title="Total weeks out of test kits and ARVs, 2014 - 2018",
       subtitle = paste0('Number of clusters = ', x))+
  theme(text=element_text(size=18))
i = i + 1

}
# ----------------------
#----------------------------
# print a pdf of plots 

pdf(paste0(dir, 'k_means_outputs/k_test_kit_stock_duration_2014_2018.pdf'),height=9, width=18)

grid.arrange(elbow_test, sil_test, nrow=1)
for(i in seq(length(list_of_plots))) { 
  p = list_of_plots[[i]]
  grid.arrange(p, sil_test, nrow=1)
} 

dev.off()

#----------------------------
#--------------------------------------------
# time stocked out and mean duration - ARVS 

# mean annual number of stock outs
# take the maximum value of sequentially counted stock outs per facility
art = dt[!is.na(astock), .(total_stockouts = max(astock, na.rm=T)), 
          by=.(facility, year, region, art_site)] # max can be 0 if no stock outs
art = art[art_site==T]

# mean duration of stock outs
dura = dt[!is.na(adur), .(dur_of_each = max(adur, na.rm=T)),
         by=.(astock, facility, year, art_site)]
dura = dura[art_site==T]

# mean duration per region
dura = dura[ ,.(mean_duration=round(mean(dur_of_each), 1)),
           by=.(facility, year)]
art = merge(art, dura, by=c('facility', 'year'), all=T)
art[is.na(mean_duration), mean_duration:=0]

#----------
# diagnostic scatter plot to see the distribution

ggplot(art, aes(x=total_stockouts, y=mean_duration))+
  geom_jitter(alpha=0.2)+
  theme_bw()

#----------
# create a matrix for cluster analysis
art_k = art[ ,.(total_stockouts, mean_duration)]

#----------------------
# calculate elbow plots and silhouette widths 

art_elbow = elbow_fun(art_k, 2, 10)
art_df = sil_fun(art_k, 2, 10)
# ----------------------
# create an elbow plot comparing the clusters

elbow_art = ggplot(art_elbow, aes(x=k, y=tot_withinss))+
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(y = "Total within-cluster sum of squares", x = "K Clusters",
       title='Elbow plot to empirically determine k clusters')+
  theme(text=element_text(size=18))

# ----------------------
# plot the silhouette widths

sil_art = ggplot(art_df, aes(x=k, y=sil_width))+
  geom_point()+
  geom_line()+
  theme_bw() +
  labs(x='K Clusters', y='Average silhouette width', 
       title='Silhouette Width to determine k clusters')+
  theme(text=element_text(size=18))

# ----------------------
i = 1
list_of_plots = NULL
for (x in seq(2:12)) {
  
  # run a test cluster
  k_clust = kmeans(art_k, centers = x)
  art[ , kcluster:=k_clust$cluster]
  
  # mark the centroids for labeling 
  art[ ,centroid_x:=mean(total_stockouts, na.rm=T), by=kcluster]
  art[ ,centroid_y:=mean(mean_duration, na.rm=T), by=kcluster]
  art[ ,label:=paste0(round(centroid_x), ", ", round(centroid_y)), by=kcluster]
  
  # create a large data table with everything 
  interim_data = copy(art)
  interim_data[ , total_clusters:=x]
  if (i ==1) full_data = interim_data
  if (1 < i) full_data = rbind(full_data, interim_data)
  
  list_of_plots[[i]] = ggplot(full_data[total_clusters==x], aes(x=total_stockouts, y=mean_duration, 
                                                                color=factor(kcluster)))+
    geom_jitter(alpha=0.6)+
    theme_bw()+
    annotate("text", x=full_data[total_clusters==x]$centroid_x, y=full_data[total_clusters==x]$centroid_y, 
             label=full_data[total_clusters==x]$label)+
    labs(x = "Total weeks out of ARVs by year", 
         y = "Mean duration of each stockout by year", color='Clusters',
         title="Total weeks out of test kits and ARVs, 2014 - 2018",
         subtitle = paste0('Number of clusters = ', x))+
    theme(text=element_text(size=18))
  i = i + 1
  
}
# ----------------------
#----------------------------
# print a pdf of plots 

pdf(paste0(dir, 
           'k_means_outputs/k_arv_stock_duration_2014_2018.pdf'),
    height=9, width=18)

grid.arrange(elbow_test, sil_test, nrow=1)
for(i in seq(length(list_of_plots))) { 
  p = list_of_plots[[i]]
  grid.arrange(p, sil_test, nrow=1)
} 

dev.off()

#----------------------------
#--------------------------------------------------
# final model 




#--------------------------------------------------









