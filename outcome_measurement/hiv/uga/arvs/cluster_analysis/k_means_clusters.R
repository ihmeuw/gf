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
# CLUSTERING TOTAL TEST KIT AND TOTAL ARV STOCK OUTS

# total arv and total test kit stock outs among all health facility
both = dt[ ,.(test_kits=sum(test_kits, na.rm=T), 
              arvs=sum(arvs, na.rm=T)), 
              by=.(facility, level, district, region)]
           
          
# create a matrix for cluster analysis
both_k = both[ ,.(test_kits, arvs)]

# ----------------------
# create an elbow plot comparing the clusters

withinss = map_dbl(1:10, function(k) {
  model = kmeans(x = both_k, center = k)
  model$tot.withinss
}) 

elbow_df = data.frame(
  k = 1:10, 
  tot_withinss = withinss)

elbow_both = ggplot(elbow_df, aes(x=k, y=tot_withinss))+
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(y = "Total within-cluster sum of squares", x = "K Clusters",
       title='Elbow plot to empirically determine k clusters')

# ----------------------
# conduct a silhouette analysis to compare the clusters

# calculate silhouette widths
sil_width = map_dbl(2:10, function(k) {
  model = pam(x = both_k, k = k)
  model$silinfo$avg.width
}) 

# create a data frame of silhouette widths
sil_df = data.frame(
  k = 2:10, 
  sil_width = sil_width)

# plot the silhouette widths
sil_both = ggplot(sil_df, aes(x=k, y=sil_width))+
  geom_point()+
  geom_line()+
  theme_bw() +
  labs(x='K Clusters', y='Average silhouette width')

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

for (x in clusts) {

# run a test cluster
k_clust = kmeans(both_k, centers = x)
both[ , kcluster:=k_clust$cluster]

# mark the centroids for labeling 
both[ ,centroid_x:=mean(test_kits, na.rm=T), by=kcluster]
both[ ,centroid_y:=mean(arvs, na.rm=T), by=kcluster]
both[ ,label:=paste0(round(centroid_x), ", ", round(centroid_y)), by=kcluster]


p2 = ggplot(both, aes(x=test_kits, y=arvs, color=factor(kcluster)))+
  geom_jitter(alpha=0.6)+
  theme_bw()+
  annotate("text", x=both$centroid_x, y=both$centroid_y, label=both$label)+
  labs(x = "Number of weeks out of test kits", 
       y = "Number of weeks out of ARVs", color='Clusters',
       title="Total weeks out of test kits and ARVs, 2014 - 2018",
       subtitle =paste0('Number of clusters = ', x))

  grid.arrange(elbow_both, sil_both, p2, nrow=1)
  
# reset the index
i = i+1
}


#--------------------------------------------

















# reset the names so they are not overwritten
names_og = c('kcluster', 'centroid_x', 'centroid_y', 'label')
names = paste0(names_og, as.character(x))
setnames(both, names_og, names)


for (x in length(list_of_plots)) {
  if(is.null(list_of_plots[[x]])) {next
  } else {print(paste("The plot", x))}
}


plots_to_print = NULL

for (x in clusts) {

}


















