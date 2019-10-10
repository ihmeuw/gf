# ARV stockouts by facility 
# visualize hierarchical clusters

# Caitlin O'Brien-Carelli
# 9/16/2019

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
# clsuter the arv data 

arv = dt[art_site==T, .(test=sum(test_kits, na.rm=T), arv=sum(arvs, na.rm=T)), 
         by=.(facility)]

# # subset for testing
# arv = arv[1:50]

# calculate the euclidean distances between the values
arv_dist = dist(arv, method = 'euclidean')

# create a cluster object by calculating the linkages and setting k clusters
hc_arv = hclust(arv_dist, method='complete')
clusters = cutree(hc_arv, h = 20)

# append the clusters 
arv[ ,cluster:=clusters]

# ----------------------
# visualize the hierarchical clusters 

ggplot(arv, aes(x=test, y=arv, color=factor(cluster))) +
  geom_jitter(size=3, alpha=0.6) +
  # scale_color_manual(values=c('#b2182b',  '#67a9cf', '#af8dc3', '#2166ac',  '#ef8a62'))+
  theme_bw() + 
  theme(text = element_text(size=18)) +
  labs(x="Weeks out of test kits", y='Weeks out of ARVs',
       color='Cluster') 

# plot the hclust object as a more complex dendrogram
dend = as.dendrogram(hc_arv)
dend_col = color_branches(dend, h = 20)
plot(dend_col)

# ----------------------
# run a k-means cluster analysis

karv = copy(arv)
karv[ , c('cluster', 'facility'):=NULL]
k_clust = kmeans(karv, centers = 5)
arv[ , kcluster:=k_clust$cluster]

arv[ ,centroid_x:=mean(test, na.rm=T), by=kcluster]
arv[ ,centroid_y:=mean(arv, na.rm=T), by=kcluster]

ggplot(arv, aes(x=test, y=arv, color=factor(kcluster)))+
  geom_jitter()+
  theme_bw()+
  annotate("text", x=arv$centroid_x, y=arv$centroid_y, label="Cluster!")+
  theme(text=element_text(size=18))+
  labs(x = "Number of weeks out of test kits", 
  y = "Number of weeks out of ARVs", color='Clusters')

# ----------------------
# create an elbow plot comparing the clusters

withinss = map_dbl(1:10, function(k) {
  model = kmeans(x = karv, center = k)
  model$tot.withinss
}) 

elbow_df = data.frame(
  k = 1:10, 
  tot_withinss = withinss)

ggplot(elbow_df, aes(x=k, y=tot_withinss))+
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(y = "Total within-cluster sum of squares", x = "K Clusters")+
  theme(text=element_text(size=18))
  

# ----------------------
# silhouetter analysis

sil = arv[ ,.(facility, test, arv)]
pam_sil = pam(sil, k = 2)
widths = pam_sil$silinfo$widths

# calculate the average silhouette width
pam_sil$silinfo$avg.width


sil[ ,width:=widths]

plot(silhouette(pam_sil))


sil_width = map_dbl(2:10, function(k) {
  model = pam(x = karv, k = k)
  model$silinfo$avg.width
}) 

sil_df = data.frame(
  k = 2:10, 
  sil_width = sil_width)

ggplot(sil_df, aes(x=k, y=sil_width))+
  geom_point()+
  geom_line()+
  theme_bw() +
  labs(x='K Clusters', y='Average silhouette width')+
  theme(text=element_text(size=18))


#--------------------------------------
# visualizing 2, 3, or 7 clusters
kv = copy(karv)

k_clust2 = kmeans(kv, centers = 2)
k_clust3 = kmeans(kv, centers = 3)
k_clust7 = kmeans(kv, centers = 7)
k_clust8 = kmeans(kv, centers = 8)

kv[ ,clust2:=k_clust2$cluster]
kv[ ,clust3:=k_clust3$cluster]
kv[ ,clust7:=k_clust7$cluster]
kv[ ,clust8:=k_clust8$cluster]

kv_long = melt(kv, id.vars=c('test', 'arv'))
kv_long$variable = factor(kv_long$variable, c('clust2', 'clust3', 'clust7', 'clust8'),
       c("Two clusters", "Three clusters", "Seven clusters", "Eight clusters"))

ggplot(kv_long, aes(x=test, y=arv, color=factor(value)))+
  geom_jitter()+
  facet_wrap(~variable)+
  theme_bw()+
  theme(text=element_text(size=18))+
  labs(color='Cluster', x='Weeks out of test kits', y="Weeks out of ARVs")


#-----------------------------------------

dt[ ,both:=(art_site==T & test_kits==T & arvs==T)]
out = dt[art_site==T, .(test=sum(test_kits, na.rm=T), arv=sum(arvs, na.rm=T),
             both=sum(both, na.rm=T)), by=.(facility)]

pam_sil = pam(out, k = 2)

# calculate the average silhouette width
pam_sil$silinfo$avg.width



sil_width = map_dbl(2:10, function(k) {
  model = pam(x = out, k = k)
  model$silinfo$avg.width
}) 

sil_df = data.frame(
  k = 2:10, 
  sil_width = sil_width)

ggplot(sil_df, aes(x=k, y=sil_width))+
  geom_point()+
  geom_line()+
  theme_bw() +
  labs(x='K Clusters', y='Average silhouette width')+
  theme(text=element_text(size=18))


#--------------------------------

calc = out[ ,.(test, arv, both)]

withinss2 = map_dbl(1:10, function(k) {
  model = kmeans(x = calc, center = k)
  model$tot.withinss
}) 

elbow_df = data.frame(
  k = 1:10, 
  tot_withinss = withinss2)

ggplot(elbow_df, aes(x=k, y=tot_withinss))+
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(y = "Total within-cluster sum of squares", x = "K Clusters")+
  theme(text=element_text(size=18))





# visualizing 2, 3, or 7 clusters
k_clust3 = kmeans(calc, centers = 3)
k_clust4 = kmeans(calc, centers = 4)
k_clust7 = kmeans(calc, centers = 7)

out[ ,clust3:=k_clust3$cluster]
out[ ,clust4:=k_clust4$cluster]
out[ ,clust7:=k_clust7$cluster]


kv_long = melt(out, id.vars=c('facility', 'test', 'arv', 'both'))
kv_long$variable = factor(kv_long$variable, c('clust3', 'clust4', 'clust7'),
                          c("Three clusters", "Four clusters", "Seven clusters"))

ggplot(kv_long, aes(x=test, y=arv, color=factor(value)))+
  geom_jitter()+
  facet_wrap(~variable)+
  theme_bw()+
  theme(text=element_text(size=18))+
  labs(color='Cluster', x='Weeks out of test kits', y="Weeks out of ARVs")

















ggplot(arv, aes(x=test, y=arv, color=factor(cluster))) +
  geom_point(size=2) +
  scale_color_manual(values=brewer.pal(6, 'RdBu'))+
  theme_bw() + 
  theme(text = element_text(size=18)) +
  labs(x="Weeks out of test kits", y='Weeks out of ARVs',
       color='Cluster') 


#---------------------------------
#dendograms

# plot the hclust objectas a denrogram
plot(hc_arv)

# plot the hclust object as a more complex dendrogram
dend = as.dendrogram(hc_arv)
dend_col = color_branches(dend, h = 10)
plot(dend_col)








five_clusters = cutree(hc_arv, k = 5)

# append the clusters 
arv[ ,cluster5:=five_clusters]

ggplot(arv, aes(x=test, y=arv, color=factor(cluster5))) +
  geom_jitter(size=2) +
  scale_color_manual(values=rev(c('#af8dc3', '#67a9cf', '#2166ac', '#b2182b', '#ef8a62')))+
  theme_bw() + 
  theme(text = element_text(size=18)) +
  labs(x="Weeks out of test kits", y='Weeks out of ARVs',
       color='Cluster') 









# # ----------------------
# # subset to only the arv stock out data and set up for measurement

jac = dt[art_site==T ,.(facility, arvs, date)][order(date)]

# create a week number variable, numbering weeks sequentially
dates = dt[ ,.(date=unique(date))][order(date)]
dates[ ,wk:=.I]
jac = merge(jac, dates, by='date', all=T)

# label the weeks for shaping wide and drop date
jac[ , week:=paste0('wk', wk)]
jac[ , c('date', 'wk'):=NULL]

# convert the logical to a numeric bc dcast is being weird
jac[arvs==T, value:=1]
jac[arvs==F, value:=0]
jac[ ,arvs:=NULL]

# create dummy variables for each week
jac = dcast(jac, facility~week, value=arvs)

# ----------------------
# calculate the distaces between health facilities

jac = jac[1:500]

# measure the distances between the facilities
jac_dist = dist(jac, method='binary')

# create a cluster object and assign the clusters
hc_jac = hclust(jac_dist, method='complete')
clusters = cutree(hc_jac, k = 5)

# append it back to the original data frame
jac[ ,cluster:=clusters]


jac_plot = jac[ , .(facility, cluster)]



ggplot(arv, aes(x=test, y=arv, color=factor(cluster))) +
  geom_jitter(size=3) +
  scale_color_manual(values=c('#b2182b',  '#67a9cf', '#af8dc3', '#2166ac',  '#ef8a62'))+
  theme_bw() +
  theme(text = element_text(size=18)) +
  labs(x="Weeks out of test kits", y='Weeks out of ARVs',
       color='Cluster')




