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
# ----------------------
# home drive 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# data directory
dir = paste0(j,  '/Project/Evaluation/GF/outcome_measurement/uga/arv_stockouts/')

# working directory to aggregate
dt = readRDS(paste0(dir, 'arv_stockouts_2013_2018.rds'))

# subset dates to before November 2018
dt = dt[year!=2013] 

# ----------------------

arv = dt[art_site==T, .(test=sum(test_kits, na.rm=T), arv=sum(arvs, na.rm=T)), 
         by=.(facility)]

# calculate the euclidean distances between the values
arv_dist = dist(arv, method = 'euclidean')

# create a cluster object and assign the clusters
hc_arv = hclust(arv_dist, method='complete')
clusters = cutree(hc_arv, k = 10)

# append the clusters 
arv[ ,cluster:=clusters]

ggplot(arv, aes(x=test, y=arv, color=factor(cluster))) +
  geom_jitter(size=3, alpha=0.6) +
  # scale_color_manual(values=c('#b2182b',  '#67a9cf', '#af8dc3', '#2166ac',  '#ef8a62'))+
  theme_bw() + 
  theme(text = element_text(size=18)) +
  labs(x="Weeks out of test kits", y='Weeks out of ARVs',
       color='Cluster') 

ggplot(arv, aes(x=test, y=arv, color=factor(cluster))) +
  geom_point(size=2) +
  scale_color_manual(values=brewer.pal(4, 'RdBu'))+
  theme_bw() + 
  theme(text = element_text(size=18)) +
  labs(x="Weeks out of test kits", y='Weeks out of ARVs',
       color='Cluster') 



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




