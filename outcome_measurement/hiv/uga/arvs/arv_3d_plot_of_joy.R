library(plotly)
library(data.table)

j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# data directory
dir = paste0(j,  '/Project/Evaluation/GF/outcome_measurement/uga/arv_stockouts/')

# working directory to aggregate
dt = readRDS(paste0(dir, 'arv_stockouts_2013_2018.rds'))

# subset dates to before November 2018
dt = dt[year!=2013] 

dt[ ,both:=(art_site==T & test_kits==T & arvs==T)]

out = dt[art_site==T, .(test=sum(test_kits, na.rm=T), arv=sum(arvs, na.rm=T),
                        both=sum(both, na.rm=T)), by=.(facility)]

# visualizing 2, 3, or 7 clusters
k_clust4 = kmeans(calc, centers = 4)
out[ ,clust4:=k_clust4$cluster]

plot_ly(out, x = ~test, y = ~arv, z = ~both, color = ~factor(clust4), 
        colors = c('#BF382A', '#0C4B8E', '#feb24c', '#c51b8a')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Test kits'),
                      yaxis = list(title = 'ARVs'),
                      zaxis = list(title = 'Both')))

#-------------------------

arv = dt[art_site==T]
arv = arv[order(facility, date)]

# calculate the duration of stock outs
for (f in unique(arv$facility)) {
  arv[facility==f, count:=seq_len(.N), by=rleid(arvs)]
  arv[facility==f, group:=rleid(arvs)]
  arv[facility==f & arvs==F, group:=NA] 
  arv[facility==f & is.na(arvs), group:=NA] 
  arv[facility==f & !is.na(group), new_group:=rleid(group)]  
  
  arv[facility==f & !is.na(new_group), duration:=max(count), by=new_group]
  arv[ , count:=NULL]
}

arv[ ,group:=NULL]
setnames(arv, 'new_group', 'group')

# calculate the mean duration
durs = arv[ ,.(duration=unique(duration)), by=.(group, facility)]

arv[ ,.(total=sum(arvs, na.rm=T), 
          ]  




