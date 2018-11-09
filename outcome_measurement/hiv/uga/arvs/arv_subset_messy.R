# Subset of graphs for IDRC
# 


#------------------------------------------------
# PDF VISUALS 

pdf(paste0(dir, '/outputs/stockout_descriptives_subset.pdf'), height=6, width=9)

#----------------------------------------
# reporting completeness graphs

# count of facilities and art sites reporting
ggplot(report[ratio==FALSE], aes(x=date, y=value, color=variable, group=variable)) +
  geom_point() +
  geom_line() +
  geom_line() +
  theme_bw() +
  facet_wrap(~indicator) +
  labs(x='Date', y='Number of health facilities', title='Number of health facilities and ART sites reporting stock out information',
       subtitle='2017 - September 2018', color="")


#-----------------------------------
# arv stockout graphs 

# arv stockout counts
ggplot(arv[variable!='Percentage of ART sites stocked out of ARVs'], aes(x=date, y=value, color=variable, group=variable)) +
  geom_point() +
  geom_line() +
  geom_line() +
  theme_bw() +
  labs(title='Number of ART sites that were stocked out of ARVs in a given week', 
       y='Number of facilities', x='Date', color="")

# percentage of art sites that reported that were stocked out
ggplot(arv[variable=='Percentage of ART sites stocked out of ARVs'], aes(x=date, y=value)) +
  geom_point() +
  geom_line() +
  geom_line() +
  theme_bw() +
  facet_wrap(~variable, scales='free_y') +
  labs(title='Percentage of ART sites that were stocked out of ARVs in a given week', 
       x='Number of facilities', y='%')

# stacked bar of weeks stocked out 
ggplot(arv_weeks2[weeks!=0 ], aes(x=weeks, y=facilities, fill=factor(year))) + 
  geom_bar(stat='identity', position='dodge') +
  theme_minimal() +
  labs(title = "Facilities stocked out of ARVs for at least one week by total weeks stocked out", 
       subtitle='Same time period: January - September', x='Number of weeks out of stock*', 
       y="Number of facilities", caption="*Does not include facilities stocked out for 0 weeks", fill='Year')

#-----------------------
# ARV stockout maps 

# map of facility-weeks of stock outs 
ggplot(arv_map, aes(x=long, y=lat, group=group, fill=weeks)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) + 
  theme_void() +
  labs(title="Total facility-weeks of ARV stockouts by district, Uganda", caption="Source: HMIS", 
       subtitle='Same time period: January - September',fill="Facility-weeks") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# number of weeks of stockout divided by art sites reporting 
ggplot(arv_map_norm, aes(x=long, y=lat, group=group, fill=mean_weeks)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
  theme_void() +
  labs(title="Mean number of weeks stocked out of ARVs per ART site by district", caption="Source: HMIS", 
       subtitle='Same time period: January - September',fill="Mean weeks per facility") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# rate of change 
ggplot(roc_map, aes(x=long, y=lat, group=group, fill=change)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=brewer.pal(9, 'Purples')) + 
  theme_void() +
  labs(title="Rate of change: facility-weeks of ARV stockouts in 2018 minus 2017", caption="Source: HMIS", 
       subtitle='Same time period: January - September',fill="Difference in weeks (2018 - 2017)") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# percentage of weeks stocked out
ggplot(stock, aes(x=long, y=lat, group=group, fill=percent_out)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=rev(ratio_colors)) + 
  theme_void() +
  labs(title="Percentage of facility-weeks stocked out of ARVs", subtitle="Weeks ART sites were stocked out/Total weeks in which ART sites reported", 
       caption='Source: HMIS', fill="% of weeks stocked out") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 


#------------------------------
# test kits

# test kit stockout counts
ggplot(test[variable!='Percentage of facilities stocked out of test kits'], aes(x=date, y=value, color=variable, group=variable)) +
  geom_point() +
  geom_line() +
  geom_line() +
  theme_bw() +
  labs(title='Number of facilities that were stocked out of HIV test kits in a given week', 
       y='Number of facilities', x='Date', color="")

# percentage of facilities that reported that were stocked out of test kits
ggplot(test[variable=='Percentage of facilities stocked out of test kits'], aes(x=date, y=value)) +
  geom_point() +
  geom_line() +
  geom_line() +
  theme_bw() +
  facet_wrap(~variable, scales='free_y') +
  labs(title='Percentage of facilities that were stocked out of HIV test kits in a given week', 
       x='Number of facilities', y='%')

# stacked bar of weeks stocked out 
ggplot(tk_weeks2[weeks!=0 ], aes(x=weeks, y=facilities, fill=factor(year))) + 
  geom_bar(stat='identity', position='dodge') +
  theme_minimal() +
  labs(title = "Facilities stocked out of HIV test kits for at least one week by total weeks stocked out", 
       subtitle='Same time period: January - September', x='Number of weeks out of stock*', 
       y="Number of facilities", caption="*Does not include facilities stocked out for 0 weeks", fill='Year')

#------------------------------------
# test kit maps

# map of facility-weeks of stock outs 
ggplot(tk_map, aes(x=long, y=lat, group=group, fill=weeks)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) + 
  theme_void() +
  labs(title="Total facility-weeks of test kit stockouts by district, Uganda", caption="Source: HMIS", 
       subtitle='Same time period: January - September',fill="Facility-weeks stocked out of tests") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# number of weeks of stockout divided by facilities reporting 
ggplot(tk_map_norm, aes(x=long, y=lat, group=group, fill=mean_weeks)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
  theme_void() +
  labs(title="Mean number of weeks stocked out of HIV test kits per facility by district", caption="Source: HMIS", 
       subtitle='Same time period: January - September',fill="Mean weeks per facility") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# rate of change 
ggplot(tk_roc_map, aes(x=long, y=lat, group=group, fill=change)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=brewer.pal(9, 'BuGn')) + 
  theme_void() +
  labs(title="Rate of change: facility-weeks of test kit stockout in 2018 minus 2017", caption="Source: HMIS", 
       subtitle='Same time period: January - September',fill="Difference in weeks (2018 - 2017)") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# percentage of weeks stocked out
ggplot(tk_stock, aes(x=long, y=lat, group=group, fill=percent_out)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=rev(ratio_colors)) + 
  theme_void() +
  labs(title="Percentage of facility-weeks stocked out", subtitle="Weeks stocked out at ART sites/Total weeks reporting from ART sites", 
       caption='Source: HMIS', fill="% of weeks stocked out") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 



#--------------------------------
# finale maps

# Number of weeks stocked out, categorical
ggplot(final[year==2018], aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~variable) +
  scale_fill_gradientn(colors=brewer.pal(9, 'YlGnBu')) + 
  theme_void() +
  labs(title="Number of facilities stocked out of ARVs by time stocked out, 2017", 
       subtitle="Cumulative: one month is equal to four weeks stocked out of ARVs", 
       caption='Source: HMIS', fill="Number of facilities") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# at least one stockout
ggplot(final[year==2017 & variable!='No stock outs reported'], aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~variable) +
  scale_fill_gradientn(colors=brewer.pal(9, 'YlGnBu')) + 
  theme_void() +
  labs(title="Number of facilities stocked out of ARVs by time stocked out, 2017", 
       subtitle="Minimum one week of stockout", 
       caption='Source: HMIS', fill="Number of facilities") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 


# Number of weeks stocked out, categorical
ggplot(final[year==2018], aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~variable) +
  scale_fill_gradientn(colors=brewer.pal(9, 'YlOrBr')) + 
  theme_void() +
  labs(title="Number of facilities stocked out of ARVs by time stocked out, 2018", 
       subtitle="Cumulative: one month is equal to four weeks stocked out of ARVs", 
       caption='Source: HMIS', fill="Number of facilities") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# at least one stockout
ggplot(final[year==2018 & variable!='No stock outs reported'], aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~variable) +
  scale_fill_gradientn(colors=brewer.pal(9, 'YlOrBr')) + 
  theme_void() +
  labs(title="Number of facilities stocked out of ARVs by time stocked out, 2018", 
       subtitle="Minimum one week of stockout", 
       caption='Source: HMIS', fill="Number of facilities") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 


#---------------------------
dev.off()

#------------------------------

# descriptive stats

x = dt[month!='2017-10-01' & month!='2017-11-01' & month!='2017-12-01' &  art_site==TRUE, .(weeks=sum(arvs, na.rm=T)), by=.(year, facility)]

x[weeks > 4, length(unique(facility)), by=year]

# mean weeks stocked out
dt[year==2018 & art_site==TRUE, .(arvs=sum(arvs, na.rm=T), length(unique(facility)))]
dt[year==2017 & month < '2017-10-01' & art_site==TRUE, .(arvs=sum(arvs, na.rm=T), length(unique(facility)))]

out = dt[year==2018 & art_site==TRUE, .(arvs=sum(arvs, na.rm=T), length(unique(facility))), by=district]
out[ , ratio:=arvs/V2]
View(out)


dt[year==2018 & art_site==TRUE, .(arvs=sum(arvs, na.rm=T), length(unique(facility)))]
dt[year==2017 & month < '2017-10-01' & art_site==TRUE, .(arvs=sum(arvs, na.rm=T), length(unique(facility)))]

out = dt[year==2018 & art_site==TRUE, .(arvs=sum(arvs, na.rm=T), length(unique(facility))), by=district]
out[ , ratio:=arvs/V2]

setnames(out, c('arvs', 'V2', 'ratio'), c('weeks_stocked_out_of_ARVs', 'art_sites_reporting', 'mean_weeks_stocked_out_per_site'))




# mean weeks stocked out
dt[year==2018, .(tks=sum(test_kits, na.rm=T), length(unique(facility)))]
dt[year==2017 & month < '2017-10-01', .(tks=sum(test_kits, na.rm=T), length(unique(facility)))]

out2 = dt[year==2018 , .(tks=sum(test_kits, na.rm=T), length(unique(facility))), by=district]
out2[ , ratio:=tks/V2]
setnames(out2, c('tks', 'V2', 'ratio'), c('weeks_stocked_out_of_test_kits', 'facilities_reporting', 'mean_weeks_stocked_out_per_facility'))


dt[year==2018 & art_site==TRUE, .(arvs=sum(arvs, na.rm=T), length(unique(facility)))]
dt[year==2017 & month < '2017-10-01' & art_site==TRUE, .(arvs=sum(arvs, na.rm=T), length(unique(facility)))]

out = dt[year==2018 & art_site==TRUE, .(arvs=sum(arvs, na.rm=T), length(unique(facility))), by=district]
out[ , ratio:=arvs/V2]
View(out)

out = merge(out, out2, by='district')

write.csv(out, paste0(dir, 'mean_weeks_stocked_out.csv'))





