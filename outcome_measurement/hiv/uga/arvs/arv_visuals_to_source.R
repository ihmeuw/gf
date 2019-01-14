#------------------------------------------------
# PDF VISUALS 
#----------------------------------------
# reporting completeness graphs

# 1 - count of facilities and art sites reporting
g1 = ggplot(report[ratio==FALSE], aes(x=date, y=value, color=variable, group=variable)) +
  geom_point(size=0.8) +
  geom_line() +
  geom_line() +
  theme_bw() +
  facet_wrap(~indicator) +
  scale_color_manual(values = brewer.pal(4, 'RdYlBu')) +
  labs(x='Date', y='Number of health facilities', title='Number of health facilities and ART sites reporting stock out information',
       subtitle='2014 - November 2018', color="")

# 2 - ratio of facilities reporting
g2 = ggplot(report[ratio==TRUE], aes(x=date, y=value, color=variable, group=variable)) +
  geom_point(size=0.8) +
  geom_line() +
  geom_line() +
  theme_bw() +
  facet_wrap(~variable) +
  labs(x='Date', y='% of facilities', title='Percentage of health facilities and ART sites reporting stock out information',
       subtitle='2014 - November 2018', color='% Reporting')

#-----------------------------------
# arv stockout graphs 

# 3 - arv stockout counts
g3 = ggplot(arv[variable!='Percentage of ART sites stocked out of ARVs'], aes(x=date, y=value, color=variable, group=variable)) +
  geom_line() +
  geom_line() +
  theme_bw() +
  labs(title='Number of ART sites that were stocked out of ARVs in a given week', 
       y='Number of facilities', x='Date', color="")

# 4  - arv stockout counts below a threshold
g4 = ggplot(arv_thresh[variable!='Percentage of ART sites stocked out of ARVs'], aes(x=date, y=value, color=variable, group=variable)) +
  geom_point(alpha=0.5, size=0.8) +
  geom_line() +
  geom_line() +
  theme_bw() +
  labs(title='Number of ART sites that were stocked out of ARVs in a given week', 
       subtitle = 'Weeks in which at least 50% of ART sites reported',
       y='Number of facilities', x='Date', color="")

# 5 - percentage of art sites that reported that were stocked out
g5 = ggplot(arv[variable=='Percentage of ART sites stocked out of ARVs'], aes(x=date, y=value)) +
  geom_point(size=0.5) +
  geom_line() +
  geom_line() +
  theme_bw() +
  facet_wrap(~variable, scales='free_y') +
  labs(title='Percentage of ART sites that were stocked out of ARVs in a given week', 
       x='Number of facilities', y='%')

#-----------------------------------
# arv stockout bar graphs
bar_color = brewer.pal(5, 'RdYlBu') 

# 6 - stacked bar of weeks stocked out 
g6 = ggplot(arv_weeks[weeks!=0], aes(x=weeks, y=facilities, fill=factor(year))) + 
  geom_bar(stat='identity', position='dodge') +
  theme_minimal() +
  scale_fill_manual(values=bar_color) +
  labs(title = "Facilities stocked out of ARVs for at least one week by total weeks stocked out", x='Number of weeks out of stock*', 
       y="Number of facilities", caption="*Does not include facilities stocked out for 0 weeks", 
       fill='Year (total weeks)',
       subtitle='2014 - November 2018')

# 7 - stacked bar of weeks stocked out 
g7 = ggplot(arv_weeks2[weeks!=0 ], aes(x=weeks, y=facilities, fill=factor(year))) + 
  geom_bar(stat='identity', position='dodge') +
  theme_minimal() +
  scale_fill_manual(values=bar_color) +
  labs(title = "Facilities stocked out of ARVs for at least one week by total weeks stocked out", 
       subtitle='Same time period: January - November', x='Number of weeks out of stock*', 
       y="Number of facilities", caption="*Does not include facilities stocked out for 0 weeks", fill='Year (total weeks)')


#-----------------------
# ARV stockout maps - 8:15

# map of facility-weeks of stock outs - 8
g8 = ggplot(arv_map, aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) + 
  theme_void() +
  labs(title="Total facility-weeks of ARV stockouts by district, Uganda", 
       caption="*A facility-week is defined as each week a facility reports", 
       subtitle='Same time period: January - November',fill="Facility-weeks*") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# mean weeks stocked out per facility - 9
g9 = ggplot(arv_map_norm, aes(x=long, y=lat, group=group, fill=mean_weeks)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
  theme_void() +
  labs(title="Mean number of weeks stocked out of ARVs per ART site by district", caption="Source: HMIS", 
       subtitle='Same time period: January - November',fill="Mean weeks per facility") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# rate of change in annual facility-weeks stocked out - 10
g10 = ggplot(roc_map, aes(x=long, y=lat, group=group, fill=change)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=rev(brewer.pal(9, 'RdYlBu'))) + 
  theme_void() +
  labs(title="Rate of change: facility-weeks of ARV stockouts in 2018 minus 2017", 
       caption="* Positive difference = more weeks stocked out in 2018 than 2017", 
       subtitle='Same time period: January - November',fill="Difference in weeks (2018 - 2017)*") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# facilities with more stockouts - 11
g11 = ggplot(roc_map_alt, aes(x=long, y=lat, group=group, fill=change)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=brewer.pal(9, 'Reds')) + 
  theme_void() +
  labs(title="Districts with more facility-weeks of ARV stockouts in 2018 than 2017 ", caption="The number of ART sites remained the same from 2017 to 2018", 
       subtitle='Same time period: January - November', fill="Difference in weeks (2018 - 2017)") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# percentage of weeks stocked out - 12
g12 = ggplot(stock, aes(x=long, y=lat, group=group, fill=percent_out)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path() + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) + 
  theme_void() +
  labs(title="Percentage of facility-weeks stocked out of ARVs", subtitle="Weeks ART sites were stocked out/Total weeks in which ART sites reported", 
       caption='Source: HMIS', fill="% of weeks stocked out") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# percentage of weeks stocked out, just 2017/18 - 13
g13 = ggplot(stock[year==2017 | year==2018], aes(x=long, y=lat, group=group, fill=percent_out)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) + 
  theme_void() +
  labs(title="Percentage of facility-weeks stocked out of ARVs", subtitle="Weeks ART sites were stocked out/Total weeks in which ART sites reported", 
       caption='Source: HMIS', fill="% of weeks stocked out") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# percentage of weeks stocked out, just 2017/18 - 13
g14 = ggplot(stock[year==2017 | year==2018], aes(x=long, y=lat, group=group, fill=percent_out)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) + 
  theme_void() +
  labs(title="Weeks ART sites were stocked out/Total weeks in which ART sites reported", 
       caption='Source: HMIS', fill="% of weeks stocked out") +
  theme(plot.title =element_text(size=16), strip.text.x = element_text(size=18), legend.text=element_text(size=14),  
        legend.title=element_text(size=14)) 

# number of weeks of stockout divided by facilities reporting, 2017/18 only - 14
g15 = ggplot(tk_map_norm[year==2017 | year==2018], aes(x=long, y=lat, group=group, fill=mean_weeks)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
  theme_void() +
  labs(title='Same time period: January - November',fill="Mean weeks per facility") +
  theme(plot.title = element_text(size=16), strip.text.x = element_text(size=18), legend.text=element_text(size=14),
        legend.title=element_text(size=14)) 

# comparison of stock outs - arvs and test kits - 15
g16 = ggplot(compare, aes(x=date, y=value, color=variable)) +
  geom_point(size=0.6) +
  geom_line() +
  geom_line() +
  theme_bw() +
  scale_color_manual(values=two) +
  labs(x='Date', y='Percent (%)', color="") +
  theme(plot.title = element_text(size=16), strip.text.x = element_text(size=18), legend.text=element_text(size=14)) 

#------------------------------
# TEST KIT GRAPHS

# test kits - 16:18

# test kit stockout counts - 16
g17 = ggplot(test[variable!='Percentage of facilities stocked out of test kits'], aes(x=date, y=value, color=variable, group=variable)) +
  geom_point(size=0.5, alpha=0.5) +
  geom_line() +
  geom_line() +
  theme_bw() +
  labs(title='Number of facilities that were stocked out of HIV test kits in a given week', 
       y='Number of facilities', x='Date', color="")

# percentage of facilities that reported that were stocked out of test kits - 17
g18 = ggplot(test[variable=='Percentage of facilities stocked out of test kits'], aes(x=date, y=value)) +
  geom_point(size = 0.5) +
  geom_line() +
  geom_line() +
  theme_bw() +
  facet_wrap(~variable, scales='free_y') +
  labs(title='Percentage of facilities that were stocked out of HIV test kits in a given week', 
       x='Number of facilities', y='%')

# comparison of stock outs - arvs and test kits - 18
g19 = ggplot(compare, aes(x=date, y=value, color=variable)) +
  geom_point(size=0.6) +
  geom_line() +
  geom_line() +
  theme_bw() +
  scale_color_manual(values=two) +
  labs(title='Percentage of facilities that were stocked out of HIV test kits in a given week', 
       x='Date', y='Percent (%)', color="")

#------------------------------
# stacked bar graphs - 19:20 

# stacked bar of weeks stocked out 
g20 = ggplot(tk_weeks, aes(x=weeks, y=facilities, fill=factor(year))) + 
  geom_bar(stat='identity', position='dodge') +
  theme_minimal() +
  scale_fill_manual(values=brewer.pal(5, 'Blues')) +
  labs(title = "Facilities stocked out of HIv test kits for at least one week by total weeks stocked out", x='Number of weeks out of stock*', 
       y="Number of facilities", caption="*Does not include facilities stocked out for 0 weeks", fill='Year',
       subtitle='January 2017 - November 2018')

# stacked bar of weeks stocked out 
g21 = ggplot(tk_weeks2[weeks!=0 ], aes(x=weeks, y=facilities, fill=factor(year))) + 
  geom_bar(stat='identity', position='dodge') +
  scale_fill_manual(values=brewer.pal(5, 'Greens'))+
  theme_minimal() +
  labs(title = "Facilities stocked out of HIV test kits for at least one week by total weeks stocked out", 
       subtitle='Same time period: January - November', x='Number of weeks out of stock*', 
       y="Number of facilities", caption="*Does not include facilities stocked out for 0 weeks", fill='Year')


#------------------------------------
# test kit maps - 21:26

# map of facility-weeks of stock outs 
g22 = ggplot(tk_map, aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) + 
  theme_void() +
  labs(title="Total facility-weeks of test kit stockouts by district, Uganda", caption="Source: HMIS", 
       subtitle='Same time period: January - November',fill="Facility-weeks stocked out of tests") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# number of weeks of stockout divided by facilities reporting 
g23 = ggplot(tk_map_norm, aes(x=long, y=lat, group=group, fill=mean_weeks)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
  theme_void() +
  labs(title="Mean number of weeks stocked out of HIV test kits per facility by district", caption="Source: HMIS", 
       subtitle='Same time period: January - November', fill="Mean weeks per facility") 

# number of weeks of stockout divided by facilities reporting, 2017/18 only
g24 = ggplot(tk_map_norm[year==2017 | year==2018], aes(x=long, y=lat, group=group, fill=mean_weeks)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
  theme_void() +
  labs(title="Mean number of weeks stocked out of HIV test kits per facility", 
       subtitle='Same time period: January - November',fill="Mean weeks per facility") +
  theme(plot.title=element_text(size=22), plot.caption=element_text(size=18)) 

# rate of change 
g25 = ggplot(tk_roc_map, aes(x=long, y=lat, group=group, fill=change)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=brewer.pal(9, 'BuGn')) + 
  theme_void() +
  labs(title="Rate of change: facility-weeks of test kit stockout in 2018 minus 2017", caption="Source: HMIS", 
       subtitle='Same time period: January - November',fill="Difference in weeks (2018 - 2017)") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# districts with more facility-weeks of stockouts
g26 = ggplot(tk_roc_map_alt, aes(x=long, y=lat, group=group, fill=change)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=brewer.pal(9, 'BuGn')) + 
  theme_void() +
  labs(title="Districts with more facility-weeks of test kit stockouts in 2018 than 2017 ", 
       subtitle='Same time period: January - September',
       fill="Difference in weeks (2018 - 2017)") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# percentage of weeks stocked out
g27 = ggplot(tk_stock, aes(x=long, y=lat, group=group, fill=percent_out)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
  theme_void() +
  labs(title="Percentage of facility-weeks stocked out", 
       subtitle="Weeks stocked out at ART sites/Total weeks reported on by ART sites", 
       caption='Source: HMIS', fill="% of weeks stocked out") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 


#--------------------------------
# facility level scatter plots - 27:29

# arv stockouts by level
g28 = ggplot(scatter[art_site==TRUE], aes(x=level2, y=arvs)) +
  geom_jitter(width=0.25, alpha=0.2) + theme_bw() + 
  labs(title='Weeks stocked out of ARVs by facility level (ART sites)', subtitle='2017 - 2018', x='Facility level',
       y='Weeks stocked out of ARVs')

# arv stockouts by level, year       
g29 = ggplot(scatter2[art_site==TRUE], aes(x=level2, y=arvs)) +
  geom_jitter(width=0.25, alpha=0.2) + 
  facet_wrap(~year) +
  labs(title='Weeks stocked out of ARVs by facility level (ART sites)', x='Facility level', 
       y='Weeks stocked out of ARVs', subtitle='Same time period: January - September') +
  theme_bw()

# test kit stockouts by level, year       
g30 = ggplot(scatter, aes(x=level2, y=test_kits)) +
  geom_jitter(width=0.25, alpha=0.5) + 
  labs(title='Weeks stocked out of HIV test kits by facility level', x='Facility level', 
       y='Weeks stocked out of HIV test kits') +
  theme_bw()

# test kit stockouts by level, year       
g31 = ggplot(scatter2, aes(x=level2, y=test_kits)) +
  geom_jitter(width=0.25, alpha=0.5) + 
  facet_wrap(~year) +
  labs(title='Weeks stocked out of HIV test kits by facility level', x='Facility level', 
       y='Weeks stocked out of HIV test kits', subtitle='Same time period: January - September') +
  theme_bw()

#--------------------------------
# finale maps - categorical stock outs - 30:33

# Number of weeks stocked out, categorical
g32 = ggplot(final[year==2018], aes(x=long, y=lat, group=group, fill=value)) + 
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
g33 = ggplot(final[year==2017 & variable!='No stock outs reported'], aes(x=long, y=lat, group=group, fill=value)) + 
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
g34 = ggplot(final[year==2018], aes(x=long, y=lat, group=group, fill=value)) + 
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
g35 = ggplot(final[year==2018 & variable!='No stock outs reported'], aes(x=long, y=lat, group=group, fill=value)) + 
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

#------------------------------


