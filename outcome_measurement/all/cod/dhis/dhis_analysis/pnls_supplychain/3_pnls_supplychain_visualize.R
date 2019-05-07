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
 # facet_wrap(~indicator) +
  scale_color_manual(values = brewer.pal(4, 'RdYlBu')) +
  labs(x='Date', y='Number of health facilities', title=paste0('Number of facilities reporting supply chain information for ', drug_name),
       subtitle=full_date_range, color="")

# 2 - ratio of facilities reporting
g2 = ggplot(report[ratio==TRUE], aes(x=date, y=value, color=variable, group=variable)) +
  geom_point(size=0.8) +
  geom_line() +
  geom_line() +
  theme_bw() +
  #facet_wrap(~variable) +
  labs(x='Date', y='% of facilities', title=paste0('Percentage of health facilities reporting supply chain information for ', drug_name),
       subtitle=full_date_range, color='% Reporting')

#-----------------------------------
# arv stockout graphs 

# 3 - arv stockout counts
g3 = ggplot(drug_so[variable!=paste0('Percentage of facilities stocked out of ', drug_name)], aes(x=date, y=value, color=variable, group=variable)) +
  geom_line() +
  geom_line() +
  theme_bw() +
  labs(title=paste0('Number of facilities that were stocked out of ', drug_name, ' in a given month'), 
       y='Number of facilities', x='Date', color="")

# 4  - arv stockout counts below a threshold
g4 = ggplot(drug_so_thresh[variable!=paste0('Percentage of facilities stocked out of ', drug_name)], aes(x=date, y=value, color=variable, group=variable)) +
  geom_point(alpha=0.5, size=0.8) +
  geom_line() +
  geom_line() +
  theme_bw() +
  labs(title='Number of facilities that were stocked out of ', drug_name, ' in a given month', 
       subtitle = 'Weeks in which at least 50% of facilities reported',
       y='Number of facilities', x='Date', color="")

# 5 - percentage of art sites that reported that were stocked out
g5 = ggplot(drug_so[variable==paste0('Percentage of facilities stocked out of ', drug_name)], aes(x=date, y=value)) +
  geom_point(size=0.5) +
  geom_line() +
  geom_line() +
  theme_bw() +
  facet_wrap(~variable, scales='free_y') +
  labs(title=paste0('Percentage of facilities that were stocked out of ', drug_name, ' in a given month'), 
       x='Number of facilities', y='%')

#-----------------------------------
# arv stockout bar graphs
bar_color = brewer.pal(5, 'RdYlBu') 

# 6 - stacked bar of weeks stocked out 
g6 = ggplot(so_days[weeks_stocked_out!=0], aes(x=weeks_stocked_out, y=facilities, fill=factor(year))) + 
  geom_bar(stat='identity', position='dodge') +
  theme_minimal() +
  scale_fill_manual(values=bar_color) +
  labs(title = paste0("Facilities stocked out of ", drug_name, " for at least one week by total weeks stocked out"), x='Number of weeks out of stock*', 
       y="Number of facilities", caption="*Does not include facilities stocked out for 0 weeks", 
       fill='Year (total weeks)',
       subtitle=full_date_range)

# 7 - stacked bar of weeks_stocked_out stocked out 
g7 = ggplot(so_days2[weeks_stocked_out!=0 ], aes(x=weeks_stocked_out, y=facilities, fill=factor(year))) + 
  geom_bar(stat='identity', position='dodge') +
  theme_minimal() +
  scale_fill_manual(values=bar_color) +
  labs(title = paste0("Facilities stocked out of ", drug_name, " for at least one week by total weeks stocked out"),
       subtitle='Same time period: January - November', x='Number of weeks out of stock*', 
       y="Number of facilities", caption="*Does not include facilities stocked out for 0 weeks", fill='Year (total weeks)')


#-----------------------
# ARV stockout maps - 8:15

# map of facility-weeks of stock outs - 8
g8 = ggplot(so_map, aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) + 
  theme_void() +
  labs(title=paste0("Total facility-weeks of stockouts for ", drug_name, ", DRC"), 
       caption="*A facility-week is defined as each week a facility reports", 
       subtitle='Same time period: January - November',fill="Facility-weeks*") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# mean weeks stocked out per facility - 9
g9 = ggplot(so_map_norm, aes(x=long, y=lat, group=group, fill=mean_days)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
  theme_void() +
  labs(title="Mean number of weeks stocked out of ", drug_name, " by district", caption="Source: PNLS", 
       subtitle='Same time period: January - November',fill="Mean days per facility") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# rate of change in annual facility-weeks stocked out - 10
g10 = ggplot(roc_map, aes(x=long, y=lat, group=group, fill=change)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01) +
  scale_fill_gradientn(colors=rev(brewer.pal(9, 'RdYlBu'))) +
  theme_void() +
  labs(title=paste0("Rate of change: facility-weeks of ", drug_name, "stockouts in 2018 minus 2017"),
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
  labs(title=paste0("Districts with more facility-weeks of stockouts for ", drug_name, " in 2018 than 2017"), 
       caption="The number of facilities remained the same from 2017 to 2018",
       subtitle='Same time period: January - November', fill="Difference in weeks (2018 - 2017)") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6))

# # percentage of weeks stocked out - 12
# g12 = ggplot(stock, aes(x=long, y=lat, group=group, fill=percent_out)) + 
#   coord_fixed() +
#   geom_polygon() + 
#   geom_path() + 
#   facet_wrap(~year) +
#   scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) + 
#   theme_void() +
#   labs(title="Percentage of facility-weeks stocked out of ARVs", subtitle="Weeks ART sites were stocked out/Total weeks in which ART sites reported", 
#        caption='Source: HMIS', fill="% of weeks stocked out") +
#   theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 
# 
# # percentage of weeks stocked out, just 2017/18 - 13
# g13 = ggplot(stock[year==2017 | year==2018], aes(x=long, y=lat, group=group, fill=percent_out)) + 
#   coord_fixed() +
#   geom_polygon() + 
#   geom_path(size=0.01) + 
#   facet_wrap(~year) +
#   scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) + 
#   theme_void() +
#   labs(title="Percentage of facility-weeks stocked out of ARVs", subtitle="Weeks ART sites were stocked out/Total weeks in which ART sites reported", 
#        caption='Source: HMIS', fill="% of weeks stocked out") +
#   theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 
# 
# # percentage of weeks stocked out, just 2017/18 - 13
# g14 = ggplot(stock[year==2017 | year==2018], aes(x=long, y=lat, group=group, fill=percent_out)) + 
#   coord_fixed() +
#   geom_polygon() + 
#   geom_path(size=0.01) + 
#   facet_wrap(~year) +
#   scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) + 
#   theme_void() +
#   labs(title="Weeks ART sites were stocked out/Total weeks in which ART sites reported", 
#        caption='Source: HMIS', fill="% of weeks stocked out") +
#   theme(plot.title =element_text(size=16), strip.text.x = element_text(size=18), legend.text=element_text(size=14),  
#         legend.title=element_text(size=14)) 
# 
# # number of weeks of stockout divided by facilities reporting, 2017/18 only - 14
# g15 = ggplot(tk_map_norm[year==2017 | year==2018], aes(x=long, y=lat, group=group, fill=mean_weeks)) + 
#   coord_fixed() +
#   geom_polygon() + 
#   geom_path(size=0.01) + 
#   facet_wrap(~year) +
#   scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
#   theme_void() +
#   labs(title='Same time period: January - November',fill="Mean weeks per facility") +
#   theme(plot.title = element_text(size=16), strip.text.x = element_text(size=18), legend.text=element_text(size=14),
#         legend.title=element_text(size=14)) 
# 
# # comparison of stock outs - arvs and test kits - 15
# g16 = ggplot(compare, aes(x=date, y=value, color=variable)) +
#   geom_point(size=0.6) +
#   geom_line() +
#   geom_line() +
#   theme_bw() +
#   scale_color_manual(values=two) +
#   labs(x='Date', y='Percent (%)', color="") +
#   theme(plot.title = element_text(size=16), strip.text.x = element_text(size=18), legend.text=element_text(size=14)) 
# 
# #------------------------------
# # TEST KIT GRAPHS
# 
# # test kits - 16:18
# 
# # test kit stockout counts - 16
# g17 = ggplot(test[variable!='Percentage of facilities stocked out of test kits'], aes(x=date, y=value, color=variable, group=variable)) +
#   geom_point(size=0.5, alpha=0.5) +
#   geom_line() +
#   geom_line() +
#   theme_bw() +
#   labs(title='Number of facilities that were stocked out of HIV test kits in a given week', 
#        y='Number of facilities', x='Date', color="")
# 
# # percentage of facilities that reported that were stocked out of test kits - 17
# g18 = ggplot(test[variable=='Percentage of facilities stocked out of test kits'], aes(x=date, y=value)) +
#   geom_point(size = 0.5) +
#   geom_line() +
#   geom_line() +
#   theme_bw() +
#   facet_wrap(~variable, scales='free_y') +
#   labs(title='Percentage of facilities that were stocked out of HIV test kits in a given week', 
#        x='Number of facilities', y='%')
# 
# # comparison of stock outs - arvs and test kits - 18
# g19 = ggplot(compare, aes(x=date, y=value, color=variable)) +
#   geom_point(size=0.6) +
#   geom_line() +
#   geom_line() +
#   theme_bw() +
#   scale_color_manual(values=two) +
#   labs(title='Percentage of facilities that were stocked out of HIV test kits in a given week', 
#        x='Date', y='Percent (%)', color="")
# 
# #------------------------------
# # stacked bar graphs - 19:20 
# 
# # stacked bar of weeks stocked out 
# g20 = ggplot(tk_weeks, aes(x=weeks, y=facilities, fill=factor(year))) + 
#   geom_bar(stat='identity', position='dodge') +
#   theme_minimal() +
#   scale_fill_manual(values=brewer.pal(5, 'Blues')) +
#   labs(title = "Facilities stocked out of HIv test kits for at least one week by total weeks stocked out", x='Number of weeks out of stock*', 
#        y="Number of facilities", caption="*Does not include facilities stocked out for 0 weeks", fill='Year',
#        subtitle='January 2017 - November 2018')
# 
# # stacked bar of weeks stocked out 
# g21 = ggplot(tk_weeks2[weeks!=0 ], aes(x=weeks, y=facilities, fill=factor(year))) + 
#   geom_bar(stat='identity', position='dodge') +
#   scale_fill_manual(values=brewer.pal(5, 'Greens'))+
#   theme_minimal() +
#   labs(title = "Facilities stocked out of HIV test kits for at least one week by total weeks stocked out", 
#        subtitle='Same time period: January - November', x='Number of weeks out of stock*', 
#        y="Number of facilities", caption="*Does not include facilities stocked out for 0 weeks", fill='Year')
# 
# 
# #------------------------------------
# # test kit maps - 21:26
# 
# # map of facility-weeks of stock outs 
# g22 = ggplot(tk_map, aes(x=long, y=lat, group=group, fill=value)) + 
#   coord_fixed() +
#   geom_polygon() + 
#   geom_path(size=0.01) + 
#   facet_wrap(~year) +
#   scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) + 
#   theme_void() +
#   labs(title="Total facility-weeks of test kit stockouts by district, Uganda", caption="Source: HMIS", 
#        subtitle='Same time period: January - November',fill="Facility-weeks stocked out of tests") +
#   theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 
# 
# # number of weeks of stockout divided by facilities reporting 
# g23 = ggplot(tk_map_norm, aes(x=long, y=lat, group=group, fill=mean_weeks)) + 
#   coord_fixed() +
#   geom_polygon() + 
#   geom_path(size=0.01) + 
#   facet_wrap(~year) +
#   scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
#   theme_void() +
#   labs(title="Mean number of weeks stocked out of HIV test kits per facility by district", caption="Source: HMIS", 
#        subtitle='Same time period: January - November', fill="Mean weeks per facility") 
# 
# # number of weeks of stockout divided by facilities reporting, 2017/18 only
# g24 = ggplot(tk_map_norm[year==2017 | year==2018], aes(x=long, y=lat, group=group, fill=mean_weeks)) + 
#   coord_fixed() +
#   geom_polygon() + 
#   geom_path(size=0.01) + 
#   facet_wrap(~year) +
#   scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
#   theme_void() +
#   labs(title="Mean number of weeks stocked out of HIV test kits per facility", 
#        subtitle='Same time period: January - November',fill="Mean weeks per facility") +
#   theme(plot.title=element_text(size=22), plot.caption=element_text(size=18)) 
# 
# # rate of change 
# g25 = ggplot(tk_roc_map, aes(x=long, y=lat, group=group, fill=change)) + 
#   coord_fixed() +
#   geom_polygon() + 
#   geom_path(size=0.01) + 
#   scale_fill_gradientn(colors=brewer.pal(9, 'BuGn')) + 
#   theme_void() +
#   labs(title="Rate of change: facility-weeks of test kit stockout in 2018 minus 2017", caption="Source: HMIS", 
#        subtitle='Same time period: January - November',fill="Difference in weeks (2018 - 2017)") +
#   theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 
# 
# # districts with more facility-weeks of stockouts
# g26 = ggplot(tk_roc_map_alt, aes(x=long, y=lat, group=group, fill=change)) + 
#   coord_fixed() +
#   geom_polygon() + 
#   geom_path(size=0.01) + 
#   scale_fill_gradientn(colors=brewer.pal(9, 'BuGn')) + 
#   theme_void() +
#   labs(title="Districts with more facility-weeks of test kit stockouts in 2018 than 2017 ", 
#        subtitle='Same time period: January - September',
#        fill="Difference in weeks (2018 - 2017)") +
#   theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 
# 
# # percentage of weeks stocked out
# g27 = ggplot(tk_stock, aes(x=long, y=lat, group=group, fill=percent_out)) + 
#   coord_fixed() +
#   geom_polygon() + 
#   geom_path(size=0.01) + 
#   facet_wrap(~year) +
#   scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
#   theme_void() +
#   labs(title="Percentage of facility-weeks stocked out", 
#        subtitle="Weeks stocked out at ART sites/Total weeks reported on by ART sites", 
#        caption='Source: HMIS', fill="% of weeks stocked out") +
#   theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 
# 
# 
# #--------------------------------
# # facility level scatter plots - 27:29
# 
# # arv stockouts by level
# g28 = ggplot(scatter[art_site==TRUE], aes(x=level2, y=arvs)) +
#   geom_jitter(width=0.25, alpha=0.2) + theme_bw() + 
#   labs(title='Weeks stocked out of ARVs by facility level (ART sites)', subtitle='2017 - 2018', x='Facility level',
#        y='Weeks stocked out of ARVs')
# 
# # arv stockouts by level, year       
# g29 = ggplot(scatter2[art_site==TRUE], aes(x=level2, y=arvs)) +
#   geom_jitter(width=0.25, alpha=0.2) + 
#   facet_wrap(~year) +
#   labs(title='Weeks stocked out of ARVs by facility level (ART sites)', x='Facility level', 
#        y='Weeks stocked out of ARVs', subtitle='Same time period: January - September') +
#   theme_bw()
# 
# # test kit stockouts by level, year       
# g30 = ggplot(scatter, aes(x=level2, y=test_kits)) +
#   geom_jitter(width=0.25, alpha=0.5) + 
#   labs(title='Weeks stocked out of HIV test kits by facility level', x='Facility level', 
#        y='Weeks stocked out of HIV test kits') +
#   theme_bw()
# 
# # test kit stockouts by level, year       
# g31 = ggplot(scatter2, aes(x=level2, y=test_kits)) +
#   geom_jitter(width=0.25, alpha=0.5) + 
#   facet_wrap(~year) +
#   labs(title='Weeks stocked out of HIV test kits by facility level', x='Facility level', 
#        y='Weeks stocked out of HIV test kits', subtitle='Same time period: January - September') +
#   theme_bw()
# 
# #--------------------------------
# # finale maps - categorical stock outs - 30:33
# 
# # Number of weeks stocked out, categorical
# g32 = ggplot(final[year==2018], aes(x=long, y=lat, group=group, fill=value)) + 
#   coord_fixed() +
#   geom_polygon() + 
#   geom_path(size=0.01) + 
#   facet_wrap(~variable) +
#   scale_fill_gradientn(colors=brewer.pal(9, 'YlGnBu')) + 
#   theme_void() +
#   labs(title="Number of facilities stocked out of ARVs by time stocked out, 2017", 
#        subtitle="Cumulative: one month is equal to four weeks stocked out of ARVs", 
#        caption='Source: HMIS', fill="Number of facilities") +
#   theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 
# 
# # at least one stockout
# g33 = ggplot(final[year==2017 & variable!='No stock outs reported'], aes(x=long, y=lat, group=group, fill=value)) + 
#   coord_fixed() +
#   geom_polygon() + 
#   geom_path(size=0.01) + 
#   facet_wrap(~variable) +
#   scale_fill_gradientn(colors=brewer.pal(9, 'YlGnBu')) + 
#   theme_void() +
#   labs(title="Number of facilities stocked out of ARVs by time stocked out, 2017", 
#        subtitle="Minimum one week of stockout", 
#        caption='Source: HMIS', fill="Number of facilities") +
#   theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 
# 
# 
# # Number of weeks stocked out, categorical
# g34 = ggplot(final[year==2018], aes(x=long, y=lat, group=group, fill=value)) + 
#   coord_fixed() +
#   geom_polygon() + 
#   geom_path(size=0.01) + 
#   facet_wrap(~variable) +
#   scale_fill_gradientn(colors=brewer.pal(9, 'YlOrBr')) + 
#   theme_void() +
#   labs(title="Number of facilities stocked out of ARVs by time stocked out, 2018", 
#        subtitle="Cumulative: one month is equal to four weeks stocked out of ARVs", 
#        caption='Source: HMIS', fill="Number of facilities") +
#   theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 
# 
# # at least one stockout
# g35 = ggplot(final[year==2018 & variable!='No stock outs reported'], aes(x=long, y=lat, group=group, fill=value)) + 
#   coord_fixed() +
#   geom_polygon() + 
#   geom_path(size=0.01) + 
#   facet_wrap(~variable) +
#   scale_fill_gradientn(colors=brewer.pal(9, 'YlOrBr')) + 
#   theme_void() +
#   labs(title="Number of facilities stocked out of ARVs by time stocked out, 2018", 
#        subtitle="Minimum one week of stockout", 
#        caption='Source: HMIS', fill="Number of facilities") +
#   theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

#------------------------------


#--------------------------------------------------------------
#Make some maps and charts
#--------------------------------------------------------------
# •	Graph national time trends for each test kit variable. For example, a graph would be facet wrapped by stock category with scales=’free_y’. It doesn’t make sense to use stock category
#as color (all on the same graph) because they are in different units – days out of stock in days, stock available is in… stuff. So, we want to facet with a continuous y axis.

tt1 = ggplot(test_kits[element_id == "Gv1UQdMw5wL"], aes(x=date, y=value)) + 
  facet_wrap(~stock_category, scales='free_y')+
  geom_point() + geom_line() + 
  theme_bw() + 
  labs(title="Determine stock categories over time, with free_y scales")

tt2 = ggplot(test_kits[element_id == "k3JmmwNHkmY"] , aes(x=date, y=value)) + 
  facet_wrap(~stock_category, scales='free_y')+
  geom_point() + geom_line() + 
  theme_bw() + 
  labs(title="Double-check gold stock categories over time, with free_y scales")

tt3 = ggplot(test_kits[element_id == "ctP0MNHiq3B"], aes(x=date, y=value)) + 
  facet_wrap(~stock_category, scales='free_y')+
  geom_point() + geom_line() + 
  theme_bw() + 
  labs(title="Uni-gold stock categories over time, with free_y scales")

# •	Make a DPS level map for each. I would start with a count of days out of stock per district (just sum by district) and make one map for each type of test kits 
#(answers the question: how many stock out days were there per district for first line versus second line test kits?). You can also facet wrap by year, 
#so you would have two maps (2017 and 2018) on the same page.


# •	Then, I would have some fun by using a distinct metric – mean stock out days per facility. The reason we want this is that you could have a district with a 
#lot of facilities that were rarely stocked out and it would look like there were more stock out days there than in a district with few facilities with long stock outs.
#So, create a data table dt[ ,total_facilities=length(unique(org_unit_id)), by=dps] and merge it into your summed data, then divide the total stockout days by the facilities.

# Make a graph for reporting completeness, and then map reporting completeness by district. 
rep1 = ggplot(report_by_date, aes(x=date, y=facilities_by_date)) + 
  geom_point() + geom_line() + 
  theme_bw() + 
  labs(title="Facilities reporting over time", y = "Number of facilities", x = "Date")

rep2 = ggplot(report_map, aes(x=long, y=lat, group=group, fill=facilities_by_dps)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
  theme_void() +
  facet_wrap(~year, strip.position = "bottom") +
  labs(title="Reporting completeness by year and district, DRC")+
  theme(plot.title=element_text(vjust=-1), plot.subtitle=element_text(vjust=6)) 


# Start with a single variable. Make a data table called ‘det’ that includes just the Determine variable and the number of days stocked out.
# Sum the total days stocked out by district as you did in your graphs: det [stock_category==’days_out_of_stock’ ,sum(value), by=.(date, dps)]


#Question - do we want to aggregate this map by year? Do we want to subset to one year and do a map for each quarter? 
m1 = ggplot(det_so_map, aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) + 
  theme_void() +
  facet_wrap(~year, strip.position="bottom") +
  labs(title="Determine total days out of stock by district, DRC", subtitle="Annual maps restricted to Jan-Aug")

m2 = ggplot(uni_so_map, aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
  theme_void() +
  facet_wrap(~year, strip.position="bottom") +
  labs(title="Uni-Gold total days out of stock by district, DRC", subtitle="Annual maps restricted to Jan-Aug") 

m3 = ggplot(doub_so_map, aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Purples'))) + 
  theme_void() +
  facet_wrap(~year, strip.position="bottom") +
  labs(title="Double-Check total days out of stock by district, DRC", subtitle="Annual maps restricted to Jan-Aug")


#Map mean test kits per facility. Try without a facet wrap, and then facet wrap by year. 
m4 = ggplot(det_per_fac_map, aes(x=long, y=lat, group=group, fill=kits_per_fac)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) + 
  theme_void() +
  facet_wrap(~year, strip.position="bottom") +
  labs(title="Mean Determine test kits per facility, by district", subtitle="Annual data restricted to Jan-Aug", 
       caption="*Denominator only includes facilities with 'available, usable stock' of test kits")

m5 = ggplot(uni_per_fac_map, aes(x=long, y=lat, group=group, fill=kits_per_fac)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
  theme_void() +
  facet_wrap(~year, strip.position="bottom") +
  labs(title="Mean Uni-Gold test kits per facility, by district", subtitle="Annual data restricted to Jan-Aug", 
       caption="*Denominator only includes facilities with 'available, usable stock' of test kits")

m6 = ggplot(doub_per_fac_map, aes(x=long, y=lat, group=group, fill=kits_per_fac)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Purples'))) + 
  theme_void() +
  facet_wrap(~year, strip.position="bottom") +
  labs(title="Mean Double-Check Gold test kits per facility, by district", subtitle="Annual data restricted to Jan-Aug", 
       caption="*Denominator only includes facilities with 'available, usable stock' of test kits")


#Map treatment stock-outs for first-line and second-line drugs 
#Caitlin- would it be helpful to aggregate to only first-line regimen and second-line regimen drugs? Maybe generate indicators for these stock-outs? 
treat1 = ggplot(azt_3tc_nvp1_map, aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Purples'))) + 
  theme_void() +
  facet_wrap(~year, strip.position = "bottom") +
  labs(title="'AZT/3TC/NVP 60/30/50 mg ces disp - 60 ces' total days out of stock by district", subtitle="Annual data restricted to Jan-Aug")

treat2 = ggplot(azt_3tc_nvp2_map, aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
  theme_void() +
  facet_wrap(~year, strip.position = "bottom") +
  labs(title="'AZT/3TC/NVP(300/150/200 mg) - 60 ces' total days out of stock by district", subtitle="Annual data restricted to Jan-Aug")

treat3 = ggplot(tdf_3tc_efv_map, aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Greens'))) + 
  theme_void() +
  facet_wrap(~year, strip.position="bottom") +
  labs(title="'TDF/3TC/EFV(300/300/600 mg) - 30 ces' total days out of stock by district", subtitle="Annual data restricted to Jan-Aug")

treat4 = ggplot(mean_so_map1, aes(x=long, y=lat, group=group, fill=treat_per_fac)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Purples'))) + 
  theme_void() +
  facet_wrap(~year, strip.position = "bottom") +
  labs(title="'AZT/3TC/NVP 60/30/50 mg ces disp - 60 ces' mean facility-days out of stock by district", subtitle="Annual data restricted to Jan-Aug", 
       caption = "*Denominator only includes facilities that reported data for the given treatment regimen")

treat5 = ggplot(mean_so_map2, aes(x=long, y=lat, group=group, fill=treat_per_fac)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
  theme_void() +
  facet_wrap(~year, strip.position = "bottom") +
  labs(title="'AZT/3TC/NVP(300/150/200 mg) - 60 ces' mean facility-days out of stock by district", subtitle="Annual data restricted to Jan-Aug", 
       caption = "*Denominator only includes facilities that reported data for the given treatment regimen")

treat6 = ggplot(mean_so_map3, aes(x=long, y=lat, group=group, fill=treat_per_fac)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Greens'))) + 
  theme_void() +
  facet_wrap(~year, strip.position="bottom") +
  labs(title="TDF/3TC/EFV mean facility-days out of stock by district", subtitle="Annual data restricted to Jan-Aug", 
       caption = "*Denominator only includes facilities that reported data for the given treatment regimen")

treat7 = ggplot(monthly_so_rate_map[year(date)==2018], aes(x=long, y=lat, group=group, fill=monthly_so_rate)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Purples'))) + 
  theme_void() +
  facet_wrap(~date, strip.position = "bottom") +
  labs(title="TDF/3TC/EFV stock-out rate per district by month", subtitle="Data controlled for reporting", 
       caption = "*Denominator only includes facilities that reported data for the given treatment regimen")

treat8 = ggplot(monthly_so_change_map[year(date)==2018], aes(x=long, y=lat, group=group, fill=status)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  theme_void() +
  scale_fill_manual(breaks = c("DECREASE", "INCREASE"), 
                    values=c("green", "red"))+
  facet_wrap(~date) +
  labs(title="TDF/3TC/EFV absolute changes in stock-out rate for 2018", subtitle="Data controlled for reporting", 
       caption = "*Denominator only includes facilities that reported data for the given treatment regimen")

treat9 = ggplot(monthly_so_change_map[year(date)==2018], aes(x=long, y=lat, group=group, fill=change)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  theme_void() +
  colScale2+
  facet_wrap(~date) +
  labs(title="TDF/3TC/EFV rate of change of stock-outs by district", subtitle="Data controlled for reporting", 
       caption = "*Denominator only includes facilities that reported data for the given treatment regimen")

#Add one more map that shows absolute changes in stock at the level of each facility. 
treat10 = ggplot(monthly_so_change_map_hp[year(date)==2018], aes(x=long, y=lat, group=group, fill=status)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  theme_void() +
  scale_fill_manual(breaks = c("DECREASE", "INCREASE"), 
                    values=c("green", "red"))+
  facet_wrap(~date) +
  labs(title="TDF/3TC/EFV absolute changes in stock-out rate for 2018, health posts only", subtitle="Data controlled for reporting", 
       caption = "*Denominator only includes facilities that reported data for the given treatment regimen")

treat11 = ggplot(monthly_so_change_map_hp[year(date)==2018], aes(x=long, y=lat, group=group, fill=change)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  theme_void() +
  colScale2+
  facet_wrap(~date) +
  labs(title="TDF/3TC/EFV rate of change of stock-outs by district, health posts only", subtitle="Data controlled for reporting", 
       caption = "*Denominator only includes facilities that reported data for the given treatment regimen")


# export the maps and graphs as a pdf
pdf(paste0(dir, 'outputs/stockout_descriptives_2013_2018.pdf'), height=6, width=12)

g1
g2
g3
g4
g5
g6
g7
g8
g9
g10
# g11
# g12
# g13
# g14
# g15
# g16
# g17
# g18
# g19
# g20
# g21
# g22
# g23
# g24
# g25
# g26
# g27
# g28
# g29
# g30 
# g31
# g32
# g33
# g34
# g35

dev.off()


# ------------------------------------------------------ 




#--------------------------------------------------------------
#Make the graphs, and write to a .PDF
#--------------------------------------------------------------
outFile = paste0(saveDir, 'drug_test_kits.pdf')
pdf(outFile, height=5.5, width=7)

tt1
tt2
tt3

m1
m2
m3

m4
m5
m6

dev.off()


#The other thing was just to run a PDF loop with a time trend for every variable (50 page PDF). Make sure you aggregate
#to the level of variable/date/stock category 
#before you do that and facet by stock category. An extra credit alternative would be to add an if statement 
#that says if(!is.na(sex)) for a given variable and then embed 
#a ggplot where color = sex, so that the ones that have sex are stratified by sex and the ones that don’t have sex have a single line on each graph.
outFile = paste0(saveDir, 'drug_overview.pdf')
pdf(outFile, height=5.5, width=7)

rep1
rep2 

for (var in unique(dt$element)){
  temp = dt[element==var]
  
  #If you don't have sex, make a general plot.
  if (nrow(temp[is.na(sex)])!=0){ 
    temp = temp[, .(date, value, element, stock_category)]
    temp = temp[, .(value=sum(value, na.rm = TRUE)), by=.(date, element, stock_category)]
    
    plot = ggplot(temp, aes(x=date, y=value)) + 
      geom_point() + geom_line() + 
      theme_bw() + 
      facet_wrap(~stock_category, scales='free_y')+
      labs(title= var)
  } else { #If you do have sex, make a plot by sex, ignoring stock category because these values are NA where the element is stratified by sex.
    temp = temp[, .(date, value, element, sex)]
    temp = temp[, .(value=sum(value)), by=.(date, element, sex)]
    
    plot = ggplot(temp, aes(x=date, y=value, color=sex)) + 
      geom_point() + geom_line() + geom_line() +
      theme_bw() + 
      labs(title= var)
  }
  print(plot) 
}
dev.off()

#Print all maps into one file 
outFile = paste0(saveDir, 'drug_treatment.pdf')
pdf(outFile, height=5.5, width=7)

treat1
treat4

treat2
treat5

treat3
treat6

treat7
treat8
treat9

treat10
treat11

dev.off()


