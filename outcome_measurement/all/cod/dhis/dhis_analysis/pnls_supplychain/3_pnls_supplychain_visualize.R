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
  labs(title=paste0('Number of facilities that were stocked out of ', drug_name, ' in a given month'), 
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
g8 = ggplot(so_map, aes(x=long, y=lat, group=group, fill=weeks_stocked_out)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) + 
  theme_void() +
  labs(title=paste0("Total facility-weeks of stockouts for ", drug_name), 
       caption="*A facility-week is defined as each week a facility reports", 
       subtitle='Same time period: January - November',fill="Facility-weeks*") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# mean weeks stocked out per facility - 9
g9 = ggplot(so_map_norm, aes(x=long, y=lat, group=group, fill=mean_weeks)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
  theme_void() +
  labs(title=paste0("Mean number of weeks stocked out of ", drug_name, " by district"), 
       subtitle='Same time period: January - November',fill="Mean weeks per facility") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# rate of change in annual facility-weeks stocked out - 10
g10 = ggplot(roc_map, aes(x=long, y=lat, group=group, fill=change)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01) +
  scale_fill_gradientn(colors=rev(brewer.pal(9, 'RdYlBu'))) +
  theme_void() +
  labs(title=paste0("Rate of change: facility-weeks of ", drug_name, " stockouts in 2018 minus 2017"),
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
g12 = ggplot(stock, aes(x=long, y=lat, group=group, fill=percent_out)) +
  coord_fixed() +
  geom_polygon() +
  geom_path() +
  facet_wrap(~year) +
  scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) +
  theme_void() +
  labs(title=paste0("Percentage of facility-weeks stocked out of ", drug_name), 
       subtitle=paste0("Weeks facilities were stocked out/Total weeks in which facilities reported on ", drug_name), 
       caption='Source: PNLS', fill="% of weeks stocked out") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6))


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

g36 = ggplot(report_map, aes(x=long, y=lat, group=group, fill=facilities_by_dps)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
  theme_void() +
  facet_wrap(~year, strip.position = "bottom") +
  labs(title=paste0("Reporting completeness by year and district for ", drug_name), fill="% of facilities reporting")+
  theme(plot.title=element_text(vjust=-1), plot.subtitle=element_text(vjust=6)) 


#Map mean test kits per facility. Try without a facet wrap, and then facet wrap by year. 
g37 = ggplot(kits_per_fac_map, aes(x=long, y=lat, group=group, fill=kits_per_fac)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) + 
  theme_void() +
  facet_wrap(~year, strip.position="bottom") +
  labs(title=paste0("Mean ", drug_name, " kits per facility, by district"), subtitle="Annual data restricted to Jan-Aug", 
       caption="*Denominator only includes facilities with 'available, usable stock' of this drug", 
       fill="Kits per facility")

g38 = ggplot(monthly_so_change_map[year(date)==2018], aes(x=long, y=lat, group=group, fill=change)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  theme_void() +
  colScale2+
  facet_wrap(~date) +
  labs(title=paste0(drug_name, " rate of change of stock-outs by district, all facilities"), subtitle="Data controlled for reporting", 
       caption = "*Denominator only includes facilities that reported data for the given treatment regimen", fill="Month-to-month change")

#Add one more map that shows absolute changes in stock at the level of each facility. 
g39 = ggplot(monthly_so_change_map_hp[year(date)==2018], aes(x=long, y=lat, group=group, fill=status)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  theme_void() +
  scale_fill_manual(breaks = c("DECREASE", "INCREASE"), 
                    values=c("green", "red"))+
  facet_wrap(~date) +
  theme(legend.title=element_blank())
  labs(title=paste0(drug_name, " absolute changes in stock-out rate for 2018, health posts only"), subtitle="Data controlled for reporting", 
       caption = "*Denominator only includes facilities that reported data for the given treatment regimen")

g40 = ggplot(monthly_so_change_map_hp[year(date)==2018], aes(x=long, y=lat, group=group, fill=change)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  theme_void() +
  colScale2+
  facet_wrap(~date) +
  labs(title=paste0(drug_name, " rate of change of stock-outs by district, health posts only"), subtitle="Data controlled for reporting", 
       caption = "*Denominator only includes facilities that reported data for the given treatment regimen", fill="Month-to-month change")


# export the maps and graphs as a pdf
outDir = "J:/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/outputs/pnls/"
pdf(paste0(outDir, drug_name, " analysis.pdf"), height=6, width=12)
  
print(g1)
print(g2)
print(g36) #Reporting completeness map 
print(g3)
print(g4)
print(g5)
print(g6)
print(g7)
print(g8)
print(g9)
print(g10)
print(g11)
print(g12)

# print(g28)
# print(g29)
# print(g30) 
# print(g31)
# print(g32)
# print(g33)
# print(g34)
# print(g35)

print(g37)
print(g38)
# print(g39)
print(g40)

dev.off()


# ------------------------------------------------------ 

#ONLY NEED TO RERUN WHEN WE GET NEW DATA - EMILY 5/14/19


#The other thing was just to run a PDF loop with a time trend for every variable (50 page PDF). Make sure you aggregate
#to the level of variable/date/stock category 
#before you do that and facet by stock category. An extra credit alternative would be to add an if statement 
#that says if(!is.na(sex)) for a given variable and then embed 
#a ggplot where color = sex, so that the ones that have sex are stratified by sex and the ones that don’t have sex have a single line on each graph.
# outFile = paste0("J:/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/outputs/pnls/supply_chain_overview.pdf")
# pdf(outFile, height=5.5, width=7)
# 
# for (var in unique(dt$element)){
#   temp = dt[element==var]
#   
#   #If you don't have sex, make a general plot.
#   if (nrow(temp[is.na(sex)])!=0){ 
#     temp = temp[, .(date, value, element, stock_category)]
#     temp = temp[, .(value=sum(value, na.rm = TRUE)), by=.(date, element, stock_category)]
#     
#     plot = ggplot(temp, aes(x=date, y=value)) + 
#       geom_point() + geom_line() + 
#       theme_bw() + 
#       facet_wrap(~stock_category, scales='free_y')+
#       labs(title= var)
#   } else { #If you do have sex, make a plot by sex, ignoring stock category because these values are NA where the element is stratified by sex.
#     temp = temp[, .(date, value, element, sex)]
#     temp = temp[, .(value=sum(value)), by=.(date, element, sex)]
#     
#     plot = ggplot(temp, aes(x=date, y=value, color=sex)) + 
#       geom_point() + geom_line() + geom_line() +
#       theme_bw() + 
#       labs(title= var)
#   }
#   print(plot) 
# }
# dev.off()


