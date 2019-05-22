#----------------------------------------------------------------
# AUTHOR: Emily Linebarger
# DATE: May/June 2019
# PURPOSE: Create general graph functions that can run on any variables in PNLS dataset
#----------------------------------------------------------------

#------------------------------------------------------
#Reporting completeness 
#------------------------------------------------------
# 1. PREP DATA: Preps reporting data table for a given drug
prep_report = function(dt=dt, drug_id, drug_name){
  FacsReportingByDate = dt[, .(facilities=length(unique(org_unit_id))), by=date]
  FacsReportingDrugByDate = dt[element_id==drug_id, .(facs_reporting_on_drug=as.numeric(length(unique(org_unit_id)))), by=date]
  
  #Did they report for the specific drug you're targeting?
  report = merge(FacsReportingByDate, FacsReportingDrugByDate, by='date', all.x=TRUE)
  
  report[is.na(facs_reporting_on_drug), facs_reporting_on_drug:=0]
  report[ , drug_reporting_ratio:=100*(facs_reporting_on_drug/facilities)]
  
  # convert to numerics and round
  report[, drug_reporting_ratio:=round(drug_reporting_ratio, 1)]
  
  # shape long
  report = melt(report, id.vars='date')
  report[grep('ratio', variable), ratio:=TRUE]
  report[!grep('ratio', variable), ratio:=FALSE]
  
  # label the variables
  report$variable = factor(report$variable, c( 'facilities',  'facs_reporting_on_drug','drug_reporting_ratio'),
                           c('Total health facilities', paste0('Reported about ', drug_name, ' stock'),
                             paste0('% of facilities reporting on ', drug_name)))
  return(report)
}

# 2. GRAPH FUNCTIONS 
# Number of facilities reporting 
gen_report1 = function(dt=dt, drug_id, drug_name, date_range){
  report = prep_report(dt, drug_id, drug_name)

  report1 = ggplot(report[ratio==FALSE], aes(x=date, y=value, color=variable, group=variable)) +
    geom_point(size=0.8) +
    geom_line() +
    geom_line() +
    theme_bw() +
    # facet_wrap(~indicator) +
    scale_color_manual(values = brewer.pal(4, 'RdYlBu')) +
    labs(x='Date', y='Number of health facilities', title=paste0('Number of facilities reporting supply chain information for ', drug_name),
         subtitle=date_range, color="")

  return(report1)
}

# 2 - ratio of facilities reporting
gen_report2 = function(dt=dt, drug_id, drug_name, date_range){
  report = prep_report(dt, drug_id, drug_name)
  
  # 2 - ratio of facilities reporting
  report2 = ggplot(report[ratio==TRUE], aes(x=date, y=value, color=variable, group=variable)) +
    geom_point(size=0.8) +
    geom_line() +
    geom_line() +
    theme_bw() +
    #facet_wrap(~variable) +
    labs(x='Date', y='% of facilities', title=paste0('Percentage of health facilities reporting supply chain information for ', drug_name),
         subtitle=date_range, color='% Reporting')
  
  return(report2)
}

gen_report3 = function(dt=dt, coord_ann=coord_ann, drug_id, drug_name, date_range){
  FacsReportingByDPS = dt[, .(facilities_by_dps=length(unique(org_unit_id))), by=c('year', 'dps')]
  report_by_dps = merge(FacsReportingByDPS, dt[element_id==drug_id], by=c('year', 'dps'))
  report_by_dps = unique(report_by_dps[, .(year, id, facilities_by_dps, dps)]) 
  report_map = merge(report_by_dps, coord_ann, by = c('id', 'year'), all.y = TRUE, allow.cartesian = TRUE)
  
  report_map1 = ggplot(report_map, aes(x=long, y=lat, group=group, fill=facilities_by_dps)) + 
    coord_fixed() +
    geom_polygon() + 
    geom_path(size=0.01) + 
    scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
    theme_void() +
    facet_wrap(~year, strip.position = "bottom") +
    labs(title=paste0("Reporting completeness by year and district for ", drug_name), fill="# of facilities reporting")+
    theme(plot.title=element_text(vjust=-1), plot.subtitle=element_text(vjust=6)) 
  return(report_map1)
}
#----------------------------------------------------
# Drug stockouts 
#----------------------------------------------------
# 1. PREP DATA 
prep_stockout = function(dt=dt, drug_id, drug_name){
  StockoutsForTargetDrug = dt[element_id==drug_id & stock_category=='number_of_days_stocked_out' & value>0 & !is.na(value)]
  StockoutsForTargetDrug[, facs_stocked_out:=as.numeric(length(unique(org_unit_id))), by=date]
  
  drug_so = StockoutsForTargetDrug[, .(facilities_stocked_out=as.numeric(length(unique(org_unit_id)))), by=date] #Count all facilities that reported stock out information for this drug, with greater than 0 days in the month. 
  drug_so = merge(drug_so, FacsReportingDrugByDate, by='date', all.x=T)
  drug_so[is.na(facs_reporting_on_drug), facs_reporting_on_drug:=0]
  drug_so[ ,ratio:=round(100*(facilities_stocked_out/facs_reporting_on_drug), 2)]
  
  # run the same code on the threshold subset
  drug_so = melt(drug_so, id.vars='date')
  drug_so$variable = factor(drug_so$variable, c('facs_reporting_on_drug', 'facilities_stocked_out', 'ratio'),
                                   c(paste0('Facilities reporting on ', drug_name), paste0('Facilities with a stockout in ', drug_name), 
                                     paste0('Percentage of facilities stocked out of ', drug_name)))
  return(drug_so)
}

# 2. GRAPH FUNCTIONS
gen_stockout1 = function(dt=dt, drug_id, drug_name){
  drug_so = prep_stockout(dt, drug_id, drug_name)
  
  stockout1 = ggplot(drug_so[variable!=paste0('Percentage of facilities stocked out of ', drug_name)], aes(x=date, y=value, color=variable, group=variable)) +
    geom_line() +
    geom_line() +
    theme_bw() +
    labs(title=paste0('Number of facilities that were stocked out of ', drug_name, ' in a given month'), 
         y='Number of facilities', x='Date', color="")
  return(stockout1)
}

gen_stockout2 = function(dt=dt, drug_id, drug_name){
  drug_so = prep_stockout(dt, drug_id)
  
  stockout2 = ggplot(drug_so[variable==paste0('Percentage of facilities stocked out of ', drug_name)], aes(x=date, y=value)) +
    geom_point(size=0.5) +
    geom_line() +
    geom_line() +
    theme_bw() +
    facet_wrap(~variable, scales='free_y') +
    labs(title=paste0('Percentage of facilities that were stocked out of ', drug_name, ' in a given month'), 
         x='Number of facilities', y='%')
  return(stockout2)
}


#----------------------------------------------------
# Drug stockouts, for faciilities reaching a reporting 
# threshold. 
#----------------------------------------------------
# 1. PREP DATA 
prep_stockout_thresh = function(dt=dt, drug_id){
  StockoutsForTargetDrug = dt[element_id==drug_id & stock_category=='number_of_days_stocked_out' & value>0 & !is.na(value)]
  StockoutsForTargetDrug[, facs_stocked_out:=as.numeric(length(unique(org_unit_id))), by=date]
  
  drug_so = StockoutsForTargetDrug[, .(facilities_stocked_out=as.numeric(length(unique(org_unit_id)))), by=date] #Count all facilities that reported stock out information for this drug, with greater than 0 days in the month. 
  drug_so = merge(drug_so, FacsReportingDrugByDate, by='date', all.x=T)
  drug_so[is.na(facs_reporting_on_drug), facs_reporting_on_drug:=0]
  drug_so[ ,ratio:=round(100*(facilities_stocked_out/facs_reporting_on_drug), 2)]
  
  # calculate 50% of facilities to graph above a reporting threshold
  n = dt[, length(unique(org_unit_id))/4 ]
  drug_so_thresh = drug_so[facs_reporting_on_drug > n]
  
  # run the same code on the threshold subset
  drug_so_thresh = melt(drug_so_thresh, id.vars='date')
  drug_so_thresh$variable = factor(drug_so_thresh$variable, c('facs_reporting_on_drug', 'facilities_stocked_out', 'ratio'),
                                   c(paste0('Facilities reporting on ', drug_name), paste0('Facilities with a stockout in ', drug_name), 
                                     paste0('Percentage of facilities stocked out of ', drug_name)))
  return(drug_so_thresh)
}

# 2. GRAPH FUNCTIONS
# 4  - arv stockout counts below a threshold
gen_stockout_thresh1 = function(dt=dt, drug_id, drug_name){
  drug_so_thresh = prep_stockout_thresh(dt, drug_id)
  
  so_thresh1 = ggplot(drug_so_thresh[variable!=paste0('Percentage of facilities stocked out of ', drug_name)], aes(x=date, y=value, color=variable, group=variable)) +
    geom_point(alpha=0.5, size=0.8) +
    geom_line() +
    geom_line() +
    theme_bw() +
    labs(title=paste0('Number of facilities that were stocked out of ', drug_name, ' in a given month'), 
         subtitle = 'Weeks in which at least 25% of facilities reported',
         y='Number of facilities', x='Date', color="")
  return(so_thresh1)
}


#--------------------------------------------
# STOCKOUT DAYS 
#--------------------------------------------
#-----------------------------------
# arv stockout bar graphs
bar_color = brewer.pal(5, 'RdYlBu') 

# 6 - stacked bar of days stocked out 
g6 = ggplot(so_days[days!=0], aes(x=days, y=facilities, fill=factor(year))) + 
  geom_bar(stat='identity', position='dodge') +
  theme_minimal() +
  scale_fill_manual(values=bar_color) +
  labs(title = paste0("Facilities stocked out of ", drug_name, " for at least one day by total days stocked out"), x='Number of days out of stock*', 
       y="Number of facilities", caption="*Does not include facilities stocked out for 0 days", 
       fill='Year (total facilities\nreporting)',
       subtitle=full_date_range)

# 7 - stacked bar of days stocked out 
g7 = ggplot(so_days2[days!=0 ], aes(x=days, y=facilities, fill=factor(year))) + 
  geom_bar(stat='identity', position='dodge') +
  theme_minimal() +
  scale_fill_manual(values=bar_color) +
  labs(title = paste0("Facilities stocked out of ", drug_name, " for at least one day by total days stocked out"),
       subtitle='Same time period: January - November', x='Number of days out of stock*', 
       y="Number of facilities", caption="*Does not include facilities stocked out for 0 days", fill='Year (total facilities\nreporting)')


#--------------------------------------------
# STOCKOUT MAPS
#--------------------------------------------

# map of facility-days of stock outs - 8
g8 = ggplot(so_map, aes(x=long, y=lat, group=group, fill=days_stocked_out)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) + 
  theme_void() +
  labs(title=paste0("Total facility-days of stockouts for ", drug_name), 
       caption="*A facility-day is defined as each day a facility reports", 
       subtitle='Same time period: January - November',fill="Facility-days*") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# mean days stocked out per facility - 9
g9 = ggplot(so_map_norm, aes(x=long, y=lat, group=group, fill=mean_days)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
  theme_void() +
  labs(title=paste0("Mean number of days stocked out of ", drug_name, " by district"), 
       subtitle='Same time period: January - November',fill="Mean days per facility") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

#--------------------------------------------
# STOCKOUT RATE-OF-CHANGE MAPS
#--------------------------------------------
# rate of change in annual facility-weeks stocked out - 10
g10 = ggplot(roc_map, aes(x=long, y=lat, group=group, fill=change)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01) +
  scale_fill_gradientn(colors=rev(brewer.pal(9, 'RdYlBu'))) +
  theme_void() +
  labs(title=paste0("Rate of change: facility-days of ", drug_name, " stockouts in 2018 minus 2017"),
       caption="* Positive difference = more days stocked out in 2018 than 2017",
       subtitle='Same time period: January - November',fill="Difference in days (2018 - 2017)*") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6))

# facilities with more stockouts - 11
g11 = ggplot(roc_map_alt, aes(x=long, y=lat, group=group, fill=change)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01) +
  scale_fill_gradientn(colors=brewer.pal(9, 'Reds')) +
  theme_void() +
  labs(title=paste0("Districts with more facility-days of stockouts for ", drug_name, " in 2018 than 2017"), 
       subtitle='Same time period: January - November', fill="Difference in days (2018 - 2017)") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6))

#--------------------------------------------
# PERCENTAGE OF DAYS STOCKED OUT
#--------------------------------------------
# # percentage of days stocked out - 12
g12 = ggplot(stock, aes(x=long, y=lat, group=group, fill=percent_out)) +
  coord_fixed() +
  geom_polygon() +
  geom_path() +
  facet_wrap(~year) +
  scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) +
  theme_void() +
  labs(title=paste0("Percentage of facility-days stocked out of ", drug_name), 
       subtitle=paste0("Days facilities were stocked out/Total days in which facilities reported on ", drug_name), 
       caption='Source: PNLS', fill="% of days stocked out") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6))


#--------------------------------------------
# FACILITY LEVEL SCATTER PLOTS
#--------------------------------------------

# stockouts by level 
g28 = ggplot(scatter, aes(x=level2, y=days_out)) +
  geom_jitter(width=0.25, alpha=0.2) + theme_bw() +
  labs(title=paste0('Days stocked out of ', drug_name, ' by facility level'), subtitle=full_date_range, x='Facility level',
       y='Days stocked out')

# stockouts by level, year
g29 = ggplot(scatter2, aes(x=level2, y=days_out)) +
  geom_jitter(width=0.25, alpha=0.2) +
  facet_wrap(~year) +
  labs(title=paste0('Days stocked out of ', drug_name, ' by facility level'), subtitle=partial_date_range, x='Facility level',
       y='Days stocked out') + 
  theme_bw()


#--------------------------------------------
# CATEGORICAL STOCKOUTS BY TIME PERIOD 
#--------------------------------------------

# Number of weeks stocked out, categorical
g32 = ggplot(final[year==2018], aes(x=long, y=lat, group=group, fill=value)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01) +
  facet_wrap(~variable) +
  scale_fill_gradientn(colors=brewer.pal(9, 'YlGnBu')) +
  theme_void() +
  labs(title=paste0("Number of facilities stocked out of ", drug_name, " by time stocked out, 2018"),
       subtitle="Cumulative: one month is equal to four weeks stocked out of ARVs",
       caption='Source: PNLS', fill="Number of facilities") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6))

# at least one stockout
g33 = ggplot(final[year==2017 & variable!='No stock outs reported'], aes(x=long, y=lat, group=group, fill=value)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01) +
  facet_wrap(~variable) +
  scale_fill_gradientn(colors=brewer.pal(9, 'YlGnBu')) +
  theme_void() +
  labs(title=paste0("Number of facilities stocked out of ", drug_name, " by time stocked out, 2017"),
       subtitle="Minimum one week of stockout",
       caption='Source: PNLS', fill="Number of facilities") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6))


# Number of weeks stocked out, categorical
g34 = ggplot(final[year==2018], aes(x=long, y=lat, group=group, fill=value)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01) +
  facet_wrap(~variable) +
  scale_fill_gradientn(colors=brewer.pal(9, 'YlOrBr')) +
  theme_void() +
  labs(title=paste0("Number of facilities stocked out of ", drug_name, " by time stocked out, 2018"),
       subtitle="Cumulative: one month is equal to four weeks stocked out of ARVs",
       caption='Source: PNLS', fill="Number of facilities") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6))

# at least one stockout
g35 = ggplot(final[year==2018 & variable!='No stock outs reported'], aes(x=long, y=lat, group=group, fill=value)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01) +
  facet_wrap(~variable) +
  scale_fill_gradientn(colors=brewer.pal(9, 'YlOrBr')) +
  theme_void() +
  labs(title=paste0("Number of facilities stocked out of ", drug_name, " by time stocked out, 2018"),
       subtitle="Minimum one week of stockout",
       caption='Source: PNLS', fill="Number of facilities") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6))

#--------------------------------------------------------------
# Additional analyses
#--------------------------------------------------------------

g36 = ggplot(report_map, aes(x=long, y=lat, group=group, fill=facilities_by_dps)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
  theme_void() +
  facet_wrap(~year, strip.position = "bottom") +
  labs(title=paste0("Reporting completeness by year and district for ", drug_name), fill="# of facilities reporting")+
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

#----------------------------------------------------------
# Breaking down categorical graphs above further. 
# Number of weeks stocked out, categorical
# at least one stockout
g41 = ggplot(final2[year==2017 & !variable%in%c('No stock outs reported', '0-2mo')], aes(x=long, y=lat, group=group, fill=value)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01) +
  facet_wrap(~variable) +
  scale_fill_gradientn(colors=brewer.pal(9, 'YlGnBu')) +
  theme_void() +
  labs(title=paste0("Number of facilities stocked out of ", drug_name, " by time stocked out, 2017"),
       subtitle="Greater than 2 months of stockout",
       caption='Source: PNLS', fill="Number of facilities") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6))

# at least one stockout
g42 = ggplot(final2[year==2018 & !variable%in%c('No stock outs reported', '0-2mo')], aes(x=long, y=lat, group=group, fill=value)) +
  coord_fixed() +
  geom_polygon() +
  geom_path(size=0.01) +
  facet_wrap(~variable) +
  scale_fill_gradientn(colors=brewer.pal(9, 'YlOrBr')) +
  theme_void() +
  labs(title=paste0("Number of facilities stocked out of ", drug_name, " by time stocked out, 2018"),
       subtitle="Greater than 2 months of stockout",
       caption='Source: PNLS', fill="Number of facilities") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6))



