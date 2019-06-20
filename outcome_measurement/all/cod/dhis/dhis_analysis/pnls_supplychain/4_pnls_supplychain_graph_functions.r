#----------------------------------------------------------------
# AUTHOR: Emily Linebarger
# DATE: May/June 2019
# PURPOSE: Create general graph functions that can run on any variables in PNLS dataset
#----------------------------------------------------------------

# General functions 
gen_annual_dt = function(dt){
  AnnualDT = dt[(date< "2018-09-01" & year == 2018) | (date < '2017-09-01' & year == 2017)] 
  return(AnnualDT)
}
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
    labs(x='Date', y='Number of health facilities', title=paste0('Number of facilities reporting* supply chain information for ', drug_name),
         subtitle=date_range, caption="*Reporting on any stock indicator", color="")

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
    labs(x='Date', y='% of facilities', title=paste0('Percentage of health facilities reporting* supply chain information for ', drug_name),
         subtitle=date_range, color='% Reporting', caption="*Reporting on any stock indicator")
  
  return(report2)
}

gen_report3 = function(dt=dt, coord_ann=coord_ann, drug_id, drug_name, date_range){
  FacsReportingByDPS = dt[, .(facilities_by_dps=length(unique(org_unit_id))), by=c('year', 'dps')]
  report_by_dps = merge(FacsReportingByDPS, dt[element_id==drug_id], by=c('year', 'dps'))
  report_by_dps = unique(report_by_dps[, .(year, id, facilities_by_dps, dps)]) 
  report_map = merge(report_by_dps, coord_ann, by = c('id', 'year'), all.y = TRUE, allow.cartesian = TRUE)
  
  #----------------------
  #For each DPS, find it's center. 
  districts = unique(report_map$dps)
  districts = districts[!is.na(districts)]
  all_centers = data.table()
  for (district in districts){
    centers = report_map[dps==district, .(long, lat)]
    center = as.data.table(centroid(centers))
    center[, dps:=district]
    all_centers = rbind(all_centers, center)
  }
  
  # Generate a labels dataset
  labels = unique(report_map[, .(id, dps, facilities_by_dps, year)])
  labels[, label:= paste0(dps, ": ", facilities_by_dps)]
  labels = merge(labels, all_centers, by=c('dps'))
  #----------------------
  
  # mean_units2 = ggplot(kits_per_facility, aes(x=long, y=lat, group=group, fill=kits_per_fac)) + 
  #   geom_polygon() + 
  #   theme_void() + 
  #   facet_wrap(~year) + 
  #   scale_fill_gradientn('Kits per\nFacility', colours=ratio_colors) + 
  #   coord_fixed(ratio=1) + 
  #   scale_x_continuous('', breaks = NULL) + 
  #   scale_y_continuous('', breaks = NULL) + 
  #   labs(title=paste0(drug_name, " kits per facility, by district"), subtitle="Annual data restricted to Jan-Aug",
  #        caption="*Denominator only includes facilities with 'available, usable stock' of this drug",
  #        fill="Kits per facility") + 
  #   theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) + 
  #   geom_label_repel(data = labels, aes(label = label, x = lon, y = lat, group = label), inherit.aes=FALSE, size=3)
  
  report_map1 = ggplot(report_map, aes(x=long, y=lat, group=group, fill=facilities_by_dps)) + 
    coord_fixed() +
    geom_polygon() + 
    geom_path(size=0.01) + 
    scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
    theme_void() +
    facet_wrap(~year, strip.position = "bottom") +
    labs(title=paste0("Reporting completeness* by year and district for ", drug_name), caption="*Facilities reporting on any stock indicator", 
         fill="# of facilities reporting")+
    geom_label_repel(data = labels, aes(label = label, x = lon, y = lat, group = label), inherit.aes=FALSE, size=3)
  return(report_map1)
}

#----------------------------------------------------
# Drug stockouts 
#----------------------------------------------------
# 1. PREP DATA 
prep_stockout = function(dt=dt, drug_id, drug_name){
  FacsReportingDrugByDate = dt[element_id==drug_id, .(facs_reporting_on_drug=as.numeric(length(unique(org_unit_id)))), by=date]
  
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
  drug_so = prep_stockout(dt, drug_id, drug_name)
  
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
# 4  stockout counts below a threshold
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
#Stacked bar graph of days stocked out
gen_so_days1 = function(dt=dt, drug_id, drug_name, date_range){
  bar_color = brewer.pal(5, 'RdYlBu') 
  
  so_days = dt[element_id==drug_id & stock_category=='number_of_days_stocked_out' & !is.na(value),
               .(days=sum(value, na.rm=T)), by=.(year, org_unit_id)] #Do we want to have greater than 0 in this calculation? 
  # Sanity check - are there any days greater than 365? This shouldn't be possible. 
  range(so_days$days)
  so_days = so_days[days >=1] #Caitlin this was just > 1 in your code? Only keep where there was one or more days of stockouts. 
  so_days = so_days[ ,.(facilities=length(unique(org_unit_id))), by=.(days, year)] #EMILY IS THIS WHAT YOU WANT HERE? 
  
  # labels 
  labels = so_days[ ,.(total=sum(facilities)), by=year]
  l17 = paste0('2017 (n=', labels[year==2017]$total, ')')
  l18 = paste0('2018 (n=', labels[year==2018]$total, ')') 
  labels_vec = c(l17, l18)
  
  so_days$year = factor(so_days$year, c(2017, 2018), 
                        labels_vec)
  
  so_days1 = ggplot(so_days[days!=0], aes(x=days, y=facilities, fill=factor(year))) +
    geom_bar(stat='identity', position='dodge') +
    theme_minimal() +
    scale_fill_manual(values=bar_color) +
    labs(title = paste0("Facilities stocked out of ", drug_name, " for at least one day by total days stocked out"), x='Number of days out of stock*',
         y="Number of facilities", caption="*Does not include facilities stocked out for 0 days",
         fill='Year (total facilities\nreporting)',
         subtitle=date_range)
  
  return(so_days1)
}


#--------------------------------------------
# STOCKOUT MAPS
#--------------------------------------------

gen_so_map1 = function(dt=dt, drug_id, drug_name, date_range){
  stockout = dt[month(date)!=12 & element_id==drug_id & stock_category=='number_of_days_stocked_out' & !is.na(value), 
                .(days_stocked_out=sum(value, na.rm=T)), by=.(year, id)] #Get the total number of days stocked out for each DPS. 
  stockout[, weeks_stocked_out:=ceiling(days_stocked_out/7)]
  so_map = merge(stockout, coord_ann, by=c('id', 'year'), all.y=TRUE)
  
  so_map1 = ggplot(so_map, aes(x=long, y=lat, group=group, fill=days_stocked_out)) + 
      coord_fixed() +
      geom_polygon() +
      geom_path(size=0.01) +
      facet_wrap(~year) +
      scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) +
      theme_void() +
      labs(title=paste0("Total facility-days of stockouts for ", drug_name),
           caption="*A facility-day is defined as each day a facility reports",
           subtitle=date_range,fill="Facility-days*") +
      theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6))
  return(so_map1)
}


# mean days stocked out per facility - 9
gen_so_map2 = function(dt=dt, coord_ann=coord_ann, drug_id, drug_name, date_range){
  stockout = dt[month(date)!=12 & element_id==drug_id & stock_category=='number_of_days_stocked_out' & !is.na(value), 
                .(days_stocked_out=sum(value, na.rm=T)), by=.(year, id)] #Get the total number of days stocked out for each DPS. 
  stockout[, weeks_stocked_out:=ceiling(days_stocked_out/7)]
  
  num_facilities = dt[element_id==drug_id & stock_category=='number_of_days_stocked_out' & !is.na(value), .(facilities=length(unique(org_unit_id))), by=.(year, id)]
  num_facilities = merge(stockout, num_facilities)
  num_facilities[ , mean_weeks:=round((weeks_stocked_out/facilities), 1)]
  num_facilities[ , mean_days:=round((days_stocked_out/facilities), 1)]
  so_map_norm = merge(num_facilities, coord_ann, by=c('id', 'year'), all.y=TRUE)
  
  so_map2 = ggplot(so_map_norm, aes(x=long, y=lat, group=group, fill=mean_days)) +
    coord_fixed() +
    geom_polygon() +
    geom_path(size=0.01) +
    facet_wrap(~year) +
    scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) +
    theme_void() +
    labs(title=paste0("Mean number of days stocked out of ", drug_name, " by district"),
         subtitle=date_range,fill="Mean days per facility") +
    theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6))
  return(so_map2)
}

#--------------------------------------------
# STOCKOUT RATE-OF-CHANGE MAPS
#--------------------------------------------
# rate of change in annual facility-weeks stocked out - 10
gen_roc1 = function(dt=dt, drug_id, drug_name, date_range){
  stockout = dt[month(date)!=12 & element_id==drug_id & stock_category=='number_of_days_stocked_out' & !is.na(value), 
                .(days_stocked_out=sum(value, na.rm=T)), by=.(year, id)] #Get the total number of days stocked out for each DPS. 
  stockout[, weeks_stocked_out:=ceiling(days_stocked_out/7)]
  stockout[ , year2:=paste0('n', year)]
  roc = dcast(data = stockout, id ~ year2, value.var=c('days_stocked_out', 'weeks_stocked_out'))
  roc[ , change:=(days_stocked_out_n2018 - days_stocked_out_n2017)]
  roc_map = merge(coord, roc, by='id')
  
  roc1 = ggplot(roc_map, aes(x=long, y=lat, group=group, fill=change)) +
    coord_fixed() +
    geom_polygon() +
    geom_path(size=0.01) +
    scale_fill_gradientn(colors=rev(brewer.pal(9, 'RdYlBu'))) +
    theme_void() +
    labs(title=paste0("Rate of change: facility-days of ", drug_name, " stockouts in 2018 minus 2017"),
         caption="* Positive difference = more days stocked out in 2018 than 2017",
         subtitle=date_range,fill="Difference in days (2018 - 2017)*") +
    theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6))
  return(roc1)
}

# # facilities with more stockouts - 11
gen_roc2 = function(dt=dt, drug_id, drug_name, date_range){
  stockout = dt[month(date)!=12 & element_id==drug_id & stock_category=='number_of_days_stocked_out' & !is.na(value), 
                .(days_stocked_out=sum(value, na.rm=T)), by=.(year, id)] #Get the total number of days stocked out for each DPS. 
  stockout[, weeks_stocked_out:=ceiling(days_stocked_out/7)]
  so_map = merge(stockout, coord_ann, by=c('id', 'year'), all.y=TRUE)
  
  # mean weeks stocked out 
  # number of weeks of stockout divided by art sites reporting 
  num_facilities = dt[element_id==drug_id & stock_category=='number_of_days_stocked_out' & !is.na(value), .(facilities=length(unique(org_unit_id))), by=.(year, id)]
  num_facilities = merge(stockout, num_facilities)
  num_facilities[ , mean_weeks:=round((weeks_stocked_out/facilities), 1)]
  num_facilities[ , mean_days:=round((days_stocked_out/facilities), 1)]
  so_map_norm = merge(num_facilities, coord_ann, by=c('id', 'year'), all.y=TRUE)
  
  # rates of change in facility-weeks per year
  stockout[ , year2:=paste0('n', year)]
  roc = dcast(data = stockout, id ~ year2, value.var=c('days_stocked_out', 'weeks_stocked_out'))
  roc[ , change:=(days_stocked_out_n2018 - days_stocked_out_n2017)]
  roc_map = merge(coord, roc, by='id')
  
  # only districts with more stockouts in 2018 than 2017
  roc_map_alt = merge(coord, roc, by='id')
  roc_map_alt[change <=0, change:=NA]
  roc2 = ggplot(roc_map, aes(x=long, y=lat, group=group, fill=change)) +
    coord_fixed() +
    geom_polygon() +
    geom_path(size=0.01) +
    scale_fill_gradientn(colors=rev(brewer.pal(9, 'RdYlBu'))) +
    theme_void() +
    labs(title=paste0("Rate of change: facility-days of ", drug_name, " stockouts in 2018 minus 2017"),
         caption="* Positive difference = more days stocked out in 2018 than 2017",
         subtitle='Same time period: January - November',fill="Difference in days (2018 - 2017)*") +
    theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6))
  
  return(roc2)
}

# # facilities with more stockouts - 11
gen_roc3 = function(dt=dt, drug_id, drug_name, date_range){
  stockout = dt[month(date)!=12 & element_id==drug_id & stock_category=='number_of_days_stocked_out' & !is.na(value), 
                .(days_stocked_out=sum(value, na.rm=T)), by=.(year, id)] #Get the total number of days stocked out for each DPS. 
  stockout[, weeks_stocked_out:=ceiling(days_stocked_out/7)]
  so_map = merge(stockout, coord_ann, by=c('id', 'year'), all.y=TRUE)
  
  # mean weeks stocked out 
  # number of weeks of stockout divided by art sites reporting 
  num_facilities = dt[element_id==drug_id & stock_category=='number_of_days_stocked_out' & !is.na(value), .(facilities=length(unique(org_unit_id))), by=.(year, id)]
  num_facilities = merge(stockout, num_facilities)
  num_facilities[ , mean_weeks:=round((weeks_stocked_out/facilities), 1)]
  num_facilities[ , mean_days:=round((days_stocked_out/facilities), 1)]
  so_map_norm = merge(num_facilities, coord_ann, by=c('id', 'year'), all.y=TRUE)
  
  # rates of change in facility-weeks per year
  stockout[ , year2:=paste0('n', year)]
  roc = dcast(data = stockout, id ~ year2, value.var=c('days_stocked_out', 'weeks_stocked_out'))
  roc[ , change:=(days_stocked_out_n2018 - days_stocked_out_n2017)]
  roc_map = merge(coord, roc, by='id')
  
  # only districts with more stockouts in 2018 than 2017
  roc_map_alt = merge(coord, roc, by='id')
  roc_map_alt[change <=0, change:=NA]
  roc3 = ggplot(roc_map_alt, aes(x=long, y=lat, group=group, fill=change)) +
    coord_fixed() +
    geom_polygon() +
    geom_path(size=0.01) +
    scale_fill_gradientn(colors=brewer.pal(9, 'Reds')) +
    theme_void() +
    labs(title=paste0("Districts with more facility-days of stockouts for ", drug_name, " in 2018 than 2017"),
         subtitle='Same time period: January - November', fill="Difference in days (2018 - 2017)") +
    theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6))
  return(roc3)
}

#--------------------------------------------
# PERCENTAGE OF DAYS STOCKED OUT
#--------------------------------------------
# # percentage of days stocked out - 12
gen_so_pct1 = function(dt=dt, drug_id, drug_name, date_range){
  stock = dt[element_id==drug_id & stock_category=="number_of_days_stocked_out", .(total_days=sum(expected_days, na.rm=T)), by=c('year', 'id')]#Count all facilities that reported stock out information for this drug, with greater than 0 days in the month. 
  stock_add = dt[element_id==drug_id & stock_category=="number_of_days_stocked_out", .(days_out=sum(value, na.rm=T)), by=c('year', 'id')]
  stock = merge(stock, stock_add, by=c('year', 'id'), all=T)
  stock[, percent_out:=round(100*(days_out/total_days), 1)]
  
  unique(stock[percent_out==100, .(id, year, total_days, days_out)]) #Verifying that there are some cases with 100% of days stocked-out. 
  stock[is.na(percent_out)]
  
  stock = merge(stock, coord_ann, by=c('id', 'year'), all.y=T)
  
  so_pct1 = ggplot(stock, aes(x=long, y=lat, group=group, fill=percent_out)) +
    coord_fixed() +
    geom_polygon() +
    geom_path() +
    facet_wrap(~year) +
    scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) +
    theme_void() +
    labs(title=paste0("Percentage of facility-days stocked out of ", drug_name),
         subtitle=paste0("Days facilities were stocked out/Total days in which facilities reported on ", drug_name),
         fill="% of days stocked out") +
    theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6))
  return(so_pct1)
}

gen_so_pct2 = function(dt=dt, drug_id, drug_name, date_range){
  stock = dt[element_id==drug_id & stock_category=="number_of_days_stocked_out", .(total_days=sum(expected_days, na.rm=T)), by=c('year', 'id', 'dps')]#Count all facilities that reported stock out information for this drug, with greater than 0 days in the month. 
  stock_add = dt[element_id==drug_id & stock_category=="number_of_days_stocked_out", .(days_out=sum(value, na.rm=T)), by=c('year', 'id', 'dps')]
  stock = merge(stock, stock_add, by=c('year', 'id', 'dps'), all=T)
  stock[, percent_out:=round(100*(days_out/total_days), 1)]
  stock = merge(stock, coord_ann, by=c('id', 'year'), all.y=T)
  
  #----------------------
  #For each DPS, find it's center. 
  districts = unique(stock$dps)
  districts = districts[!is.na(districts)]
  all_centers = data.table()
  for (district in districts){
    centers = stock[dps==district, .(long, lat)]
    center = as.data.table(centroid(centers))
    center[, dps:=district]
    all_centers = rbind(all_centers, center)
  }

  # Generate a labels dataset
  labels = unique(stock[, .(id, dps, percent_out, year)])
  labels[, label:= paste0(dps, ": ", percent_out, "%")]
  labels = merge(labels, all_centers, by=c('dps'))
  #----------------------
  
  so_pct2 = ggplot(stock, aes(x=long, y=lat, group=group, fill=percent_out)) + 
    geom_polygon() + 
    theme_void() + 
    facet_wrap(~year) + 
    scale_fill_gradientn('% Facilties\nstocked out', colours=ratio_colors) + 
    coord_fixed(ratio=1) + 
    scale_x_continuous('', breaks = NULL) + 
    scale_y_continuous('', breaks = NULL) + 
    labs(title=paste0("Percentage of facility-days stocked out of ", drug_name),
         subtitle=paste0("Days facilities were stocked out/Total days in which facilities reported on ", drug_name),
         fill="% of days stocked out") +
    theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) + 
    geom_label_repel(data = labels, aes(label = label, x = lon, y = lat, group = label), inherit.aes=FALSE, size=3)
  
  return(so_pct2)
}

# #--------------------------------------------
# # FACILITY LEVEL SCATTER PLOTS
# #--------------------------------------------

#All unique levels. 
gen_scatter1 = function(dt=dt, drug_id, drug_name, date_range){
  scatter = dt[element_id==drug_id & stock_category=='number_of_days_stocked_out', .(days_out=sum(value, na.rm=T)), by=.(org_unit_id, level)]
  scatter[, weeks_out:=days_out/7]
  
  scatter1 = ggplot(scatter[!is.na(level)], aes(x=level, y=days_out)) +
      geom_jitter(width=0.25, alpha=0.2) + theme_bw() +
    theme(axis.text.x = element_text(angle = 30, hjust=1)) + 
      labs(title=paste0('Days stocked out of ', drug_name, ' by facility level'), subtitle=full_date_range, x='Facility level',
           y='Days stocked out')
  
  return(scatter1)
}

#Clustered levels. 
gen_scatter2 = function(dt=dt, drug_id, drug_name, date_range){
  scatter = dt[element_id==drug_id & stock_category=='number_of_days_stocked_out', .(days_out=sum(value, na.rm=T)), by=.(org_unit_id, level2)]
  scatter[, weeks_out:=days_out/7]
  
  scatter2 = ggplot(scatter, aes(x=level2, y=days_out)) +
    geom_jitter(width=0.25, alpha=0.2) + theme_bw() +
    labs(title=paste0('Days stocked out of ', drug_name, ' by facility level'), subtitle=full_date_range, x='Facility level',
         y='Days stocked out')
  
  return(scatter2)
}

#Same graph as scatter 1, just facet wrapped by year. 
gen_scatter3 = function(dt=dt, drug_id, drug_name, date_range){
  scatter = dt[element_id==drug_id & stock_category=='number_of_days_stocked_out', .(days_out=sum(value, na.rm=T)), by=.(org_unit_id, level2, year)]
  scatter[, weeks_out:=days_out/7]
  
  scatter3 = ggplot(scatter, aes(x=level2, y=days_out)) +
    geom_jitter(width=0.25, alpha=0.2) +
    facet_wrap(~year) +
    labs(title=paste0('Days stocked out of ', drug_name, ' by facility level'), subtitle=date_range, x='Facility level',
         y='Days stocked out') +
    theme_bw()
  
  return(scatter3)
}

#--------------------------------------------
# CATEGORICAL STOCKOUTS BY TIME PERIOD
#--------------------------------------------
gen_categorical = function(dt, coord_ann, drug_id, drug_name, date_range){
  final = dt[element_id==drug_id & stock_category=="number_of_days_stocked_out",.(days_out=sum(value, na.rm=T)) , by=.(org_unit_id, year, id) ]
  final = final[ ,.(org_unit_id=length(unique(org_unit_id))), by=.(days_out, year, id)]
  final[ ,months:=(days_out/30)]
  final[months==0, category:='no_stock_out']
  final[0 < months & months <= 1, category:='one_day_2_mos']
  final[1 < months & months <= 2, category:='two_4_mos']
  final[2 < months, category:='four_months']
  final = final[ ,.(value=sum(org_unit_id)), by=.(year, id, variable=category)]
  final = dcast(final, year+id ~ variable)
  
  final[is.na(no_stock_out), no_stock_out:=0]
  final[is.na(one_day_2_mos), one_day_2_mos:=0]
  final[is.na(two_4_mos), two_4_mos:=0]
  final[is.na(four_months), four_months:=0]
  
  final = merge(final, coord_ann, by=c('id', 'year'), all.y=TRUE)
  final = melt(final, id.vars=c('year', 'id', 'long', 'lat', 'order', 'hole',
                                'piece', 'group'))
  
  final$variable = factor(final$variable, c('no_stock_out', 'one_day_2_mos',
                                            'two_4_mos', 'four_months'),
                          c('No stock outs reported',
                            '1 day - 1 month ', '1+ - 2 months ', '2+ months'))
  
  return(final)
}

# Number of weeks stocked out, categorical
gen_cat1_2017 = function(dt=dt, coord_ann, drug_id, drug_name, date_range){
  final = gen_categorical(dt, coord_ann, drug_id, drug_name, date_range)
  
  cat1 = ggplot(final[year==2018], aes(x=long, y=lat, group=group, fill=value)) +
    coord_fixed() +
    geom_polygon() +
    geom_path(size=0.01) +
    facet_wrap(~variable) +
    scale_fill_gradientn(colors=brewer.pal(9, 'YlGnBu')) +
    theme_void() +
    labs(title=paste0("Number of facilities stocked out of ", drug_name, " by time stocked out, 2018"),
        fill="Number of facilities") +
    theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6))
  return(cat1)
}
# at least one stockout
gen_cat1_2017 = function(dt=dt, coord_ann, drug_id, drug_name, date_range){
  final = gen_categorical(dt, coord_ann, drug_id, drug_name, date_range)
  
  cat2 = ggplot(final[year==2017 & variable!='No stock outs reported'], aes(x=long, y=lat, group=group, fill=value)) +
    coord_fixed() +
    geom_polygon() +
    geom_path(size=0.01) +
    facet_wrap(~variable) +
    scale_fill_gradientn(colors=brewer.pal(9, 'YlGnBu')) +
    theme_void() +
    labs(title=paste0("Number of facilities stocked out of ", drug_name, " by time stocked out, 2017"),
         subtitle="Minimum one day of stockout",
         fill="Number of facilities") +
    theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6))
  return(cat2)
}

# Number of weeks stocked out, categorical
gen_cat1_2018 = function(dt=dt, coord_ann, drug_id, drug_name, date_range){
  final = gen_categorical(dt, coord_ann, drug_id, drug_name, date_range)
  
  cat3 = ggplot(final[year==2018], aes(x=long, y=lat, group=group, fill=value)) +
    coord_fixed() +
    geom_polygon() +
    geom_path(size=0.01) +
    facet_wrap(~variable) +
    scale_fill_gradientn(colors=brewer.pal(9, 'YlOrBr')) +
    theme_void() +
    labs(title=paste0("Number of facilities stocked out of ", drug_name, " by time stocked out, 2018"),
         fill="Number of facilities") +
    theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6))
  return(cat3)
}

# at least one stockout
gen_cat2_2018 = function(dt=dt, coord_ann, drug_id, drug_name, date_range){
  final = gen_categorical(dt, coord_ann, drug_id, drug_name, date_range) 
  
  cat4 = ggplot(final[year==2018 & variable!='No stock outs reported'], aes(x=long, y=lat, group=group, fill=value)) +
    coord_fixed() +
    geom_polygon() +
    geom_path(size=0.01) +
    facet_wrap(~variable) +
    scale_fill_gradientn(colors=brewer.pal(9, 'YlOrBr')) +
    theme_void() +
    labs(title=paste0("Number of facilities stocked out of ", drug_name, " by time stocked out, 2018"),
         subtitle="Minimum one day of stockout",
         fill="Number of facilities") +
    theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6))
  return(cat4)
}

#---------------------------------------------
# MORE GRANULAR CATEGORICAL GRAPHS
#---------------------------------------------
gen_categorical2 = function(dt, coord_ann, drug_id, drug_name, date_range){
  final2 = dt[element_id==drug_id & stock_category=="number_of_days_stocked_out",.(days_out=sum(value, na.rm=T)) , by=.(org_unit_id, year, id) ]
  final2 = final2[ ,.(org_unit_id=length(unique(org_unit_id))), by=.(days_out, year, id)]
  final2[ ,months:=(days_out/30)]
  
  final2[months==0, category:='no_stock_out']
  final2[0 < months & months <= 2, category:='zero_two_months']
  final2[2 < months & months <= 4, category:='two_four_months']
  final2[4 < months & months <=6, category:='four_six_months']
  final2[6 < months & months <=8, category:='six_eight_months']
  final2[8 < months & months <=10, category:='eight_ten_months']
  final2[months>10, category:='ten_year_months']
  
  final2 = final2[ ,.(value=sum(org_unit_id)), by=.(year, id, variable=category)]
  final2 = dcast(final2, year+id ~ variable)
  
  for (var in c('no_stock_out', 'zero_two_months', 'two_four_months', 'four_six_months', 'six_eight_months', 'eight_ten_months',
                'ten_year_months')){
    if (var%in%names(final2)){
      final2[is.na(get(var)), (var):=0]
    }
  }
  
  final2 = merge(final2, coord_ann, by=c('id', 'year'), all.y=TRUE)
  final2 = melt(final2, id.vars=c('year', 'id', 'long', 'lat', 'order', 'hole',
                                  'piece', 'group'))
  
  final2$variable = factor(final2$variable,  c('no_stock_out', 'zero_two_months', 'two_four_months', 'four_six_months', 
                                               'six_eight_months', 'eight_ten_months',
                                               'ten_year_months'),
                           c('No stock outs reported', '0-2 mo', '2-4 mo', '4-6 mo', '6-8 mo', '8-10 mo', '10-12 mo'))
  
  return(final2)
}

# Number of weeks stocked out, categorical
# at least one stockout
gen_cat3_2017 = function(dt=dt, coord_ann, drug_id, drug_name, date_range){
  final2 = gen_categorical2(dt, coord_ann, drug_id, drug_name, date_range)
  
  cat3_2017 = ggplot(final2[year==2017 & !variable%in%c('No stock outs reported', '0-2mo')], aes(x=long, y=lat, group=group, fill=value)) +
    coord_fixed() +
    geom_polygon() +
    geom_path(size=0.01) +
    facet_wrap(~variable) +
    scale_fill_gradientn(colors=brewer.pal(9, 'YlGnBu')) +
    theme_void() +
    labs(title=paste0("Number of facilities stocked out of ", drug_name, " by time stocked out, 2017"),
         subtitle="Greater than 2 months of stockout",
         fill="Number of facilities") +
    theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6))
    return(cat3_2017)
}
# at least one stockout
gen_cat3_2018 = function(dt=dt, coord_ann, drug_id, drug_name, date_range){
  final2 = gen_categorical2(dt, coord_ann, drug_id, drug_name, date_range)
  
  cat3_2018 = ggplot(final2[year==2018 & !variable%in%c('No stock outs reported', '0-2mo')], aes(x=long, y=lat, group=group, fill=value)) +
    coord_fixed() +
    geom_polygon() +
    geom_path(size=0.01) +
    facet_wrap(~variable) +
    scale_fill_gradientn(colors=brewer.pal(9, 'YlOrBr')) +
    theme_void() +
    labs(title=paste0("Number of facilities stocked out of ", drug_name, " by time stocked out, 2018"),
         subtitle="Greater than 2 months of stockout",
         fill="Number of facilities") +
    theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6))
  return(cat3_2018)
}
#--------------------------------------------------------------
# Mean drug units per facility
#--------------------------------------------------------------

#Map mean test kits per facility. Try without a facet wrap, and then facet wrap by year.
gen_mean_units1 = function(dt=dt, coord_ann=coord_ann, drug_id, drug_name, date_range){
  facs_per_district = dt[element_id==drug_id & stock_category == "available_usable_stock", .(facs=length(unique(org_unit_id))), by=c('dps','year')] #Find the number of facs per district with available test-kit stock. 
  
  kits_per_facility = dt[element_id==drug_id & stock_category == "available_usable_stock", .(element_id, value, dps, year, id, date)]
  kits_per_facility = kits_per_facility[(date< "2018-09-01" & year == 2018) | (date < '2017-09-01' & year == 2017)] #Subset to handle time lags. 
  kits_per_facility = kits_per_facility[, .(value=sum(value, na.rm = TRUE)), by = c('element_id', 'dps', 'year', 'id')] #Collapse here, because you want to get rid of the date-level. 
  kits_per_facility = merge(kits_per_facility, facs_per_district, by=c('dps', 'year'), all.x = TRUE)
  kits_per_facility[, kits_per_fac:=round(value/facs, 2)]
  
  #Merge with coordinate system so it can be mapped 
  kits_per_fac_map = merge(kits_per_facility, coord_ann, by=c('id', 'year'), all.y=TRUE)
  mean_units1 = ggplot(kits_per_fac_map, aes(x=long, y=lat, group=group, fill=kits_per_fac)) +
    coord_fixed() +
    geom_polygon() +
    geom_path(size=0.01) +
    scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) +
    theme_void() +
    facet_wrap(~year, strip.position="bottom") +
    labs(title=paste0(drug_name, " units per facility, by district"), subtitle="Annual data restricted to Jan-Aug",
         caption="*Denominator only includes facilities with 'available, usable stock' of this drug",
         fill="Units per facility")
  return(mean_units1)
}


#Map mean test kits per facility. Try without a facet wrap, and then facet wrap by year.
gen_mean_units2 = function(dt=dt, coord_ann=coord_ann, drug_id, drug_name, date_range){
  facs_per_district = dt[element_id==drug_id & stock_category == "available_usable_stock", .(facs=length(unique(org_unit_id))), by=c('dps','year')] #Find the number of facs per district with available test-kit stock. 
  
  kits_per_facility = dt[element_id==drug_id & stock_category == "available_usable_stock", .(element_id, value, dps, year, id, date)]
  kits_per_facility = kits_per_facility[(date< "2018-09-01" & year == 2018) | (date < '2017-09-01' & year == 2017)] #Subset to handle time lags. 
  kits_per_facility = kits_per_facility[, .(value=sum(value, na.rm = TRUE)), by = c('element_id', 'dps', 'year', 'id')] #Collapse here, because you want to get rid of the date-level. 
  kits_per_facility = merge(kits_per_facility, facs_per_district, by=c('dps', 'year'), all.x = TRUE)
  kits_per_facility[, kits_per_fac:=round(value/facs, 2)]
  kits_per_facility = merge(kits_per_facility, coord_ann, by=c('id', 'year'), all.y=TRUE)
  
  #----------------------
  #For each DPS, find it's center. 
  districts = unique(kits_per_facility$dps)
  districts = districts[!is.na(districts)]
  all_centers = data.table()
  for (district in districts){
    centers = kits_per_facility[dps==district, .(long, lat)]
    center = as.data.table(centroid(centers))
    center[, dps:=district]
    all_centers = rbind(all_centers, center)
  }
  
  # Generate a labels dataset
  labels = unique(kits_per_facility[, .(id, dps, kits_per_fac, year)])
  labels[, label:= paste0(dps, ": ", kits_per_fac)]
  labels = merge(labels, all_centers, by=c('dps'))
  #----------------------
  
  mean_units2 = ggplot(kits_per_facility, aes(x=long, y=lat, group=group, fill=kits_per_fac)) + 
    geom_polygon() + 
    theme_void() + 
    facet_wrap(~year) + 
    scale_fill_gradientn('Units per\nFacility', colours=ratio_colors) + 
    coord_fixed(ratio=1) + 
    scale_x_continuous('', breaks = NULL) + 
    scale_y_continuous('', breaks = NULL) + 
    labs(title=paste0(drug_name, " units per facility, by district"), subtitle="Annual data restricted to Jan-Aug",
         caption="*Denominator only includes facilities with 'available, usable stock' of this drug",
         fill="Units per facility") + 
    theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) + 
    geom_label_repel(data = labels, aes(label = label, x = lon, y = lat, group = label), inherit.aes=FALSE, size=3)
  return(mean_units2)
}

#--------------------------------------------------------------
# Stock out rate of change by month
#--------------------------------------------------------------
gen_monthly_so_roc1 = function(dt=dt, coord_months=coord_months, drug_id, drug_name, date_range){
  facs_per_district = dt[stock_category == "number_of_days_stocked_out" & !is.na(value) & element_id==drug_id, .(facs=length(unique(org_unit_id))), by=c('dps', 'date')] #Exclude impossible day values here. 
  
  monthly_so_rate_dps = dt[stock_category == 'number_of_days_stocked_out' &!is.na(value) & element_id==drug_id, .(id, value, date, expected_days, dps)]
  monthly_so_rate_dps = monthly_so_rate_dps[, .(value=sum(value, na.rm = TRUE)), by=c('id', 'date', 'expected_days', 'dps')]
  
  dups = monthly_so_rate_dps[duplicated(monthly_so_rate_dps, by=c('id', 'date', 'dps'))]
  dups = merge(dups, monthly_so_rate_dps, by=c('id', 'date', 'dps'))
  stopifnot(nrow(dups)==0)
  
  monthly_so_rate_dps = merge(monthly_so_rate_dps, facs_per_district, by=c('dps', 'date'))
  
  #Generate a variable at the dps level. 
  monthly_so_rate_dps[, expected_days_dps:=expected_days*facs]
  monthly_so_rate_dps[, monthly_so_rate:=round(value/expected_days_dps, 2)]
  
  monthly_so_rate_map = merge(monthly_so_rate_dps, coord_months, by=c('id', 'date'), all.y=TRUE) 
  monthly_so_change = data.table()
  for (district in unique(monthly_so_rate_dps$dps)){
    # i = 1. If stock out days for i = 2 are higher than me, 'increase'. Otherwise 'decrease'. 
    temp = monthly_so_rate_dps[dps==district]
    if (nrow(temp)!=1){
      for (i in 2:nrow(temp)-1){
        temp$status[i+1] = ifelse(temp$monthly_so_rate[i]<temp$monthly_so_rate[i+1], "INCREASE", "DECREASE")
        temp$change[i+1] = temp$monthly_so_rate[i+1]-temp$monthly_so_rate[i]
      }
    }
    monthly_so_change= rbind(monthly_so_change, temp, fill=T)
  }
  
  monthly_so_change_map = merge(monthly_so_change, coord_months, by=c('id', 'date'), all.y=TRUE) 
  monthly_so_roc1 = ggplot(monthly_so_change_map[year(date)==2018], aes(x=long, y=lat, group=group, fill=change)) +
    coord_fixed() +
    geom_polygon() +
    geom_path(size=0.01) +
    theme_void() +
    colScale2+
    facet_wrap(~date) +
    labs(title=paste0(drug_name, " rate of change of stock-outs by district, all facilities"), subtitle="Data controlled for reporting",
         caption = "*Denominator only includes facilities that reported data for the given treatment regimen", fill="Month-to-month change")
  return(monthly_so_roc1)
}

#---------------------------------------
# Full months-stocked out
#---------------------------------------
gen_full_months = function(dt=dt, coord_months, drug_id, drug_name, date_range){
  full_months = dt[stock_category == "number_of_days_stocked_out" & !is.na(value) & element_id==drug_id
                   & value==expected_days] 
  full_months[, count:=1]
  full_months = full_months[, .(facs_full_month=sum(count)), by=c('date', 'id', 'dps')]
  facs_per_district = dt[stock_category == "number_of_days_stocked_out" & !is.na(value) & element_id==drug_id,
                         .(facs=length(unique(org_unit_id))), by=c('dps', 'date')] #Exclude impossible day values here. 
  
  full_months = merge(full_months, facs_per_district, by=c('dps', 'date'))
  full_months[, full_month_rate:=(facs_full_month/facs)*100]
  
  full_months = merge(full_months, coord_months, by=c('id', 'date'), all.y=TRUE) 
  return(full_months)
}

gen_full_months1_17 = function(dt=dt, coord_months, drug_id, drug_name, date_range){
  
  full_months = gen_full_months(dt=dt, coord_months, drug_id, drug_name, date_range)
  full_months1 = ggplot(full_months[year(date)==2017], aes(x=long, y=lat, group=group, fill=facs_full_month)) +
    coord_fixed() +
    geom_polygon() +
    geom_path(size=0.01) +
    theme_void() +
    scale_2017 + 
    facet_wrap(~date) +
    labs(title=paste0("Count of facilities with a full month of stockout for\n ", drug_name, 
                      " by DPS in 2017"),
         caption = "*Denominator only includes facilities that reported data for the given treatment regimen", 
         fill = "# of facilities")
  return(full_months1)
  
}

gen_full_months1_18 = function(dt=dt, coord_months, drug_id, drug_name, date_range){
  
  full_months = gen_full_months(dt=dt, coord_months, drug_id, drug_name, date_range)
  full_months1 = ggplot(full_months[year(date)==2018], aes(x=long, y=lat, group=group, fill=facs_full_month)) +
    coord_fixed() +
    geom_polygon() +
    geom_path(size=0.01) +
    theme_void() +
    scale_2018 + 
    facet_wrap(~date) +
    labs(title=paste0("Count of facilities with a full month of stockout for\n ", drug_name, 
                      " by DPS in 2018"),
         caption = "*Denominator only includes facilities that reported data for the given treatment regimen", 
         fill = "# of facilities")
  return(full_months1)
  
}

gen_full_months2_17 = function(dt=dt, coord_months, drug_id, drug_name, date_range){
  
  full_months = gen_full_months(dt=dt, coord_months, drug_id, drug_name, date_range)
  full_months2 = ggplot(full_months[year(date)==2017], aes(x=long, y=lat, group=group, fill=full_month_rate)) +
    coord_fixed() +
    geom_polygon() +
    geom_path(size=0.01) +
    theme_void() +
    scale_2017 + 
    facet_wrap(~date) +
    labs(title=paste0("Proportion of facilities reporting that had a full month \nstocked out of ", drug_name, 
                      " in 2017"),
         caption = "# of facilities with a full month stocked out/# of facilities reporting for this district in this month", 
         fill = "% of facilities")
  return(full_months2)
  
}

gen_full_months2_18 = function(dt=dt, coord_months, drug_id, drug_name, date_range){
  
  full_months = gen_full_months(dt=dt, coord_months, drug_id, drug_name, date_range)
  full_months2 = ggplot(full_months[year(date)==2018], aes(x=long, y=lat, group=group, fill=full_month_rate)) +
    coord_fixed() +
    geom_polygon() +
    geom_path(size=0.01) +
    theme_void() +
    scale_2018 + 
    facet_wrap(~date) +
    labs(title=paste0("Proportion of facilities reporting that had a full month \nstocked out of ", drug_name, 
                      " in 2018"),
         caption = "# of facilities with a full month stocked out/# of facilities reporting for this district in this month", 
         fill = "% of facilities")
  return(full_months2)
  
}