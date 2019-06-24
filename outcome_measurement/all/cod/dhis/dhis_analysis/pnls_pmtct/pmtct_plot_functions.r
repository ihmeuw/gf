#----------------------------------------------------------------
# AUTHOR: Emily Linebarger
# DATE: May/June 2019
# PURPOSE: Create general graph functions that can run on any variables in PNLS dataset
#----------------------------------------------------------------

#------------------------------------------------------
#Reporting completeness 
#------------------------------------------------------
# 1. PREP DATA: Preps reporting data table for a given drug
prep_report = function(dt=dt, number){
  FacsReportingByDate = dt[, .(facilities=length(unique(org_unit_id))), by=date]
  FacsReportingindByDate = dt[element_no==number, .(facs_reporting_on_ind=as.numeric(length(unique(org_unit_id)))), by=date]
  
  #Did they report for the specific ind you're targeting?
  report = merge(FacsReportingByDate, FacsReportingindByDate, by='date', all.x=TRUE)
  
  report[is.na(facs_reporting_on_ind), facs_reporting_on_ind:=0]
  report[ , ind_reporting_ratio:=100*(facs_reporting_on_ind/facilities)]
  
  # convert to numerics and round
  report[, ind_reporting_ratio:=round(ind_reporting_ratio, 1)]
  return(report)
}

# 2. GRAPH FUNCTIONS 
# Number of facilities reporting 
gen_report1 = function(dt=dt, number, ind_name){
  report = prep_report(dt, number)

  report1 = ggplot(report, aes(x=date, y=ind_reporting_ratio)) +
    geom_point(size=0.8) +
    geom_line() +
    geom_line() +
    theme_bw() +
    # facet_wrap(~indicator) +
    scale_color_manual(values = brewer.pal(4, 'RdYlBu')) +
    labs(x='Date', y='% of total facilities', title=paste0('Reporting completeness for indicator: "', ind_name, '"'), 
         caption="*All subpopulations included")

  return(report1)
}

gen_indicator_map1 = function(dt=dt, number, ind_name, subpop_name){
  indicator = dt[element_no==number & subpop==subpop_name, .(value=sum(value, na.rm=T)), by=c('year', 'dps')]
 
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

