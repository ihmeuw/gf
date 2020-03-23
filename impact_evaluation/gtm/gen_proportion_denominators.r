#-------------------------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Generate a table of denominators to successfully collapse 
#   municipal-level proportions and rates  for GTM TB model 
# DATE: October 2019 
#-------------------------------------------------------------------------

# The three denominators we need to collapse proportions and rates are 
#   population, TB cases treated, and HIV cases treated by municipality. 

# We'll make those two denominators both quarterly and annually. 

#Read in outputs and impacts data. 
outputs = fread(outputsFile)
outputs = outputs[, .(year, quarter, department, municipality, `Cases Notified_value_m_quarterly`, `HIV/TB Cases Notified_value_m_quarterly`)]
names(outputs) = c('year', 'quarter', 'department', 'municipality','cases_notified', 'hivtb_cases_notified')

impacts = fread(impactFile)
impacts = impacts[, .(year, quarter, department, municipality, `Case Notification Rate_value_m_quarterly`)]
names(impacts) = c('year', 'quarter', 'department', 'municipality','case_notification_rate')

#Merge these together. 
municipality_denom_q = merge(outputs, impacts, by=c('year', 'quarter', 'department', 'municipality'), all=TRUE)
municipality_denom_q[, population:=cases_notified/case_notification_rate]
municipality_denom_q = municipality_denom_q[, .(year, quarter, department, municipality, cases_notified, hivtb_cases_notified, population)]

municipality_denom_q[, quarter:=((quarter/4)-0.25)]

#Generate an annual-level dataset as well, also at the municipality level. 
municipality_denom_a = municipality_denom_q[, .(cases_notified=sum(cases_notified), hivtb_cases_notified=sum(hivtb_cases_notified), 
                                                population=sum(population)), by=c('year', 'department', 'municipality')]
#Run a few graphs to make sure these calculated denominators make sense. 
# municipality_denom_q[, date:=year+((quarter/4)-0.25)]
# municip_melt = melt(municipality_denom_q[, .(date, municipality, cases_notified, hivtb_cases_notified, population)], id.vars=c('date', 'municipality'))
# pdf("C:/Users/elineb/Desktop/gtm_population_by_municip.pdf") # Should save this somewhere better
# for (m in unique(municip_melt$municipality)){
#   p = ggplot(municip_melt[municipality==m], aes(x=date, y=value, color=variable)) +
#     geom_point() + 
#     geom_line() + 
#     theme_bw() + 
#     facet_wrap(~variable, scales="free") + 
#     labs(title=paste0("Population for municipality ", m, " over time"), color="Denominator")
#   print(p)
# }
# 
# dev.off() 