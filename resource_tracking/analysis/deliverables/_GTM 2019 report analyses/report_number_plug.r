# Number plug for Guatemala report 2019-2020 
# Emily Linebarger, December 2 2019 

#-----------------------
# PREP DATA 
#-----------------------
rm(list=ls())
library(data.table) 

save_loc = "J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_GTM 2019 annual report/"

absorption = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/GTM/prepped_data/absorption_gtm.rds")
gos = readRDS("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/gos/prepped_data/prepped_gos_data.rds")
cost_categories = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/tableau_data/all_cost_categories.rds")

# What's the average absorption rate, by disease, from 2013-2018? 
# First, remove overlap in semesters from the PUDRs and bind together with the GOS. 
gos = gos[loc_name=="gtm" & start_date>="2013-01-01"]

# Limit absorption so you don't re-cover dates from GOS 
gos_coverage = unique(gos[, .(grant, grant_period, start_date)][order(grant, grant_period, start_date)])
for (i in 1:nrow(gos_coverage)) { 
  absorption = absorption[grant!=gos_coverage$grant[i] & grant_period!=gos_coverage$grant_period[i] & start_date!=gos_coverage$start_date[i]]
}
# Review the remaining data visually 
unique(absorption[, .(grant, grant_period, start_date)][order(grant, grant_period, start_date)])

# Combine this data together. 
plot_data = rbind(gos, absorption, use.names=T, fill=T)
range(plot_data$start_date) # Should be 2013-2018 

#---------------------------------
# Analyses 
#----------------------------------

# How much was budgeted and spent for malaria from 2013-2018? (Figure 14)
figure_14=plot_data[disease=="malaria", .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('year')]
figure_14[, absorption:=round((expenditure/budget)*100, 1)]
figure_14 = melt(figure_14, id.vars=c('year', 'absorption'))
p = ggplot(figure_14, aes(x=year, y=value, fill=variable)) + 
  geom_bar(stat="identity", position="dodge") + 
  theme_bw(base_size=16) + 
  scale_y_continuous(labels=scales::dollar) + 
  labs(title="Global Fund budget for malaria", subtitle="2013-2018", caption="*Does not include RSSH", x="Year", y="", fill="")
ggsave(paste0(save_loc, "figure_14_malaria.png"), p, height=8, width=11)

# What's the average absorption rate by disease over 2013-2018? 
figure_15 = plot_data[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('disease')]
figure_15[, absorption:=round((expenditure/budget)*100, 1)]

# How about by grant disease, to collapse RSSH in? 
figure_15_2 = plot_data[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('grant_disease')]
figure_15_2[, absorption:=round((expenditure/budget)*100, 1)] # It's practically the same. 

# What's the average absorption rate, by disease, on 'personal services' and 'materials' from 2013-2018? 
# We will get as close as we can using cost category data. 