# Emily Linebarger 
# Making UGA GHE graphic, and quick statistics. 


#Funding landscape graph. 
odah = readRDS("J:/Project/Evaluation/GF/resource_tracking/_odah/prepped_data/other_dah_actuals_all_uga.rds")

ghe_tb = readRDS("J:/Project/Evaluation/GF/resource_tracking/_ghe/fgh_ghe_actuals_tb/prepped_data/ghe_actuals_tb.rds")
ghe_tb = ghe_tb[loc_name=="uga"]
ghe_tb = ghe_tb[source_type=="nha"]

odah1 = odah[disease=="tb", .(disbursement=sum(disbursement, na.rm=TRUE)), by=c('channel_agg', 'year')]
ghe_tb1 = ghe_tb[, .(disbursement=sum(expenditure, na.rm=TRUE)), by=c('year')]
ghe_tb1[, channel_agg:="Government Health Expenditure"]

dt = rbind(odah1, ghe_tb1)
dt = dt[year>=2010 & year<=2017]

dt[channel_agg=="UN agencies, The World Bank and other regional development banks", 
   channel_agg:="UN agencies, The World Bank and\n other regional development banks"]

dt[, channel_agg:=factor(channel_agg, levels=c("Government Health Expenditure", "Multilateral organizations (GAVI, CEPI)", "NGOs and foundations", "Other bilateral assistance",                                                      
                                                  "U.S. bilateral assistance", "UN agencies, The World Bank and\n other regional development banks",
                                                  "The Global Fund"))]

#Overall funding landscape
funding_landscape = ggplot(data = dt, aes(x = year, y = disbursement, fill = channel_agg)) + 
  geom_ribbon(aes(ymin = 0, ymax = disbursement), position = "stack") + 
  theme_bw(base_size = 18) + theme(legend.title = element_blank())+
  scale_y_continuous(expand = c(0,0), labels = scales::dollar) +
  scale_x_continuous(expand = c(0,0))+
  scale_fill_brewer(palette = "RdYlBu") +
  labs(x = "Year", y = "Disbursement", title = paste0("Funding landscape in Uganda for TB, including GHE"), 
       caption="*GHE data only available from 2010-2015")

ggsave("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/UGA TB Deliverable September 2019/funding_lanscape.png", funding_landscape, height=10, width=14)

ghe_only = ggplot(data = dt[channel_agg=="GHE"], aes(x = year, y = disbursement)) + 
  geom_ribbon(aes(ymin = 0, ymax = disbursement), position = "stack", fill="firebrick3") + 
  theme_bw(base_size = 18) + theme(legend.title = element_blank())+
  scale_y_continuous(expand = c(0,0), labels = scales::dollar) +
  scale_fill_brewer(palette = "RdYlBu") +
  labs(x = "Year", y = "Expenditure", title = "TB Government Health Expenditure", 
       caption="*GHE data only available from 2010-2015\nGHE data extracted from NHAs")

ggsave("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/UGA TB Deliverable September 2019/ghe_spending.png", ghe_only, height=10, width=14)


#What % of the average TB spend does Uganda TB represent? 
non_ghe = dt[year>=2010 & year<=2015 & channel_agg!="GHE", .(non_ghe=sum(disbursement, na.rm=T)), by='year']
ghe = dt[year>=2010 & year<=2015 & channel_agg=="GHE", .(ghe=sum(disbursement, na.rm=T)), by='year']
compare = merge(non_ghe, ghe, by='year')

compare[, avg_ghe:=round((ghe/non_ghe)*100, 2)]


#Within UGA GHE, how much is TB? 
hiv = readRDS("J:/Project/Evaluation/GF/resource_tracking/_ghe/fgh_ghe_estimates_hiv/prepped_data/prepped_fgh_estimates.rds")
hiv = hiv[loc_name=="uga" & financing_source=="ghe" & fin_data_type=="model_estimates"]
hiv = hiv[, .(disbursement=sum(disbursement, na.rm=T)), by='year']
hiv[, disease:="hiv"]

malaria = readRDS("J:/Project/Evaluation/GF/resource_tracking/_ghe/fgh_ghe_actuals_malaria/prepped_data/ghe_actuals_malaria.rds")
malaria = malaria[loc_name=="uga" & source_type=="nha", .(disbursement=sum(value, na.rm=TRUE)), by=c('year')]
malaria[, disease:="malaria"]
 #Doing this because there are no NHAs, and the global fund sources are duplicates with WMR. 

tb = ghe_tb[, .(disbursement=sum(expenditure, na.rm=TRUE)), by='year']
tb[, disease:="tb"]

dt2 = rbindlist(list(hiv, malaria, tb))
dt2_wide = dcast(dt2, year~disease, value.var='disbursement')

dt2_wide[, total:=sum(hiv, malaria, tb, na.rm=TRUE), by='year']
dt2_wide[, tb_prop:=round((tb/total)*100, 2)]

dt2 = dt2[year>=2011 & year<=2014]

dt2[, disease:=factor(disease, levels=c('tb', 'malaria', 'hiv'), labels=c('TB', 'Malaria', 'HIV'))]

ghe_only2 = ggplot(data = dt2, aes(x = year, y = disbursement, fill=disease)) + 
  geom_ribbon(aes(ymin = 0, ymax = disbursement), position = "stack") + 
  theme_bw(base_size = 18) + 
  theme(legend.title = element_blank())+
  scale_y_continuous(labels = scales::dollar) +
  scale_fill_brewer(palette = "RdYlBu") +
  labs(x = "Year", y = "Expenditure", title = "Government Health Expenditure by disease", 
       caption="*Data only available for all diseases from 2010-2014\nGHE data extracted from NHAs", 
       fill="Disease")

ggsave("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/UGA TB Deliverable September 2019/ghe_spending_all.png", ghe_only2, height=10, width=14)

library(gridExtra)
grid.arrange(ghe_only2, ghe_only, nrow=1) 

ggsave("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/UGA TB Deliverable September 2019/ghe_spending_combined.png", grid.arrange(ghe_only2, ghe_only, nrow=1), height=10, width=15)
