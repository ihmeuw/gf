# Compare PUDRs to SICOIN - graphs for GTM report
# Emily Linebarger 11/19/2019 

dt = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/tableau_data/final_expenditures.rds")

dt[, year:=year(start_date)]
dt = dt[loc_name=="gtm" & start_date>="2013-01-01" & end_date<="2018-10-01", .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), 
        by=c('year', 'disease')]
dt[, absorption:=round((expenditure/budget)*100, 1)]
dt[, label:=paste0(absorption, "%")]
dt[, disease:=toupper(disease)]
dt[disease=="MALARIA", disease:="Malaria"]

p1 = ggplot(dt, aes(x=year, y=absorption, label=label, fill=disease))+ 
  geom_bar(stat="identity") + 
  geom_text(vjust=0) + 
  theme_bw() + 
  facet_wrap(~disease) + 
  labs(title="Absorption by disease for Global Fund grants from 2013-2018", x="Year", 
       y="Absorption", fill="Disease")

ggsave("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_GTM 2019 annual report/gf_absorption.png", p1, height=8, width=12)
