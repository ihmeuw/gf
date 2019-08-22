# Make quick DRC funding landscape for malaria only. 
save_loc = "J:/Project/Evaluation/GF/resource_tracking/visualizations/random/cod/august_2019_workshop/"

odah = readRDS("J:/Project/Evaluation/GF/resource_tracking/_fgh/prepped_data/other_dah_actuals_all_cod.rds")
odah = odah[disease=="malaria"]
odah[channel_agg=="UN agencies, The World Bank and other regional development banks", channel_agg:="UN agencies, The World Bank and\nother regional development banks"]

ghe = readRDS("J:/Project/Evaluation/GF/resource_tracking/_fgh/prepped_data/ghe_actuals_malaria.rds")
ghe = ghe[loc_name=="cod" & !is.na(value)]

#Check for ghe source overlap. 
ghe_overlap = unique(ghe[, .(year, source_type)])
ghe_overlap[, seq:=seq(0, 3, by=1), by='year']

#Only duplicates are in years where there are NHAs - so keep those. 
ghe = ghe[!(year%in%c(2012, 2013, 2014) & source_type!='nha')]

#Check again to make sure. 
ghe_overlap = unique(ghe[, .(year, source_type)])
ghe_overlap[, seq:=seq(0, 3, by=1), by='year']



#-----------------------------------------------
# MAKE GRAPHS 
#-----------------------------------------------
odah_agg = odah[year>=2003, .(disbursement=sum(disbursement, na.rm=T)), by=c('year', 'channel_agg', 'gf_module')]
odah_agg[channel_agg=="UN agencies, The World Bank and other regional development banks", channel_agg:="UN agencies, The World Bank and\nother regional development banks"]

#Disaggregate by funder and module where available. 
funding_landscape1 = ggplot(data = odah_agg, aes(x = year, y = disbursement, fill = channel_agg)) + 
  geom_ribbon(aes(ymin = 0, ymax = disbursement), position = "stack") + 
  theme_bw(base_size = 14) + theme(legend.title = element_blank())+
  scale_y_continuous(labels = scales::dollar) +
  scale_x_continuous(expand = c(0,0))+
  facet_wrap(~gf_module) + 
  scale_fill_brewer(palette = "RdYlBu") +
  labs(x = "Year", y = "Disbursement", title = "Funding landscape by malaria intervention, Other DAH 2003-2018")

ggsave(paste0(save_loc, "odah_by_funder_and_module.png"), plot = funding_landscape1, height=6, width=14)

#Disaggregate just by funder. 
odah_agg2 = odah[year>=2003, .(disbursement=sum(disbursement, na.rm=T)), by=c('year', 'channel_agg')]
ghe_agg = ghe[year>=2003, .(disbursement=sum(value, na.rm=T)), by=c('year')]
ghe_agg[, channel_agg:='GHE']

merge_file = rbind(odah_agg2, ghe_agg)
funding_landscape2 = ggplot(data = merge_file, aes(x = year, y = disbursement, fill = channel_agg)) + 
  geom_ribbon(aes(ymin = 0, ymax = disbursement), position = "stack") + 
  theme_bw(base_size = 14) + theme(legend.title = element_blank())+
  scale_y_continuous(labels = scales::dollar) +
  scale_x_continuous(expand = c(0,0))+
  scale_fill_brewer(palette = "RdYlBu") +
  labs(x = "Year", y = "Disbursement", title = "Malaria funding landscape, 2003-2018", caption="*GHE data only available for 2006-2016, not including 2010.")

ggsave(paste0(save_loc, "odah_by_funder_includes_ghe.png"), plot = funding_landscape2, height=6, width=14)


#Disaggregate just by module.
odah_agg3 = odah[year>=2003, .(disbursement=sum(disbursement, na.rm=T)), by=c('year', 'gf_module')]

funding_landscape3 = ggplot(data = odah_agg3, aes(x = year, y = disbursement, fill = gf_module)) + 
  geom_ribbon(aes(ymin = 0, ymax = disbursement), position = "stack") + 
  theme_bw(base_size = 14) + theme(legend.title = element_blank())+
  scale_y_continuous(labels = scales::dollar) +
  scale_x_continuous(expand = c(0,0))+
  scale_fill_brewer(palette = "RdYlBu") +
  labs(x = "Year", y = "Disbursement", title = "Other DAH Malaria Spending, 2003-2018")

ggsave(paste0(save_loc, "odah_by_gf_module.png"), plot = funding_landscape3, height=6, width=14)

