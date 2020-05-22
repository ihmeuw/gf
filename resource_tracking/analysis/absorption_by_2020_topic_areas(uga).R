rm(list=ls())

library(data.table)
library(ggplot2)
library(scales)
library(readxl)
options(scipen=100)

out_dir = 'J:/Project/Evaluation/GF/resource_tracking/visualizations2020/'

dt_cuml = data.table(read.csv('C:/Users/abatzel/Box Sync/Global Fund Files/UGA/prepped_data/cumulative_absorption_uga_2020-05-20.csv'))
dt_rec = data.table(read.csv('C:/Users/abatzel/Box Sync/Global Fund Files/UGA/prepped_data/most_recent_absorption_uga_2020-05-20.csv'))
topic_areas = data.table(read_xlsx('J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/identifyTopicAreas_PCE2020_forSubsetting.xlsx'))
ta_uga = topic_areas[loc_name == "Uganda"]
ta_uga = ta_uga[, loc_name := NULL]

plot_data = merge(dt_cuml, ta_uga, by= c('disease', 'gf_module', 'gf_intervention'), all = TRUE)

# there are some NAs in 
  # - Topic Area: three rows, where there apparently were not thse module/interventions in the budget
  #    revisions but they are in the PUDRs? Keep these, and just set isTA to FALSE for now
plot_data[is.na(isTopicArea), isTopicArea := FALSE]
  # - There are some that are NA for grant/most of the data (all that's in the PUDR), which means that 
  #    they were in the budgets but NOT in the PU/DRs. We'll also want to follow up on this. But, for now,
  #    I'm just going to drop them from the data.
plot_data = plot_data[!is.na(grant)]

# sum data by Topic Area
plot_data_onlyTA = plot_data[isTopicArea == TRUE, ]
plot_data[ isTopicArea==FALSE & is.na(topicAreaDesc), topicAreaDesc := 'Not Topic Area']
plot_data[topicAreaDesc == 'CRS', topicAreaDesc := 'Direct CSS']

plot_data = plot_data[, .(budget=sum(cumulative_budget, na.rm=T), expenditure=sum(cumulative_expenditure, na.rm=T)), 
               by=c('grant', 'grant_period', 'start_date', 'end_date', 'isTopicArea', 'topicAreaDesc')]

plot_data[, absorption:=(expenditure/budget)*100]
plot_data[, absorption:=round(absorption, 2)]

# save this as a table:
table = plot_data[, .(grant, topicAreaDesc, budget, expenditure, absorption)]
names(table) = c('Grant', 'Topic Area', 'Budget', 'Expenditure', 'Absorption (%)')
setorderv(table, cols = c('Grant', 'Topic Area'))
table[, `Budget` := round(`Budget`)]
table[, `Expenditure` := round(`Expenditure`)]
table[, `Absorption (%)` := round(((`Expenditure`/`Budget`)*100), 2)]
write.csv(table, paste0(out_dir, 'UGA-Cumulative Absorption for Topic Areas (2018-S1 2019).csv'), row.names = FALSE)

# Add a label for the figure
plot_data[, barLabel:=paste0(dollar(expenditure), " (", absorption, "%)")]

# Melt data 
plot_data = melt(plot_data, id.vars=names(plot_data)[!names(plot_data) %in% c('budget', 'expenditure')])
plot_data[variable=="budget", barLabel:=""]
plot_data[variable=="budget", variable:="Budget"]
plot_data[variable=="expenditure", variable:="Expenditure"]

# Bar graph with only topic areas
p1 = ggplot(plot_data[topicAreaDesc != 'Not Topic Area'], aes(x=topicAreaDesc, y=value, fill=variable, label=barLabel)) + 
  geom_bar(stat="identity", position="identity") + 
  theme_bw(base_size=12) + 
  coord_flip() + 
  scale_y_continuous(labels = scales::dollar) + 
  labs(x='Topic Area', y="", fill="", title = 'Cumulative Absorption for PCE 2020 Topic Areas; 2018 - 2019 first semester') + 
  facet_wrap(~grant, scales = 'free') +
  theme(legend.position = 'bottom') +
  geom_text(hjust=0, size = 3) 
print(p1)

# Bar graph with the rest of the grant too
p2 = ggplot(plot_data[], aes(x=topicAreaDesc, y=value, fill=variable, label=barLabel)) + 
  geom_bar(stat="identity", position="identity") + 
  theme_bw(base_size=12) + 
  coord_flip() + 
  scale_y_continuous(labels = scales::dollar) + 
  labs(x='Topic Area', y="", fill="", title = 'Cumulative Absorption for PCE 2020 Topic Areas; 2018 - 2019 first semester') + 
  facet_wrap(~grant, scales = 'free') +
  theme(legend.position = 'bottom') +
  geom_text(hjust=0, size = 3) 
print(p2)

setnames(plot_data_onlyTA, c('cumulative_budget', 'cumulative_expenditure', 'cumulative_absorption'), c('budget', 'expenditure', 'absorption'))
plot_data_onlyTA[, barLabel:=paste0(dollar(expenditure), " (", absorption, "%)")]
plot_data_onlyTA = melt(plot_data_onlyTA, id.vars=names(plot_data_onlyTA)[!names(plot_data_onlyTA) %in% c('budget', 'expenditure')])
plot_data_onlyTA[variable=="budget", barLabel:=""]
plot_data_onlyTA[variable=="budget", variable:="Budget"]
plot_data_onlyTA[variable=="expenditure", variable:="Expenditure"]

plot_data_onlyTA[, xVar := paste0(topicAreaDesc, ": ", gf_intervention)]

# Bar graph of only topic areas by intervention 
p3 = ggplot(plot_data_onlyTA, aes(x=xVar, y=value, fill=variable, label=barLabel)) + 
  geom_bar(stat="identity", position="identity") + 
  theme_bw(base_size=12) + 
  coord_flip() + 
  scale_y_continuous(labels = scales::dollar) + 
  labs(x='Topic Area: Intervention', y="", fill="", title = 'Cumulative Absorption for PCE 2020 Topic Areas; 2018 - 2019 first semester') + 
  facet_wrap(~grant, scales = 'free') +
  theme(legend.position = 'bottom') +
  geom_text(hjust=0, size = 3) 
print(p3)


pdf(paste0(out_dir, 'UGA_absorption_topic_areas.pdf'), height = 12, width = 15)
print(p1)
print(p2)
print(p3)
dev.off()



