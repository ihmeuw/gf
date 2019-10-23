#------------------------------------------
# Calculate national treatment success rates in GTM 
# using model data - for Synthesis mad libs 
# Emily Linebarger, October 21, 2019
#-------------------------------------------

library(data.table) 
library(ggplot2) 

outputs = fread("J:/Project/Evaluation/GF/impact_evaluation/gtm/raw_data/outputs_8.9.19.csv")
impacts = fread("J:/Project/Evaluation/GF/impact_evaluation/gtm/raw_data/impact_8.6.19.csv")

#Subset data 
outputs = unique(outputs[, .(date, department,`Cases Started on Treatment_value_d`)]) #There should be just one value per department. 
impacts = unique(impacts[, .(date, department, `Treatment Success Rate_value_d`)]) 

#Merge together 
dt = merge(outputs, impacts, by=c('date', 'department'), all=T)

#Fix names 
names(dt) = gsub(" ", "_", names(dt))
names(dt) = gsub("_value_d", "", names(dt))
names(dt) = tolower(names(dt))

#Run calculations 
dt[, num_successfully_treated:=cases_started_on_treatment*treatment_success_rate, by=c('date')]
dt[, natl_cases_started:=sum(cases_started_on_treatment), by=c('date')]
dt[, natl_cases_treated:=sum(num_successfully_treated), by=c('date')]

dt[, natl_tx_success_rate:=(natl_cases_treated/natl_cases_started)*100, by=c('date')]

natl_rates = unique(dt[, .(date, natl_tx_success_rate)])

#What has the trend been nationally? 
p1 = ggplot(natl_rates, aes(x=date, y=natl_tx_success_rate)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  labs(title="National TB Treatment Success Rate", x="Date", y="Tx. Success Rate(%)")

dt[, department:=as.character(department)]
#Has there been any variation by department? 
p2 = ggplot(dt, aes(x=date, y=treatment_success_rate, group=department, color=department)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  labs(title="TB Treatment Success Rate, by Department", x="Date", y="Tx. Success Rate(%)", color="Department")

p1 
p2
