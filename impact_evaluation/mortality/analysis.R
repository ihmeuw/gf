# Drivers of morality analysis for Senegal

source('C:/Users/frc2/Documents/gf/impact_evaluation/drc/set_up_r.r')
library(data.table)
library(ggplot2)
library(GGally)
# load 

# plot correlations
ggpairs(data[location_name=="Senegal",c('Deaths', 'Incidence', 'mi_ratio'), with=F])

# transform variables
data[,log_incidence_rate:=log(Incidence)]
data[,logit_mi_ratio:=logit(mi_ratio)]

# graph transformed data
ggpairs(data[location_name=="Senegal",c('Deaths', 'log_incidence_rate', 'logit_mi_ratio'), with=F])
