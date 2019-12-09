# Author: Francisco Rios Casas
# Title: sen_treatment_success_rate national
# Date: July 25 2019
# Create time series graphs of TB data to look for outliers

# ------------------
# set up
library(data.table)
library(ggplot2)
# -------------------

# load data and codebook
data <- fread("J:/Project/Evaluation/GF/impact_evaluation/sen/prepped_data/prepped_outputs_outcomes.csv")
#codebook <- fread("J:/Project/Evaluation/GF/impact_evaluation/sen/raw_data/codebook2.csv")

# subset data as necessary
#data = data[annee>=2014 & annee<201]
#data$year <- as.integer(data$date)

# sum by region
data2 = data[, lapply(.SD, sum, na.rm=TRUE), by=c('region','annee'), 
             .SDcols='tot_genexpert']

# sum by national total
data3 = data[, lapply(.SD, sum, na.rm=TRUE), by=c('annee'), 
             .SDcols='tot_genexpert']


# by regions
ggplot(data2, aes(y=tot_genexpert, x=annee, col=region))+
  geom_point() +
  geom_line() +
  labs(title="", y='', x='') +
  theme_bw()+
  scale_y_continuous(limits = c(0, 700), breaks = seq(0, 700, by = 50))

# by regions
ggplot(data3, aes(y=tot_genexpert, x=annee))+
  geom_point() +
  geom_line() +
  labs(title="", y='', x='') +
  theme_bw()+
  scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, by = 50))
