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
data <- readRDS("J:/Project/Evaluation/GF/impact_evaluation/sen/prepped_data/inputs_outputs.rds")
codebook <- fread("J:/Project/Evaluation/GF/impact_evaluation/sen/raw_data/codebook2.csv")

# subset data as necessary
data = data[date>=2014 & date<2018.75]

# set parameters for graphs
regions = unique(data$region)

keep <- c('ntr_all', 'gueris_taux', 'tpm_chimio_enf') # choose indicators to keep ###

data2 <- data[,.(mean_tx_sr=sum(gueris_taux),by=c(date))]
View(data2)

data2 = data[, lapply(.SD, sum), by=c('region','date'), 
            .SDcols=names(data)[!names(data) %in% c('region','date')]]

data3 = data[, lapply(.SD, mean), by=c('date'), .SDcols='gueris_taux']

ggplot(data3, aes(y=gueris_taux, x=date))+
  geom_point() +
  geom_line() +
  labs(title="", y='', x='') +
  theme_bw()+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .20))

data4 = data[, lapply(.SD, mean, na.rm=TRUE), by=c('date'), .SDcols='mdr_success_rate']

ggplot(data4, aes(y=mdr_success_rate, x=date))+
  geom_point() +
  geom_line() +
  labs(title="", y='', x='') +
  theme_bw()+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .20))+
  las(title="MDR Success Rate")
