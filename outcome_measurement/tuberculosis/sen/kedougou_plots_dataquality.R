# Author: Francisco Rios Casas
# Title: sen_tb_outliers_grouped
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
data = data[region=="KEDOUGOU"]

# to graph certain indicators together on one page
data = melt(data, id.vars=c('date', 'region'))
data = merge(data, codebook, by.x='variable', by.y='Code', keep=TRUE)

# set parameters for graphs
variables = unique(data$variable)

plots = list()

i=1
for(v in variables) {
  plots[[i]] = ggplot(data[variable==v], aes(y=value, x=date, group=v)) +
    geom_point() +
    geom_line() +
    labs(title=data$English[which(data$variable==v)], y='', x='') +
    theme_bw()
  i=i+1
}


# save a pdf file of graphs
outputFilePath = "J:/Project/Evaluation/GF/impact_evaluation/sen/data_quality_tests/tb/kedougou_plots.pdf"

pdf(outputFilePath, height=5.5, width=9)
i=1
for(v in variables) {
  print(plots[[i]])
  i=i+1
}
dev.off()