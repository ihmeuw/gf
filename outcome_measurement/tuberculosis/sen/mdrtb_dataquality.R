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

# subset data as necessary
data = data[date>=2014 & date<2018.75]

# set parameters for graphs
regions = unique(data$region)

keep <- c('other_dah_T3', 'exp_T3', 'dx_count', 'tx_count', 'mdr_tx_rate') # choose indicators to keep ###

# to graph certain indicators together on one page
data = melt(data, id.vars=c('region','date'))
data = data[variable %in% keep]

plots = list()

i=1
for(r in regions) {
  plots[[i]] = ggplot(data[region==r], aes(y=value, x=date)) +
    geom_point() +
    geom_line() +
    facet_wrap(~variable, scales = "free")
    labs(title=r, y='', x='') +
    theme_bw()
  i=i+1
}


# save a pdf file of graphs
outputFilePath = "J:/Project/Evaluation/GF/impact_evaluation/sen/data_quality_tests/tb/mdr_tb_by_region.pdf"

pdf(outputFilePath, height=5.5, width=9)
i=1
for(r in regions) {
  print(plots[[i]])
  i=i+1
}
dev.off()