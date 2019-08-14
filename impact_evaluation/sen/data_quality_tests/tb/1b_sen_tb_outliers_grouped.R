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
data <- readRDS("C:/Users/frc2/Documents/data/tb/prepped_data/sen_tb_indicators_2.rds")
codebook <- fread("C:/Users/frc2/Documents/data/tb/prepped_data/codebook2.csv")

# set parameters for graphs
data[, date:=annee+((trimestre-1)/4)] # create a new data variable
centre = unique(data$centre)

keep <- c('gueris_total', 'gueris_taux', 'trait_tot', 'trait_pc') # choose indicators to keep ###

# to graph certain indciators together on one page
data = melt(data, id.vars=c('region','centre','type','annee','trimestre','date'))
data = data[variable %in% keep] 
data = merge(data, codebook, by.x='variable', by.y='Code')

plots = list()

i=1
for(c in centre) {
  plots[[i]] = ggplot(data[centre==c], aes(y=value, x=date)) +
    geom_point() +
    geom_line() +
    facet_wrap(~English, scales='free') +
    labs(title=c, y='', x='') +
    theme_bw()
  i=i+1
}


# save a pdf file of graphs
outputFilePath = "J:/Project/Evaluation/GF/impact_evaluation/sen/data_quality_tests/tb/cured_grouped.pdf"

pdf(outputFilePath, height=5.5, width=9)
i=1
for(c in centre) {
  print(plots[[i]])
  i=i+1
}
dev.off()

