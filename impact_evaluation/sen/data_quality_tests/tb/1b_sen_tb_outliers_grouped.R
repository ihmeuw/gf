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

# to graph certain indciators together on one page
data = melt(data, id.vars=c('region','centre','type','annee','trimestre','date'))
data = data[variable %in% c('tpm_fp_nc', 'tpm_fp_em', 'tpm_fp_ef', 'tpm_fp_am')] # choose indicators to keep
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
outputFilePath = "C:/Users/frc2/Documents/data_quality/tb/TPM_data_sen.pdf"

pdf(outputFilePath, height=5.5, width=9)
i=1
for(c in centre) {
  print(plots[[i]])
  i=i+1
}
dev.off()

