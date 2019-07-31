# Author: Francisco Rios Casas
# Title: regional_sen_tb_outliers_grouped
# Date: July 25 2019
# Create time series graphs of TB data to look for outliers
# grouped by Region

#set up
library(data.table)
library(ggplot2)
# -------------------

data <- readRDS("C:/Users/frc2/Documents/data/tb/prepped_data/sen_tb_indicators_2.rds")
codebook <- fread("C:/Users/frc2/Documents/data/tb/prepped_data/codebook2.csv")

data[, date:=annee+((trimestre-1)/4)]
data = data[, lapply(.SD, sum), by=c('region','annee','trimestre','date'), .SDcols=names(data)[!names(data) %in% c('region','annee','trimestre','date','centre','type')]]

# set parameters for graphs
ylabel = codebook[Code=='tpm_fp_nc']$English
regions = unique(data$region)
plots = list()

i=1
for(r in regions) {
  plots[[i]] = ggplot(data[region==r], aes(y=tpm_fp_nc, x=date)) + # y=variable to be plotted
    geom_point() + 
    geom_line() +
    labs(title=r, y=ylabel, x='') +
    theme_bw()
  i=i+1
}

# save a pdf file of graphs
outputFilePath = "C:/Users/frc2/Documents/data_quality/tb/regional_pulmonary_tb_nc_outliers.pdf"

pdf(outputFilePath, height=5.5, width=9)
i=1
for(r in regions) {
  print(plots[[i]])
  i=i+1
}
dev.off()