# Author: Francisco Rios Casas
# Title: sen_tb_outliers
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
ylabel = codebook[Code=='perf_lab']$English # label name to be plotted
data[, date:=annee+((trimestre-1)/4)]
centre = unique(data$centre)
plots = list()

i=1
for(c in centre) {
plots[[i]] = ggplot(data[centre==c], aes(y=perf_lab, x=date)) + # y=variable to be plotted
  geom_point() + 
  geom_line() +
  labs(title=c, y=ylabel, x='') +
  theme_bw()
i=i+1
}

# save a pdf file of graphs
outputFilePath = "J:/Project/Evaluation/GF/impact_evaluation/sen/data_quality_tests/tb/perf_lab.pdf" # change title

pdf(outputFilePath, height=5.5, width=9)
i=1
for(c in centre) {
print(plots[[i]])
i=i+1
}
dev.off()



