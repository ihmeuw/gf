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
data <- readRDS("J:/Project/Evaluation/GF/outcome_measurement/sen/prepped_data/sen_tb_indicators.rds")
codebook <- fread("J:/Project/Evaluation/GF/outcome_measurement/sen/prepped_data/codebook.csv")

ylabel = codebook[code=='tpm_nc']$English
data[, date:=year+((trimestre-1)/4)]
districts = unique(data$district)
plots = list()

i=1
for(d in districts) {
plots[[i]] = ggplot(data[district==d], aes(y=tpm_nc, x=date)) + 
  geom_point() + 
  geom_line() +
  labs(title='', y=ylabel, x='') +
  theme_bw()
i=i+1
}

pdf(outputFilePath, height=5.5, width=9)
i=1
for(d in districts) {
print(plots[[i]])
i=i+1
}
dev.off()

# for creating panels within one sheet
data = melt(data, id.vars=c('region','district','annee','trimestre','date'))
data = data[variable %in% c('tpm_nc', 'tpm_tr', 'tpm_tfn', 'tpm_fnf')]
data = merge(data, codebook, by.x='variable', by.y='code')
plots = list()
i=1
for(d in districts) {
plots[[i]] = ggplot(data[district==d], aes(y=tpm_nc, x=date)) +
geom_point() +
geom_line() +
facet_wrap(~English, scales='free') +
labs(title='', y=ylabel, x='') +
theme_bw()
i=i+1
}

data = data[, lapply(.SD, sum), by=c('region','annee','trimestre','date'), SDcols=names(data)[!names(data) %in% c('region','annee','trimestre','date','district')]]
