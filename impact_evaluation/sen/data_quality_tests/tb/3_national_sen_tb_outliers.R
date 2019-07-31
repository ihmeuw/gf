# Author: Francisco Rios Casas
# Title: national_sen_tb_outliers_grouped
# Date: July 25 2019
# Create time series graphs of TB data to look for outliers
# National data

#set up
library(data.table)
library(ggplot2)

# load data and codebook
data <- readRDS("C:/Users/frc2/Documents/data/tb/prepped_data/sen_tb_indicators_2.rds")
codebook <- fread("C:/Users/frc2/Documents/data/tb/prepped_data/codebook2.csv")

# reformat data
data[, date:=annee+((trimestre-1)/4)] # create date variable
data = data[, lapply(.SD, sum), by=c('annee','trimestre','date'), .SDcols=names(data)[!names(data) %in% c('region','annee','trimestre','date','centre', 'type')]]

ylabel=codebook[Code=='tpm_fp_nc']$English
ggplot(data, aes(y=tpm_fp_nc, x=date)) + # y=variable to be plotted
  geom_point() + 
  geom_line() +
  labs(title='', y=ylabel, x='') +
  theme_bw()

ylabel=codebook[code=='echec_tot']$English
ggplot(data, aes(y=echec_tot, x=date)) + # y=variable to be plotted
  geom_point() + 
  geom_line() +
  labs(title='', y=ylabel, x='') +
  theme_bw()

# to graph certain indciators together on one page
# reshape the data
data = melt(data, id.vars=c('annee','trimestre','date'))
data = data[variable %in% c('tpm_fp_nc', 'tpm_fp_em', 'tpm_fp_ef', 'tpm_fp_am', 'tpm_fp_af', 'tpm_fp_nc_m', 'tpm_fp_nc_f')] # choose indicators to keep
data = merge(data, codebook, by.x='variable', by.y='Code')

plot1 <- ggplot(data, aes(y=value, x=date)) +
  geom_point() +
  geom_line() +
  facet_wrap(~English, scales='free') +
  labs(title='', y='', x='') +
  theme_bw()

# save a pdf file of graphs
outputFilePath = "C:/Users/frc2/Documents/data_quality/tb/national_TPM_FP_data.pdf"

pdf(outputFilePath, height=6.5, width=11)
plot1
dev.off()

