# ----------------------------------------------------
# David Phillips / Audrey Batzel
# 
# 8/23/2019
# Measure drivers of mortality using GBD estimates / data at the national level
# Note: set the working directory to the root of the repository
# ----------------------------------------------------

# ----------------
# Set up R
rm(list=ls())
library(data.table)
library(car) # for "Anova" not "anova"
library(GGally)
library(gridExtra)
library(boot)
library(RColorBrewer)
# ----------------

# --------------------------------------------------------------------------------------
# Files and directories

# root directory
dir = 'J:/Project/Evaluation/GF/impact_evaluation/'

# input files
inFile = paste0(dir, 'mortality/prepped_data/tb_malaria_pce_countries_data.rds') 
# # try this one to compare?
# inFile = paste0(dir, 'mortality/prepped_data/tb_pce_data.rds') # getting same results with original data

# output files
# outFile set in loop by country and disease
# --------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------
# functions
# --------------------------------------------------------------------------------------
# setwd('C:/local/gf/')
source('impact_evaluation/mortality/functions/estimate_explained_variance.R')

# define smithsonTransform function
smithsonTransform = function(x) { 
  N=length( x[!is.na(x)] )
  prop_lsqueeze = logit(((x*(N-1))+0.5)/N)}
# --------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------
# Load/prep data
# --------------------------------------------------------------------------------------
data = readRDS(inFile)
setnames(data, 'Deaths', 'mortality_rate')
setnames(data, 'Incidence', 'cases_var')
# --------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------
# subset and loop through country-disease pairs
# --------------------------------------------------------------------------------------
country_disease = unique(data[, .(disease, country)])
all_data = copy(data)

for (row in 1:nrow(country_disease)){
  subset_country = country_disease[row, country]
  subset_disease = country_disease[row, disease]
  
  data = all_data[ country == subset_country & disease == subset_disease, ]
  
  # --------------------------------------------------------------------------------------
  # Set up variables
  # --------------------------------------------------------------------------------------
  # transform variables
  offset1 = quantile(data[mortality_rate>0]$mortality_rate,.01)
  offset2 = quantile(data[cases_var>0]$cases_var,.01)
  offset3 = quantile(data[mi_ratio>0]$mi_ratio,.01)
  data[, log_mortality_rate:=log(mortality_rate+offset1)]
  data[, log_cases_var:=log(cases_var+offset2)]
  data[, tmp:=mi_ratio]
  # data[tmp==0, tmp:=offset3]
  data[tmp>=1, tmp:=1]
  data[, logit_mi_ratio:=smithsonTransform(tmp)]
  data$tmp=NULL
  
  # z-standardize
  data[, mortality_rate_std:=(mortality_rate-mean(mortality_rate))/sd(mortality_rate)]
  data[, log_cases_var_std:=(log_cases_var-mean(log_cases_var))/sd(log_cases_var)]
  data[, logit_mi_ratio_std:=(logit_mi_ratio-mean(logit_mi_ratio))/sd(logit_mi_ratio)]
  
  # --------------------------------------------------------------------------------------
  # graph transformed data
  # --------------------------------------------------------------------------------------
  ggpairs_fig = ggpairs(data[, c('mortality_rate_std','log_cases_var_std','logit_mi_ratio_std'), with=F])
  
  # --------------------------------------------------------------------------------------
  # get estimates and explained variances
  # --------------------------------------------------------------------------------------
  evs = estEV(data)
  
  evs_mean = evs[, .(explained_variance=mean(explained_variance)), by='variable']
  options(scipen=999)
  evs_mean
  # --------------------------------------------------------------------------------------
  
  # --------------------------------------------------------------------------------------
  # Set up to graph
  # --------------------------------------------------------------------------------------
  # set up graph data
  graphData = evs_mean
  graphData[variable=='log_cases_var_std', 
            label:=paste('Incidence -', round(explained_variance*100, 1),'%')]
  graphData[variable=='logit_mi_ratio_std', 
            label:=paste('Case Fatality -', round(explained_variance*100, 1),'%')]
  graphData[variable=='Residuals', 
            label:=paste('Unexplained by Model -', round(explained_variance*100, 1),'%')]
  
  # set up national
  id_vars = c('year', 'country', 'disease')
  national = melt(data[, c(id_vars, 'mortality_rate', 'cases_var', 'mi_ratio'), with = FALSE], id.vars=id_vars)
  national[variable=='mortality_rate', variable:='TB Mortality Rate (per 100,000)']
  national[variable=='cases_var', variable:='TB Case Notification Rate (per 100,000)']
  
  # colors
  cols = brewer.pal(3, 'Paired')
  cols = c(cols[c(3,2)], '#969696')
  # --------------------------------------------------------------------------------------
  
  # --------------------------------------------------------------------------------------
  # Graph
  # --------------------------------------------------------------------------------------
  outFile = paste0(dir, 'mortality/visualizations/explained_variance/', subset_country, '_', subset_disease, '_mortality_explained_variance_usingGBDestimates.pdf')
  # open pdf
  pdf(outFile, height=5.5, width=8)
  
  print(ggpairs_fig)
  
  cap ='Case fatality approximated by mortality:incidence ratio\nMortality and incidence rates come from GBD 2017 national estimates'
  
  # graph national EV
  print(ggplot(graphData, aes(y=explained_variance, x=1, fill=label)) + 
          geom_bar(width=1, color='gray90', stat='identity', position='stack') + 
          geom_text(aes(label=label), size=3, position=position_stack(vjust=.5)) +
          annotate('text', label='Declining\nMortality\nRates', y=0, x=-0.5, size=5) +
          coord_polar(theta='y') + 
          scale_fill_manual('', values=cols) +
          labs(title='Impact on Mortality Rate', 
               caption=cap) + 
          theme_void() + 
          theme(legend.position='none'))
  
  # graph national trends
  x_var = 'year'
  print(ggplot(national, aes(y=value, x=get(x_var))) + 
          geom_point() +
          geom_smooth() + 
          facet_wrap(~variable, scales='free') + 
          labs(title='National Trends in Reported Mortality and Case Notification', 
               caption='2017 mortality rate estimated based on trend') + 
          theme_bw())

  # close pdf
  dev.off()
  # --------------------------------------------------------------------------------------
}
