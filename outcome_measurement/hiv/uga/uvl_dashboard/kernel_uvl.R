# ----------------------------------------------
# Caitlin O'Brien-Carelli

# 5/11/2018
# pliots for terg presentation
# ----------------------------------------------
# Set up R

rm(list=ls())
library(data.table)
library(rgeos)
library(raster)
library(ggplot2)
library(rgdal)
library(tibble)
library(dplyr)
library(RColorBrewer)
library(Amelia)
library(MASS)
library(gtools)

# set input/output directory
# ----------------------------------------------
dir <- 'J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard'

# upload the data with month, year, sex
uvl1 <- readRDS(paste0(dir, "/sex_data.rds"))
uganda_vl <- uvl1

uganda_vl <- uganda_vl[!(month==3 & year==2018)] 
uganda_vl <- uganda_vl[sex!='Unknown'] 


uvl <- uganda_vl[ ,.(valid_results=sum(valid_results), suppressed=sum(suppressed)), by=.(district_id, dist_name, year)]
uvl[ , ratio:=(100*(suppressed/valid_results))]

colors <- c('#CAF270', '#73D487', '#30B097', '#288993', '#41607A', '#453B52')


uvl1 <- uvl[valid_results>10]

pdf('J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape_agg/outputs/kernels.pdf', height=6, width=9)

ggplot(uvl1, aes(x=factor(year), y=ratio)) + 
geom_violin(fill=colors[4], alpha=.7) +
labs(title='Equity', subtitle='Geographic Spread', y='Viral load Suppression', 
     x=sprintf('< Density of Districts >'), caption='Restricted to districts with >10 valid samples to avoid bias from scale-up') +
    theme_minimal(base_size=16) + 
    theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5), 
    axis.title.x=element_text(size=11))

ggplot(uvl1[year!=2014], aes(x=factor(year), y=ratio)) + 
  geom_violin(fill=colors[4], alpha=.7) +
  labs(title='Equity', subtitle='Geographic Spread', y='Viral load Suppression', 
       x=sprintf('< Density of Districts >'), caption='Restricted to districts with >10 valid samples to avoid bias from scale-up') +
  theme_minimal(base_size=16) + 
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5), 
        axis.title.x=element_text(size=11))

dev.off()
