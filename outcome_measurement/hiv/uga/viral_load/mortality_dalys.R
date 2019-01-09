# Calculate deaths and DALYs
# Caitlin O'Brien-Carelli
# Prep UVL data for analysis
# 12/31/2018
#
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(ggplot2)
library(stringr) 
library(plyr)
library(data.table)
# --------------------

# --------------------
# detect if operating on windows or on the cluster 

root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set working directory
dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/lbd_prev')
setwd(dir)

#--------------------------------------------------------
# import the mmortality and dalys data from gbd 

dt = fread(paste0(dir, '/gbd_deaths_dalys.csv'), stringsAsFactors = FALSE)
tri_sex = c('#bd0026', '#74c476', '#3182bd')

# re-label 'both' as 'total'
dt$sex = factor(dt$sex, c('Both', 'Female', 'Male'), 
                c('Total', 'Female', 'Male'))

# change the order of deaths and dalys
dt$measure = factor(dt$measure, c('Deaths' , 'DALYs (Disability-Adjusted Life Years)'), 
                c('Deaths', 'DALYs (Disability-Adjusted Life Years)'))

#-------------------------
# create plot

pdf(paste0(dir, '/deaths_dalys_graph.pdf'), height=6, width=12)

ggplot(dt[metric=='Number'], aes(x=year, y=val, color=factor(sex))) + 
  geom_point(size=0.9) + 
  geom_line() +
  facet_wrap(~measure, scales='free_y') +
  labs(x='Year', y='Count', color='Sex') +
  scale_color_manual(values=tri_sex) +
  theme_bw() +
  theme(legend.text = element_text(size=16),
        legend.title = element_text(size=18),
        axis.title =  element_text(size=14),
        axis.text = element_text(size=12),
        strip.text.x = element_text(size=16)) +
        scale_y_continuous(labels = scales::comma) 

dev.off()

#-------------------------
