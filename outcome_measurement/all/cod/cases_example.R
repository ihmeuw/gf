# Use prepped SNIS data to create a Tableau data set

# ----------------------------------------------
# Caitlin O'Brien-Carelli
# 9/10/2018
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(xlsx) # does not work on the cluster
library(stringr) 
# --------------------

# --------------------
# set working directories 

# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

# set the folder for 
folder <- 'prepped/'

# import the tableau data sets and rbind them together 
base <- readRDS(paste0(dir, folder, 'tabl_base.rds'))

#-----------------------------------------------

base[ ,.(unique(element_eng)), by=element_id]

base[element_id=='aZwnLALknnj' , graph:='suspected']
base[element_id=='aK0QXqm8Zxn' | element_id=='JXm8J8GRJxI', graph:='presumed']
base[element_id=='rfeqp2kdOGi' | element_id=='nRm30I4w9En', graph:='confirmed']

base <- base[!(is.na(graph))]

# remove outlier
base[date=='2018-02-01' & element_id=='aZwnLALknnj', max(value)]
base[value==3713200, value:=NA]

base[date=='2017-04-01' & element_id=='rfeqp2kdOGi', max(value)]
base[value==170172, value:=NA]
#----------------------------------------------

mal <- base[ ,.(value=sum(value, na.rm=T)), by=.(date, element, graph)]

cases <- base[element_id=='aZwnLALknnj' | element_id=='rfeqp2kdOGi' | element_id=='aK0QXqm8Zxn'] 
cases[element_id=='rfeqp2kdOGi', element:='A 1.4 Paludisme présumé + A 1.4 Paludisme simple confirmé' ]          
cases[element_id=='aK0QXqm8Zxn', element:='A 1.4 Paludisme présumé + A 1.4 Paludisme simple confirmé' ]          
cases <- cases[ ,.(value=sum(value, na.rm=T)), by=.(date, element)]


pdf(paste0(dir,'outputs/malaria_cases_question.pdf'), height=6, width=12)

ggplot(mal, aes(x=date, y=value, color=element)) +
  geom_point() +
  geom_line() +
  facet_wrap(~graph) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)

ggplot(mal, aes(x=date, y=value, color=element)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)


ggplot(cases, aes(x=date, y=value, color=element)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_y_continuous(labels = scales::comma) +
  labs(title='Comparison of suspected cases with presumed/confirmed', caption='Presumed and confirmed combined by Caitlin, not SNIS')

dev.off()
