# Caitlin O'Brien-Carelli
#
# 1/9/2019
# Reshape the Early Infant Diagnosis data sets together
# Merge in the information about facilities
# Creates a usable EID data set 
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(ggplot2)
library(stringr) 
library(plyr)
library(RColorBrewer)
# --------------------

# -----------------------------------------------
# detect if operating on windows or on the cluster 

j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# --------------
# set files and directories for the uganda viral load data

# set the working directory to loop over the downloaded files
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/eid/')
setwd(dir)

# load the data 
dt = readRDS(paste0(dir, '/eid_2015_2018.rds'))

#---------------------------------
# tests performed by type
tests = dt[ ,.(value=sum(total_tests)), by=.(date, sex, variable=pcr)]

# facilities reporting
facilities = dt[ ,.(value=length(unique(id))), by=date]
facilities[ , sex:='Facilities Reporting']
facilities[ , variable:='facility']

# tests and facilities reporting by sex
tests = rbind(tests, facilities)

# label the variables
tests$variable = factor(tests$variable, c('first', 'second', 'repeat', 'facility'),
       c('1st PCR', '2nd PCR', 'Repeat PCR', 'Health facility'))

# graph tests and facilities reporting by sex
ggplot(tests[sex!='Unknown' & year(date)!=2015], aes(x=date, y=value, color=variable)) +
         geom_point() +
         geom_line() +
         facet_wrap(~sex, scales='free_y') +
         scale_color_manual(values=brewer.pal(4, 'RdYlBu')) +
         theme_bw() +
         labs(x='Date', y='Count', color='')
 
#---------------------------------
# positivity ratio

pos = dt[ ,.(value=round(100*(sum(hiv_pos_inf)/sum(total_tests)), 1)), by=.(date, sex, pcr)]

# labels 
f = dt[sex=='Female', sum(total_tests)]
m = dt[sex=='Male', sum(total_tests)]
u = dt[sex=='Unknown', sum(total_tests)]

# 
pos[sex=='Female', sex_alt:=paste0('Female (n=', f, ')')]
pos[sex=='Male', sex_alt:=paste0('Male (n=', m, ')')]
pos[sex=='Unknown', sex_alt:=paste0('Uknown (n=', u, ')')]

# label pcr tests
pos$pcr = factor(pos$pcr, c('first', 'second', 'repeat'),
               c('1st PCR', '2nd PCR', 'Repeat PCR'))

# positivity ratio
ggplot(pos, aes(x=date, y=value, color=sex_alt)) +
  geom_point() +
  geom_line() +
  facet_wrap(~pcr, scales='free_y') +
  scale_color_manual(values=brewer.pal(4, 'RdYlBu')) +
  theme_bw() +
  labs(x='Date', y='Percent positive (%)', color='Sex (n = total tests performed)')

#-------------------------------------
# art initiation

art = dt[ ,lapply(.SD, sum), .SDcols=2:4, by=.(date, sex)]
art[  ,ratio:=round(100*(art_initiated/hiv_pos_inf), 1)]

ggplot(art[sex!='Unknown'], aes(x=date, y=ratio, color=sex)) +
  geom_point(alpha=0.6) +
  geom_line(alpha=0.6) +
  scale_color_manual(values=c('#d73027', '#313695')) +
  theme_bw() +
  labs(x='Date', y='Percent initiated (%)', color='Sex')

#-------------------------------------
# totals

# tests performed by type
tests = dt[ ,.(value=sum(total_tests)), by=date]

# graph tests and facilities reporting by sex
ggplot(tests, aes(x=date, y=value)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x='Date', y='Count', color='')

# art initiation percentage
art_tot = dt[ ,lapply(.SD, sum), .SDcols=2:4, by=date]
art_tot[  ,ratio:=round(100*(art_initiated/hiv_pos_inf), 1)]

ggplot(art_tot, aes(x=date, y=ratio)) +
  geom_point(alpha=0.6) +
  geom_line(alpha=0.6) +
  scale_color_manual(values=c('#d73027', '#313695')) +
  theme_bw() +
  labs(x='Date', y='Percent initiated (%)', color='Sex')

art_sub = art_tot[ ,,lapply(.SD, sum), .SDcols=3:4, by=date]


