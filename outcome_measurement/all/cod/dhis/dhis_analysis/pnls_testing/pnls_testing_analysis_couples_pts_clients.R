# Visualize the PNLS HIV testing data 
# Couples, patients and clients only to parse these specific variables
# ----------------------------------------------
# Caitlin O'Brien-Carelli
# 8/15/2019
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr) 
library(openxlsx)
library(RColorBrewer)
library(gridExtra)
library(grid)
# --------------------

# ------------------------------------
# set working directories

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
# dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

# local directory
dir = "C:/Users/ccarelli/Documents/pnls_data/"

# # read in the data 
# dt = readRDS(paste0(dir, 'prepped/pnls_final/pnls_vct_final.rds'))

# read in the data 
dt = readRDS(paste0(dir, 'pnls_vct_final.rds'))

#------------------------------------
# minor outlier screen

dt = dt[value!=769776 & value!=29841 & value!=10000 & value!=6985 & !(variable=='HIV+' & value==510)]

# outliers noticed
dt = dt[!(subpop=='msm' & value==2757)] # one wonderful facility keeps reporting 2757 MSM

# round the values to integers
dt[ ,value:=round(value)]

#------------------------------------
# COLOR SCHEMES

quad_colors = c('#542788','#66bd63', '#b2182b', '#4575b4')
sex_colors = c('#31a354', '#b2182b', '#4575b4')
tri_colors = c('#a50026', '#fdae61', '#abd9e9')
test_colors = c('#a50026', '#fdae61',  '#4575b4')
colors12 = c(brewer.pal(11, 'Spectral'), '#a6d96a')
bi= c("#fdae61", "#8073ac")
op = c('#f1a340', '#998ec3')

#----------------------------------------------------
# visualizations of client variables

# create a client-specific data set
clt = dt[subpop=='client']

# create a testing and outcomes variable
clt[grepl("HIV+", variable) | grepl("indeterm", variable) | grepl("management", variable), divide:='Testing outcome']
clt[is.na(divide), divide:='Testing output']

# time trend for each
clt_time = clt[ ,.(value=sum(value)), by=.(date, variable, divide)]

#---------------------------
# export a pdf

pdf(paste0(dir, '/pnls_clients_pt_couples_graphs.pdf'), width=14, height=9)

#---------------------------
# map clients tested against patients tested
comp = dt[variable=='Clients tested and received the results' | variable=="New patients who received a treatment consultation, were tested and received the results"]
comp = comp[ ,.(value=sum(value)), by=.(date, variable)]

ggplot(comp, aes(x=date, y=value, color=variable)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(x='Date', y='Count', color='', 
       title='Which of these should be used as total patients tested?')+
  theme(text=element_text(size=18))
#---------------------------

#---------------------------
# all variables on the same graph 
ggplot(clt_time, aes(x=date, y=value, color=variable)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(x='Date', y='Count', color='', title='HIV testing: clients')+
  theme(text=element_text(size=18))

# all variables on the same graph, divided by testing versus outcomes
ggplot(clt_time, aes(x=date, y=value, color=variable)) +
  geom_line() +
  geom_point() +
  facet_wrap(~divide, scales='free_y')+
  theme_bw() +
  labs(x='Date', y='Count', color='', title='HIV testing: clients')+
  theme(text=element_text(size=18))

# testing output variables on the same graph
ggplot(clt_time[divide=='Testing output'], aes(x=date, y=value, color=variable)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(x='Date', y='Count', color='', title='HIV testing: clients')+
  theme(text=element_text(size=18))

# testing output variables on the same graph
ggplot(clt_time[divide=='Testing outcome'], aes(x=date, y=value, color=variable)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(x='Date', y='Count', color='', title='HIV testing: clients')+
  theme(text=element_text(size=18))

# all variables on their own graphs, comparative
ggplot(clt_time, aes(x=date, y=value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~variable, scales='free_y')+
  theme_bw() +
  labs(x='Date', y='Count', title='HIV testing: clients')+
  theme(text=element_text(size=18))+ 
  scale_y_continuous(labels = scales::comma)

  
#--------------------
# comparative bar graphs for all clinet variables by year

clt[ ,year:=year(date)]
clt_yr = clt[ ,.(value=sum(value)), by=.(year, variable, divide)]

ggplot(clt_yr[year==2017], aes(x=reorder(variable, -value), y=value, fill=divide))+
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~divide, scales='free') +
  scale_fill_manual(values=tri_colors)+
  labs(x='', y='Count',
       title='All variables for clients: 2017', color='', fill='') +
  geom_text(aes(label=value), vjust=-1, position=position_dodge(0.9)) +
  theme_bw()+
  theme(text=element_text(size=18), axis.text.x = (element_text(angle=90)))+
  scale_y_continuous(labels = scales::comma)

ggplot(clt_yr[year==2018], aes(x=reorder(variable, -value), y=value, fill=divide))+
geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~divide, scales='free')+
  scale_fill_manual(values=tri_colors)+
  geom_text(aes(label=value), vjust=-1, position=position_dodge(0.9)) +
  labs(x='', y='Count',
       title='All variables for clients: 2018', color='', fill='') +
       theme_bw()+
       theme(text=element_text(size=18), axis.text.x = (element_text(angle=90)))+
  scale_y_continuous(labels = scales::comma)


#----------------------------------------------------
# visualizations of PATIENTS variables

# create a client-specific data set
pt = dt[subpop=='patient']

# time trend for each
pt_time = pt[ ,.(value=sum(value)), by=.(date, variable)]

#---------------------------

#---------------------------
# all variables on the same graph 
ggplot(pt_time, aes(x=date, y=value, color=variable)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(x='Date', y='Count', color='', title='HIV testing: patients')+
  theme(text=element_text(size=18))

# all variables on their own graphs, comparative
ggplot(pt_time, aes(x=date, y=value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~variable, scales='free_y')+
  theme_bw() +
  labs(x='Date', y='Count', title='HIV testing: patients')+
  theme(text=element_text(size=18))+ 
  scale_y_continuous(labels = scales::comma)

#--------------------
# comparative bar graphs for all clinet variables by year

pt[ ,year:=year(date)]
pt_yr = pt[ ,.(value=sum(value)), by=.(year, variable)]

ggplot(pt_yr[year==2017], aes(x=reorder(variable, -value), y=value, fill=variable))+
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values=rev(quad_colors)) +
  labs(x='', y='Count',
       title='All variables for patients: 2017', fill='') +
  geom_text(aes(label=value), vjust=-1, position=position_dodge(0.9)) +
  theme_bw()+
  theme(text=element_text(size=18), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_continuous(labels = scales::comma)

ggplot(pt_yr[year==2018], aes(x=reorder(variable, -value), y=value, fill=variable))+
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values=rev(quad_colors)) +
  labs(x='', y='Count',
       title='All variables for patients: 2018', fill='') +
  geom_text(aes(label=value), vjust=-1, position=position_dodge(0.9)) +
  theme_bw()+
  theme(text=element_text(size=18), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_continuous(labels = scales::comma)
#-----------------------------------------

#----------------------------------------------------
# visualizations of COUPLES variables

# create a client-specific data set
cpl = dt[subpop=='couple']

# create a testing and outcomes variable
cpl[, divide:='Testing output']
cpl[grepl("cordant", variable) | grepl("treatment", variable), divide:='Testing outcome']

# time trend for each
cpl_time = cpl[ ,.(value=sum(value)), by=.(date, variable, divide)]

#---------------------------

#---------------------------
# all variables on the same graph 
ggplot(cpl_time, aes(x=date, y=value, color=variable)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(x='Date', y='Count', color='', title='HIV testing: couples')+
  theme(text=element_text(size=18))

# all variables on the same graph 
ggplot(cpl_time, aes(x=date, y=value, color=variable)) +
  geom_line() +
  geom_point() +
  facet_wrap(~divide, scales='free')+
  theme_bw() +
  labs(x='Date', y='Count', color='', title='HIV testing: couples')+
  theme(text=element_text(size=18))

# all variables on their own graphs, comparative
ggplot(cpl_time, aes(x=date, y=value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~variable, scales='free_y')+
  theme_bw() +
  labs(x='Date', y='Count', title='HIV testing: couples')+
  theme(text=element_text(size=18))+ 
  scale_y_continuous(labels = scales::comma)

#--------------------
# comparative bar graphs for all clinet variables by year

cpl[ ,year:=year(date)]
cpl_yr = cpl[ ,.(value=sum(value)), by=.(year, variable)]

ggplot(cpl_yr[year==2017], aes(x=reorder(variable, -value), y=value, fill=variable))+
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values=rev(colors12)) +
  labs(x='', y='Count',
       title='All variables for couples: 2017', fill='') +
  geom_text(aes(label=value), vjust=-1, position=position_dodge(0.9)) +
  theme_bw()+
  theme(text=element_text(size=18), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_continuous(labels = scales::comma)

ggplot(cpl_yr[year==2018], aes(x=reorder(variable, -value), y=value, label=value, fill=variable))+
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values=rev(colors12)) +
  geom_text(aes(label=value), vjust=-1, position=position_dodge(0.9)) +
  labs(x='', y='Count',
       title='All variables for couples: 2018', fill='') +
  theme_bw()+
  theme(text=element_text(size=18), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_continuous(labels = scales::comma)

#--------------------

#--------------------

dev.off()

#--------------------










