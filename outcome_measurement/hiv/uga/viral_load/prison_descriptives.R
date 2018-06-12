# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 5/31/2018
# Descriptive statistics and maps for the Uganda Viral Load Dashboard
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


# --------------------
# detect if on windows or on the cluster 

if (Sys.info()[1] == 'Windows') {
  username <- "ccarelli"
  root <- "J:/"
} else {
  username <- Sys.getenv("USER")
  root <- "/home/j/"
}

# ----------------------------------------------
# Files and directories

# set input/output directory
dir <- paste0(root, 'Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard')

# upload the data with month, year, sex
uganda_vl <- readRDS(paste0(dir, "/sex_data.rds"))

prisons <- uganda_vl[prison==1]

prisons <- prisons[!(facility_id==3293 & sex=='Female')]

# ----------------------------------------------

prisons[, unique(facility_name)]

prison_total <- prisons[ ,  .(patients_received=sum(patients_received),
                              valid_results=sum(valid_results),
                              suppressed=sum(suppressed), 
                              ratio=100*(sum(suppressed)/sum(valid_results))),
                         , by=.(sex, date)]

prison_total_long <- melt(prison_total, id.vars=c('sex', 'date'))

prison_total_long$variable <- factor(prison_total_long$variable, levels=c("patients_received",
                                "valid_results", "suppressed", "ratio"), 
                               labels=c("Patients submitting samples","Valid test results", "Suppressed", 
                                     "Percent virally suppressed"))

prison_graph <- prisons[ ,  .(valid_results=sum(valid_results),
                              suppressed=sum(suppressed), 
                              patients_received=sum(patients_received),
                              ratio=100*(sum(suppressed)/sum(valid_results))),
                              by=.(facility_id, facility_name, sex, date)]

prison_graph_long <- melt(prison_graph, id.vars=c('facility_id', 'facility_name', 'sex', 'date'))

prison_graph_long$variable <- factor(prison_graph_long$variable, levels=c("patients_received",
                                      "valid_results", "suppressed", "ratio"), 
                                     labels=c("Patients submitting samples","Valid test results", "Suppressed", 
                                              "Percent virally suppressed"))

prison_bar <- prisons[ ,.(suppressed=sum(suppressed),
                          valid_results=sum(valid_results)), by=date]

prison_bar_sex <- prisons[ ,.(suppressed=sum(suppressed),
                          valid_results=sum(valid_results)), by=.(sex, date)]


prison_bar_all <- uganda_vl[year >= 2015,.(suppressed=sum(suppressed),
                            valid_results=sum(valid_results)), by=.(date, prison)]

prison_bar_all$prison <- factor(prison_bar_all$prison, levels=c('TRUE', 'FALSE'),
                                labels=c("Incarcerated", "Not incarcerated"))

prison_bar_all[ , not_suppressed:=(valid_results - suppressed)]

# ----------------------------------------------

list_of_plots = NULL
i=1

for (f in unique(prison_graph_long$facility_id)) {

  name <- unique(prison_graph_long[facility_id==f]$facility_name)
  
  list_of_plots[[i]] <- ggplot(prison_graph_long[facility_id==f], 
  aes(x=date, y=value, color=factor(sex), group=sex)) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable, scales='free_y') +
  labs(title=name,
       x='Date', caption="Source: Uganda Viral Load Dashboard", color='Sex') +
  theme_bw() +
  theme(axis.title.y=element_blank()) 
  
  i=i+1
}

bar_colors <- c('Not Suppressed'='#de2d26', 'Suppressed'='#fc9272')

pdf('J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape_agg/outputs/prisons.pdf', height=6, width=9)

ggplot(prison_total_long,
       aes(x=date, y=value, color=factor(sex), group=sex)) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable, scales='free_y') +
  labs(title='Viral load testing in prison-based health facilities',
       x='Date', caption="Source: Uganda Viral Load Dashboard", col='Sex') +
  theme_bw() +
  theme(axis.title.y=element_blank()) 

ggplot(prison_bar, aes(x=date, y=valid_results, fill='Not Suppressed')) + 
  geom_bar(stat="identity") + 
  geom_bar(aes(y=suppressed, fill='Suppressed'), stat='identity') + 
  scale_fill_manual(name='', values=bar_colors) + theme_minimal() +
  labs(title = "Viral suppression among incarcerated PLHIV", x='Date', y="Total valid test results", 
       caption="Source: Uganda VL Dashboard")

ggplot(prison_bar_sex, aes(x=date, y=valid_results, fill='Not Suppressed')) + 
  geom_bar(stat="identity") + 
  geom_bar(aes(y=suppressed, fill='Suppressed'), stat='identity') + 
  scale_fill_manual(name='', values=bar_colors) + theme_minimal() +
  facet_wrap(~sex) +
  labs(title = "Viral suppression among incarcerated PLHIV by sex", x='Date', y="Total valid test results", 
       caption="Source: Uganda VL Dashboard")


ggplot(prison_bar_all, aes(x=date, y=valid_results, fill='Not Suppressed')) + 
  geom_bar(stat="identity") + 
  geom_bar(aes(y=suppressed, fill='Suppressed'), stat='identity') + 
  scale_fill_manual(name='', values=bar_colors) + theme_minimal() +
  facet_wrap(~prison, scales='free_y') +
  labs(title = "Viral suppression among PLHIV by incarcerated status", x='Date', y="Total valid test results", 
       caption="Source: Uganda Viral Load Dashboard")



prison_bar_fill <- prison_bar_all[ , .(suppressed=sum(suppressed), not_suppressed=sum(not_suppressed)), by=.(date, prison)]
prison_bar_long <- melt(prison_bar_fill, id.vars=c('prison', 'date'))
  
  ggplot(prison_bar_long, aes(x = date, fill = value)) +
    geom_bar(position = "fill") +
    facet_wrap(~prison) +
    scale_fill_manual(values=bar_colors)



for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 

dev.off()
