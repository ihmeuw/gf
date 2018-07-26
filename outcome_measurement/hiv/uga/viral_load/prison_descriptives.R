# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 6/11/2018
# Descriptive statistics and maps for incarcerated persons
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
# Print a list of facilities and patients
prisons[, (patients_received=sum(patients_received)), by=.(facility_name, sex)]

# ----------------------------------------------
# create tables for graphs 

pr <- uganda_vl[ ,.(patients_received=sum(patients_received),
                    ratio=100*(sum(suppressed)/sum(valid_results))),
                    by=.(date, prison)]

prlong <- melt(pr, id.vars=c('date', 'prison'))

prlong$prison <- factor(prlong$prison, levels=c("TRUE", "FALSE"), labels=c("Incarcerated", 
                                                    "Not Incarcerated"))


prlong$variable <- factor(prlong$variable, levels=c("patients_received", "ratio"), 
                          labels=c("Patients submitting for viral load testing", 
                                    "Percent virally suppressed"))

ggplot(prlong, aes(x=date, y=value, color=prison, group=prison)) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable, scales='free_y') +
  theme_bw()


#------------------------------------------

pr1 <- uganda_vl[ ,.(valid_results=sum(valid_results),

                 by=.(date, prison)]

pr1long <- melt(pr1, id.vars=c('date', 'prison'))

pr1long$prison <- factor(pr1long$prison, levels=c("TRUE", "FALSE"), labels=c("Incarcerated", 
                                                                           "Not Incarcerated"))

pr1long$variable <- factor(pr1long$variable, levels=c("valid_results", "suppressed"), 
                          labels=c("Valid viral load test results", 
                                   "Virally suppressed"))

ggplot(pr1long[prison=='Incarcerated'], aes(x=date, y=value, color=variable, group=variable)) +
  geom_point() +
  geom_line() +
  theme_bw()







#-----------------------------------------------


# totals among incarcerated persons - patients, results, suppressed, ratio
prison_total <- prisons[ ,  .(patients_received=sum(patients_received),
                              valid_results=sum(valid_results),
                              suppressed=sum(suppressed), 
                              ratio=100*(sum(suppressed)/sum(valid_results))),
                             , by=.(sex, date)]

prison_total_long <- melt(prison_total, id.vars=c('sex', 'date'))

prison_total_long$variable <- factor(prison_total_long$variable, levels=c("patients_received",
                                "valid_results", "suppressed", "ratio"), 
                               labels=c("Patients submitting samples","Valid test results", "Number of suppressed persons", 
                                     "Percent virally suppressed"))

# ---------------------
# total by facility - patients, results, suppressed, ratio
prison_graph <- prisons[ ,  .(valid_results=sum(valid_results),
                              suppressed=sum(suppressed), 
                              patients_received=sum(patients_received),
                              ratio=100*(sum(suppressed)/sum(valid_results))),
                              by=.(facility_id, facility_name, sex, date)]

prison_graph_long <- melt(prison_graph, id.vars=c('facility_id', 'facility_name', 'sex', 'date'))

prison_graph_long$variable <- factor(prison_graph_long$variable, levels=c("patients_received",
                                      "valid_results", "suppressed", "ratio"), 
                                     labels=c("Patients submitting samples","Valid test results", "Number of suppressed persons", 
                                              "Percent virally suppressed"))
# ---------------------
# bar graphs of suppressed/not suppressed
prison_bar <- prisons[ ,.(suppressed=sum(suppressed),
                          valid_results=sum(valid_results)), by=date]

prison_bar_sex <- prisons[ ,.(suppressed=sum(suppressed),
                          valid_results=sum(valid_results)), by=.(sex, date)]

# ---------------------
# compare incarcerated persons to non-incarcerated
prison_bar_all <- uganda_vl[year >= 2015,.(suppressed=sum(suppressed),
                            valid_results=sum(valid_results)), by=.(date, prison)]

prison_bar_all$prison <- factor(prison_bar_all$prison, levels=c('TRUE', 'FALSE'),
                                labels=c("Incarcerated", "Not incarcerated"))

prison_bar_all[ , not_suppressed:=(valid_results - suppressed)]

#----------------
pr <- uganda_vl[ ,.(ratio=100*(sum(suppressed)/sum(valid_results))), 
                     by=.(date, prison, sex)]

pr$prison <- factor(pr$prison, levels=c('TRUE', 'FALSE'),
                                labels=c("Incarcerated", "Not incarcerated"))


ggplot(pr, aes(x=date, y=ratio, color=sex, group=sex)) +
  geom_point()+
  geom_line() +
  facet_wrap(~prison) +
  theme_bw()

# shape long facet wrap by incarcerated status
pr1 <- uganda_vl[ ,.(ratio=100*(sum(suppressed)/sum(valid_results)), 
                       valid_results=sum(valid_results)), 
                    by=.(date, prison)]

pr1$prison <- factor(pr1$prison, levels=c('TRUE', 'FALSE'),
                    labels=c("Incarcerated", "Not incarcerated"))

ggplot(pr, aes(x=date, y=ratio, color=prison, group=prison)) +
  geom_point()+
  geom_line() +
  facet_wrap(~prison) +
  theme_bw()



# ---------------------
# viral suppression ratio graphs by incarcerated status
prison_ratio <- prison_bar_all
prison_ratio[ , ratio:=100*(suppressed/valid_results)]

# ----------------------------------------------
# plots of patients, results, suppressed, ratio by facility 

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
       x='Date', y='Percent virally suppressed', caption="Source: Uganda Viral Load Dashboard", color='Sex') +
  theme_bw() +
  theme(axis.title.y=element_blank()) 
  
  i=i+1
}


# ----------------------------------------------
# plots of ratio by facility with point size as patients_received

list_of_plots2 = NULL
i=1

for (f in unique(prison_graph_long$facility_id)) {
  
  name <- unique(prison_graph_long[facility_id==f]$facility_name)
  
  list_of_plots2[[i]] <- ggplot(prison_graph[facility_id==f], 
                               aes(x=date, y=ratio, color=factor(sex), group=sex)) +
                              geom_point(aes(size=patients_received)) +
                              geom_line() +
                              labs(title=name,
                              x='Date', y='Percent virally suppressed', caption="Source: Uganda Viral Load Dashboard", color='Sex') +
                              theme_bw() +
                              theme(axis.title.y=element_blank()) 
  
  i=i+1
}

# ------------------------

bar_colors <- c('Not Suppressed'='#de2d26', 'Suppressed'='#fc9272')

pdf(paste0(root, 'Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape_agg/outputs/prisons.pdf'), height=6, width=9)

# ------------------------
# Summary graphs 

# Patients, test results, suppressed, ratio, by sex in prisons
ggplot(prison_total_long,
       aes(x=date, y=value, color=factor(sex), group=sex)) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable, scales='free_y') +
  labs(title='Viral load testing in prison-based health facilities by sex',
       x='Date', caption="Source: Uganda Viral Load Dashboard", col='Sex') +
  theme_bw() +
  theme(axis.title.y=element_blank()) 

# bar graph suppressed/not suppressed in prisons
ggplot(prison_bar, aes(x=date, y=valid_results, fill='Not Suppressed')) + 
  geom_bar(stat="identity") + 
  geom_bar(aes(y=suppressed, fill='Suppressed'), stat='identity') + 
  scale_fill_manual(name='', values=bar_colors) + theme_minimal() +
  labs(title = "Viral suppression among incarcerated PLHIV", x='Date', y="Total valid test results", 
       caption="Source: Uganda VL Dashboard")

# bar graph suppressed/not suppressed in prisons by sex
ggplot(prison_bar_sex, aes(x=date, y=valid_results, fill='Not Suppressed')) + 
  geom_bar(stat="identity") + 
  geom_bar(aes(y=suppressed, fill='Suppressed'), stat='identity') + 
  scale_fill_manual(name='', values=bar_colors) + theme_minimal() +
  facet_wrap(~sex) +
  labs(title = "Viral suppression among incarcerated PLHIV by sex", x='Date', y="Total valid test results", 
       caption="Source: Uganda VL Dashboard")

# bar graph suppressed/not suppressed by incarcerated or not
ggplot(prison_bar_all, aes(x=date, y=valid_results, fill='Not Suppressed')) + 
  geom_bar(stat="identity") + 
  geom_bar(aes(y=suppressed, fill='Suppressed'), stat='identity') + 
  scale_fill_manual(name='', values=bar_colors) + theme_minimal() +
  facet_wrap(~prison, scales='free_y') +
  labs(title = "Viral suppression among PLHIV by incarcerated status", x='Date', y="Total valid test results", 
       caption="Source: Uganda Viral Load Dashboard")

# viral suppression ratio by incarcerated status
ggplot(prison_ratio, aes(x=date, y=ratio)) +
  geom_point() +
  geom_line() +
  facet_wrap(~prison) +
  theme_bw()

# viral suppression ratio among incarcerated persons with sample size represented
ggplot(prison_ratio[prison=='Incarcerated'], aes(x=date, y=ratio)) +
  geom_point(aes(size=valid_results)) +
  geom_line() +
  facet_wrap(~prison) +
  theme_bw()

for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 

for(i in seq(length(list_of_plots2))) { 
  print(list_of_plots2[[i]])
} 

dev.off()
