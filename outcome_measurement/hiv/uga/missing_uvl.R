# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
#4/27/2018
# Descriptive statistics and maps for the Uganda Viral Load Dashboard
# ----------------------------------------------
# Set up R

rm(list=ls())
library(data.table)
library(ggplot2)
library(tibble)
library(dplyr)
library(RColorBrewer)


# set input/output directory
# ----------------------------------------------
dir <- 'J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard'

# upload the data with month, year, sex
uganda_vl <- readRDS(paste0(dir, "/sex_data.rds"))

# drop out the current month
uganda_vl <- uganda_vl[!(month==3 & year==2018)]


# ----------------------------------------------
# check for missing data by district and facility

# There are no facilities with 0 patients received
uganda_vl[patients_received==0, facility_id]

# There are no facilities with 0 samples received
uganda_vl[samples_received==0, facility_id]

# --------------------


#--------------------------------
# FACILITIES
# create a data table for patients received and samples received

# store identifiers
idVars <- c("facility_id", "facility_name", "month", "year")


# create a long data set with totals by district
uvl_fac <- uganda_vl[,
                     .(patients_received=sum(patients_received), samples_received = sum(samples_received)),
                     by=idVars]

# reshape indicators long for district data
uvl_fac <- melt(uvl_fac, id.vars=idVars)


# label the variables for graph titles and put the graphs in an intuitive order
uvl_fac$variable <- factor(uvl_fac$variable, 
                           levels=c("patients_received", "samples_received"), 
                           labels=c("Patients Received", "Samples Received"))

# single test graph
f=27
name <- unique(uvl_fac[facility_id==f]$facility_name)

ggplot(uvl_fac[facility_id==f], aes(y=value, x=factor(month), color=factor(year), group=year)) + 
  geom_point() +
  geom_line(alpha=0.5) + 
  facet_wrap(~variable, scales='free_y') +
  xlab("Date") + ylab("Count") + theme_bw() + labs(title=name, color="Year")



# ---------------------------------
# loop over all facilities printing patients and samples received

list_of_plots = NULL
i=1

t <- c()

for(f in unique(uvl_fac$facility_id)) {
  # look up district name
  name <- unique(uvl_fac[facility_id==f]$facility_name)
  
  # make your graph
  
  list_of_plots[[i]] <-  ggplot(uvl_fac[facility_id==f], aes(x=factor(month), y=value, color=factor(year), group=year)) + 
            geom_point(size=0.5) + 
            geom_line(alpha=0.5) + 
            facet_wrap(~variable, scales='free_y') +
            labs(title=name, x="Month", y="Count", color="Year") + theme_bw()
  
  
  i=i+1
  
  t <- c(t, warnings())
  
}

pdf('J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape_agg/outputs/uvl_facilities.pdf', height=6, width=9)
  
  for(i in seq(length(list_of_plots))) { 
    print(list_of_plots[[i]])
  } 

dev.off()

# print a list of the facility ids that produced error messages
error_msg <- cbind(error = t, facility_id = unique(uvl_fac$facility_id)) 
error_msg <- data.table(error_msg)
error_msg <- error_msg[!is.na(error_msg)]



#--------------------------------
# FACILITIES BY SEX
# Check for missing data within facilities, facet wrapping by sex

# store identifiers
idVars4 <- c("facility_id", "facility_name", "sex", "month", "year")


# create a long data set with totals by district
fac_sex <- uganda_vl[,
                     .(patients_received=sum(patients_received), samples_received = sum(samples_received)),
                     by=idVars4]

# reshape indicators long for district data
fac_sex <- melt(fac_sex, id.vars=idVars4)

fac_sex[, date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]

# label the variables for graph titles and put the graphs in an intuitive order
fac_sex$variable <- factor(fac_sex$variable, 
                           levels=c("patients_received", "samples_received"), 
                           labels=c("Patients Received", "Samples Received"))

# single test graph
f=27
name2 <- unique(fac_sex[facility_id==f]$facility_name)

ggplot(fac_sex[facility_id==f], aes(y=value, x=date, color=sex, group=sex)) + 
  geom_point() +
  geom_line(alpha=0.5) + 
  facet_wrap(~variable, scales='free_y') +
  labs(title=name2, x="Date", y="Count", color="Sex") +
  theme_bw()



# ---------------------------------
# loop over all facilities printing patients and samples received

list_of_plots = NULL
i=1

for(f in unique(fac_sex$facility_id)) {
  
  # set the title to the facility name
  name2 <- unique(fac_sex[facility_id==f]$facility_name)
  
  # create a graph of the monthly data stratified by sex
  list_of_plots[[i]] <- ggplot(fac_sex[facility_id==f], aes(y=value, x=date, color=sex)) + 
    geom_point() +
    geom_line(alpha=0.5) + 
    facet_wrap(~variable, scales='free_y') +
    labs(title=name2, x="Date", y="Count", color="Sex") +
    theme_bw()
  
  i=i+1
  
}

pdf(paste0(dir,'/webscrape_agg/outputs/uvl_facilities_sex.pdf'), height=6, width=9)

for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 

dev.off()


#--------------------------------
# FACILITIES 2 
# loops over all facilities printing patients_received only

# create a data table with total patients_received by district
uvl_fac2 <- uganda_vl[,
                     .(patients_received=sum(patients_received)),
                     by=.(facility_id, facility_name, month, year)]

# add date variable
uvl_fac2[, date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]


# -----------
# loop over all facilities and export as a PDF
# prints samples received, dbs samples, patients, results, valid results, and suppressed by sex, date for each district

list_of_plots = NULL
i=1

for(f in unique(uvl_fac$facility_id)) {
 
   # look up district name
  name <-  unique(uvl_fac2[facility_id==f]$facility_name)
  
  # graph for each facility
  list_of_plots[[i]] <- ggplot(uvl_fac2[facility_id==f], aes(x=date, y=patients_received)) + 
    geom_point(size=0.5) + 
    geom_line(alpha=0.5) + 
   labs(title=name, x="Date", y="Patients Received") + theme_bw()
  
  i=i+1
  
}

pdf('J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape_agg/outputs/uvl_facilities2.pdf', height=6, width=9)
for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
}

dev.off()

#-----------------------------------


#----------------------
# DISTRICTS

# store identifiers
idVars <- c("district_id", "dist_name", "sex", "month", "year")

uganda_vl1 <- uganda_vl[,
                        .( dist_name=dist_name, district_id=district_id, sex=sex, month=month, year=year,
                           patients_received= sum(patients_received), samples_received = sum(samples_received), 
                           dbs_samples=sum(dbs_samples), total_results=sum(total_results), 
                           valid_results = sum(valid_results), suppressed = sum(suppressed))]


# create a long data set with totals by district
uvl_dist <- uganda_vl1[,
                       .( patients_received= sum(patients_received), samples_received = sum(samples_received), 
                          dbs_samples=sum(dbs_samples), total_results=sum(total_results), 
                          valid_results = sum(valid_results), suppressed = sum(suppressed)),
                       by=idVars]

# reshape indicators long for district data
uvl_dist <- melt(uvl_dist, id.vars=idVars)

# add date variable
uvl_dist[, date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]

# label the variables for graph titles and put the graphs in an intuitive order
uvl_dist$variable <- factor(uvl_dist$variable, 
                            levels=c("patients_received", "samples_received", "dbs_samples", "total_results", "valid_results", "suppressed"), 
                            labels=c("Patients Received", "Samples Received","DBS Samples", "Total Results", "Valid Results", "Suppressed"))

# single test graphs
f=15
name <- unique(uvl_dist[district_id==f]$dist_name)
ggplot(uvl_dist[district_id==f], aes(y=value, x=date, color=sex)) +
  geom_point(size=0.5) +
  geom_line(alpha=0.5) +
  facet_wrap(~variable) +
  xlab("Date") + ylab("Count") + theme_bw() + labs(title=name)


# -----------
# loop over all districts and export as a PDF
# prints samples received, dbs samples, patients, results, valid results, and suppressed by sex, date for each district

pdf('J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape_agg/outputs/uvl_districts.pdf', height=6, width=9)
for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
}

dev.off()

list_of_plots = NULL
i=1
for(f in unique(uvl_dist$district_id)) {
  # look up district name
  name <-  unique(uvl_dist[district_id==f]$dist_name)
  
  # make your graph
  
  list_of_plots[[i]] <- ggplot(uvl_dist[district_id==f], aes(y=value, x=date, color=sex)) + 
    geom_point(size=0.5) + 
    geom_line(alpha=0.5) + 
    facet_wrap(~variable, scales='free_y') + labs(title=name, x="Date", y="Count") + theme_bw()
  
  i=i+1
  
}



#--------------------------------
# DBS VERSUS PLASMA
# create a data table for patients received and samples received

# store identifiers
idVars <- c("facility_id", "facility_name", "month", "year")

# create a long data set with totals by district
uvl_fac <- uganda_vl[,
                     .(samples_received = sum(samples_received),
                     dbs_samples=sum(dbs_samples), plasma_samples=sum(plasma_samples)),
                     by=idVars]

# reshape indicators long for district data
uvl_fac <- melt(uvl_fac, id.vars=idVars)


# label the variables for graph titles and put the graphs in an intuitive order
uvl_fac$variable <- factor(uvl_fac$variable, 
                           levels=c("samples_received", "dbs_samples", "plasma_samples"), 
                           labels=c("Samples Received", "DBS Samples", "Plasma"))

# add a date variable
uvl_fac[, date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]

# single test graph
f=27
name <- unique(uvl_fac[facility_id==f]$facility_name)

ggplot(uvl_fac[facility_id==f], aes(y=value, x=date)) + 
  geom_point() +
  geom_line(alpha=0.5) + 
  facet_wrap(~variable, scales='free_y') +
  xlab("Date") + ylab("Count") + theme_bw() + labs(title=name)



# ---------------------------------
# loop over all facilities printing patients and samples received

list_of_plots = NULL
i=1

t <- c()

for(f in unique(uvl_fac$facility_id)) {
  # look up district name
  name <- unique(uvl_fac[facility_id==f]$facility_name)
  
  # make your graph
  
  list_of_plots[[i]] <-  ggplot(uvl_fac[facility_id==f], aes(x=date, y=value)) + 
    geom_point() + 
    geom_line(alpha=0.5) + 
    facet_wrap(~variable, scales='free_y') +
    labs(title=name, x="Date", y="Count") + theme_bw()
  
  
  i=i+1
  
  t <- c(t, warnings())
  
}

pdf('J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape_agg/outputs/dbs_plasma.pdf', height=6, width=9)

for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 

dev.off()

