# ----------------------------------------------
# Irena Chen
#
# 7/12/2018
# This script joins the case notification data and the bed nets data and produces a scatterplot of the two
# The current working directory should be the same as this code

##note: probably run this on the cluster since the shape files and case notifications dataset are pretty big
# ----------------------------------------------
# Set up R
rm(list=ls())
library(data.table)
library(reshape2)
library(stringr)
library(readxl)
library(rlang)
library(zoo)
library(lubridate)

# ---------------------------------------------
## set up directories
# ---------------------------------------------
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j') # have to declare the J Drive differently on the cluster 
import_dir <-  paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/')
export_dir <- paste0(j, import_dir, 'visualizations/')

# ---------------------------------------------
## load data 
# ---------------------------------------------
case_notifications <- data.table(read.csv(paste0(import_dir, "MALARIA/case_notifications/EPIVIGILA - Semana 52 2017 csv/Epivigila a Dic 2017.csv")))
muni_facility_data <-  data.table(read.csv(paste0(import_dir, "MALARIA/case_notifications/EPIVIGILA - Semana 52 2017 csv/DataConfig-Establecimientos.csv")))
muni_codes_and_names <-  data.table(read.csv(paste0(import_dir, "muni_codes_and_names.csv")))
bed_net_data <- data.table(read.csv(paste0(import_dir, "prepped_data/bednet_prepped_data.csv")))
##subset by the disease t


##subset by the disease type that we want 
# the DataConfig - PatoSNVS csv has the specific malaria pathologies that these codes represent
malData <- case_notifications[Id_patologia%in%c(27, 28, 29)] 
bed_net_data <- bed_net_data[year%in%c(2015, 2016, 2017)]
# ----------------------------------------------
## shape the case notification data so that we can plot it
# ----------------------------------------------
setnames(malData, c("year", "week", "facility_id", "disease_id", "age_group", "cases", "male_id", "female_id"))
## grab the facility and municipality ids from the facility dataset
muni_facility_data <- muni_facility_data[,c("Padre", "ID_ESTABLECIMIENTO"), with=FALSE]
setnames(muni_facility_data, c("loc_id", "facility_id"))

## join on facility id to get the municipality code: 

mal_muni_data <- merge(muni_facility_data, malData, by="facility_id")
#subset by years:

mal_muni_data <- mal_muni_data[year%in%c(2015, 2016, 2017)]

# right now, we don't care about the other variables (just location )
byVars = names(mal_muni_data)[names(mal_muni_data)%in%c('loc_id')]
malaria_dataset= mal_muni_data[, list(cases=sum(na.omit(cases))), by=byVars]

byVars = names(bed_net_data)[names(bed_net_data)%in%c('adm2')]
bed_nets= bed_net_data[, list(bed_nets=sum(na.omit(bed_nets))), by=byVars]

# loc_id has leading zeroes, which the shape files don't have
## this gets rid of it: 
malaria_dataset$adm2 <- as.numeric(lapply(malaria_dataset$loc_id, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE)))


## merge the bed net data and the case notification data
nets_and_cases <- merge(bed_nets, malaria_dataset, by="adm2", all.y=TRUE) ##there are more municipalities in the case dataset than bed nets

## merge the outcome measurement data with the list of municipality names and departments
totalData <- merge(nets_and_cases, muni_codes_and_names, by="adm2", all.x=TRUE)

deptData= totalData[, list(bed_nets=sum(na.omit(bed_nets)), cases=sum(na.omit(cases))), by=c("department")]

# ----------------------------------------------
## # Basic scatter plot
# ----------------------------------------------


plot1 <- (ggplot(deptData, aes(x=cases, y=bed_nets/1000, color=department))  +
            geom_point(size=3) +
           #uncomment if you want to log transform one of the variables: scale_x_continuous(trans='log2') +
            geom_text_repel(aes(label=department)) + labs(x="Cases", y="Bed Nets Distributed (thousands)",
                                                          title = "Scatterplot of Bed Nets vs. Cases by Department \r
                                                          (Aggregated over 2015-2017)"))



# ----------------------------------------------
## export the list of maps as a PDF 
# ----------------------------------------------

pdf(paste0(export_dir, "bed_nets_vs_cases_scatterplot.pdf"), height=6, width=9)
invisible(lapply(gtm_plots, print))
dev.off()










