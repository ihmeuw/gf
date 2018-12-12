# Audrey Batzel 
# 12-10-18
#
# Figures for results chain for DRC report 
setwd('C:/local/gf/')
# ----------------------------------------------


# --------------------
# Set up R / install packages
# --------------------
rm(list=ls())
library(data.table)
library(stringr)
library(reshape2)
library(ggplot2)
library(lubridate)
library(readxl)
library(stats)
library(Rcpp)
library(Amelia)
# --------------------  

# ---------------------------------------------------
# Files and directories
# ---------------------------------------------------
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir <-paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/')
dir_pnlp <- paste0(dir, 'prepped_data/PNLP/')
dir_dhis = paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/')

# input files
pnlp_natl <- "post_imputation/imputedData_run2_agg_country.rds"
dhis_base <- "base_services_drc_01_2017_09_2018_prepped.rds"
dhis_sigl <- "sigl_drc_01_2015_07_2018_prepped.rds"
# output files

# functions
source('./core/standardizeHZNames.R')
source('./core/standardizeDPSNames.R')
# ---------------------------------------------------

# ---------------------------------------------------
# Load prepped data
# ---------------------------------------------------
pnlp <- readRDS(paste0(dir_pnlp, pnlp_natl)) # pnlp data at the national level by month and indicator
snis <- readRDS(paste0(dir_dhis, dhis_base)) # snis data still needs to be aggregated

sigl <- readRDS(paste0(dir_dhis, dhis_sigl))
# ---------------------------------------------------

# ---------------------------------------------------
# Standardize data sets to combine them
# ---------------------------------------------------
# subset to just malaria
snis$type <- trimws(snis$type)  # for some reason there is both "malaria" and "malaria " in the type unique values
snis <- snis[ type == "malaria", ]

# aggregate SNIS data to be at the national level
snis_natl <- snis[, .(natlValue= sum(value, na.rm=TRUE)), by=c("date", "year", "element", "element_eng", "category")]  
    # unique identifiers at the natl level are date, element/element_eng, and category

pnlp$year <- year(pnlp$date)
pnlp <- pnlp[year >= 2015]
# ---------------------------------------------------

# ---------------------------------------------------
# subset data
# ---------------------------------------------------
# activities
pnlp_act <- pnlp[ indicator %in% c("ASAQreceived", "ITN", "ArtLum", "RDT"), ]
pnlp_act <- pnlp_act[ subpopulation %in% c("received", "14yrsAndOlder", "1to5yrs", "2to11mos", "6to13yrs"), ]



# outputs
pnlp_out <-  pnlp[ indicator %in% c("ASAQused", "ITN", "ArtLum", "RDT", "SP") & subpopulation != "received", ]
pnlp_out <-  pnlp_out[ subpopulation != "positive", ]




# ---------------------------------------------------

# ---------------------------------------------------
# graphs
# ---------------------------------------------------

g <- ggplot(pnlp_act, aes(x=date, y=mean, color=subpopulation)) + 
  theme_bw() + geom_point() + geom_line() +
  ggtitle(paste0("")) +
  ylab("Date") + xlab("Count") + labs(caption = "") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_blank(), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14)) +
  facet_wrap( ~indicator, scales = "free_y" )
print(g)

g <- ggplot(pnlp_out, aes(x=date, y=mean, color=subpopulation)) + 
  theme_bw() + geom_point() + geom_line() +
  ggtitle(paste0("")) +
  ylab("Date") + xlab("Count") + labs(caption = "") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_blank(), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14)) +
  facet_wrap( ~indicator, scales = "free_y" )
print(g)


# ---------------------------------------------------

