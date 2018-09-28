# Audrey Batzel
# 9/27/18
#
# compare world pop populations to populations in pnlt data
# ----------------------------------------------

# ----------------------------------------------
## Set up R / install packages 
# ----------------------------------------------
rm(list=ls())
library(raster)
library(rgeos)
library(data.table)
library(ggplot2)
library(maptools)
# ----------------------------------------------


# ----------------------------------------------
## set up the directories to read/export files: 
# ----------------------------------------------
# Directories
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j/')

export_dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLT_data/')
data_dir <- paste0(j, "/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLT/")
worldpop_dir <- paste0(j, "/Project/Evaluation/GF/outcome_measurement/cod/worldpop_data/")
core_code <- ('C:/local/gf/core/standardizeDPSNames.R')
source(core_code)

# input files
pnlt_data_dep <- "PNLT_prepped_data.csv"
pnlt_data_case_outcomes17 <- "PNLT_case_outcomes_2017.csv"
pnlt_data_case_outcomes18 <- "PNLT_case_outcomes_2018.csv"
pnlt_outcomes_18_tb_hiv <- "PNLT_case_outcomes_2018_TBHIV.csv"
pnlt_case_screening_17 <- "PNLT_case_screening_2017.csv"
pop_estimates_2015 <- "2015_pop_estimates_by_dps.csv"

# output files
# ----------------------------------------------


# ----------------------------------------------
## load data / prep continued (move to prep code)
# ----------------------------------------------
dt <- read.csv(paste0(data_dir, pnlt_case_screening_17))
dt <- dt[, .(dps, pop_tot, quarter, file_year)]

dt$quarter <- gsub("T", "", dt$quarter)
dt[, date:=paste0(file_year, "-0", quarter, "-01")]

dt$date <- as.Date(dt$date)
dt$pop_tot <- as.numeric(dt$pop_tot)
dt <- dt[date=="2017-04-01"]
dt <- dt[, .(dps, pop_tot)]
dt[dps %in% c("kongo-central-est", "kongo-central-ouest"), pop_tot := sum(pop_tot)]
dt[dps=="kongo-central-est", dps:= "kongo-central"]
dt <- dt[dps != "kongo-central-ouest"]

dt_pop <- read.csv(paste0(worldpop_dir, pop_estimates_2015))
dt_pop <- as.data.table(dt_pop)
dt_pop$province <- as.character(dt_pop$province)
dt_pop$dps <- standardizeDPSNames(dt_pop$province)
dt_pop[is.na(dps), dps:= "equateur"]
dt_pop <- dt_pop[, .(dps, pop)]

dt_pops <- merge(dt, dt_pop, by="dps")
setnames(dt_pops, "pop_tot", "pop_pnlt")
setnames(dt_pops, "pop", "pop_worldpop")


g <- ggplot(dt_pops, aes(pop_worldpop, pop_pnlt, label=dps) ) + theme_bw() + 
  geom_point() + coord_fixed(ratio=1) + geom_abline() + 
  geom_text(aes(label=dps), hjust=0, vjust=0) +
  xlim(0, 10000000) + ylim(0, 10000000) 
  
print(g)






