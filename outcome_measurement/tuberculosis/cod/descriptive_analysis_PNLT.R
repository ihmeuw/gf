# ----------------------------------------------
# Audrey Batzel
# 8/7/2018
# Map visualizations of PNLT data
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

# input files
pnlt_data_dep <- "PNLT_prepped_data.csv"
pnlt_data_case_outcomes <- "PNLT_case_outcomes_2017.csv"


# output files
# ----------------------------------------------


# ----------------------------------------------
## load data
# ----------------------------------------------
dt <- read.csv(paste0(data_dir, pnlt_data_case_outcomes))
dt <- as.data.table(dt)

dt[, X:=NULL]
# ----------------------------------------------


# ----------------------------------------------
## variable set up
# ----------------------------------------------
id_vars <- c('dps', 'sheet', 'tb_type', 'data_year', 'file_year', 'quarter')
tb_type <- unique(dt$tb_type)
dps_names <- unique(dt$dps)
# ----------------------------------------------


# ----------------------------------------------
## MOVE TO PREP CODE:
# ----------------------------------------------
# set quarters:
dt$quarter <- gsub("T", "", dt$quarter)
dt$quarter <- as.numeric(dt$quarter)

# rename cols
setnames(dt, "TB_type", "tb_type")

# change class of vars
dt$tb_type <- as.character(dt$tb_type)
dt$dps <- as.character(dt$dps)

# create percent case_eval column:
dt[, percent_cases_eval := cas_eval/tot_cas_reg]
dt[, percent_healed := healed/tot_cas_reg]

# melt data
melted_dt <- melt.data.table(dt, id.vars=id_vars, variable.name = "outcome")
melted_dt$outcome <- as.character(melted_dt$outcome)
melted_dt <- melted_dt[ outcome != "cas_not_eval"]
melted_dt <- melted_dt[ outcome != "ca_eval"]

# rename TB types

# ----------------------------------------------


# ----------------------------------------------
## graphing function / loop through dps and tb_type and make graphs
# ----------------------------------------------
# makeGraph <- function(data_table, dps){
#   g <- ggplot(data_table[dps == dps,], aes(quarter, value, color=outcome) ) + theme_bw() + 
#     geom_point() + 
#     geom_line() + 
#     ggtitle(dps) +
#     facet_wrap(~ tb_type, scales="free_y")
#   return(g)
# }

for (i in tb_type){
  pdf(paste0(export_dir, i, "_graph.pdf"), height = 9, width = 11)
  
  for (j in dps_names){
    
    g <- ggplot(melted_dt[dps == j & tb_type == i,], aes(quarter, value, color=outcome, ymin=0) ) + theme_bw() + 
      geom_point() + 
      geom_line() + 
      ggtitle(paste0("dps: ", j, " and tb type: ", i)) +
      facet_wrap(~ outcome, scales="free_y") +
      guides(color=FALSE)
    
    print(g)
  }
  
  dev.off()
}



