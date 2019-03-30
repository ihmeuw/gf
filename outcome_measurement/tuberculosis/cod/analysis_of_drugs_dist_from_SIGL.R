setwd('C:/local/gf/')
# ----------------------------------------------

# --------------------
# Set up R / install packages
## -------------------
rm(list=ls())
library(data.table)
library(lubridate)
library(stringr)
library(ggplot2)
# -------------------------
# Files and directories
# directories
data_path <- "J:/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/"
catalogue_path <-"J:/Project/Evaluation/GF/outcome_measurement/cod/dhis/catalogues/"
dir_pnlp = paste0('J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/post_imputation/')
out_dir = 'J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/SNIS_all/'

# input files
data <-"sigl_drc_01_2015_07_2018_prepped.rds"
cat <- "data_elements_cod.csv"
pnlp = "archive/imputedData_run2_agg_dps.rds"

# output files 
# older files:
# prepped_data <- "drugs_consumed_lost_available.rds"
# drugs_dist <- "tb_mal_drugs_distributed.rds"

# functions
source('./core/standardizeHZNames.R')
source('./core/standardizeDPSNames.R')
# source('./outcome_measurement/malaria/cod/function_rename_variable.R')
# -------------------------

# ---------------------------------------------------
# Load prepped data
dt_dist <- readRDS(paste0(data_path, drugs_dist))
dt_pnlp <- readRDS(paste0(dir_pnlp, pnlp))
# ---------------------------------------------------

# ---------------------------------------------------
dt_dist$dps <- standardizeDPSNames(dt_dist$dps)
dt_pnlp$dps <- standardizeDPSNames(dt_pnlp$dps)
setnames(dt_pnlp, "mean", "value")

dist_dps <- dt_dist[, .(received = sum(received, na.rm = TRUE)), by = .(dps, date, drug)]
dist_dps$date <- as.Date(dist_dps$date)
dist_dps <- dist_dps[grepl("ASAQ", drug), ]

dt_pnlp <- dt_pnlp[indicator %in% c("ASAQreceived")]
dt_pnlp$date <- as.Date(dt_pnlp$date)
dt_pnlp[, drug:= paste0(indicator, "_", subpopulation)]

dt_pnlp$source = "PNLP"
dist_dps$source = "SIGL"

dt_pnlp[, drug := gsub("received", "", drug)]
dist_dps[, drug := gsub("12to59mos", "1to5yrs", drug)]

setnames(dt_pnlp, "value", "received")
dt_pnlp = dt_pnlp[, names(dist_dps), with = FALSE]
vars = unique(dt_pnlp$drug)
dist_dps = dist_dps[ drug %in% vars, ]
dt_compare <- rbindlist(list(dist_dps, dt_pnlp), use.names=TRUE)


pdf(paste0(out_dir, "comparison_of_pnlp_sigl_drugs_received_03_28_19.pdf"), height = 12, width = 15)
for (d in unique(dt_compare$dps)){
  g <- ggplot(dt_compare[dps == d], aes(x=date, y=received, color=source)) + 
    geom_point() + geom_line() +
    ggtitle(paste0("Time series showing PNLP and calculated variable using SIGL in ", d)) +
    ylab("Drugs/doses received") + xlab("Date") + theme_bw() +
    theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16), 
          legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14)) +
    facet_wrap( ~drug, scales = "free_y") +
    scale_y_continuous( label= scales :: comma ) + 
    guides(color = guide_legend(title= "Data Source:"))
  print(g)
}
dev.off()


