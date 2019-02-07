setwd('C:/local/gf/')
# ----------------------------------------------

# --------------------
# Set up R / install packages
## -------------------
rm(list=ls())
library(data.table)
library(lubridate)
library(ggplot2)
# -------------------------
# Files and directories
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir <-paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/')
snis_dir <- paste0(dir, 'dhis/prepped/')
pnlp_dir <- paste0(dir, 'prepped_data/PNLP/')
out_dir <- paste0(dir, "dhis/compare_pnlp/compare_drugs_rec/")
dir_dhis = paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/prepped/')
  
# input files
pnlp_file <- 'pre_imputation_PNLP_2017.csv'
snis_drugs <- "tb_mal_drugs_distributed.rds"
pnlp_after_MI <- "post_imputation/imputedData_run2_agg_hz.rds"
dhis_base <- "base_services_drc_01_2017_09_2018_prepped.rds"

# output file
output <- "comparison of drugs received.pdf"

# functions
source('./core/standardizeHZNames.R')
source('./core/standardizeDPSNames.R')
# -------------------------

# ---------------------------------------------------
# Load prepped data
# ---------------------------------------------------
# snis <- readRDS(paste0(snis_dir, snis_drugs))
# pnlp <- read.csv(paste0(pnlp_dir, pnlp_file), stringsAsFactors = FALSE)
# pnlp <- as.data.table(pnlp)
# snis <- snis[year=="2017",] # ***NOTE: PNLP is already just 2017 data***

pnlp <- readRDS(paste0(pnlp_dir, pnlp_after_MI))
snis <- readRDS(paste0(dir_dhis, dhis_base))

pnlp[, health_zone := standardizeHZNames(health_zone)]
snis[, health_zone := standardizeHZNames(health_zone)]

pnlp[, dps := standardizeDPSNames(dps)]
snis[dps == "Maindombe", dps:= "mai-ndombe"]
snis[, dps := standardizeDPSNames(dps)]
# ---------------------------------------------------

# ---------------------------------------------------
# Subset to relevant vars and aggregate for comparison
# ---------------------------------------------------
# check for duplicated "unique identifiers
dups <- pnlp[duplicated(pnlp[, .(dps, health_zone, date)])]
  # duplicated values come from standardizing health zone names - some health zones are actually part of other ones,
  # so their names got changed to one that already exists. 
  # FIX: sum over health zone for all cols after subsetting (we want to add these values together)  - for now, ignore/remove NAs
pnlp <- pnlp[, .(dps, health_zone, date, year, ASAQreceived_14yrsAndOlder, ASAQreceived_1to5yrs, ASAQreceived_2to11mos, 
                 ASAQreceived_6to13yrs, ArtLum_received, ASAQused_14yrsAndOlder, ASAQused_1to5yrs, ASAQused_2to11mos, 
                 ASAQused_6to13yrs, ArtLum_used, SP_1st, SP_2nd, SP_3rd)]
pnlp_all <- colnames(pnlp)
pnlp_id <- c("dps", "health_zone", "date", "year")
pnlp_sd <- pnlp_all[!pnlp_all %in% pnlp_id]

pnlp <- pnlp[, lapply(.SD, sum, na.rm=TRUE), by=pnlp_id, .SDcols = pnlp_sd]

#melt pnlp data
pnlp <- melt.data.table(pnlp, id.vars=c("dps", "health_zone", "date", "year"), variable.name="drug", variable.factor = FALSE)

# edit pnlp vars to match snis/vice versa
pnlp[grepl("received", drug), variable := "received"]
pnlp[is.na(variable), variable:="consumed"]
pnlp[, drug:= gsub("received", "", drug)]
pnlp[, drug:= gsub("used", "", drug)]
pnlp[, drug:= gsub("ArtLum_", "AL", drug)]
pnlp[grepl("SP", drug), drug := "SP"]
snis[, drug:= gsub("ASAQ_12to59mos", "ASAQ_1to5yrs", drug)]
snis[grepl("AL", drug), drug:= "AL"]

# pnlp - sum over (drug + variable) to get SP totals
pnlp <- pnlp[, .(value= sum(value, na.rm=TRUE)), by=c("dps", "health_zone", "date", "year", "drug", "variable")]

# subset snis
snis <- snis[, .(dps, health_zone, date, year, type, drug, available, lost, consumed, received)]
# melt so structure matches pnlp
snis <- melt.data.table(snis, id.vars=c("dps", "health_zone", "date", "year", "type", "drug"), variable.factor=FALSE)

# aggregate SNIS to health zone level (and sum over drugs too)
snis <- snis[, .(value= sum(value, na.rm=TRUE)), by=c("dps", "health_zone", "date", "year", "type", "drug", "variable")]

# subset snis to just malaria drugs and the vars in pnlp
snis <- snis[variable %in% c("received", "consumed") & type=="malaria", ]
# ---------------------------------------------------

# ---------------------------------------------------
# Make figures comparing PNLP to DHIS 
# ---------------------------------------------------
# merge data sets
setnames(snis, "value", "value_snis")
setnames(pnlp, "value", "value_pnlp")
pnlp$date <- as.Date(pnlp$date)
snis$date <- as.Date(snis$date)

dt <- merge(snis, pnlp, by=c("dps", "health_zone", "date", "drug", "variable"))
dt <- dt[, .(dps, health_zone, date, drug, variable, value_snis, value_pnlp)]
  
  # go back and check which rows are dropped

drugs_vars <- unique(dt[, .(drug, variable)])

pdf(paste0(out_dir, output), height = 9, width = 11)
for(row in 1:nrow(drugs_vars)) {
  d = drugs_vars[row, drug]
  v = drugs_vars[row, variable]
  dt_graph <- dt[ drug == d & variable == v, ]
  dt_graph <- dt_graph[!is.na(value_snis) & !is.na(value_pnlp), ]
  dt_graph <- dt_graph[value_snis != 0 & value_pnlp != 0 ]
  
  g <- ggplot(dt_graph, aes(x=value_snis, y=value_pnlp, color=dps)) + 
      theme_bw() + geom_abline(alpha= 0.8) + geom_point() + 
      ggtitle(paste0("SNIS and PNLP comparison for ", d, " " , v, " (N= ", nrow(dt_graph), ")")) +
      ylab("Value SNIS") + xlab("Value PNLP") + labs(caption = "Points represent health zone - months,\n health zone - months where PNLP or SNIS value is missing or 0 excluded") +
      theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_blank(), 
            legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14))
  print(g)
}
dev.off()
# ---------------------------------------------------





