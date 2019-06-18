# Audrey Batzel 
# 1-30-19
#
# Combine PNLP and base data
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
library(openxlsx)
# --------------------  

# ----------------------------------------------
# Overview - Files and Directories
# ----------------------------------------------
# data directory
# when run on Unix, data directory needs to be set to /home/j (to run on the cluster), so set this here:
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/post_imputation/')
out_dir = "J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/mi_examples/"
  
# input files:
inFile = paste0(dir, "imputedData_run_0_001_aggVars_lagsLeads_condensed_hz_median.rds") #post_imputation

# output file:
out_folder = "median_midpoint/"

# functions
source('./core/standardizeHZNames.R')
source('./core/standardizeDPSNames.R')

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# read in variable naming file:
variable_matching = read.xlsx("./outcome_measurement/malaria/cod/match_dhis_names.xlsx")
variable_matching = as.data.table(variable_matching)
variable_matching = variable_matching[!is.na(indicator),]
# ----------------------------------------------

# ----------------------------------------------
# Load data 
# ----------------------------------------------
pnlp <- readRDS(inFile) 
pnlp[ !is.na(lower) & !is.na(upper) , imputed := TRUE]
pnlp[ is.na(lower) & is.na(upper) , imputed := FALSE]

# pnlp[, health_zone := standardizeHZNames(health_zone)] 
# pnlp[, dps := standardizeDPSNames(dps)] 

# # pnlp - standardize indicator names
# pnlp[indicator=="ITN", indicator:= "LLIN"]
# pnlp[indicator=="newCasesMalariaMild", indicator:= "newCasesMalariaSimpleConf"]
# pnlp[indicator=="mildMalariaTreated", indicator:= "simpleConfMalariaTreated"]
# pnlp[ indicator == "ArtLum" & subpopulation == "used", indicator := "ALused"]
# pnlp[ indicator == "ArtLum" & subpopulation =="received", indicator := "ALreceived"]

# subset = pnlp[ indicator == "RDT" & subpopulation == "completed"]

# fix where for some reason upper or lower is not missing where the other is (in these cases the value was not imputed)
# subset[is.na(lower) & !is.na(upper) & upper < (value + 1), upper := NA] # not necessary with new imputed data? 
# subset[is.na(upper) & !is.na(lower) & lower < (value + 1), lower := NA] 
# subset[ !is.na(lower) & !is.na(upper) , imputed := TRUE]
# subset[ is.na(lower) & is.na(upper) , imputed := FALSE]

hzDPS = unique(pnlp[,.(health_zone, dps)])
hzDPS[, id := .I]
inds = unique(pnlp$variable)
inds = c("ANC_1st", "SP_1st", "ASAQreceived_14yrsAndOlder", "ASAQused_1to5yrs", "SSCACT", "SSCRDT_completed", "malariaDeaths_under5", 
         "newCasesMalariaSevere_5andOlder", "newCasesMalariaMild_pregnantWomen", "ITN_received", "ITN_distAtANC")

for (ind in inds) {
  outFile = paste0(out_dir, out_folder, "MI_results_examples_", ind, ".pdf")
  # sample = sample(hzDPS$id, 30)
  random_subset_hzDPS = copy(hzDPS) #hzDPS[id %in% sample, ]
  
  pdf(outFile, height = 9, width = 12)
  for (row in 1:nrow(random_subset_hzDPS)) {
  
    g <- ggplot(pnlp[ health_zone==(random_subset_hzDPS[row, health_zone]) & variable == ind, ], aes(x=date, y=value, color = imputed, shape = imputed)) + 
      theme_bw()+
      geom_point(size=2) + 
      scale_shape_manual(values=c(19, 1), labels = c("no", "yes")) + 
      scale_color_manual(values = c("TRUE" = "tomato3", "FALSE" = "steelblue1"), labels = c("no", "yes")) +
      geom_errorbar( aes(ymin=lower, ymax=upper, width=75), alpha=0.4) + 
      ggtitle(paste0("Multiple imputation for ", ind, " in the health zone ", simpleCap(random_subset_hzDPS[row, health_zone]), " (", simpleCap(random_subset_hzDPS[row, dps]), ")")) +
      labs(shape="Imputed Value", color="Imputed Value", y = "Count", x = "Date (month - year)", caption = "Source: Programme National de Lutte contre le Paludisme (PNLP)") +
      theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16), legend.text =element_text(size=14),
            plot.title = element_text(size=18), plot.caption = element_text(size=14)) 
    
    print(g)
    
  }
  dev.off()
}

# # figure for libenge example
# ggplot(libenge[variable %in% c("RDT_completed", "RDT_received")], aes(x=date, y=mean, color = variable)) + 
#   geom_point(data = libenge[variable %in% c("RDT_completed", "RDT_received") & !is.na(upper) & !is.na(lower) ], shape = 1, size = 2) +
#   geom_line() +
#   theme_bw() + 
#   facet_wrap(~variable, scales = "free") + 
#   guides(color = FALSE)
