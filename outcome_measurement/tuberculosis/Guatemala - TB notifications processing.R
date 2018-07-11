# ----------------------------------------------
# Guillermo Ambrosio (CIEAR)
#
# 2017-11-14
# Explore TB outcome data from MOH notifications databases.


# ----------------------------------------------
# Dependencies:
library(data.table)
library(stringr)
library(readxl)
library(ggplot2)
library(gridExtra)


# ----Configure------------------------------------------
saveGraphs = T
codePath = "PCE/gf/"

# Requirements:
source(paste0(codePath, "core/GT_load_data.R"), encoding = "UTF-8")
source(paste0(codePath, "core/GT_helper_functions.R"), encoding = "UTF-8")

# ------- Load TB notifications aggregated database ----------------
tbnots = read.csv("./PCE/Outcome Measurement Data/TUBERCULOSIS/GTM - TB notifications 2012-2017.csv")

tbnots = data.table(tbnots)
ggplot(data = tbnots[toupper(CONDICIONINGRESO)=="NUEVO",.(Count = .N),by=.(YearMonth)][,.(Count, YearMonth =factor(YearMonth))]) + 
    geom_col(aes(YearMonth, Count))  + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))
