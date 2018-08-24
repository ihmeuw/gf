# ----------------------------------------------
# Audrey Batzel
#
# 8/21/18
# 
# Compare GF provinces to non-GF provinces at the health zone level;
# and using the donor listed in the pnlp data
setwd('C:/local/gf/')
# ----------------------------------------------


# --------------------
# Set up R / install packages
rm(list=ls())

library(data.table)
library(stringr)
library(reshape2)
library(ggplot2)
library(stats)
library(Rcpp)
library(readxl)
# --------------------  


# ----------------------------------------------
# Overview - Files and Directories

# data directory
# when run on Unix, data directory needs to be set to /home/j (to run on the cluster), so set this here:
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir_data = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/')
dir_pop = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/worldpop_data/')
dir_cod = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/')

# input files
hz_data <- "imputedData_run2_condensed_hz.rds"
funders <- "fullData_dps_standardized.csv"
funder_change <- "funders_data.xlsx"

# output files
output_dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_analysis/')
# ----------------------------------------------


# -----------------------------
# Load data

# load the imputed data at the hz level
dt <- readRDS(paste0(dir_data, hz_data))
dt$date <- as.Date(dt$date)
#dt <- dt[dps!="0",]

# load the data on funder changes to merge with dt
funder_changes <- read_excel(paste0(dir_cod, funder_change))
funder_changes <- as.data.table(funder_changes)

# load the world pop estimates at the hz level?
# -----------------------------


# ----------------------------------------------
# 1) # We want to identify which hzs were funded by which partner/funder
# Using the PNLP data, this turns out to not be very helpful... so see 2)

# # load in full data where dps/hz was standardized (might have to play around with this to find it?)
  # funder_data <- read.csv(paste0(dir_data, funders))
  # funder_data <- as.data.table(funder_data)
# # subset to just the relevant vars
  # funder_data <- funder_data[, .(dps, health_zone, donor, year, month, date)]

# # merge funder data with full data
#   funder_data$date <- as.Date(funder_data$date)
#   funder_data$dps <- as.character(funder_data$dps)
#   funder_data$health_zone <- as.character(funder_data$health_zone)

#   dt_merge <- merge(dt, funder_data, all.x=T, by=c("dps", "health_zone", "date"))

# # mark where gf was a donor
#   dt_merge <- dt_merge[grepl("FM", donor), GF:= TRUE]
#   setnames(dt_merge, "donor", "funder")


# 2) # Use document from PNLS of funder changes from Rationalisation process in DRC to track
# where funder changes and do an analysis of this process

# aucun hz is not in the data but is in funder changes so ignore this row and get rid of notes column at the end
funder_changes <- funder_changes[health_zone != "aucun", 1:6]

# merge
dt <- merge(dt, funder_changes, all = TRUE, by=c("dps", "health_zone"))

# make a column to mark whether or not a hz is part of the rationalisation process
dt <- dt[, rat := ifelse(is.na(change), 0, 1)]
dt <- dt[is.na(change), change:=0]
# ----------------------------------------------


# ----------------------------------------------
# # Clean dps names 
dt$dps <- gsub(" ", "-", dt$dps)
dt[dps=="bas-congo", dps:= "kongo-central"]
dt <- dt[dps!="0",]
# ----------------------------------------------


# ----------------------------------------------
# 1)# # Check where entire DPS are missing funder data by year
    # dt_merge$year <- year(dt_merge$date)
    # missing_funder <- dt_merge[, .(missing= sum(is.na(funder)), .N), by=c("dps", "year")]
    # missing_funder <- missing_funder[, percent_missing:= (missing/N)*100]
    # 
    # kinshasa_funder <- dt_merge[dps=="kinshasa", .N, by="funder"]
    # funder_breakdown <- dt_merge[year %in% 2014:2017, .N, by=.(year, dps, health_zone, funder)]
    # 
    # number_of_funders <- dt_merge[year %in% 2014:2017, .(number_of_funders = uniqueN(funder)), by=.(dps, health_zone, year)]
    # change_in_funders2 <- number_of_funders[ number_of_funders != shift(number_of_funders, 1L, type="lead"), 
    #                                         .(dps, health_zone, year, number_of_funders, previous_number= shift(number_of_funders, 1L, type="lead")), 
    #                                         by=.(dps, health_zone)]
    # 
    # change_in_funders <- number_of_funders[ number_of_funders != shift(number_of_funders, 1L, type="lag"), 
    #                                         .(year, number_of_funders, previous_number= shift(number_of_funders, 1L, type="lag")), 
    #                                         by=.(dps, health_zone)]
# ----------------------------------------------


# ----------------------------------------------
# subset to just acts used in dt
acts_used <- dt[indicator %in% c("ArtLum", "ASAQused") & subpopulation!="received", ]
acts_used <- acts_used[, .(totActsUsed = sum(mean)), by=c("dps", "health_zone", "date", "old_funder", "new_funder", "change", "covered_by_GF_now", "rat")] 
  # after date, the other vars should be unique to health_zone, this is just so they remain in the data, and we can combine
  # subsetting with this summing

cases <- dt[indicator %in% c("newCasesMalariaMild", "newCasesMalariaSevere"),]
cases <- cases[, .(totCasesMal = sum(mean)), by=c("dps", "health_zone", "date", "old_funder", "new_funder", "change", "covered_by_GF_now", "rat")]

acts_used_cases <- merge(acts_used, cases, by=c("dps", "health_zone", "date", "old_funder", "new_funder", "change", "covered_by_GF_now", "rat"))
# ----------------------------------------------


# ----------------------------------------------
# Graph ACTs used by whether or not hz in rationalisation

# total sum of ACTs by rationalisation yes/no (rat = 0 or 1)
compare <- acts_used[, totACTs := sum(totActsUsed), by= c("date", "rat")]
compare$rat <- as.character(compare$rat)

g <- ggplot(compare[,], aes(x=date, y=totACTs, color = rat)) + theme_bw()+
  geom_point() + geom_line() + guides(color=guide_legend(title="Rationalisation occurred")) +
  ggtitle(paste0("ACT doses distributed to patients; aggregated by whether \nor not the health zone was part of the rationalisation process")) +
  ylab("Doses") + xlab("Date")  + scale_y_continuous()
print(g)

# total sum of cases by rationalisation yes/no (rat = 0 or 1)
compare_byCases <- acts_used_cases[, .(totACTs = sum(totActsUsed), totCases = sum(totCasesMal)), by= c("date", "rat")]
compare_byCases <- compare_byCases[, actsByCases := totACTs/totCases, by= c("date", "rat")]
compare_byCases <- compare_byCases[rat==1, rat:="yes"]
compare_byCases <- compare_byCases[rat==0, rat:="no"]

g <- ggplot(compare_byCases, aes(x=date, y=actsByCases, color = rat)) + theme_bw()+
  geom_point() + geom_line() + guides(color=guide_legend(title="Transitioned Funders")) +
  ggtitle(paste0("ACT doses per case distributed to patients; aggregated to health zone level by \nwhether or not the health zone was part of the rationalisation process")) +
  ylab("Doses per case") + xlab("Date")  + scale_y_continuous()
print(g)

# ----------------------------------------------


# ----------------------------------------------
# Graph ACTs used by type of change in rationalisation process

# total sum of ACTs by rationalisation yes/no (rat = 0 or 1)
acts_used <- acts_used[change==5, change:=2]
compare <- acts_used[, totACTs := sum(totActsUsed), by= c("date", "change")]
compare$change <- as.character(compare$change)
compare <- compare[change != "0",]
compare <- compare[change==1, change:="PEPFAR to GF"]
compare <- compare[change==2, change:="GF to PEPFAR"]
compare <- compare[change==3, change:="GF/PEPFAR to GF only"]
compare <- compare[change==4, change:="GF/PEPFAR to PEPFAR only"]
compare <- compare[change==6, change:="MAP fund to GF"]

g <- ggplot(compare[], aes(x=date, y=totACTs, color = change)) + theme_bw()+
  geom_point() + geom_line() + guides(color=guide_legend(title="Type of change in funders")) +
  ggtitle(paste0("ACT doses distributed to patients aggregated by change \nin funders during rationalisation process (hz-level)")) +
  ylab("Doses") + xlab("Date")  + scale_y_continuous()
print(g)
# ----------------------------------------------


# ----------------------------------------------
# Graph ACTs used by whether or not it went from duplication to single funder
acts_used <- acts_used[ change %in% c(3, 4), duplicated:= 1]
acts_used <- acts_used[ change %in% c(1, 2, 5, 6), duplicated:= 0]
compare <- acts_used[, totACTs := sum(totActsUsed), by= c("date", "duplicated")]
compare$duplicated <- as.character(compare$duplicated)
compare <- compare[change != "0",]
compare <- compare[duplicated=="1", duplicated:="yes"]
compare <- compare[duplicated=="0", duplicated:="no"]

g <- ggplot(compare[], aes(x=date, y=totACTs, color = duplicated)) + theme_bw()+
  geom_point() + geom_line() + guides(color=guide_legend(title="")) +
  ggtitle(paste0("ACT doses distributed to patients aggregated by whether or not \n the health zone had two funders before rationalisation")) +
  ylab("Doses") + xlab("Date")  + scale_y_continuous()
print(g)
# ----------------------------------------------


# ----------------------------------------------
# Graph ACTs used by whether or not GF is managing hz after rationalisation process
# ----------------------------------------------








