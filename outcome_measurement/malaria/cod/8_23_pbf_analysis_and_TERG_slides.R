# ----------------------------------------------
# Audrey Batzel
#
# 8/23/18
# 
# Compare provinces involved in PBF to those not doing PBF; make other graphs for TERG slides
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


# ----------------------------------------------
# Analysis of PBF
# Graph ACTs used by whether or not dps in is pbf program

dps_all <- unique(dt$dps)
dps_pbf <- c("kwango", "kwilu", "mai-ndombe", "equateur", "tshuapa", "mongala", "nord-ubangi", "tanganyika", "lualaba", "haut-lomami", "sud-ubangi", "haut-katanga")
dps_not_pdf <- dps_all[!dps_all %in% dps_pbf]

acts_used <- dt[indicator %in% c("ArtLum", "ASAQused") & subpopulation!="received", ]
acts_used <- acts_used[, pbf := ifelse(dps %in% dps_pbf, "yes", "no")]
dt_pbf <- acts_used[, .(totActsPBF = sum(mean)), by=c("date", "pbf")] 

g <- ggplot(dt_pbf[], aes(x=date, y=totActsPBF, color = pbf)) + theme_bw()+
  geom_point() + geom_line() + guides(color=guide_legend(title="PBF program")) +
  ggtitle(paste0("ACT doses distributed to patients; aggregated by whether or \nnot DPS is in PBF program")) +
  ylab("Doses of ACTs") + xlab("Date")  + scale_y_continuous()
print(g)

cases <- dt[indicator %in% c("newCasesMalariaMild", "newCasesMalariaSevere"),]
cases <- cases[, pbf := ifelse(dps %in% dps_pbf, "yes", "no")]
cases <- cases[, .(totCases = sum(mean)), by=c("date", "pbf")] 

acts_used_cases <- merge(dt_pbf, cases, by=c("date", "pbf"))
pbf_compare_byCases <- acts_used_cases[, actsByCases := totActsPBF/totCases, by= c("date", "pbf")]

g <- ggplot(pbf_compare_byCases[], aes(x=date, y=actsByCases, color = pbf)) + theme_bw()+
  geom_point() + geom_line() + guides(color=guide_legend(title="PBF program")) +
  ggtitle(paste0("ACT doses distributed to patients per malaria case; aggregated by whether or \nnot DPS is in PBF program")) +
  ylab("Doses of ACTs per case") + xlab("Date")  + scale_y_continuous()
print(g)

casesTreated <- dt[indicator %in% c("mildMalariaTreated", "severeMalariaTreated"),]
casesTreated <- casesTreated[, pbf := ifelse(dps %in% dps_pbf, "yes", "no")]
casesTreated <- casesTreated[, .(totCasesTreated = sum(mean)), by=c("date", "pbf")] 

casesTreated_vs_cases <- merge(cases, casesTreated, by=c("date", "pbf"))
pbf_compare <- casesTreated_vs_cases[, casesProp := totCasesTreated/totCases, by= c("date", "pbf")]

g <- ggplot(pbf_compare[date >= '2016-01-01',], aes(x=date, y=casesProp, color = pbf)) + theme_bw()+
  geom_point() + geom_line() + guides(color=guide_legend(title="PBF program")) +
  ggtitle(paste0("The proportion of cases treated out of total cases; \naggregated by DPS and by whether or not DPS is in PBF program")) +
  ylab("Proportion of Cases Treated") + xlab("Date")  + scale_y_continuous() + ylim(NA, 1)
print(g)

# coverage for MTK
cases <- dt[indicator %in% c("newCasesMalariaMild", "newCasesMalariaSevere"),]
cases_MTK <- cases[dps %in% c("maniema", "tshopo", "kinshasa"),]
cases_MTK <- cases_MTK[, .(totCases = sum(mean)), by=c("date", "dps")] 

casesTreated <- dt[indicator %in% c("mildMalariaTreated", "severeMalariaTreated"),]
casesTreated_MTK <- casesTreated[dps %in% c("maniema", "tshopo", "kinshasa")]
casesTreated_MTK <- casesTreated_MTK[, .(totCasesTreated = sum(mean)), by=c("date", "dps")] 

casesTreated_vs_cases <- merge(cases_MTK, casesTreated_MTK, by=c("date", "dps"))
casesTreated_vs_cases <- casesTreated_vs_cases[, casesProp := totCasesTreated/totCases, by= c("date", "dps")]

g <- ggplot(casesTreated_vs_cases, aes(x=date, y=casesProp, color = dps)) + theme_bw()+
  geom_point() + geom_line() + guides(color=guide_legend(title="DPS")) +
  ggtitle(paste0("The proportion of cases treated out of total cases in Maniema, Tshopo, and Kinshasa over time")) +
  ylab("Proportion of Cases Treated") + xlab("Date")  + scale_y_continuous()
print(g)

g <- ggplot(casesTreated_vs_cases, aes(x=date, y=casesProp, color = dps)) + theme_bw()+
  geom_point() + geom_line() + guides(color=guide_legend(title="DPS")) + guides(color=FALSE) +
  ggtitle(paste0("The proportion of cases treated out of total cases in Kinshasa, Maniema, and Tshopo over time")) +
  ylab("Proportion of Cases Treated") + xlab("Date")  + scale_y_continuous() +
  facet_wrap( ~dps)
print(g)

# total outputs ACTs used - whole country
acts_used <- dt[indicator %in% c("ArtLum", "ASAQused") & subpopulation!="received", ]
acts_used_COD <- acts_used[, .(totActs = sum(mean)), by=c("date")] 

g <- ggplot(acts_used_COD, aes(x=date, y=totActs)) + theme_bw()+
  geom_point(color='darkblue') + geom_line(color='darkblue') + 
  ggtitle(paste0("ACT doses distributed to patients for all of DRC over time")) +
  ylab("Doses of ACTs") + xlab("Date")  + scale_y_continuous()
print(g)

cases <- dt[indicator %in% c("newCasesMalariaMild", "newCasesMalariaSevere"),]
cases_COD <- cases[, .(totCases = sum(mean)), by=c("date")] 

casesTreated <- dt[indicator %in% c("mildMalariaTreated", "severeMalariaTreated"),]
casesTreated_COD <- casesTreated[, .(totCasesTreated = sum(mean)), by=c("date")] 

casesTreated_vs_cases <- merge(cases_COD, casesTreated_COD, by=c("date"))
casesTreated_vs_cases <- casesTreated_vs_cases[, casesProp := totCasesTreated/totCases, by= c("date")]

g <- ggplot(casesTreated_vs_cases, aes(x=date, y=casesProp)) + theme_bw()+
  geom_point(color='darkblue') + geom_line(color='darkblue') + 
  ggtitle(paste0("The proportion of cases treated out of total cases in all of DRC over time")) +
  ylab("Proportion of Cases Treated") + xlab("Date")  + scale_y_continuous()
print(g)
# ----------------------------------------------
