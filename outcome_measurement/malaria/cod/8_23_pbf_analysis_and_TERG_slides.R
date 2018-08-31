# ----------------------------------------------
# Audrey Batzel
#
# 8/23/18
# 
# Compare provinces involved in PBF to those not doing PBF; make other graphs for TERG slides
setwd('C:/local/gf/')
# ----------------------------------------------


# --------------------
## Set up R / install packages
# --------------------
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
## Overview - Files and Directories
# ----------------------------------------------
# data directory
# when run on Unix, data directory needs to be set to /home/j (to run on the cluster), so set this here:
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir_data = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/')
dir_pop = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/worldpop_data/')
dir_cod = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/')
dir_pbf = paste0(dir_cod, 'PBF/')

# input files
hz_data <- "imputedData_run2_condensed_hz.rds"
cod_data <- "imputedData_run2_condensed_country.rds"
cod_data_cases_byYear <- "imputedData_run2_condensed_country_byYear.rds"
funders <- "fullData_dps_standardized.csv"
funder_change <- "funders_data.xlsx"
hzs_pbf <- "Cartographie FBP RDC23012017.xlsx"
data_before_MI <- "fullData_dps_standardized.csv"

# output files
output_dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_analysis/')
# ----------------------------------------------


# -----------------------------
## Load data
# -----------------------------
# load the imputed data at the hz level
dt <- readRDS(paste0(dir_data, hz_data))
dt$date <- as.Date(dt$date)

# imputed data at the country level
dt_cod <- readRDS(paste0(dir_data, cod_data))

# data before imputation 
dt_before_MI <- read.csv(paste0(dir_data, data_before_MI))
# -----------------------------


# ----------------------------------------------
# # Clean dps names 
# ----------------------------------------------
dt$dps <- gsub(" ", "-", dt$dps)
dt[dps=="bas-congo", dps:= "kongo-central"]
dt <- dt[dps!="0",]

dt$health_zone <- gsub(" ", "-", dt$health_zone)
# ----------------------------------------------


# ----------------------------------------------
## Added value of imputation analysis
# ----------------------------------------------
dt_before_MI <- dt_before_MI[, c("date", "dps", "health_zone", "newCasesMalariaMild_5andOlder", "newCasesMalariaMild_pregnantWomen", "newCasesMalariaMild_under5", 
                                 "newCasesMalariaSevere_5andOlder", "newCasesMalariaSevere_pregnantWomen", "newCasesMalariaSevere_under5")]

dt_before_MI <- melt(dt_before_MI, id.vars = c('date', 'dps', 'health_zone'))
dt_before_MI <- as.data.table(dt_before_MI)

dt_before_MI_tot <- dt_before_MI[, .(totCasesBeforeMI = sum(value, na.rm=TRUE)), by="date"]
dt_before_MI_tot$date <- as.Date(dt_before_MI_tot$date)

# # create a variable that captures whether or not a value was imputed
# dt[, imputedValue := ifelse( is.na(lower) & is.na(upper), "no", "yes")] 
# # graph the sum of cases over time 
# cases_MI <- dt[indicator %in% c("newCasesMalariaMild", "newCasesMalariaSevere"),]
# 
# cases_MI <- cases_MI[, totCasesAfterMI := sum(totCases), by=c("date")]
# cases_MI <- cases_MI[imputedValue=="no", ]
# melt_cases_MI <- melt.data.table(cases_MI, id.vars=c("date", "imputedValue"), measure.vars = c("totCases", "totCasesAfterMI"))

dt_after_MI <- dt[indicator %in% c("newCasesMalariaMild", "newCasesMalariaSevere"),]
dt_after_MI_tot <- dt_after_MI[, .(totCasesAfterMI = sum(mean)), by="date"] 
dt_after_MI_tot$date <- as.Date(dt_after_MI_tot$date)

dt_MI <- merge(dt_before_MI_tot, dt_after_MI_tot, all=TRUE, by="date")
melt_dt_MI <- melt.data.table(dt_MI, id.vars=c("date"), measure.vars = c("totCasesBeforeMI", "totCasesAfterMI"))

pdf(paste0(output_dir, "cases_before_and_after_MI.pdf"), height=9, width=11)
g <- ggplot(melt_dt_MI, aes(x=date, y=value, color = variable)) + theme_bw()+
  geom_point() + geom_line() + guides(color=guide_legend(title="")) +
  ggtitle(paste0("Total number of Malaria cases (uncomplicated and severe) before \nand after Multiple Imputation")) +
  ylab("Number of cases") + xlab("Date")  + scale_y_continuous() + scale_color_manual(labels = c("Before MI", "After MI"), values = c("tomato", "skyblue3")) +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=15), legend.title=element_text(size=16), 
        legend.text =element_text(size=14), plot.title = element_text(size=18))
print(g)
dev.off()

melt_dt_MI$year <- year(melt_dt_MI$date)
dt_MI_stats <- melt_dt_MI[, .(yearTotal = sum(value)), by=.(year, variable)]
dt_MI_stats <- dcast(dt_MI_stats, year ~ variable, value.var = "yearTotal")
dt_MI_stats <- as.data.table(dt_MI_stats)
dt_MI_stats <- dt_MI_stats[, yearDiff := (totCasesAfterMI - totCasesBeforeMI)]
dt_MI_stats <- dt_MI_stats[, percentChange := ((totCasesAfterMI - totCasesBeforeMI)/totCasesBeforeMI)*100]

write.csv(dt_MI_stats, paste0(dir_data, "stats_before_and_after_MI.csv"))

# calculate uncertainty interval - on cluster so I can aggregate first....
# then read in data to visualize results
dt_cod_cases_byYear <- readRDS(paste0(dir_data, cod_data_cases_byYear))

# ----------------------------------------------


# ----------------------------------------------
## Analysis of PBF
# ----------------------------------------------
# Graph ACTs used by whether or not dps in is pbf program

# dps_all <- unique(dt$dps)
# dps_pbf <- c("kwango", "kwilu", "mai-ndombe", "equateur", "tshuapa", "mongala", "nord-ubangi", "tanganyika", "lualaba", "haut-lomami", "sud-ubangi", "haut-katanga")
# dps_not_pdf <- dps_all[!dps_all %in% dps_pbf]

# acts_used <- dt[indicator %in% c("ArtLum", "ASAQused") & subpopulation!="received", ]
# acts_used <- acts_used[, pbf := ifelse(dps %in% dps_pbf, "yes", "no")]
# dt_pbf <- acts_used[, .(totActsPBF = sum(mean)), by=c("date", "pbf")] 
# 
# g <- ggplot(dt_pbf[], aes(x=date, y=totActsPBF, color = pbf)) + theme_bw()+
#   geom_point() + geom_line() + guides(color=guide_legend(title="PBF program")) +
#   ggtitle(paste0("ACT doses distributed to patients; aggregated by whether or \nnot DPS is in PBF program")) +
#   ylab("Doses of ACTs") + xlab("Date")  + scale_y_continuous()
# print(g)
# 
# cases <- dt[indicator %in% c("newCasesMalariaMild", "newCasesMalariaSevere"),]
# cases <- cases[, pbf := ifelse(dps %in% dps_pbf, "yes", "no")]
# cases <- cases[, .(totCases = sum(mean)), by=c("date", "pbf")] 
# acts_used_cases <- merge(dt_pbf, cases, by=c("date", "pbf"))
# pbf_compare_byCases <- acts_used_cases[, actsByCases := totActsPBF/totCases, by= c("date", "pbf")]

# g <- ggplot(pbf_compare_byCases[], aes(x=date, y=actsByCases, color = pbf)) + theme_bw()+
#   geom_point() + geom_line() + guides(color=guide_legend(title="PBF program")) +
#   ggtitle(paste0("ACT doses distributed to patients per malaria case; aggregated by whether or \nnot DPS is in PBF program")) +
#   ylab("Doses of ACTs per case") + xlab("Date")  + scale_y_continuous()
# print(g)
# 
# casesTreated <- dt[indicator %in% c("mildMalariaTreated", "severeMalariaTreated"),]
# casesTreated <- casesTreated[, pbf := ifelse(dps %in% dps_pbf, "yes", "no")]
# casesTreated <- casesTreated[, .(totCasesTreated = sum(mean)), by=c("date", "pbf")] 
# 
# casesTreated_vs_cases <- merge(cases, casesTreated, by=c("date", "pbf"))
# pbf_compare <- casesTreated_vs_cases[, casesProp := totCasesTreated/totCases, by= c("date", "pbf")]
# 
# g <- ggplot(pbf_compare[date >= '2016-01-01',], aes(x=date, y=casesProp, color = pbf)) + theme_bw()+
#   geom_point() + geom_line() + guides(color=guide_legend(title="PBF program")) +
#   ggtitle(paste0("The proportion of cases treated out of total cases; \naggregated by DPS and by whether or not DPS is in PBF program")) +
#   ylab("Proportion of Cases Treated") + xlab("Date")  + scale_y_continuous() + ylim(NA, 1)
# print(g)
# ----------------------------------------------
# Graph cases treated over cases by whether or not HEALTH ZONE is in PBF (within provinces and then between all provinces)

hz_pbf <- read_excel(paste0(dir_pbf, hzs_pbf)) 
hz_pbf <- as.data.table(hz_pbf)
hz_pbf <- hz_pbf[-c(1:7), -c(1,4:7)]
colnames(hz_pbf) <- c("dps", "health_zone")
hz_pbf$dps <- tolower(hz_pbf$dps)
hz_pbf$health_zone <- tolower(hz_pbf$health_zone)
hz_pbf$dps <- gsub(" ", "-", hz_pbf$dps)
hz_pbf$health_zone <- gsub(" ", "-", hz_pbf$health_zone)
hz_pbf$dps <- gsub("---", "-", hz_pbf$dps)
hz_pbf <- hz_pbf[!is.na(dps) & !is.na(health_zone),]


# dps in pdf : c("kwango", "kwilu", "mai-ndombe", "equateur", "tshuapa", "mongala", "nord-ubangi", "tanganyika", "lualaba", "haut-lomami", "sud-ubangi", "haut-katanga")
# edit health zone names to match dt  

# all hzs c("kwango", "kwilu", "mai-ndombe", "equateur", "tshuapa", "mongala", "sud-ubangi") are PBF

# missing some hzs in haut-katanga, haut-lomami, lualaba (all but 2 are PBF), nord-kivu, sud-kivu, maniema

# new dps that weren't listed before- maniema, nord-kivu, sud-kivu
  # kasai-central(just one hz), kasai-oriental(just one hz), sankuru (just two hz)

# dps that were listed before that don't have any health zones in pbf- nord-ubangi, tanganyika

hz_pbf <- hz_pbf[dps == "equateur" & health_zone == "mankanza", health_zone := "makanza"]
hz_pbf <- hz_pbf[dps == "equateur" & health_zone == "lilanga-bobangi", health_zone := "lilanga-bobanga"]
hz_pbf <- hz_pbf[dps == "kwango" & health_zone == "kasongolunda", health_zone := "kasongo-lunda"]
hz_pbf <- hz_pbf[dps == "kwango" & health_zone == "mwela-lemba", health_zone := "mwela-lembwa"]
hz_pbf <- hz_pbf[dps == "kwilu" & health_zone == "yasabonga", health_zone := "yasa-bonga"]
hz_pbf <- hz_pbf[dps == "mai-ndombe" & health_zone == "bandjow-moke", health_zone := "bandjow"]
hz_pbf <- hz_pbf[dps == "mai-ndombe" & health_zone == "ntandembelo", health_zone := "ntandembele"]
hz_pbf <- hz_pbf[dps == "mai-ndombe" & health_zone == "pendjaw", health_zone := "pendjwa"]
hz_pbf <- hz_pbf[dps == "mongala" & health_zone == "bongandanga", health_zone := "bongandanganda"]
hz_pbf <- hz_pbf[dps == "mongala" & health_zone == "boso-mondanda", health_zone := "bosomondanda"]
hz_pbf <- hz_pbf[dps == "mongala" & health_zone == "manzi", health_zone := "bosomanzi"]
hz_pbf <- hz_pbf[dps == "sud-ubangi" & health_zone == "bogosenubea", health_zone := "bogosenusebea"]
hz_pbf <- hz_pbf[dps == "tshuapa" & health_zone == "busanga", health_zone := "bosanga"]

hz_pbf <- hz_pbf[dps == "haut-katanga" & health_zone == "sakania", health_zone := "sakanya"]
hz_pbf <- hz_pbf[dps == "haut-lomami" & health_zone == "kabondo-dianda", health_zone := "kabond-dianda"]
hz_pbf <- hz_pbf[dps == "kolwezi", dps := "lualaba"]
hz_pbf <- hz_pbf[dps == "sud-maniema", dps := "maniema"]
hz_pbf <- hz_pbf[dps == "maniema" & health_zone == "saramabila", health_zone := "saramabil"]
hz_pbf <- hz_pbf[dps == "nord-kivu" & health_zone == "kirotshé", health_zone := "kirotshe"]
hz_pbf <- hz_pbf[dps == "sud-kivu" & health_zone == "kimbi-lulenge", health_zone := "lulenge"]
hz_pbf <- hz_pbf[dps == "sankuru" & health_zone == "wembonyama", health_zone := "wembo-nyama"]
hz_pbf <- hz_pbf[dps == "lualaba" & health_zone == "kazenze", health_zone := "kanzenze"]
hz_pbf <- hz_pbf[dps == "lualaba" & health_zone == "lualaba", health_zone := "lwalaba"]

hz_pbf <- hz_pbf[, pbf:="yes"]

dt_pbf <- merge(dt, hz_pbf, all=TRUE, by=c("dps", "health_zone"))
dt_pbf <- dt_pbf[is.na(pbf), pbf:= "no"]

dps_mixed_pbf <-c("haut-katanga", "haut-lomami", "lualaba", "maniema", "nord-kivu", "sud-kivu")

make_graph_mixed_pbf <- function(d){
cases <- dt_pbf[ dps == d & indicator %in% c("newCasesMalariaMild", "newCasesMalariaSevere"),]
cases <- cases[, .(totCases = sum(mean)), by=c("date", "pbf")] 

casesTreated <- dt_pbf[dps == d & indicator %in% c("mildMalariaTreated", "severeMalariaTreated"),]
casesTreated <- casesTreated[, .(totCasesTreated = sum(mean)), by=c("date", "pbf")] 

casesTreated_vs_cases <- merge(cases, casesTreated, by=c("date", "pbf"))
pbf_compare <- casesTreated_vs_cases[, casesProp := totCasesTreated/totCases, by= c("date", "pbf")]

pdf(paste0(output_dir, "PBF(hzLevel)_propCasesTreated_wholeTS_", d, ".pdf"), height = 9, width = 11)

g <- ggplot(pbf_compare[], aes(x=date, y=casesProp, color = pbf)) + theme_bw()+
  geom_point() + geom_line() + guides(color=guide_legend(title="PBF program")) +
  ggtitle(paste0("The proportion of cases treated out of total cases in ", d, "; \naggregated by whether or not the health zone is in PBF program")) +
  ylab("Proportion of Cases Treated") + xlab("Date")  + scale_y_continuous() + ylim(NA, 1)
print(g)

dev.off()
}

for (d in dps_mixed_pbf){
  make_graph_mixed_pbf(d)
}
# ----------------------------------------------


# ----------------------------------------------
## coverage for MTK
# ----------------------------------------------
cases <- dt[indicator %in% c("newCasesMalariaMild", "newCasesMalariaSevere"),]
cases_MTK <- cases[dps %in% c("maniema", "tshopo", "kinshasa"),]
cases_MTK <- cases_MTK[, .(totCases = sum(mean)), by=c("date", "dps")] 

casesTreated <- dt[indicator %in% c("mildMalariaTreated", "severeMalariaTreated"),]
casesTreated_MTK <- casesTreated[dps %in% c("maniema", "tshopo", "kinshasa")]
casesTreated_MTK <- casesTreated_MTK[, .(totCasesTreated = sum(mean)), by=c("date", "dps")] 

casesTreated_vs_cases <- merge(cases_MTK, casesTreated_MTK, by=c("date", "dps"))
casesTreated_vs_cases <- casesTreated_vs_cases[, casesProp := totCasesTreated/totCases, by= c("date", "dps")]
casesTreated_vs_cases$year <- year(casesTreated_vs_cases$date)

pdf(paste0(output_dir, 'coverage_MTK_(casesTreatedvsCases)_2015on.pdf'), height=9, width=11)
g <- ggplot(casesTreated_vs_cases[year>=2015,], aes(x=date, y=casesProp, color = dps)) + theme_bw()+
  geom_point() + geom_line() + guides(color=guide_legend(title="DPS")) +
  ggtitle(paste0("The proportion of cases treated out of total cases in Maniema, Tshopo, and Kinshasa")) +
  ylab("Proportion of Cases Treated") + xlab("Date")  + scale_y_continuous() +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=15), legend.title=element_text(size=16), 
        legend.text =element_text(size=14), plot.title = element_text(size=18))
print(g)
dev.off()

pdf(paste0(output_dir, 'coverage_MTK_(casesTreatedvsCases)_faceted_2015on.pdf'), height=9, width=11)
g <- ggplot(casesTreated_vs_cases[year>=2015,], aes(x=date, y=casesProp, color = dps)) + theme_bw()+
  geom_point() + geom_line() + guides(color=guide_legend(title="DPS")) + guides(color=FALSE) +
  ggtitle(paste0("The proportion of cases treated out of total cases in Kinshasa, Maniema, and Tshopo")) +
  ylab("Proportion of Cases Treated") + xlab("Date")  + scale_y_continuous() +
  facet_wrap( ~dps) + theme(axis.text.x=element_text(angle=60, hjust=1)) +
  theme(axis.text=element_text(size=13), axis.title=element_text(size=15), strip.text = element_text(size = 15), plot.title = element_text(size=18))
print(g)
dev.off()


# total outputs ACTs used - whole country
acts_used <- dt[indicator %in% c("ArtLum", "ASAQused") & subpopulation!="received", ]
acts_used_COD <- acts_used[, .(totActs = sum(mean)), by=c("date")] 

g <- ggplot(acts_used_COD, aes(x=date, y=totActs)) + theme_bw()+
  geom_point(color='darkblue') + geom_line(color='darkblue') + 
  ggtitle(paste0("ACT doses distributed to patients for all of DRC over time")) +
  ylab("Doses of ACTs") + xlab("Date")  + scale_y_continuous()
print(g)

# coverage cases treated - whole country
cases <- dt[indicator %in% c("newCasesMalariaMild", "newCasesMalariaSevere", "suspectedMalaria"),]
cases_COD <- cases[, .(totCases = sum(mean)), by=c("date")] 

casesTreated <- dt[indicator %in% c("mildMalariaTreated", "severeMalariaTreated"),]
casesTreated_COD <- casesTreated[, .(totCasesTreated = sum(mean)), by=c("date")] 

casesTreated_vs_cases <- merge(cases_COD, casesTreated_COD, by=c("date"))
casesTreated_vs_cases <- casesTreated_vs_cases[, casesProp := totCasesTreated/totCases, by= c("date")]

casesTreated_vs_cases$year <- year(casesTreated_vs_cases$date)

pdf(paste0(output_dir, 'coverage_COD_casesTreatedvsCases(incSuspected)_TS.pdf'), height=9, width=11)
g <- ggplot(casesTreated_vs_cases[], aes(x=date, y=casesProp)) + theme_bw()+
  geom_point(color='darkblue') + geom_line(color='darkblue') + 
  ggtitle(paste0("The proportion of cases treated out of total cases (including suspected cases) in all of DRC over time")) +
  ylab("Proportion of Cases Treated") + xlab("Date") + ylim(0, 0.60)
print(g)
dev.off()
# ----------------------------------------------
