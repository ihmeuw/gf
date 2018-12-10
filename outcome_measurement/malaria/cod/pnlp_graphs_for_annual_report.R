# ----------------------------------------------
# Audrey Batzel
#
# 10/8/18
# 
# Move code to new script to create graphs for the annual report
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
library(grid)
library(gridExtra)
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
hz_data <- "post_imputation/imputedData_run2_agg_hz.rds"
cod_data <- "post_imputation/imputedData_run2_agg_country.rds"
dps_data <- "post_imputation/imputedData_run2_agg_dps.rds"
cod_data_cases_byYear <- "imputedData_run2_condensed_country_byYear.rds"
funders <- "fullData_dps_standardized.csv"
funder_change <- "funders_data.xlsx"
hzs_pbf <- "Cartographie FBP RDC23012017.xlsx"
data_before_MI <- "fullData_dps_standardized.csv"

# output files
output_dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_analysis/')
cases_treated_graph = paste0(output_dir, 'MTKandOthers_propCasesTreated_timeSeries.pdf')
natl_doses_graph = paste0(output_dir, 'natl_dosesDist_timeSeries.pdf')
missing_data_cases_treated = paste0(output_dir, 'MTK_missing_data_cases_treated.pdf')
suspected_cases_vs_tests_completed = paste0(output_dir, 'natl_suspected_cases_vs_tests_completed.pdf')
prop_treated_presumed = paste0(output_dir, 'MTK_prop_presumed_treated.pdf')
missing_data_presumed_cases = paste0(output_dir, 'MTK_missing_data_presumed_cases.pdf')
# ----------------------------------------------


# -----------------------------
## Load data
# -----------------------------
# imputed data at the country level
dt <- readRDS(paste0(dir_data, cod_data))
dt_dps <- readRDS(paste0(dir_data, dps_data))
dt_hz <- readRDS(paste0(dir_data, hz_data))

# original cleaned data
dt_before_MI <- data.table(read.csv(paste0(dir_data, data_before_MI), stringsAsFactors = FALSE))

# check data before imputation:
chws <- dt_before_MI[, c( "date", "dps", "health_zone", colnames(dt_before_MI)[grepl("SSC", colnames(dt_before_MI))]), with=FALSE]
presumed_cases <- dt_before_MI[, .(date, dps, health_zone, presumedMalaria_under5, presumedMalaria_5andOlder, presumedMalaria_pregnantWomen)]

check <- dt_before_MI[, .(date, dps, health_zone, newCasesMalariaMild_5andOlder, mildMalariaTreated_5andOlder,  newCasesMalariaSevere_5andOlder, severeMalariaTreated_5andOlder,
                          newCasesMalariaMild_under5, mildMalariaTreated_under5,  newCasesMalariaSevere_under5, severeMalariaTreated_under5,
                          newCasesMalariaMild_pregnantWomen, mildMalariaTreated_pregnantWomen,  newCasesMalariaSevere_pregnantWomen, severeMalariaTreated_pregnantWomen)]

check <- dt_before_MI[, .(date, dps, health_zone, ASAQused_total, ArtLum_used)]
# -----------------------------


# ----------------------------------------------
## Graphs to put together for annual report
# ----------------------------------------------
# total outputs ACTs used - whole country
dt$variable <- as.character(dt$variable)

tests_completed <- dt[indicator %in% c("smearTest", "RDT") & subpopulation != "positive", ]
tests_completed_COD <- tests_completed[, .(totTests = sum(mean)), by=c("date")] 

acts_used <- dt[indicator %in% c("ArtLum", "ASAQused") & subpopulation != "received", ]
acts_used_COD <- acts_used[, .(totActs = sum(mean)), by=c("date")] 

tests_acts_COD <- merge(tests_completed_COD, acts_used_COD, by="date")
tests_acts_COD <- melt.data.table(tests_acts_COD, id.vars="date")

g1 <- ggplot(tests_acts_COD, aes(x=date, y=value, color=variable)) + theme_bw()+
  geom_point() + geom_line() + 
  ggtitle(paste0("First-line antimalarial medications prescribed to patients \nand tests completed (national)")) +
  ylab("Count") + xlab("Date")  + scale_y_continuous() +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16), legend.title=element_blank(), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=12, vjust = 0)) +
  labs(caption= "Source: Programme National de Lutte contre le Paludisme (PNLP)") + 
  scale_color_manual(labels = c("Tests completed", "Doses prescribed"), values = c("#56B4E9", "darkblue"))
g1

g1 <- ggplot(acts_used_COD, aes(x=date, y=totActs)) + theme_bw()+
  geom_point(color="darkblue") + geom_line(color="darkblue") + 
  ggtitle(paste0("First-line antimalarial medications prescribed to patients (national)")) +
  ylab("Number of doses") + xlab("Date")  + scale_y_continuous(labels = scales :: comma) +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16), legend.title=element_blank(), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=12, vjust = 0)) 
g1

pdf(natl_doses_graph, height= 9, width= 10)
print(g1)
dev.off()
# ----------------------------------------------
# coverage national level; 2014 on

# # coverage cases treated - mtk vs others
# dt_copy <- copy(dt_hz)
# dt_copy[dps== "maniema", group:="Maniema"]
# dt_copy[dps== "tshopo", group:="Tshopo"]
# dt_copy[dps== "kinshasa", group:="Kinshasa"]
# dt_copy[!dps %in% c("maniema", "tshopo", "kinshasa"), group:="All other provinces"]

# confirmed cases treated over confirmed cases
cases <- dt[indicator %in% c("newCasesMalariaMild", "newCasesMalariaSevere"),]
cases <- cases[, .(confCases = sum(mean)), by=c("date")] 
# cases <- dcast.data.table(cases, date ~ indicator)

casesTreated <- dt[indicator %in% c("mildMalariaTreated", "severeMalariaTreated"),]
casesTreated <- casesTreated[, .(confCasesTreated = sum(mean)), by=c("date")] 
# casesTreated <- dcast.data.table(casesTreated, date ~ indicator)

casesTreated_vs_cases <- merge(cases, casesTreated, by=c("date"))
casesTreated_vs_cases <- casesTreated_vs_cases[, propCasesTreated := confCasesTreated/confCases, by= c("date")]
# casesTreated_vs_cases <- casesTreated_vs_cases[, propSimpleCasesTreated := newCasesMalariaMild/mildMalariaTreated, by= c("date")]
# casesTreated_vs_cases <- casesTreated_vs_cases[, propSevereCasesTreated := newCasesMalariaSevere/severeMalariaTreated, by= c("date")]

graphData <- casesTreated_vs_cases[, .(date, propCasesTreated)]
# graphData <- casesTreated_vs_cases[, .(date, propSimpleCasesTreated, propSevereCasesTreated)]
# graphData <- melt.data.table(graphData, id.vars="date")

g2 <- ggplot(graphData[date>= "2014-01-01"], aes(x=date, y=propCasesTreated)) + theme_bw()+
  geom_point(color="darkblue") + geom_line(color="darkblue") + 
  ggtitle(paste0("Proportion of confirmed cases treated according to national policy")) +
  ylab("Proportion of Cases Treated") + xlab("Date") + ylim(0.75, 1.0) +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_blank(), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=15, vjust = 0)) +
  labs(caption= "Source: Programme National de Lutte contre le Paludisme (PNLP)") 
g2

pdf(cases_treated_graph, height= 9, width= 10)
print(g2)
dev.off()
  
# -----------------------------
# --- missing data analysis ---
missing <- copy(dt_before_MI)
missing$date <- as.Date(missing$date)
missing <- missing[dps %in% c("maniema", "tshopo", "kinshasa"), .(date, year, dps, health_zone, newCasesMalariaMild_5andOlder, mildMalariaTreated_5andOlder,  newCasesMalariaSevere_5andOlder, 
                                                                  severeMalariaTreated_5andOlder,newCasesMalariaMild_under5, mildMalariaTreated_under5,  newCasesMalariaSevere_under5, 
                                                                  severeMalariaTreated_under5, newCasesMalariaMild_pregnantWomen, mildMalariaTreated_pregnantWomen, 
                                                                  newCasesMalariaSevere_pregnantWomen, severeMalariaTreated_pregnantWomen)]
missing <- melt.data.table(missing, id.vars = c("date", "year", "dps", "health_zone"))
missing <- missing[,  c("indicator", "subpop") := tstrsplit(variable, "_", fixed=TRUE)]
missing <- missing[date>="2014-01-01",]

missing <- missing[ , n_missing_by_dps := sum(is.na(value)), by="dps"]
missing <- missing[ , n_missing_by_dps_date := sum(is.na(value)), by=c("dps", "date")]
missing <- missing[ , n_missing_by_dps_date_indicator := sum(is.na(value)), by=c("dps", "date", "indicator")]

missing_by_dps <- unique(missing[, .(dps, n_missing_by_dps)])
missing_by_dps_date <- unique(missing[, .(dps, date, n_missing_by_dps_date)])
missing_by_dps_date_indicator <- unique(missing[, .(dps, date, indicator, n_missing_by_dps_date_indicator)])
missing_wide <- dcast.data.table(missing_by_dps_date_indicator, dps + date ~indicator, value.var = "n_missing_by_dps_date_indicator")

g_missing <- ggplot(missing_by_dps_date_indicator, aes(x=date, y=n_missing_by_dps_date_indicator, color= dps)) + theme_bw()+
  geom_point() + geom_line() + 
  ggtitle(paste0("Number of missing data points by DPS, date, and variable")) +
  ylab("Number missing") + xlab("Date") + 
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_blank(), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=12, vjust = 0)) +
  facet_wrap( ~ indicator)
g_missing

pdf(missing_data_cases_treated, height= 9, width= 12)
print(g_missing)
dev.off()
# -----------------------------

# ----------------------------------------------
# ratio of tests completed to suspected cases

sus_cases_COD <- dt[indicator=="suspectedMalaria", .(totSusCases = sum(mean)), by=c("date")] 

susCases_vs_tests <- merge(sus_cases_COD, tests_completed_COD, by=c("date"))
susCases_vs_tests <- susCases_vs_tests[, ratio := totTests/totSusCases, by= c("date")]

g3 <- ggplot(susCases_vs_tests, aes(x=date, y=ratio)) + theme_bw()+
  geom_point() + geom_line() + 
  ggtitle(paste0("Ratio of tests completed to suspected cases (national)")) +
  ylab("Ratio") + xlab("Date") + ylim(0.5, NA) +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_blank(), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=12, vjust = 0)) +
  labs(caption= "Source: Programme National de Lutte contre le Paludisme (PNLP)") 
g3

pdf(suspected_cases_vs_tests_completed, height= 9, width= 12)
print(g3)
dev.off()

# ----------------------------------------------

# ----------------------------------------------
# proportion of cases treated that are presumed (just use 2017 and later)

# presumed cases + confirmed cases treated over confirmed cases plus
pres_cases <- dt_copy[indicator %in% c("presumedMalaria"),]
pres_cases <- pres_cases[, .(presCases = sum(mean)), by=c("date", "group")]

all_cases_treated <- dt_copy[indicator %in% c("mildMalariaTreated", "severeMalariaTreated", "presumedMalaria"),]
all_cases_treated <- all_cases_treated[, .(allCasesTreated = sum(mean)), by=c("date", "group")]

prop_tx_presumed <- merge(pres_cases, all_cases_treated, by=c("date", "group"))
prop_tx_presumed <- prop_tx_presumed[, prop := presCases/allCasesTreated, by= c("date", "group")]

g4 <- ggplot(prop_tx_presumed[date>= "2017-01-01"], aes(x=date, y=prop, color= group)) + theme_bw()+
  geom_point() + geom_line() + 
  ggtitle(paste0("Proportion of cases treated that were presumed cases")) +
  ylab("Proportion") + xlab("Date") + 
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_blank(), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=12, vjust = 0)) +
  labs(caption= "Source: Programme National de Lutte contre le Paludisme (PNLP)") 
g4

pdf(prop_treated_presumed, height= 9, width= 12)
print(g4)
dev.off()

missing <- copy(dt_before_MI)
missing$date <- as.Date(missing$date)
missing <- missing[dps %in% c("maniema", "tshopo", "kinshasa"), .(date, year, dps, health_zone, presumedMalaria_5andOlder, presumedMalaria_under5, 
                                                                  presumedMalaria_pregnantWomen)]

missing <- melt.data.table(missing, id.vars = c("date", "year", "dps", "health_zone"))
missing <- missing[date>="2017-01-01",]

missing <- missing[ , n_missing_by_dps := sum(is.na(value)), by="dps"]
missing <- missing[ , n_missing_by_dps_date := sum(is.na(value)), by=c("dps", "date")]
missing <- missing[ , n_missing_by_dps_date_indicator := sum(is.na(value)), by=c("dps", "date", "variable")]

missing_by_dps <- unique(missing[, .(dps, n_missing_by_dps)])
missing_by_dps_date <- unique(missing[, .(dps, date, n_missing_by_dps_date)])
missing_by_dps_date_indicator <- unique(missing[, .(dps, date, variable, n_missing_by_dps_date_indicator)])
missing_wide <- dcast.data.table(missing_by_dps_date_indicator, dps + date ~indicator, value.var = "n_missing_by_dps_date_indicator")

g_missing <- ggplot(missing_by_dps_date, aes(x=date, y=n_missing_by_dps_date, color= dps)) + theme_bw()+
  geom_point() + geom_line() + 
  ggtitle(paste0("Number of missing data points by DPS and date for presumed malaria cases")) +
  ylab("Number missing") + xlab("Date") + 
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_blank(), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=12, vjust = 0)) 
g_missing




pdf(missing_data_presumed_cases, height= 9, width= 12)
print(g_missing)
dev.off()
# ----------------------------------------------

# ----------------------------------------------
# # Put graphs together
# graphs = arrangeGrob(g1, g2, nrow=2, ncol=2)
# 
# # Save graph
# pdf(graphFile, height=10, width=15)
# grid.newpage()
# grid.draw(graphs)
# dev.off()

# ----------------------------------------------