# Audrey Batzel 
# 6/24/19
#
# Graph for drc malaria deep dive
setwd('C:/local/gf/')
# ----------------------------------------------

# --------------------
# Set up R / install packages
# --------------------
rm(list=ls())
library(data.table)
library(ggplot2)
library(lubridate)
# --------------------  

# ----------------------------------------------
# Overview - Files and Directories
# ----------------------------------------------
# data directory
# when run on Unix, data directory needs to be set to /home/j (to run on the cluster), so set this here:
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/')

# input files
combined_data = paste0(dir, "impact_evaluation/cod/prepped_data/base_pnlp_sigl_combined_data_hz_level.rds")
before_MI_pnlp = paste0(dir, "outcome_measurement/cod/prepped_data/PNLP/PNLP_dt_forMI_updated_6_10_19.rds")

# file listing health zones
hzFile = paste0(dir, '/outcome_measurement/cod/prepped_data/Unicef HZ extraction_BH.csv')

# output files
outFile = "impact_evaluation/cod/visualizations/RDTsCompleted_overSuspectedCases_excludingPartialDPS.pdf"
outFile2 = "impact_evaluation/cod/visualizations/RDTsCompleted_overFeverCases_sscSpecific_excludingPartialDPS.pdf"
  
# functions
source('./core/standardizeHZNames.R')
source('./core/standardizeDPSNames.R')
# ----------------------------------------------

# ----------------------------------------------
# read in data
# ----------------------------------------------
dt = readRDS(combined_data)
pnlp = readRDS(before_MI_pnlp)
hzList = fread(hzFile)
# ----------------------------------------------

# ----------------------------------------------
# subset data to RDT testing data and cases
# ----------------------------------------------
dt[ element == "B 10.2 Cas de fièvre dans une zone à risque du paludisme", indicator := "SSCfevers"]

dt = dt[indicator %in% c("suspectedMalaria", "RDT", "RDT_completed", "SSCRDT", "SSCfevers")]
dt = dt[!subpopulation %in% c("positive", "received")]
dt[indicator == "RDT" & subpopulation == "completed", indicator := "RDT_completed"]
dt[element == "SSCRDT_completed", indicator := "SSCRDT_completed"]

dt = dt[, .(value = sum(value, na.rm = TRUE)), by = .(dps, health_zone, date, year, indicator)]

# cast wide
dt = dcast.data.table(dt, dps + health_zone + date + year ~ indicator)
# ----------------------------------------------

# ----------------------------------------------
# merge on hz list to idenitfy hz's where intervention took place
# ----------------------------------------------
# subset columns
hzList = hzList[,'health_zone']

# check for health zones in the list that aren't in the data 
hzList$health_zone[!hzList$health_zone %in% dt$health_zone]

# identify "intervention" health zones in the data
hzList[, intervention:=1]
dt = merge(dt, hzList, by='health_zone', all.x=TRUE)
dt[is.na(intervention), intervention:=0]
dt[, intervention_label:=ifelse(intervention==1, '2. Intervention', '1. Control')]

# remake figures - drop hz's from control group in DPS (will have complete intervention group but not complete control group)
# take out tshopo, kongo-central, bas-uele, kinshasa, ituri
dt[ dps %in% c('tshopo', 'kongo-central', 'bas-uele', 'kinshasa', 'ituri') & intervention == 0, intervention := NA]
dt = dt[!is.na(intervention)]
# ----------------------------------------------

# ----------------------------------------------
# sum to national level (but divided by intervention/control groups)
# ----------------------------------------------
# sum RDT and suspectedMalaria by date and intervention for graphing
sd_cols = c("SSCRDT_completed", "SSCfevers","RDT_completed", "suspectedMalaria")
dt_natl = dt[, lapply(.SD, sum, na.rm = TRUE), by = c("date", "year", "intervention", "intervention_label"), .SDcols = sd_cols]
# calculate RDTs / suspected cases rates at SSC and all facilities
dt_natl[ , SSCRDT_over_fevers := SSCRDT_completed / SSCfevers]
dt_natl[ , RDT_over_suspCases := RDT_completed / suspectedMalaria]
# ----------------------------------------------

# ----------------------------------------------
# graphing
# ----------------------------------------------
# note suspected cases only in PNLP from 2014, on
RDT_over_susp= ggplot(dt_natl[ year >= 2014, ], aes(x = date, y = RDT_over_suspCases, color = intervention_label)) + geom_point() + geom_line() +
  ggtitle("RDTs completed per suspected case of malaria (all ages, all facilities)") + xlab("Date (month and year)") + ylab("RDTs completed per suspected case") +
  theme_bw() + labs(caption = "Source: Programme National de Lutte contre le Paludisme (PNLP) 2015-2017; DHIS2 2018-present") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16), legend.text =element_text(size=14),
        plot.title = element_text(size=18), plot.caption = element_text(size=14)) + theme(legend.title=element_blank())
RDT_over_susp

pdf(paste0(dir, outFile), height = 9, width = 11)
print(RDT_over_susp)
dev.off()

# note SSC data only in pnlp from 2015, on
RDT_over_fevers_ssc= ggplot(dt_natl[ year >= 2015, ], aes(x = date, y = SSCRDT_over_fevers, color = intervention_label)) + geom_point() + geom_line() + 
  ggtitle("RDTs completed per case of fever at SSC (all ages)") + xlab("Date (month and year)") + ylab("RDTs completed per case of fever") +
  theme_bw() + labs(caption = "Source: Programme National de Lutte contre le Paludisme (PNLP) 2015-2017; DHIS2 2018-present") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16), legend.text =element_text(size=14),
        plot.title = element_text(size=18), plot.caption = element_text(size=14)) + theme(legend.title=element_blank())
RDT_over_fevers_ssc

pdf(paste0(dir, outFile2), height = 9, width = 11)
print(RDT_over_fevers_ssc)
dev.off()

# rdt_0s = dt[RDT_completed == 0, .N, by = "date"]
# qplot(date, N, data = rdt_0s, geom = "line") 
# ----------------------------------------------

# ----------------------------------------------
# DiD analysis (from David's code ssc_impact.R)
# ----------------------------------------------
# identify before/after
dt[, period:=ifelse(year<2017, 0, 1)]
dt[, period_label:=ifelse(period==1, '2. After 2017', '1. Before 2017')]
dt[, rdt_over_susp_rate := RDT_completed / suspectedMalaria]
dt = dt[!is.na(rdt_over_susp_rate)]
dt = dt[ suspectedMalaria != 0,]

# take averages for graph
means = dt[, .(rdt_over_susp_rate=mean(rdt_over_susp_rate), 
               lower_pctle=quantile(rdt_over_susp_rate, 0.025), 
               upper_pctle=quantile(rdt_over_susp_rate, 0.975)), 
           by=c('period_label','intervention_label', 'period','intervention')]

# Run analysis
# offset log transform
offset = quantile(dt[rdt_over_susp_rate>0]$rdt_over_susp_rate, .01)
dt[, log_rdt_over_susp_rate:=log(rdt_over_susp_rate+offset)]

# difference in differences with OLS
lmFit = lm(formula = rdt_over_susp_rate~intervention*period, data = dt)
summary(lmFit)

# difference in differences with OLS on log-rdts used rate
lmFitLog = lm(formula = log_rdt_over_susp_rate~intervention*period, data = dt)
summary(lmFit)

# difference in differences with health zone random effect for correlated errors
lmeFit = lmer(formula = log_rdt_over_susp_rate~intervention*period + (1|health_zone), data = dt)
summary(lmeFit)

# predict from lmFit for graph
means = cbind(means, (predict(lmFit, interval='confidence', newdata=means)))
# ----------------------------------------------

# ----------------------------------------------
# DiD Graph
# ----------------------------------------------
# traditional DiD graph
ggplot(means, aes(y=rdt_over_susp_rate, ymin=lower_pctle, ymax=upper_pctle, x=period_label, color=intervention_label)) + 
  geom_pointrange(position=position_dodge(width=.2), size=1) + 
  geom_line(aes(group=intervention_label), position=position_dodge(width=.2), size=1) + 
  labs(title='Comparison of Health Zones', 
       subtitle='With and Without Full Package of iCCM Services', 
       y='RDTs used per suspected case', x='Period', color='Health Zones', 
       caption='Points and ranges show mean, \n 2.5th and 97.5th percentiles of health zones') + 
  theme_bw()

# traditional DiD graph with model estimates on it
ggplot(means, aes(y=fit, ymin=lwr, ymax=upr, x=period_label, color=intervention_label)) + 
  geom_pointrange(position=position_dodge(width=.2), size=.65) + 
  geom_line(aes(group=intervention_label), position=position_dodge(width=.2), size=1) + 
  labs(y='RDTs used per suspected case', x='Period', color='Health Zones', 
       caption='Points and ranges show midpoint, \n upper and lower 95% confidence interval from model') + 
  theme_bw()
# ----------------------------------------------

# # first version of this analysis:
# # ----------------------------------------------
# # subset data
# # ----------------------------------------------
# # load in combined data for SNIS (2018 on)
# dt = dt[indicator %in% c("suspectedMalaria", "RDT_completed") & data_set == "base" & subpopulation == "under5", ]
# 
# # Load pre-MI PNLP data for 2017 and earlier
# idVars = c("dps", "health_zone", "date", "year", "id", "donor", "operational_support_partner", "population")
# pnlp = melt.data.table(pnlp, id.vars = idVars, variable.factor = FALSE)
# 
# #subset variables
# inds = unique(pnlp$variable)
# keep_inds = inds[grepl("suspected", inds, ignore.case = TRUE)]
# keep_inds = c(keep_inds, inds[grepl("rdt", inds, ignore.case = TRUE)])
# keep_inds = keep_inds[!grepl("stock|received|ssc|5andOlder|pregnant|positive", keep_inds, ignore.case = TRUE)]
# 
# pnlp = pnlp[ variable %in% keep_inds ]
# 
# # remove variables where all values are missing for a given year
# all_na = pnlp[ , .(all_missing = all(is.na(value))), by = c("year", "variable")]
# pnlp = merge(pnlp, all_na, all = TRUE, by = c("year", "variable"))
# 
# pnlp = pnlp[ all_missing == FALSE, ]
# 
# pnlp = pnlp[year >= 2015]
# 
# pnlp[, dps := standardizeDPSNames(dps)]
# pnlp[, health_zone := standardizeHZNames(health_zone)]
# pnlp[health_zone == "kashobwe", health_zone := "kasenga" ]
# pnlp[health_zone == "alimbongo", health_zone := "lubero"]
# pnlp[health_zone == "kibirizi", health_zone := "rutshuru"]
# pnlp[health_zone == "mabalako", health_zone := "beni"]
# pnlp[health_zone == "kamango", health_zone := "mutwanga"]
# pnlp = pnlp[, .(value = sum(value, na.rm = FALSE)), by = .(dps, health_zone, date, year, variable)]
# 
# # monthly (natl) completeness for pnlp
# pnlp[, num_missing := sum(is.na(value)), by = c("date", "variable")]
# pnlp[, total_values := .N, by = c("date", "variable")]
# comp = unique(pnlp[, .(date, variable, num_missing, total_values)])
# comp[ , percent_missing :=((num_missing / total_values) *100)]
# 
# completeness = ggplot(comp, aes(x = date, y = percent_missing)) + geom_point() + geom_line() + 
#   ggtitle("Percent of health zones missing in DRC PNLP data by date") + xlab("Date (month and year)") + ylab("Percent missing") +
#   theme_bw() + labs(caption = "Source: Programme National de Lutte contre le Paludisme (PNLP)") +
#   theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16), legend.text =element_text(size=14),
#         plot.title = element_text(size=18), plot.caption = element_text(size=14)) + facet_wrap( ~ variable) 
# completeness
# 
# pnlp[, c("indicator", "subpopulation") := tstrsplit(variable, "_", fixed=TRUE)]
# pnlp[indicator == "RDT", indicator:= "RDT_completed"]
# 
# pnlp[, c('subpopulation', 'num_missing', 'total_values', 'variable'):=NULL]
# dt[, c('subpopulation', 'element', 'element_eng', 'data_set'):=NULL]
# # ----------------------------------------------
# 
# # ----------------------------------------------
# # combine data sources
# # ----------------------------------------------
# dt = rbind(pnlp, dt)
# dt = dcast.data.table(dt, dps + health_zone + date + year ~ indicator)
# comp2 = copy(dt)
# comp2[, total := .N, by = c("date")]
# comp2[ (is.na(RDT_completed) & !is.na(suspectedMalaria)) | (!is.na(RDT_completed) & is.na(suspectedMalaria)) , is_missing := 1 ]
# comp2 = comp2[, .(num_missing = sum(is_missing, na.rm =TRUE)), by = c("date", "total")]
# 
# comp2[ , percent_missing := ((num_missing / total) *100)]
# 
# completeness2 = ggplot(comp2, aes(x = date, y = percent_missing)) + geom_point() + geom_line() + 
#   ggtitle("Percent of health zones missing in DRC PNLP data by date") + xlab("Date (month and year)") + ylab("Percent missing") +
#   theme_bw() + labs(caption = "Source: Programme National de Lutte contre le Paludisme (PNLP)") +
#   theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16), legend.text =element_text(size=14),
#         plot.title = element_text(size=18), plot.caption = element_text(size=14)) 
# completeness2
# # ----------------------------------------------
# 
# # ----------------------------------------------
# # merge in intervention categories
# # ----------------------------------------------
# # subset columns
# hzList = hzList[,'health_zone']
# 
# # check for health zones in the list that aren't in the data 
# hzList$health_zone[!hzList$health_zone %in% dt$health_zone]
# 
# # identify "intervention" health zones in the data
# hzList[, intervention:=1]
# dt = merge(dt, hzList, by='health_zone', all.x=TRUE)
# dt[is.na(intervention), intervention:=0]
# dt[, intervention_label:=ifelse(intervention==1, '2. Intervention', '1. Control')]
# 
# # sum RDT and suspectedMalaria by date and intervention
# sd_cols = c("RDT_completed", "suspectedMalaria")
# dt_natl = dt[!(is.na(RDT_completed) & !is.na(suspectedMalaria)), ]
# dt_natl = dt_natl[, lapply(.SD, sum, na.rm = TRUE), by = c("date", "intervention", "intervention_label"), .SDcols = sd_cols]
# 
# dt_natl[ , RDT_over_susp := RDT_completed / suspectedMalaria]
# RDT_over_susp= ggplot(dt_natl, aes(x = date, y = RDT_over_susp, color = intervention_label)) + geom_point() + geom_line() + 
#   ggtitle("RDTs completed per suspected case of malaria in children under 5") + xlab("Date (month and year)") + ylab("RDTs completed per suspected case") +
#   theme_bw() + labs(caption = "Source: Programme National de Lutte contre le Paludisme (PNLP) 2015-2017; DHIS2 2018-present") +
#   theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16), legend.text =element_text(size=14),
#         plot.title = element_text(size=18), plot.caption = element_text(size=14)) + theme(legend.title=element_blank())
# RDT_over_susp
# # ----------------------------------------------
