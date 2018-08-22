# ----------------------------------------------
# Audrey Batzel
#
# 8/15/18
# 
# Compare GF provinces to non-GF provinces
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
# --------------------  


# ----------------------------------------------
# Overview - Files and Directories

# data directory
# when run on Unix, data directory needs to be set to /home/j (to run on the cluster), so set this here:
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir_data = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/')
dir_pop = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/worldpop_data/')

# input files
dps_data <- "imputedData_run2_condensed_dps.rds"
pop_data <- '2015_pop_estimates_by_dps(aggregated raster).csv'
pop_data10 <- '2010_pop_estimates_by_dps(aggregated raster).csv'

# output files
merged_pops <- "2015_and_2010_pop_estimates.csv"
output_dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_analysis/')
compare_acts <- "compare_gf_provinces_acts.pdf"
compare_acts_stockouts <- "compare_gf_provinces_stockouts.pdf"
compare_itns <- "compare_gf_provinces_itns.pdf"
compare_acts_by_age <- "compare_gf_provinces_acts_by_age.pdf"
# ----------------------------------------------


# -----------------------------
# Load data

# load the imputed data at DPS level
dt <- readRDS(paste0(dir_data, dps_data))
dt$year <- year(dt$date)
dt <- dt[year>=2015 & dps != "0",]

# load the world pop estimates
pop <- read.csv(paste0(dir_pop, pop_data))
pop <- as.data.table(pop)
pop$X <- NULL

pop10 <- read.csv(paste0(dir_pop, pop_data10))
pop10 <- as.data.table(pop10)
pop10$X <- NULL
setnames(pop10, "pop", "pop10")
# -----------------------------


# ----------------------------------------------
# # Clean dps names in pop data
# setnames(pop, "province", "dps")
# pop[dps=="�???quateur", dps:= "equateur"]
# pop[dps=="Bas-Uélé", dps:= "bas-uele"]
# pop[dps=="Haut-Uélé", dps:= "haut-uele"]
# pop[dps=="Maï-Ndombe", dps:= "mai-ndombe"]
# pop$dps <- gsub("Kasaï", "kasai", pop$dps)
# pop$dps <- gsub("�???", "e", pop$dps)
# pop$dps <- tolower(pop$dps)

# setnames(pop10, "province", "dps")
# pop10[dps=="�???quateur", dps:= "equateur"]
# pop10[dps=="Bas-Uélé", dps:= "bas-uele"]
# pop10[dps=="Haut-Uélé", dps:= "haut-uele"]
# pop10[dps=="Maï-Ndombe", dps:= "mai-ndombe"]
# pop10$dps <- gsub("Kasaï", "kasai", pop10$dps)
# pop10$dps <- gsub("�???", "e", pop10$dps)
# pop10$dps <- tolower(pop10$dps)

# # resave pop so that it has these names preserved, then the above steps aren't necessary
# write.csv(pop, paste0(dir_pop, pop_data))

# Merge pop with pop10
pop_15_10 <- merge(pop10, pop, by="dps")
setnames( pop_15_10, "pop", "pop15")

# # Save pops merged together
# write.csv( pop_15_10, paste0(dir_pop, merged_pops))

dt$dps <- gsub(" ", "-", dt$dps)
dt[dps=="bas-congo", dps:= "kongo-central"]
# ----------------------------------------------


# ----------------------------------------------
# Merge pop estimates with data
dt <- merge(dt, pop, by="dps", all=TRUE)
# ----------------------------------------------


# ----------------------------------------------
# Divide by Worldpop population estimates
dt[, valuePerCapita := mean/pop]
# ----------------------------------------------


# ----------------------------------------------
# Mark GF provinces vs non-GF provinces
all_dps <- unique(dt$dps)
non_gf_dps <- c("sankuru", "sud-kivu", "tanganyika", "kasai", "kasai-central", "kasai-oriental", "lomami", "haut-lomami", "lualaba", "haut-katanga")
gf_dps <- all_dps[!all_dps %in% non_gf_dps]

dt[dps %in% gf_dps, gf := "yes"]
dt[dps %in% non_gf_dps, gf := "no"]
# ----------------------------------------------


# ----------------------------------------------
# Compare ACTs received and used between GF and non-GF per capita

# ACTs received

acts_rec <- dt[indicator %in% c("ArtLum", "ASAQreceived") & subpopulation!="used", ]

acts_rec <- acts_rec[, .(totActsRec = sum(valuePerCapita)), by=c("dps", "year", "date", "gf")]

acts_rec <- acts_rec[, totActsRecPer100k := totActsRec * 100000]

acts_rec_compare <- acts_rec[, .(avgGFvsNotGF = mean(totActsRecPer100k)), by= c("year", "date", "gf")]

g <- ggplot(acts_rec_compare[year == 2015, ], aes(x=date, y=avgGFvsNotGF, color = gf)) + theme_bw()+
  geom_point() + geom_line() +
  ggtitle(paste0("ACT doses per capita distributed to health facilities 2015")) +
  ylab("Doses per 100,000") + xlab("Date")  + scale_y_continuous()
print(g)

g <- ggplot(acts_rec_compare[ ], aes(x=date, y=avgGFvsNotGF, color = gf)) + theme_bw()+
  geom_point() + geom_line() +
  ggtitle(paste0("ACT doses per capita distributed to health facilities (*based on 2015 population estimates)")) +
  ylab("Doses per 100,000") + xlab("Date")  + scale_y_continuous()
print(g)


# ACTs used

acts_use <- dt[indicator %in% c("ArtLum", "ASAQused") & subpopulation!="received", ]

acts_use <- acts_use[, .(totActsUse = sum(valuePerCapita)), by=c("dps", "year", "date", "gf")]

acts_use <- acts_use[, totActsUsePer100k := totActsUse * 100000]

acts_use_compare <- acts_use[, .(avgGFvsNotGF = mean(totActsUsePer100k)), by= c("year", "date", "gf")]

g <- ggplot(acts_use_compare[year == 2015, ], aes(x=date, y=avgGFvsNotGF, color = gf)) + theme_bw()+
  geom_point() + geom_line() +
  ggtitle(paste0("ACT doses per capita distributed to patients 2015")) +
  ylab("Doses per 100,000") + xlab("Date")  + scale_y_continuous()
print(g)

g <- ggplot(acts_use_compare[ ], aes(x=date, y=avgGFvsNotGF, color = gf)) + theme_bw()+
  geom_point() + geom_line() +
  ggtitle(paste0("ACT doses per capita distributed to patients (*based on 2015 population estimates)")) +
  ylab("Doses per 100,000") + xlab("Date")  + scale_y_continuous()
print(g)

pdf( paste0(output_dir, compare_acts), height=9, width=11 )
dev.off()

# ACTs received by age group
acts_rec_by_age <- dt[indicator %in% c("ASAQreceived"), ]
acts_rec_by_age <- acts_rec_by_age[, valuePer100k := valuePerCapita * 100000]
acts_rec_by_age <- acts_rec_by_age[, .(avgGFvsNotGF = mean(valuePer100k)), by= c("year", "date", "gf", "indicator", "subpopulation")]

g <- ggplot(acts_rec_by_age[ ], aes(x=date, y=avgGFvsNotGF, color = gf)) + theme_bw()+
  geom_point() + geom_line() +
  ggtitle(paste0("ASAQ doses per capita distributed to health facilities")) +
  facet_wrap( ~subpopulation ) +
  ylab("Doses per 100,000 people") + xlab("Date")  + scale_y_continuous()
print(g)

# ACTs used by age group
acts_use_by_age <- dt[indicator %in% c("ASAQused"), ]
acts_use_by_age <- acts_use_by_age[, valuePer100k := valuePerCapita * 100000]
acts_use_by_age <- acts_use_by_age[, .(avgGFvsNotGF = mean(valuePer100k)), by= c("year", "date", "gf", "indicator", "subpopulation")]

g <- ggplot(acts_use_by_age[ ], aes(x=date, y=avgGFvsNotGF, color = gf)) + theme_bw()+
  geom_point() + geom_line() +
  ggtitle(paste0("ASAQ doses per capita distributed to patients")) +
  facet_wrap( ~subpopulation ) +
  ylab("Doses per 100,000 people") + xlab("Date")  + scale_y_continuous()
print(g)

pdf( paste0(output_dir, compare_acts_by_age), height=9, width=11)
dev.off()
# ----------------------------------------------


# ----------------------------------------------
# Compare ACT stockouts between GF and non-GF per capita

act_stockouts <- dt[indicator %in% c("stockOutartLum", "stockOutASAQ", "healthFacilities"), ]

act_stockouts$lower <- NULL
act_stockouts$upper <- NULL
act_stockouts$valuePerCapita <-NULL
act_stockouts$province <- NULL
setnames(act_stockouts, "mean", "value")

act_stockouts[indicator=="stockOutartLum", subpopulation:="none"]

act_stockouts$variable <- paste(act_stockouts$indicator, act_stockouts$subpopulation, sep = "_")
act_stockouts$indicator <- NULL
act_stockouts$subpopulation <- NULL

dt_wide <- dcast(act_stockouts, dps + year + date + gf +pop ~ variable, value.var= "value")
dt_wide <- as.data.table(dt_wide)

dt_wide <- dt_wide[, totASAQstockouts:= stockOutASAQ_14yrsAndOlder + stockOutASAQ_1to5yrs + stockOutASAQ_2to11mos + stockOutASAQ_6to13yrs]
dt_wide <- dt_wide[, totASAQstockoutDays := totASAQstockouts/healthFacilities_total]

dt_wide <- dt_wide[, artLumStockoutDays := stockOutartLum_none/healthFacilities_total]

dt_wide <- dt_wide[, ASAQinjStockoutDays := stockOutASAQ_inj/healthFacilities_total]

dt_wide <- dt_wide[, totASAQstockoutsIncInj:= stockOutASAQ_14yrsAndOlder + stockOutASAQ_1to5yrs + stockOutASAQ_2to11mos + stockOutASAQ_6to13yrs + stockOutASAQ_inj]
dt_wide <- dt_wide[, ASAQstockoutDaysIncInj := totASAQstockoutsIncInj/healthFacilities_total]

dt_wide <- dt_wide[, totACTstockouts := stockOutASAQ_14yrsAndOlder + stockOutASAQ_1to5yrs + stockOutASAQ_2to11mos + stockOutASAQ_6to13yrs + stockOutartLum_none]
dt_wide <- dt_wide[, totACTstockoutDays := totACTstockouts/healthFacilities_total]

act_stockouts_wide <- dt_wide[, c('dps', 'year', 'date', 'gf', 'pop', 'ASAQstockoutDaysIncInj', 'totASAQstockoutDays', 'artLumStockoutDays', 'ASAQinjStockoutDays', 'totACTstockoutDays')]
act_stockouts_long <- melt(act_stockouts_wide, id.vars= c('dps', 'year', 'date', 'pop', 'gf'))
act_stockouts_long <- act_stockouts_long[, valuePerCapita := value/pop]

compare_act_stockouts <- act_stockouts_long[, .(avgGFvsNotGF = mean(value)), by= c("year", "date", "gf", "variable")]

g <- ggplot(compare_act_stockouts[variable == "totACTstockoutDays", ], aes(x=date, y=avgGFvsNotGF, color = gf)) + theme_bw() +
  geom_point() + geom_line() +
  ggtitle("Stock-outs of ACTs in GF vs. non-GF provinces (AL and ASAQ not including ASAQ injectables)") +
  ylab("Average days of stock-outs per facility") + xlab("Date") 
print(g)

# variable names for labeling
stockout_names <- c(
  `ASAQstockoutDaysIncInj`= "Stockouts of ASAQ including injectables",
  `ASAQstockoutDays`= "Stockouts of ASAQ pills",
  `artLumStockoutDays`= "Stockouts of AL",
  `ASAQinjStockoutDays`= "Stockouts of ASAQ injectables",
  `totASAQstockoutDays`= "Stockouts of ASAQ pills" )

g <- ggplot(compare_act_stockouts[variable != "ASAQstockoutDaysIncInj" & variable != "totACTstockoutDays", ], aes(x=date, y=avgGFvsNotGF, color = gf)) + theme_bw() +
  geom_point() + geom_line() +
  facet_wrap( ~ variable, labeller=as_labeller(stockout_names)) +
  ggtitle("Stock-outs of ACTs in GF vs. non-GF provinces by drug type") +
  ylab("Average days of stock-outs per facility") + xlab("Date") 
print(g)

pdf( paste0(output_dir, compare_acts_stockouts), height=9, width=11 )
dev.off()
# ----------------------------------------------


# ----------------------------------------------
# Compare ITNs received and distributed between GF and non-GF per capita

# ITNs received

itns_rec <- dt[indicator == "ITN" & subpopulation == "received"]

itns_rec <- itns_rec[, .(totITNsRec = sum(valuePerCapita)), by=c("dps", "year", "date", "gf")]

itns_rec <- itns_rec[, totITNsRecPer100k := totITNsRec * 100000]

itns_rec_compare <- itns_rec[, .(avgGFvsNotGF = mean(totITNsRecPer100k)), by= c("year", "date", "gf")]

g <- ggplot(itns_rec_compare[year == 2015, ], aes(x=date, y=avgGFvsNotGF, color = gf)) + theme_bw()+
  geom_point() + geom_line() +
  ggtitle(paste0("ITNs received")) +
  ylab("ITNs per 100,000 people") + xlab("Date")  + scale_y_continuous()
print(g)

g <- ggplot(itns_rec_compare[], aes(x=date, y=avgGFvsNotGF, color = gf)) + theme_bw()+
  geom_point() + geom_line() +
  ggtitle(paste0("ITNs received")) +
  ylab("ITNs per 100,000 people") + xlab("Date")  + scale_y_continuous()
print(g)


# ITNs distributed

itns_dist <- dt[indicator == "ITN" & subpopulation != "received"]

itns_dist <- itns_dist[, .(totITNsDist = sum(valuePerCapita)), by=c("dps", "year", "date", "gf")]

itns_dist <- itns_dist[, totITNsDistPer100k := totITNsDist * 100000]

itns_dist_compare <- itns_dist[, .(avgGFvsNotGF = mean(totITNsDistPer100k)), by= c("year", "date", "gf")]

g <- ggplot(itns_dist_compare[year == 2015, ], aes(x=date, y=avgGFvsNotGF, color = gf)) + theme_bw()+
  geom_point() + geom_line() +
  ggtitle(paste0("ITNs distributed")) +
  ylab("ITNs per 100,000 people") + xlab("Date")  + scale_y_continuous()
print(g)

g <- ggplot(itns_dist_compare[], aes(x=date, y=avgGFvsNotGF, color = gf)) + theme_bw()+
  geom_point() + geom_line() +
  ggtitle(paste0("ITNs distributed")) +
  ylab("ITNs per 100,000 people") + xlab("Date")  + scale_y_continuous()
print(g)

pdf( paste0(output_dir, compare_itns), height=9, width=11 )
dev.off()
# ----------------------------------------------



