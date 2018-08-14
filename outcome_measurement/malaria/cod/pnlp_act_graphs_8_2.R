
# ----------------------------------------------
# Audrey Batzel
#
# 8/2/18

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
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/')
imputed_data_low_tol <- 'imputedData_forGraphing_run2.rds' # hz level calculations


output_dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_analysis/')
maniema_act_stockouts_graphs <- 'maniema_act_stockouts_by_hz_2015to2017.pdf'
mtk_acts_received_graphs <- "mtk_acts_received_by_hz_2015to2017.pdf"
mtk_acts_received_graphs_byAge <- "mtk_acts_received_by_age_by_hz_2015to2017.pdf"
mtk_acts_received_by_dps <- "mtk_acts_received_by_dps_2015to2017.pdf"
mtk_acts_used_by_dps <- "mtk_acts_used_by_dps_2015to2017.pdf"
# ----------------------------------------------

# ----------------------------------------------
# read in imputed data (tol 0.001) that has been condensed down to mean and variance across the 50 imputations (at hz level)
dt <- readRDS(paste0(dir, imputed_data_low_tol))

# there are some (all?) cases where the original value was 0, and it was set to a low percentile value instead of 0 for log transformation but not
# changed back to 0, so set all of those cases to be 0.
dt[value==0, mean:=0]

dt<- dt[dps != "0"]

dt$year <- year(dt$date)

dt <- dt[year>=2015, ]

dtOrig <- copy(dt)
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# subset to MTK
MTK <- c("maniema", "tshopo", "kinshasa")
dt_MTK <- dtOrig[dps %in% MTK,]
dt_maniema <- dtOrig[dps=="maniema",]
dt_tshopo <- dtOrig[dps=="tshopo",]
dt_kinshasa <- dtOrig[dps=="kinshasa",]

# will loop through health zones so get health zones names
hz_vector_m <- unique(dt_maniema$health_zone)
hz_vector_t <- unique(dt_tshopo$health_zone)
hz_vector_k <- unique(dt_kinshasa$health_zone)
hz_vector_mtk <- unique(dt_MTK$health_zone)
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# variable names for labeling
stockout_names <- c(
  `ASAQstockoutDaysIncInj`= "Stockouts of ASAQ including injectables",
  `ASAQstockoutDays`= "Stockouts of ASAQ pills",
  `artLumStockoutDays`= "Stockouts of AL",
  `ASAQinjStockoutDays`= "Stockouts of ASAQ injectables")

act_names <- c(
  `totASAQ` = 'ASAQ',
  `ArtLum_received` = 'AL'
)

# function to capitalize names
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
dtACTStockouts_maniema <- dt_maniema[indicator %in% c("stockOutartLum", "stockOutASAQ", "healthFacilities"), ]

# dtACTStockouts_maniema <- dtACTStockouts_maniema[indicator %in% c("stockOutartLum", "stockOutASAQ"), totStockoutsHZ := sum(mean), by=c("date", "health_zone")]
# dtACTStockouts_maniema[subpopulation %in% c("14yrsAndOlder", "1to5yrs", "2to11mos", "6to13yrs"), totASAQstockouts := sum(mean), by=c("date", "health_zone")]
# dtACTStockouts_maniema <- dtACTStockouts_maniema[, ASAQstockoutDays := (totASAQstockouts/ indicator$healthFacilities_total), by=c("date", "health_zone")]

dt <- dtACTStockouts_maniema[, c('dps', 'health_zone', 'date', 'mean', 'variable')]
dt_wide <- dcast(dt, dps + health_zone + date ~ variable, value.var= "mean")
dt_wide <- as.data.table(dt_wide)
dt_wide <- dt_wide[, totASAQstockouts:= stockOutASAQ_14yrsAndOlder + stockOutASAQ_1to5yrs + stockOutASAQ_2to11mos + stockOutASAQ_6to13yrs]
dt_wide <- dt_wide[, ASAQstockoutDays := totASAQstockouts/healthFacilities_total]
dt_wide <- dt_wide[, artLumStockoutDays := stockOutartLum/healthFacilities_total]
dt_wide <- dt_wide[, ASAQinjStockoutDays := stockOutASAQ_inj/healthFacilities_total]
dt_wide <- dt_wide[, totASAQstockoutsIncInj:= stockOutASAQ_14yrsAndOlder + stockOutASAQ_1to5yrs + stockOutASAQ_2to11mos + stockOutASAQ_6to13yrs + stockOutASAQ_inj]
dt_wide <- dt_wide[, ASAQstockoutDaysIncInj := totASAQstockoutsIncInj/healthFacilities_total]

dt_wide <- dt_wide[, c('dps', 'health_zone', 'date', 'ASAQstockoutDaysIncInj', 'ASAQstockoutDays', 'artLumStockoutDays', 'ASAQinjStockoutDays')]
dt_graph <- melt(dt_wide, id.vars= c('dps', 'health_zone', 'date'))
dt_graph$year <- year(dt_graph$date)
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
pdf( paste0(output_dir, maniema_act_stockouts_graphs), height=6, width=12)
for(h in hz_vector_m){
  g <- ggplot(dt_graph[variable != "ASAQstockoutDaysIncInj" & health_zone==h, ], aes(x=date, y=value, color = variable)) + theme_bw()+
    geom_point() + geom_line() +
    facet_wrap( ~ variable, scales="free_y", labeller=as_labeller(stockout_names)) +
    ggtitle(paste0("Stock-outs of ACTs in ", simpleCap(h), " (Maniema)")) + guides(color=FALSE) +
    ylab("Days of stock-outs per facility") + xlab("Date") 
  
  print(g)
}
dev.off()
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# calculate number of imputed values of stockouts of asaq inj
# test <- dtACTStockouts_maniema[variable=="stockOutASAQ_inj",]
# imputedTot <- test[isMissing==TRUE, .(.N), by=c("health_zone", "year")]
# setnames(imputedTot, "N", "total_imputed")
# total_mos <- setkey(test,health_zone,year)[CJ(unique(health_zone), unique(year)), .N, by=.EACHI]
# setnames(total_mos, "N", "total_mos")
# imputed <- merge(imputedTot, total_mos, all=T, by=c("health_zone", "year"))
# imputed <- imputed[is.na(total_imputed), total_imputed:=0]
# imputed <- imputed[, percent_imputed := ((total_imputed/12) * 100)]
# imputed[,total_mos:=NULL]
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# g <- ggplot(dt_graph[year >= 2015, ], aes(x=date, y=value, color = health_zone)) + theme_bw()+
#   geom_point() + #guides(color=FALSE) +
#   facet_wrap( ~ variable, scales="free_y", labeller=as_labeller(stockout_names)) +
#   ylim(0, 12)
# 
# +
#   ggtitle(paste0("Stock-outs of ACTs in ", simpleCap(h), " (Maniema)")) + 
#   ylab("Days of stock-outs per facility") + xlab("Date") 
# 
# print(g)
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
dt_MTK_acts <- dt_MTK[variable %in% c("ArtLum_received", "ASAQreceived_2to11mos", "ASAQreceived_6to13yrs", "ASAQreceived_1to5yrs", "ASAQreceived_14yrsAndOlder"), ]

dt <- dt_MTK_acts[, c('dps', 'health_zone', 'date', 'year', 'mean', 'variable')]

dt_wide <- dcast(dt, dps + health_zone + date + year ~ variable, value.var= "mean")
dt_wide <- as.data.table(dt_wide)

dt_wide <- dt_wide[, totASAQ := ASAQreceived_2to11mos + ASAQreceived_6to13yrs + ASAQreceived_1to5yrs + ASAQreceived_14yrsAndOlder]

dt_graph <- melt(dt_wide, id.vars= c('dps', 'health_zone', 'date', 'year'))
dt_graph <- dt_graph[variable %in% c("ArtLum_received", "totASAQ")]
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
pdf( paste0(output_dir, mtk_acts_received_graphs), height=6, width=12)
for(h in hz_vector_mtk){
  g <- ggplot(dt_graph[health_zone==h, ], aes(x=date, y=value, color = variable)) + theme_bw()+
    geom_point() + geom_line() +
    facet_wrap( ~ variable, scales="free_y", labeller=as_labeller(act_names)) +
    ggtitle(paste0("ACT doses distributed to health facilities ", simpleCap(h), " (", simpleCap(dt_graph[health_zone==h, dps]), ")")) + guides(color=FALSE) +
    ylab("Doses") + xlab("Date") 
  print(g)
}
dev.off()

# setnames(dt_MTK_acts, "isMissing", "was_imputed")
# 
# pdf( paste0(output_dir, mtk_acts_received_graphs_byAge), height=6, width=12)
# for(h in hz_vector_mtk){
#   g <- ggplot(dt_MTK_acts[health_zone==h, ], aes(x=date, y=mean, color = variable)) + theme_bw()+
#     geom_point(aes(shape=was_imputed), size=2) + scale_shape_manual(values=c(19, 1)) + geom_errorbar(aes(ymin=lower, ymax=upper, y=NULL, width=75), alpha=0.4) + geom_line(alpha=0.9, size=0.60) +
#     ggtitle(paste0("ACT doses distributed to health facilities in ", simpleCap(h), " (", simpleCap(dt_graph[health_zone==h, dps]), ")")) +
#     ylab("Doses") + xlab("Date") + facet_wrap( ~ indicator, scales="free_y", labeller = as_labeller(act_names))
#   
#   print(g)
# }
# dev.off()

#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# facet grid
dt_dps <- readRDS(paste0(dir, "imputedData_run2_condensed_dps.rds"))

dt_dps$year <- year(dt_dps$date)

dt_dps_MTK <- dt_dps[dps %in% MTK & year >= 2015,]

MTK_acts_rec <- dt_dps_MTK[indicator %in% c("ArtLum", "ASAQreceived") & subpopulation!="used", ]

MTK_acts_rec_sum <- MTK_acts_rec[, drugTotal := sum(mean), by=c("indicator", "date", "dps")]

MTK_acts_rec_sum <- MTK_acts_rec_sum[, actTotal := sum(mean), by=c("date", "dps")]

pdf( paste0(output_dir, mtk_acts_received_by_dps), height=6, width=12)

g <- ggplot(MTK_acts_rec_sum, aes(x=date, y=drugTotal, color = indicator)) + theme_bw()+
  geom_point() + geom_line() +
  facet_grid(indicator ~ dps, scales="free_y") +
  ggtitle(paste0("ACT doses distributed to health facilities")) + guides(color=FALSE) +
  ylab("Doses") + xlab("Date")  + scale_y_continuous(labels = comma)
print(g)

g <- ggplot(MTK_acts_rec_sum, aes(x=date, y=actTotal, color = dps)) + theme_bw()+
  geom_point() + geom_line() +
  ggtitle(paste0("ACT doses distributed to health facilities")) +
  ylab("Doses of ACTs") + xlab("Date")  + scale_y_continuous(labels = comma)
print(g)

dev.off()

#----------------------------------------------------------------------------------------------------

MTK_acts_used <- dt_dps_MTK[indicator %in% c("ArtLum", "ASAQused") & subpopulation!="received", ]

MTK_acts_used_sum <- MTK_acts_used[, drugTotal := sum(mean), by=c("indicator", "date", "dps")]

MTK_acts_used_sum <- MTK_acts_used_sum[, actTotal := sum(mean), by=c("date", "dps")]


pdf( paste0(output_dir, mtk_acts_used_by_dps), height=6, width=12)

g <- ggplot(MTK_acts_used_sum, aes(x=date, y=drugTotal, color = indicator)) + theme_bw()+
  geom_point() + geom_line() +
  facet_grid(indicator ~ dps, scales="free_y") +
  ggtitle(paste0("ACT doses distributed to patients")) + guides(color=FALSE) +
  ylab("Doses") + xlab("Date")  + scale_y_continuous(labels = comma)
print(g)

g <- ggplot(MTK_acts_used_sum, aes(x=date, y=actTotal, color = dps)) + theme_bw()+
  geom_point() + geom_line() +
  ggtitle(paste0("ACT doses distributed to patients")) +
  ylab("Doses of ACTs") + xlab("Date")  + scale_y_continuous(labels = comma)
print(g)

dev.off()








