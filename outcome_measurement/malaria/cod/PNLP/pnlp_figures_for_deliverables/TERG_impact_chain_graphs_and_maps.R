

#===========FOR TERG IMPACT CHAIN SLIDES================================================================================
# ----------------------------------------------
# Audrey Batzel
#
# 6/28/18
# Working with the imputed data and creating visualizations for TERG impact chain slides

setwd('C:/local/gf/')
# ----------------------------------------------


# --------------------
# Set up R / install packages
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
# --------------------  


# ----------------------------------------------
# Overview - Files and Directories

# data directory
# when run on Unix, data directory needs to be set to /home/j (to run on the cluster), so set this here:
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/')

# input file:
input <- "PNLP_2010to2017_preppedForMI.csv"
imputedData1 <- "imputedData_forGraphing_run1.rds"
imputedData2 <- "imputedData_forGraphing_run2.rds"

# source in variable names
variable_names <-"./outcome_measurement/malaria/cod/variable_names.R"
source(variable_names)

# output
output_dir <- "J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/"
# ----------------------------------------------


# ----------------------------------------------
# read in data table prepped by prep_for_MI.R
dtOrig <- fread(paste0(dir, input)) 
dps_names <- unique(dtOrig$dps)
dps_names <- dps_names[!dps_names %in% c("0")]
# ----------------------------------------------


# ----------------------------------------------
# read in data table prepped by prep_for_MI.R
dt1 <- readRDS(paste0(dir, imputedData1))
dt1[is.na(subpopulation), subpopulation:="none"]
dt2 <- readRDS(paste0(dir, imputedData2))
dt2[is.na(subpopulation), subpopulation:="none"]
# ----------------------------------------------


# ---------------------------------------------- 
# function to capitalize hz and dps names
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
# ----------------------------------------------


# ---------------------------------------------- 

# ---------------------------------------------- 
# GRAPHS FOR GODEFROID FOR PNLP MEETING 
healthFac <- colnames(dtOrig)[grep("healthFacilities", colnames(dtOrig))]
dt<- dtOrig[, c("date", "province11_name", "health_zone", "dps", healthFac), with=F]

dt$date <- as.Date(dt$date)
dt$year <- year(dt$date)

dtMelt <- melt(dt, id.vars= c("date", "province11_name", "health_zone", "dps", "year"), variable.name="indicator")


# make a dt of all health zone/dps pairs in dt to loop through (some hz names are used in more than one dps)
hzDPS <- dtOrig[, .(health_zone, dps)]
# remove duplicates to have just unique values:
hzDPS <- unique(hzDPS) 
hz_vector <- hzDPS[["health_zone"]]


pdf((paste0(output_dir, "Time Series for Number of Health Facilities (HZ level).pdf")), height=6, width=9)   

for (row in 1:nrow(hzDPS)){
  g <- ggplot(dtMelt[ health_zone==(hzDPS[row, health_zone]) & dps== (hzDPS[row, dps]) & indicator %in% c("healthFacilities_total", "healthFacilities_numReported")], aes(x=date, y=value, color=indicator)) + 
    
    theme_bw()+ geom_point() + geom_line(alpha=0.9, size=0.60) +
    
    ggtitle(paste0("Time Series for ", simpleCap(hzDPS[row, health_zone]), " (", simpleCap(hzDPS[row, dps]), ")")) +
    
    ylab("Total Health Facilities") + xlab("Date") + ylim(0, NA)
  
  print(g)
}

dev.off()  

#--------------------------------------------
dt$healthFacilities_adjusted <- NA
dt$healthFacilities_adjusted <- as.numeric(dt$healthFacilities_adjusted)
dt$healthFacilities_total <- as.numeric(dt$healthFacilities_total)

dt2 <- dt[year %in% c("2010", "2011", "2012"), healthFacilities_adjusted := max(healthFacilities_total, na.rm=T), by=c("health_zone", "year")]
dt2 <- dt2[healthFacilities_adjusted == "-Inf", healthFacilities_adjusted:=NA]
#dt2 <- merge(dt2, dt, all.x=T, by=c("date", "province11_name", "health_zone", "dps", "year", "healthFacilities_total", "healthFacilities_numReported"))

dt2$healthFacilities_adjusted <- as.numeric(dt2$healthFacilities_adjusted)
dt2$healthFacilities_total <- as.numeric(dt2$healthFacilities_total)

dt2[is.na(healthFacilities_adjusted) & !is.na(healthFacilities_total), healthFacilities_adjusted := healthFacilities_total]

dt2$date <- as.Date(dt2$date)

dtMelt2 <- melt(dt2, id.vars= c("date", "province11_name", "health_zone", "dps", "year"), variable.name="indicator")


pdf((paste0(output_dir, "Time Series for Number of Health Facilities (HZ level), Adjusted Values.pdf")), height=6, width=9)   

for (row in 1:nrow(hzDPS)){
  g <- ggplot(dtMelt2[ health_zone==(hzDPS[row, health_zone]) & dps== (hzDPS[row, dps])& indicator %in% c("healthFacilities_adjusted", "healthFacilities_numReported"),], aes(x=date, y=value, color=indicator)) + 
    
    theme_bw()+ geom_point() + geom_line(alpha=0.9, size=0.60) +
    
    ggtitle(paste0("Time Series for ", simpleCap(hzDPS[row, health_zone]), " (", simpleCap(hzDPS[row, dps]), ")")) +
    
    ylab("Total Health Facilities") + xlab("Date") + ylim(0, NA)
  
  print(g)
}

dev.off() 

#--------------------------------------------
  # dps total facilities

dt3 <- dt2[, .(healthFacilities_DPStotal = sum(healthFacilities_max)), by=c("dps", "date")]

dt3$date <- as.Date(dt3$date)

dpsNames <- unique(hzDPS[dps!="0", dps])

pdf((paste0(output_dir, "Time Series for Number of Health Facilities (DPS level), Adjusted Values.pdf")), height=6, width=9)   

  g <- ggplot(dt3[dps %in% dpsNames[23:26],], aes(x=date, y=healthFacilities_DPStotal, color=dps)) + 
    
    theme_bw()+ geom_point() + geom_line(alpha=0.9, size=0.60) +
    
    ggtitle(paste0("Time Series for Health Facilities at DPS Level")) +
    
    ylab("Total Health Facilities") + xlab("Date") + ylim(0, NA)
  
  print(g)


dev.off() 

#--------------------------------------------
#--------------------------------------------
dt2$year <- year(dt2$date)

dtACTStockouts <- dt2[indicator %in% c("stockOutartLum", "stockOutASAQ")& subpopulation != "inj" & dps !=0, ]
dtACTStockouts <- dtACTStockouts[, .(totStockouts = sum(mean)), by=c("dps", "date")]
dtACTStockouts <- dtACTStockouts[, totStockoutsHZ := sum(mean), by=c("dps", "date", "health_zone")]
dtACTStockouts[, totStockoutsbyType := sum(mean), by=c("dps", "date", "subpopulation")]

#forTERG is in prep for MI because I took it from the deterministically imputed health facilities
forTERG <- forTERG[, c(1:5, 9:10)]
forTERG2 <- melt(forTERG, id.vars= c("health_zone", "year", "province11_name", "date", "dps"))

dtFacilities <- forTERG2[variable=="healthFacilities_max" & dps !=0, ]
dtFacilities <- dtFacilities[, .(totFacilities = sum(value, na.rm=T)), by=c("dps", "date")]
dtFacilities$date <- as.Date(dtFacilities$date)
dtACTStockouts$date <- as.Date(dtACTStockouts$date)
dtGraph <- merge(dtFacilities, dtACTStockouts, all=T,  by=c("dps", "date"))
dtGraph <- dtGraph[, stockOutsByFacilities := totStockouts/totFacilities]

facilities <- forTERG[dps %in% MTK, .(totalFacilitiesDPS=(sum(healthFacilities_max))), by=.(dps, date)]
facilities$date <- as.Date(facilities$date)
ggplot(facilities, aes(x=date, y=totalFacilitiesDPS, color=dps, group=dps)) +
  geom_point() +
  geom_line()
#--------------------------------------------

facilitiesKIN <- dt[dps=="kinshasa", .(healthFacilities_max, healthFacilities_totalOrig, healthFacilities_numReported, healthFacilities_adjusted), by=.(health_zone, date)]
facilitiesKIN$date <- as.Date(facilitiesKIN$date)
facilitiesITU <- dt[dps=="ituri", .(healthFacilities_max, healthFacilities_totalOrig, healthFacilities_numReported, healthFacilities_adjusted), by=.(health_zone, date)]
facilitiesITU$date <- as.Date(facilitiesITU$date)

weirdHZ <- facilitiesKIN[healthFacilities_max>100, unique(health_zone)]
weirdHZitu <- facilitiesITU[healthFacilities_max>100, unique(health_zone)]

ggplot(facilitiesKIN[health_zone %in% weirdHZ[1:3],], aes(x=date, y=healthFacilities_adjusted, color=health_zone, group=health_zone)) +
  geom_point() +
  geom_line()

# ggplot(dtACTStockouts[health_zone %in% weirdHZ[1:3],], aes(x=date, y=totStockoutsHZ, color=health_zone, group=health_zone)) +
#   geom_point() +
#   geom_line()

ggplot(facilitiesKIN[health_zone %in% weirdHZ[6],], aes(x=date, y=healthFacilities_max, color=health_zone, group=health_zone)) +
  geom_point() +
  geom_line()

# ggplot(dtACTStockouts[health_zone %in% weirdHZ[4:6],], aes(x=date, y=totStockoutsHZ, color=health_zone, group=health_zone)) +
#   geom_point() +
#   geom_line()

ggplot(facilitiesKIN[health_zone %in% weirdHZ[7:10],], aes(x=date, y=healthFacilities_max, color=health_zone, group=health_zone)) +
  geom_point() +
  geom_line()

# ggplot(dtACTStockouts[health_zone %in% weirdHZ[7:10],], aes(x=date, y=totStockoutsHZ, color=health_zone, group=health_zone)) +
#   geom_point() +
#   geom_line()
# ---------------------------------------------- 

dtACTReceived <- dt2[indicator %in% c("ASAQreceived", "ArtLum") & subpopulation!="used" & dps !=0, ]
dtACTReceived[, totACTReceived := sum(mean), by=c("dps", "date")]
dtACTReceived[, totACTReceivedbyType := sum(mean), by=c("dps", "date", "subpopulation")]

totals <- dtACTReceived[year==2017 & dps %in% MTK, .(MTKmean = sum(mean)), by=c("date")]
totals[, mean(MTKmean)]  #mean per month of MTK in 2017

totals <- dtACTReceived[year==2017, .(drcMean = sum(mean)), by=c("date")]
totals[, mean(drcMean)]  # whole country


totals <- dtACTReceived[dps=="kinshasa", .(monthTotKin = sum(mean)), by=c("date", "year")]
totals[, mean(monthTotKin), c("year")]
totals <- dtACTReceived[year==2017 & dps=="tshopo", .(monthTotKin = sum(mean)), by=c("date")]
totals[, mean(monthTotKin)]

dtACTUsed <- dt2[indicator %in% c("ASAQused", "ArtLum") & subpopulation!="received" & dps !=0, ]
dtACTUsed[, totACTUsed := sum(mean), by=c("dps", "date")]
dtACTUsed[, totACTUsedbyType := sum(mean), by=c("dps", "date", "subpopulation")]

totals <- dtACTUsed[year==2017 & dps %in% MTK, .(MTKmean = sum(mean)), by=c("date")]
totals[, mean(MTKmean)]  #mean per month of MTK in 2017
totals <- dtACTUsed[year==2017, .(drcMean = sum(mean)), by=c("date")]
totals[, mean(drcMean)]  # whole country


totals <- dtACTUsed[dps=="tshopo", .(monthTotKin = sum(mean)), by=c("date", "year")]
totals[, mean(monthTotKin), c("year")]
totals <- dtACTUsed[year==2017 & dps=="kinshasa", .(monthTotKin = sum(mean)), by=c("date")]
totals[, mean(monthTotKin)]

MTK <- c("maniema", "tshopo", "kinshasa")

dps_names <- c(
  `kinshasa` = "Kinshasa",
  `tshopo` = "Tshopo",
  `maniema` = "Maniema"
)

pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/GraphsForTERG.pdf", height=6, width=10)
g <- ggplot(dtGraph[dps %in% c("maniema", "tshopo", "kinshasa"), ], aes(x=date, y=stockOutsByFacilities, color = dps)) + theme_bw()+
  geom_point() + geom_line() +
  ggtitle(paste0("Time Series for Stock-outs of ACTs")) +
  labs(color="DPS") + ylab("Days of stock-outs per facility") + xlab("Date") +
  theme(plot.title = element_text(size = 18), legend.title=element_text(size=18), legend.text=element_text(size=18),
        axis.title=element_text(size=14), axis.text=element_text(size=12))+
  scale_color_discrete(name="DPS",
                       breaks=c("kinshasa", "maniema", "tshopo"),
                       labels=c("Kinshasa", "Maniema", "Tshopo"))
print(g)

g <- ggplot(dtACTReceived[dps %in% c("maniema", "tshopo", "kinshasa"), ], aes(x=date, y=totACTReceived, color = dps)) + theme_bw()+
  geom_point() + geom_line() +
  ggtitle(paste0("Time Series for ACTs Received by Health Zones")) +
  labs(color="DPS:") + ylab("Doses of ACTs") + xlab("Date") + scale_y_continuous(labels = scales::comma) +
  theme(plot.title = element_text(size = 18), legend.title=element_text(size=18), legend.text=element_text(size=18),
        axis.title=element_text(size=18), axis.text=element_text(size=12))+
  scale_color_discrete(name="DPS",
                       breaks=c("kinshasa", "maniema", "tshopo"),
                       labels=c("Kinshasa", "Maniema", "Tshopo"))+
  facet_grid(~dps, labeller=as_labeller(dps_names)) +
  theme(legend.position="bottom")

print(g)

g <- ggplot(dtACTUsed[dps %in% MTK, ], aes(x=date, y=totACTUsed, color = dps)) + theme_bw()+
  geom_point() + geom_line() +
  ggtitle(paste0("Time Series for ACTs Distributed to Patients")) +
  labs(color="DPS") + ylab("Doses of ACTs") + xlab("Date") + scale_y_continuous(labels = scales::comma) +
  theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=13), 
        axis.title=element_text(size=18), axis.text=element_text(size=12))+
  scale_color_discrete(name="DPS",
                       breaks=c("kinshasa", "maniema", "tshopo"),
                       labels=c("Kinshasa", "Maniema", "Tshopo"))
print(g)
#----------------- 
graphDPS <- function(d){
  g <- ggplot(dtASAQStockouts[dps==d,], aes(x=date, y=totStockoutsbyType, color = subpopulation)) + theme_bw()+
    geom_point() + geom_line() +
    ggtitle(paste0("Time Series for Stock-outs (", simpleCap(d), ")")) +
    labs(color="Age Group") + ylab("Days of Stock-outs") + xlab("Date") +
    scale_color_discrete(name="Age Group",
                         breaks=c("14yrsAndOlder", "6to13yrs", "1to5yrs", "2to11mos" ),
                         labels=c("14 yrs+", "6 to 13 yrs", "1 to 5 yrs", "2 to 11 mos")) +
    theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=13))
  
  print(g)
  
  g <- ggplot(dtASAQReceived[dps==d,], aes(x=date, y=totASAQReceivedbyType, color = subpopulation)) + theme_bw()+
    geom_point() + geom_line() +
    ggtitle(paste0("Time Series for ASAQ Received by Health Zones (", simpleCap(d), ")")) +
    labs(color="Age Group") + ylab("Doses of ASAQ") + xlab("Date") + scale_y_continuous(labels = scales::comma) +
    scale_color_discrete(name="Age Group",
                         breaks=c("14yrsAndOlder", "6to13yrs", "1to5yrs", "2to11mos" ),
                         labels=c("14 yrs+", "6 to 13 yrs", "1 to 5 yrs", "2 to 11 mos")) +
    theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=13))
  print(g)
  
  g <- ggplot(dtASAQUsed[dps==d, ], aes(x=date, y=totASAQUsedbyType, color = subpopulation)) + theme_bw()+
    geom_point() + geom_line() +
    ggtitle(paste0("Time Series for ASAQ Distributed to Patients (", simpleCap(d), ")")) +
    labs(color="Age Group") + ylab("Doses of ASAQ") + xlab("Date") + scale_y_continuous(labels = scales::comma) +
    scale_color_discrete(name="Age Group",
                         breaks=c("14yrsAndOlder", "6to13yrs", "1to5yrs", "2to11mos" ),
                         labels=c("14 yrs+", "6 to 13 yrs", "1 to 5 yrs", "2 to 11 mos")) +
    theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=13))
  print(g)
}
#-----------------
for (i in c("maniema", "tshopo", "kinshasa")){ graphDPS(i) }

dev.off()
#========================================================================================================================  