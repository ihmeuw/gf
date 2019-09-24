
# ----------------------------------------------
# Audrey Batzel
#
# 7/2/18

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
imputed_data_dps <- ""# dps level calculations
  
output_dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_analysis/')
# ----------------------------------------------

# ----------------------------------------------
# read in imputed data (tol 0.001) that has been condensed down to mean and variance across the 50 imputations (at hz level)
dt <- readRDS(paste0(dir, imputed_data_low_tol))

# there are some (all?) cases where the original value was 0, and it was set to a low percentile value instead of 0 for log transformation but not
# changed back to 0, so set all of those cases to be 0.
dt[value==0, mean:=0]

dt<- dt[dps != "0"]

dtAllYears <- copy(dt)
dtAllYears$year <- year(dtAllYears$date)

# subset to 2015-2017
dt <- dt[date>="2015-01-01",]

MTK <- c("maniema", "tshopo", "kinshasa")

dtOrig <- copy(dt)
dtOrig$year <- year(dtOrig$date)

# aggregate those values into one value for the year, by variable
# sum "mean" (the imputed value) across dates by variable
dt <- dt[, threeYrValue:= sum(mean), by=c("province", "dps", "health_zone", "variable")]
  # to eliminate other variables use this code:
    dt <- dt[, .(threeYrValue= sum(mean)), by=c("province", "dps", "health_zone", "variable")]
    dt <- unique(dt)
    
# cast wide for graphing
dtOrigWide <- dcast(dtOrig, province+dps+health_zone+date ~ variable, value.var= "mean")    
dtOrigWide$year <- year(dtOrigWide$date)
dtOrigWide <- as.data.table(dtOrigWide)

dtOrigWide[, totCasesMalariaSimple:=newCasesMalariaMild_5andOlder + newCasesMalariaMild_pregnantWomen + newCasesMalariaMild_under5]
dtOrigWide[, totCasesMalariaSevere:=newCasesMalariaSevere_5andOlder + newCasesMalariaSevere_pregnantWomen + newCasesMalariaSevere_under5]
dtOrigWide[, totCasesMalaria:= totCasesMalariaSevere + totCasesMalariaSimple]
# ITN_received already present
dtOrigWide[, ITN_distTot := ITN_distAtANC + ITN_distAtPreschool]
dtOrigWide[, totASAQused := ASAQused_14yrsAndOlder + ASAQused_1to5yrs + ASAQused_2to11mos + ASAQused_2to11mos]
dtOrigWide[, totASAQreceived := ASAQreceived_14yrsAndOlder + ASAQreceived_1to5yrs + ASAQreceived_2to11mos + ASAQreceived_6to13yrs]
dtOrigWide[, totSPused := SP_1st + SP_2nd + SP_3rd]
# ArtLum_received and ArtLum_used] don't need to be aggregated
dtOrigWide[, totDrugsReceived := totASAQreceived + ArtLum_received]
dtOrigWide[, totDrugsUsed := totASAQused + ArtLum_used]
dtOrigWide[, totDrugsUsedIncSP := totASAQused + totSPused + ArtLum_used]
dtOrigWide[, totCasesTreated := mildMalariaTreated_5andOlder + mildMalariaTreated_pregnantWomen + mildMalariaTreated_under5 + severeMalariaTreated_5andOlder + severeMalariaTreated_pregnantWomen + severeMalariaTreated_under5]
# ----------------------------------------------

# ----------------------------------------------
dtWide <- dcast(dt, province+dps+health_zone ~ variable, value.var = "threeYrValue")
dtWide <- as.data.table(dtWide)

# calculate total cases of malaria, by severe and uncomplicated malaria, and then the sum of those
dtWide[, totCasesMalariaSimple:=newCasesMalariaMild_5andOlder + newCasesMalariaMild_pregnantWomen + newCasesMalariaMild_under5]
dtWide[, totCasesMalariaSevere:=newCasesMalariaSevere_5andOlder + newCasesMalariaSevere_pregnantWomen + newCasesMalariaSevere_under5]
dtWide[, totCasesMalaria:= totCasesMalariaSevere + totCasesMalariaSimple]
# ITN_received already present
dtWide[, ITN_distTot := ITN_distAtANC + ITN_distAtPreschool]
dtWide[, totASAQused := ASAQused_14yrsAndOlder + ASAQused_1to5yrs + ASAQused_2to11mos + ASAQused_2to11mos]
dtWide[, totASAQreceived := ASAQreceived_14yrsAndOlder + ASAQreceived_1to5yrs + ASAQreceived_2to11mos + ASAQreceived_6to13yrs]
dtWide[, totSPused := SP_1st + SP_2nd + SP_3rd]
# ArtLum_received and ArtLum_used] don't need to be aggregated
dtWide[, totDrugsReceived := totASAQreceived + ArtLum_received]
dtWide[, totDrugsUsed := totASAQused + ArtLum_used]
dtWide[, totDrugsUsedIncSP := totASAQused + totSPused + ArtLum_used]
dtWide[, totCasesTreated := mildMalariaTreated_5andOlder + mildMalariaTreated_pregnantWomen + mildMalariaTreated_under5 + severeMalariaTreated_5andOlder + severeMalariaTreated_pregnantWomen + severeMalariaTreated_under5]
# ----------------------------------------------

# ----------------------------------------------
# agg by DPS, across all years in 2015-2017
dtAggDPS <- dt[, .(aggThreeYrValue = sum(threeYrValue)), by=c( "province", "dps", "variable")]
dtAggDPSWide <- dcast(dtAggDPS, province+dps ~ variable, value.var = "aggThreeYrValue")
dtAggDPSWide <- as.data.table(dtAggDPSWide)

dtAggDPSWide[, totCasesMalariaSimple:=newCasesMalariaMild_5andOlder + newCasesMalariaMild_pregnantWomen + newCasesMalariaMild_under5]
dtAggDPSWide[, totCasesMalariaSevere:=newCasesMalariaSevere_5andOlder + newCasesMalariaSevere_pregnantWomen + newCasesMalariaSevere_under5]
dtAggDPSWide[, totCasesMalaria:= totCasesMalariaSevere + totCasesMalariaSimple]
# ITN_received already present
dtAggDPSWide[, ITN_distTot := ITN_distAtANC + ITN_distAtPreschool]
dtAggDPSWide[, totASAQused := ASAQused_14yrsAndOlder + ASAQused_1to5yrs + ASAQused_2to11mos + ASAQused_2to11mos]
dtAggDPSWide[, totASAQreceived := ASAQreceived_14yrsAndOlder + ASAQreceived_1to5yrs + ASAQreceived_2to11mos + ASAQreceived_6to13yrs]
# ArtLum_received and ArtLum_used] don't need to be aggregated
dtAggDPSWide[, totSPused := SP_1st + SP_2nd + SP_3rd]
dtAggDPSWide[, totDrugsReceived := totASAQreceived + ArtLum_received]
dtAggDPSWide[, totDrugsUsed := totASAQused + ArtLum_used]
dtAggDPSWide[, totDrugsUsedIncSP := totASAQused + totSPused + ArtLum_used]
dtAggDPSWide[, totCasesTreated := mildMalariaTreated_5andOlder + mildMalariaTreated_pregnantWomen + mildMalariaTreated_under5 + severeMalariaTreated_5andOlder + severeMalariaTreated_pregnantWomen + severeMalariaTreated_under5]
# ----------------------------------------------

# ----------------------------------------------
# agg by DPS and year
dtOrigAgg <- dtOrig[, .(aggValue = sum(mean)), by=c( "province", "dps", "year", "variable")]
dtOrigAggWide <- dcast(dtOrigAgg, province+dps+year ~ variable, value.var = "aggValue")
dtOrigAggWide <- as.data.table(dtOrigAggWide)

dtOrigAggWide[, totCasesMalariaSimple:=newCasesMalariaMild_5andOlder + newCasesMalariaMild_pregnantWomen + newCasesMalariaMild_under5]
dtOrigAggWide[, totCasesMalariaSevere:=newCasesMalariaSevere_5andOlder + newCasesMalariaSevere_pregnantWomen + newCasesMalariaSevere_under5]
dtOrigAggWide[, totCasesMalaria:= totCasesMalariaSevere + totCasesMalariaSimple]
# ITN_received already present
dtOrigAggWide[, ITN_distTot := ITN_distAtANC + ITN_distAtPreschool]
dtOrigAggWide[, totASAQused := ASAQused_14yrsAndOlder + ASAQused_1to5yrs + ASAQused_2to11mos + ASAQused_2to11mos]
dtOrigAggWide[, totASAQreceived := ASAQreceived_14yrsAndOlder + ASAQreceived_1to5yrs + ASAQreceived_2to11mos + ASAQreceived_6to13yrs]
# ArtLum_received and ArtLum_used] don't need to be aggregated
dtOrigAggWide[, totSPused := SP_1st + SP_2nd + SP_3rd]
dtOrigAggWide[, totDrugsReceived := totASAQreceived + ArtLum_received]
dtOrigAggWide[, totDrugsUsed := totASAQused + ArtLum_used]
dtOrigAggWide[, totDrugsUsedIncSP := totASAQused + totSPused + ArtLum_used]
dtOrigAggWide[, totCasesTreated := mildMalariaTreated_5andOlder + mildMalariaTreated_pregnantWomen + mildMalariaTreated_under5 + severeMalariaTreated_5andOlder + severeMalariaTreated_pregnantWomen + severeMalariaTreated_under5]
# ----------------------------------------------

# ----------------------------------------------
dtAllYearsAgg <- dtAllYears[, .(aggValue = sum(mean)), by=c( "province", "dps", "year", "variable")]
dtAllYearsAggWide <- dcast(dtAllYearsAgg, province+dps+year ~ variable, value.var = "aggValue")
dtAllYearsAggWide <- as.data.table(dtAllYearsAggWide)

dtAllYearsAggWide[, totCasesMalariaSimple:=newCasesMalariaMild_5andOlder + newCasesMalariaMild_pregnantWomen + newCasesMalariaMild_under5]
dtAllYearsAggWide[, totCasesMalariaSevere:=newCasesMalariaSevere_5andOlder + newCasesMalariaSevere_pregnantWomen + newCasesMalariaSevere_under5]
dtAllYearsAggWide[, totCasesMalaria:= totCasesMalariaSevere + totCasesMalariaSimple]
# ITN_received already present
dtAllYearsAggWide[, ITN_distTot := ITN_distAtANC + ITN_distAtPreschool]
dtAllYearsAggWide[, totASAQused := ASAQused_14yrsAndOlder + ASAQused_1to5yrs + ASAQused_2to11mos + ASAQused_2to11mos]
dtAllYearsAggWide[, totASAQreceived := ASAQreceived_14yrsAndOlder + ASAQreceived_1to5yrs + ASAQreceived_2to11mos + ASAQreceived_6to13yrs]
# ArtLum_received and ArtLum_used] don't need to be aggregated
dtAllYearsAggWide[, totSPused := SP_1st + SP_2nd + SP_3rd]
dtAllYearsAggWide[, totDrugsReceived := totASAQreceived + ArtLum_received]
dtAllYearsAggWide[, totDrugsUsed := totASAQused + ArtLum_used]
dtAllYearsAggWide[, totDrugsUsedIncSP := totASAQused + totSPused + ArtLum_used]
dtAllYearsAggWide[, totCasesTreated := mildMalariaTreated_5andOlder + mildMalariaTreated_pregnantWomen + mildMalariaTreated_under5 + severeMalariaTreated_5andOlder + severeMalariaTreated_pregnantWomen + severeMalariaTreated_under5]
# ----------------------------------------------

# ----------------------------------------------
# graph bed nets received vs. cases of malaria, HZ LEVEL
# ----------------------------------------------
g1 <- ggplot(dtWide, aes(x=totCasesMalaria, y=ITN_received)) + theme_bw() +
  
  geom_point(aes(color = dps), size=2) + ggtitle("Bed nets distributed to health facilities vs. total cases of malaria;\n2015-2017 aggregated, points are health zones, colored by DPS") + 
  
  # theme(legend.position="none") +
  
  scale_y_continuous(labels = scales::comma) + scale_x_continuous(labels = scales::comma) +
  
  ylab("Number of ITNs distributed") + xlab("Cases of malaria") +
  
  geom_smooth(method='lm')

print(g1)
# ----------------------------------------------
# same graph, with cases log()
# ----------------------------------------------
g2 <- ggplot(dtWide, aes(x=log(totCasesMalaria), y=ITN_received)) + theme_bw() +
  
  geom_point(aes(color = dps), size=2) + ggtitle("Bed nets distributed to health facilities vs. total cases of malaria;\n2015-2017 aggregated, points are health zones, colored by DPS") + 
  
  # theme(legend.position="none") + 
  
  scale_y_continuous(labels = scales::comma) + scale_x_continuous(labels = scales::comma) +
  
  ylab("Number of ITNs distributed") + xlab("Cases of malaria, log scale") +
  
  geom_smooth(method='lm')

print(g2)
# ----------------------------------------------
# same graph, just MTK
# ----------------------------------------------
g3 <- ggplot(dtWide[dps %in% MTK], aes(x=log(totCasesMalaria), y=ITN_received, label=health_zone)) + theme_bw() +
  
  geom_point(aes(color = dps), size=3) + ggtitle("Bed nets distributed to health facilities vs. total cases of malaria;\n2015-2017 aggregated, points are health zones, colored by DPS") + 
  
  # theme(legend.position="none") + 
  
  scale_y_continuous(labels = scales::comma) + scale_x_continuous(labels = scales::comma) +
  
  ylab("Number of ITNs distributed") + xlab("Cases of malaria, log scale") +
  
  geom_smooth(method='lm') +
  
  geom_text( data= subset(dtWide[dps %in% MTK], ITN_received>50000) )

print(g3)
# ----------------------------------------------
# same scatterplot but faceted by year for 2015-2017
# ----------------------------------------------
g4 <- ggplot(dtOrigWide[dps %in% MTK], aes(x=totCasesMalaria, y=ITN_received)) + theme_bw() +
  
  geom_point(aes(color = dps), size=2) + ggtitle("Bed nets distributed to health facilities vs. total cases of malaria;\n2015-2017, points are health zones, colored by DPS") + 
  
  # theme(legend.position="none") +
  
  scale_y_continuous(labels = scales::comma) + scale_x_continuous(labels = scales::comma) +
  
  ylab("Number of ITNs distributed") + xlab("Cases of malaria") +
  
  facet_wrap(~year) +
  
  geom_smooth(method='lm') 

print(g4)
# ----------------------------------------------


# ----------------------------------------------
# graph bed nets received vs. cases of malaria, DPS LEVEL
# ----------------------------------------------
g5 <- ggplot(dtAggDPSWide, aes(x=totCasesMalaria/100000, y=ITN_received/100000)) + theme_bw() +
  
  geom_point(aes(color = dps), size=4) + ggtitle("Bed nets distributed to health facilities vs. total cases of malaria;\n2015-2017 aggregated, by dps") + 
  
  theme(legend.position="none") +
  
  ylab("Number of ITNs distributed (in 100,000s)") + xlab("Cases of malaria (in 100,000s)") +
  
  scale_y_continuous(labels = scales::comma) + scale_x_continuous(labels = scales::comma) +
  
  geom_smooth(method='lm') +
  
  geom_text(label=dtAggDPSWide$dps, color="grey60") +
  
  geom_text(data=dtAggDPSWide[dps=="haut-lomami",], color="black", label="haut-lomami") +
  
  geom_text(data=dtAggDPSWide[dps=="haut-katanga",], color="black", label="haut-katanga") +
  
  geom_text(data=dtAggDPSWide[dps=="haut-uele",], color="black", label="haut-uele") +
  
  geom_text(data=dtAggDPSWide[dps=="nord kivu",], color="black", label="nord kivu") +
  
  geom_text(data=dtAggDPSWide[dps=="kasai oriental",], color="black", label="kasai-oriental") +
  
  geom_text(data=dtAggDPSWide[dps=="kwilu",], color="black", label="kwilu") + 
  
  geom_text(data=dtAggDPSWide[dps=="lomami",], color="black", label="lomami") +
  
  geom_text(data=dtAggDPSWide[dps=="equateur",], color="black", label="equateur") + 
  
  geom_text(data=dtAggDPSWide[dps=="bas congo",], color="black", label="bas congo")

print(g5)
# ----------------------------------------------
# same graph, with cases log()
# ----------------------------------------------
g6 <- ggplot(dtAggDPSWide, aes(x=log(totCasesMalaria), y=ITN_received/100000)) + theme_bw() +
  
  geom_point(aes(color = dps), size=4) + ggtitle("Bed nets distributed to health facilities vs. total cases of malaria;\n2015-2017 aggregated, by dps") + 
  
  theme(legend.position="none") +
  
  ylab("Number of ITNs distributed (in 100,000s)") + xlab("Cases of malaria, log scale") + 
  
  geom_smooth(method='lm') +
  
  scale_y_continuous(labels = scales::comma) + 
  
  geom_text(label=dtAggDPSWide$dps, color="grey60") +
  
  geom_text(data=dtAggDPSWide[dps=="haut-lomami",], color="black", label="haut-lomami") +

  geom_text(data=dtAggDPSWide[dps=="haut-katanga",], color="black", label="haut-katanga") +

  geom_text(data=dtAggDPSWide[dps=="haut-uele",], color="black", label="haut-uele") +

  geom_text(data=dtAggDPSWide[dps=="nord kivu",], color="black", label="nord kivu") +

  geom_text(data=dtAggDPSWide[dps=="kasai oriental",], color="black", label="kasai-oriental") +

  geom_text(data=dtAggDPSWide[dps=="kwilu",], color="black", label="kwilu") + 
  
  geom_text(data=dtAggDPSWide[dps=="lomami",], color="black", label="lomami") +
  
  geom_text(data=dtAggDPSWide[dps=="equateur",], color="black", label="equateur") + 
  
  geom_text(data=dtAggDPSWide[dps=="bas congo",], color="black", label="bas congo")

print(g6)
# ----------------------------------------------
# same graph, with year faceted
# ----------------------------------------------
# ----------------------------------------------
g7 <- ggplot(dtOrigAggWide, aes(x=log(totCasesMalaria), y=ITN_received/100000)) + theme_bw() +
  
  geom_point(aes(color = dps), size=4) + ggtitle("Bed nets distributed to health facilities vs. total cases of malaria;\n2015-2017, by dps") + 
  
  theme(legend.position="none") +
  
  ylab("Number of ITNs distributed (in 100,000s)") + xlab("Cases of malaria, log scale") + 
  
  scale_y_continuous(labels = scales::comma) + 
  
  geom_text(label=dtOrigAggWide$dps, color="grey60") +
  
  geom_text(data=dtOrigAggWide[dps=="haut-lomami",], color="black", label="haut-lomami") +
  
  geom_text(data=dtOrigAggWide[dps=="haut-katanga",], color="black", label="haut-katanga") +
  
  geom_text(data=dtOrigAggWide[dps=="haut-uele",], color="black", label="haut-uele") +
  
  geom_text(data=dtOrigAggWide[dps=="nord kivu",], color="black", label="nord kivu") +
  
  geom_text(data=dtOrigAggWide[dps=="kasai oriental",], color="black", label="kasai-oriental") +
  
  geom_text(data=dtOrigAggWide[dps=="kwilu",], color="black", label="kwilu") + 
  
  geom_text(data=dtOrigAggWide[dps=="lomami",], color="black", label="lomami") +
  
  geom_text(data=dtOrigAggWide[dps=="equateur",], color="black", label="equateur") + 
  
  geom_text(data=dtOrigAggWide[dps=="bas congo",], color="black", label="bas congo")  +
  
  facet_wrap(~year) +
  
  geom_smooth(method='lm')

print(g7)
# ----------------------------------------------
g7 <- ggplot(dtAllYearsAggWide, aes(x=log(totCasesMalaria), y=ITN_received/100000)) + theme_bw() +
  
  geom_point(aes(color = dps), size=3) + ggtitle("Bed nets distributed to health facilities vs. total cases of malaria;\n2015-2017, by dps") + 
  
  # theme(legend.position="none") +
  
  ylab("Number of ITNs distributed (in 100,000s)") + xlab("Cases of malaria, log scale") + 
  
  scale_y_continuous(labels = scales::comma) + 
  
  # geom_text(label=dtAllYearsAggWide$dps, color="black") +
  
  geom_smooth(method='lm') +
  
  facet_wrap(~year) 

print(g7)
# ----------------------------------------------

# ------------------------------------------------------------------------------------------------------------------------------------------

# ----------------------------------------------
# graph medications distributed to health facilities vs. cases treated
# ----------------------------------------------
# hz level
g8 <- ggplot(dtWide, aes(x=totCasesTreated/100000, y=totDrugsReceived/100000, label=health_zone)) + theme_bw() +
  
  geom_point(aes(color = dps), size=2) + ggtitle("Medications (ASAQ and AL) distributed to health facilities vs. total cases treated (uncomplicated and severe malaria); \n2015-2017 aggregated, HZ level colored by DPS") + 
  
  # theme(legend.position="none") +
  
  ylab("Number of antimalarials distributed (in 100,000s)") + xlab("Cases of malaria treated (in 100,000s)") +
  
  geom_smooth(method='lm') +
  
  scale_y_continuous(labels = scales::comma) + scale_x_continuous(labels = scales::comma) +
  
  geom_text( data= subset(dtWide, totDrugsReceived>750000) )
  
print(g8)
# ----------------------------------------------
g9 <- ggplot(dtWide, aes(x=totCasesTreated/100000, y=totASAQreceived/100000, label=health_zone)) + theme_bw() +
  
  geom_point(aes(color = dps), size=2) + ggtitle("ASAQ distributed to health facilities vs. total cases treated (uncomplicated and severe malaria); \n2015-2017 aggregated, HZ level colored by DPS") + 
  
  # theme(legend.position="none") +
  
  ylab("Number of ASAQ doses distributed (in 100,000s)") + xlab("Cases of malaria treated (in 100,000s)") +
  
  geom_smooth(method='lm') +
  
  scale_y_continuous(labels = scales::comma) + scale_x_continuous(labels = scales::comma) +
  
  geom_text( data= subset(dtWide, totASAQreceived>750000) )

print(g9)
# ----------------------------------------------
g10 <- ggplot(dtWide, aes(x=totCasesTreated/100000, y=ArtLum_received, label=health_zone)) + theme_bw() +
  
  geom_point(aes(color = dps), size=2) + ggtitle("AL distributed to health facilities vs. total cases treated (uncomplicated and severe malaria); \n2015-2017 aggregated, HZ level colored by DPS") + 
  
  # theme(legend.position="none") +
  
  ylab("Number of AL doses distributed") + xlab("Cases of malaria treated (in 100,000s)") +
  
  geom_smooth(method='lm') +
  
  scale_y_continuous(labels = scales::comma) + scale_x_continuous(labels = scales::comma) +
  
  geom_text( data= subset(dtWide, ArtLum_received>50000) )

print(g10)
# ----------------------------------------------

# ----------------------------------------------
# same graphs, with cases treated log()
# ----------------------------------------------
# hz level
g11 <- ggplot(dtWide, aes(x=log(totCasesTreated), y=totDrugsReceived/100000, label=health_zone)) + theme_bw() +
  
  geom_point(aes(color = dps), size=2) + ggtitle("Medications (ASAQ and AL) distributed to health facilities vs. total cases treated (uncomplicated and severe malaria); \n2015-2017 aggregated, HZ level colored by DPS") + 
  
  # theme(legend.position="none") +
  
  ylab("Number of antimalarials distributed (in 100,000s)") + xlab("Cases of malaria treated, log scale") +
  
  geom_smooth(method='lm') +
  
  scale_y_continuous(labels = scales::comma) +
  
  geom_text( data= subset(dtWide, totDrugsReceived>750000) )

print(g11)
# ----------------------------------------------
g12 <- ggplot(dtWide, aes(x=log(totCasesTreated), y=totASAQreceived/100000, label=health_zone)) + theme_bw() +
  
  geom_point(aes(color = dps), size=2) + ggtitle("ASAQ distributed to health facilities vs. total cases treated (uncomplicated and severe malaria); \n2015-2017 aggregated, HZ level colored by DPS") + 
  
  # theme(legend.position="none") +
  
  ylab("Number of ASAQ doses distributed (in 100,000s)") + xlab("Cases of malaria treated, log scale") +
  
  geom_smooth(method='lm') +
  
  scale_y_continuous(labels = scales::comma) +
  
  geom_text( data= subset(dtWide, totASAQreceived>750000) )

print(g12)
# ----------------------------------------------
g13 <- ggplot(dtWide, aes(x=log(totCasesTreated), y=ArtLum_received, label=health_zone)) + theme_bw() +
  
  geom_point(aes(color = dps), size=2) + ggtitle("AL distributed to health facilities vs. total cases treated (uncomplicated and severe malaria); \n2015-2017 aggregated, HZ level colored by DPS") + 
  
  # theme(legend.position="none") +
  
  ylab("Number of AL doses distributed") + xlab("Cases of malaria treated, log scale") +
  
  geom_smooth(method='lm') +
  
  scale_y_continuous(labels = scales::comma) +
  
  geom_text( data= subset(dtWide, ArtLum_received>50000) )

print(g13)
# ----------------------------------------------


# hz level
g11 <- ggplot(dtWide[dps %in% MTK], aes(x=log(totCasesTreated), y=totDrugsReceived/100000, label=health_zone)) + theme_bw() +
  
  geom_point(aes(color = dps), size=2) + ggtitle("Medications (ASAQ and AL) distributed to health facilities vs. total cases treated (uncomplicated and severe malaria); \n2015-2017 aggregated, HZ level colored by DPS") + 
  
  # theme(legend.position="none") +
  
  ylab("Number of antimalarials distributed (in 100,000s)") + xlab("Cases of malaria treated, log scale") +
  
  geom_smooth(method='lm') +
  
  scale_y_continuous(labels = scales::comma) +
  
  geom_text( data= subset(dtWide[dps %in% MTK], totDrugsReceived>250000) )

print(g11)
# ----------------------------------------------
g12 <- ggplot(dtWide[dps %in% MTK], aes(x=log(totCasesTreated), y=totASAQreceived/100000, label=health_zone)) + theme_bw() +
  
  geom_point(aes(color = dps), size=2) + ggtitle("ASAQ distributed to health facilities vs. total cases treated (uncomplicated and severe malaria); \n2015-2017 aggregated, HZ level colored by DPS") + 
  
  # theme(legend.position="none") +
  
  ylab("Number of ASAQ doses distributed (in 100,000s)") + xlab("Cases of malaria treated, log scale") +
  
  geom_smooth(method='lm') +
  
  scale_y_continuous(labels = scales::comma) +
  
  geom_text( data= subset(dtWide[dps %in% MTK], totASAQreceived>250000) )

print(g12)
# ----------------------------------------------
g13 <- ggplot(dtWide[dps %in% MTK], aes(x=log(totCasesTreated), y=ArtLum_received, label=health_zone)) + theme_bw() +
  
  geom_point(aes(color = dps), size=2) + ggtitle("AL distributed to health facilities vs. total cases treated (uncomplicated and severe malaria); \n2015-2017 aggregated, HZ level colored by DPS") + 
  
  # theme(legend.position="none") +
  
  ylab("Number of AL doses distributed") + xlab("Cases of malaria treated, log scale") +
  
  geom_smooth(method='lm') +
  
  scale_y_continuous(labels = scales::comma) +
  
  geom_text( data= subset(dtWide[dps %in% MTK], ArtLum_received>20000) )

print(g13)
# ----------------------------------------------


# ----------------------------------------------
#  graph medications distributed to health facilities vs. cases treated
# ----------------------------------------------
# dps level
g <- ggplot(dtAggDPSWide, aes(x=totCasesTreated/100000, y=totDrugsReceived/100000, label=dps)) + theme_bw() +
  
  geom_point(aes(color = dps), size=4) + ggtitle("Medications (ASAQ and AL) distributed to health facilities vs. total cases treated (uncomplicated and severe malaria); \n2015-2017 aggregated, DPS level") + 
  
  theme(legend.position="none") +
  
  ylab("Number of antimalarials distributed (in 100,000s)") + xlab("Cases of malaria treated (in 100,000s)") +
  
  geom_smooth(method='lm') +
  
  geom_text(color="grey50") +
  
  scale_y_continuous(labels = scales::comma) + scale_x_continuous(labels = scales::comma)
 
print(g)
# ----------------------------------------------

# ----------------------------------------------
# same graph, with cases log()
# ----------------------------------------------
# dps level
g <- ggplot(dtAggDPSWide, aes(x=log(totCasesTreated), y=totDrugsReceived/100000, label=dps)) + theme_bw() +
  
  geom_point(aes(color = dps), size=4) + ggtitle("Medications (ASAQ and AL) distributed to health facilities vs. total cases treated (uncomplicated and severe malaria); \n2015-2017 aggregated, DPS level") + 
  
  theme(legend.position="none") +
  
  ylab("Number of antimalarials distributed (in 100,000s)") + xlab("Cases of malaria treated, log scale") +
  
  geom_smooth(method='lm') +
  
  geom_text(color="grey50") +
  
  scale_y_continuous(labels = scales::comma) + scale_x_continuous(labels = scales::comma)

print(g)
# ----------------------------------------------

g <- ggplot(dtOrigAggWide, aes(x=totCasesTreated/100000, y=totDrugsReceived/100000, label=dps)) + theme_bw() +
  
  geom_point(aes(color = dps), size=3) + ggtitle("Medications (ASAQ and AL) distributed to health facilities vs. total cases treated (uncomplicated and severe malaria); \n2015-2017, DPS level") + 
  
  # theme(legend.position="none") +
  
  scale_y_continuous(labels = scales::comma) + scale_x_continuous(labels = scales::comma) +
  
  ylab("Number of antimalarials distributed (100,000s)") + xlab("Cases of malaria treated (100,000s)") +
  
  facet_wrap(~year) +

  geom_smooth(method='lm')

print(g)


g <- ggplot(dtAllYearsAggWide, aes(x=totCasesTreated/100000, y=totDrugsReceived/100000, label=dps)) + theme_bw() +
  
  geom_point(aes(color = dps), size=3) + ggtitle("Medications (ASAQ and AL) distributed to health facilities vs. total cases treated (uncomplicated and severe malaria); \n2015-2017, DPS level") + 
  
  # theme(legend.position="none") +
  
  scale_y_continuous(labels = scales::comma) + scale_x_continuous(labels = scales::comma) +
  
  ylab("Number of antimalarials distributed (100,000s)") + xlab("Cases of malaria treated (100,000s)") +
  
  facet_wrap(~year) +
  
  geom_smooth(method='lm') 

print(g)

# ----------------------------------------------


pdf(paste0(output_dir, "Bed nets distributed vs. cases of malaria graphs.pdf"), height= 9, width = 11)

dev.off()

pdf(paste0(output_dir, "Antimalarials distributed vs. cases of malaria treated graphs.pdf"), height= 9, width = 11)

dev.off()

