# Francisco Rios 
# August 21, 2019
# PUDR Indicator Analyses: Potentially for Synthesis

# ------------------------------------
# set up
library(data.table)
library(ggplot2)

# ------------------------------------
# read in all_prepped_data.rds
DT <- readRDS("J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/prepped_data/all_prepped_data.rds")
#--------------------------------------

#--------------------------------------
# minor data prep
#--------------------------------------

# change lfa_results_cheivement_ratio to numeric value
DT$lfa_result_achievement_ratio <- as.numeric(DT$lfa_result_achievement_ratio)

# aggregate other_dah to the modular level with proper code value and variable name
DT = DT[, c("indicator_code", "indicator_description", "indicator_misc") := tstrsplit(indicator, ": ", fixed=TRUE)]
# change indicator code to factor
DT$indicator_code <- as.factor(DT$indicator_code)
#--------------------------------------


# ------------------------------------
# Data check ideas:
# ------------------------------------
# How often do PR, LFA, and GF values agree?
# Which of the three is the most complete? 

# Visualize how often targets are met?
# Which targets are met most often?
# 

# ------------------------------------
# Country-specific analyses
# ------------------------------------
# plot how many are above their target and how many are below the target

# how do the baseline, target, and findings compare between countries
# i.e., how similar or different are the countries in their baselines values

# waterfall plots for each outcome of interest for percent change from baseline

# several yaers of data as well. Perhaps time series? 

# plot OR of meeting goals

# plot target plot 
# see here: https://www.axisgroup.com/data-industry-insights-blog/that-which-we-call-target-plot

# ------------------------------------
# High levle analyses:

# 1 -----
# Progress on indicators for all countries by disease for the 2018-2020 grant period

# change missing to 1.5
# DThiv$lfa_result_achievement_ratio[is.na(DT2$lfa_result_achievement_ratio)] <- 1.5

DThiv <- DT[(disease=="hiv"),.(loc_name, indicator_code, lfa_result_achievement_ratio)]
DTtb <- DT[(disease=="tb"),.(loc_name, indicator_code, lfa_result_achievement_ratio)]
DThivtb <- DT[(disease=="hiv/tb"),.(loc_name, indicator_code, lfa_result_achievement_ratio)]
DTmal <- DT[(disease=="malaria"),.(loc_name, indicator_code, lfa_result_achievement_ratio)]

a <- ggplot(DTtb, aes(x=indicator_code, y=lfa_result_achievement_ratio, color=loc_name)) +
  geom_point() +
  labs(title="TB") +
  coord_flip()

b <- ggplot(DThivtb, aes(x=indicator_code, y=lfa_result_achievement_ratio, color=loc_name)) +
  geom_point() + 
  labs(title="HIV")+
  coord_flip()

c <- ggplot(DTmal, aes(x=indicator_code, y=lfa_result_achievement_ratio, color=loc_name)) +
  geom_point() + 
  labs(title="Malaria") +
  coord_flip()

d <- ggplot(DThiv, aes(x=indicator_code, y=lfa_result_achievement_ratio, color=loc_name)) +
  geom_point() + 
  labs(title="HIV")
  coord_flip()

# --------------------------
# Save PDF of files
pdf("J:/Project/Evaluation/GF/special_assessments/pudr_framework_Indicators/sample1.pdf", height=5.5, width=9)
a
b
c
d
dev.off()
