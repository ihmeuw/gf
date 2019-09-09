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
# essential data prep
#--------------------------------------

# change lfa_results_cheivement_ratio to numeric value
DT$lfa_result_achievement_ratio <- as.numeric(DT$lfa_result_achievement_ratio)

# clean code
DT$indicator[which(DT$indicator=='M&amp;E-2: Proportion de rapports reçus des formations sanitaires  par rapport aux rapports attendus attendus au cours de la période de rapportage')] <- "M&E-2: Proportion de rapports reçus des formations sanitaires  par rapport aux rapports attendus attendus au cours de la période de rapportage"
DT$indicator[which(DT$indicator=="M&amp;E-1: Pourcentage d'entités déclarantes présentant leurs rapports dans les délais selon les directives nationales")] <- "M&E-1: Pourcentage d'entités déclarantes présentant leurs rapports dans les délais selon les directives nationales"
DT$indicator[which(DT$indicator=='M&amp;E-1: Percentage of HMIS or other routine reporting units submitting timely reports according to national guidelines')] <- "M&E-1: Percentage of HMIS or other routine reporting units submitting timely reports according to national guidelines"
DT$indicator[which(DT$indicator=='M&amp;E-2: Proportion of facility reports received over the reports expected during the reporting period')] <- "ME-2: Proportion of facility reports received over the reports expected during the reporting period"
DT$indicator[which(DT$indicator=='Nombre et pourcentage de structures de santé (Centres de Santé intégrés) qui rapporte une rupture de stock des tests de diagnostic rapide (TDR) malaria')] <- "RDT SO: Nombre et pourcentage de structures de santé (Centres de Santé intégrés) qui rapporte une rupture de stock des tests de diagnostic rapide (TDR) malaria"
DT$indicator[which(DT$indicator=='Number and percentage of health facilities (Integrated health facilities) reporting stock out of malaria rapid diagnostic test (RDT)')] <- "RDT SO: Number and percentage of health facilities (Integrated health facilities) reporting stock out of malaria rapid diagnostic test (RDT)"
DT$indicator[which(DT$indicator=='GP other-2 Nombre de SVS ayant recu le kit PEP dans les 72h')] <- "GP other-2: Nombre de SVS ayant recu le kit PEP dans les 72h"

# aggregate other_dah to the modular level with proper code value and variable name
DT = DT[, c("indicator_code", "indicator_description", "indicator_misc") := tstrsplit(indicator, ": ", fixed=TRUE)]

# change indicator code to factor
DT$indicator_code <- as.factor(DT$indicator_code)

# Additional variables

# for subpopulations

# recalculate acheivement ratio
DT$ihme_achievement_ratio <- DT$lfa_result_n/DT$target_n
# write funciton to plug in achievemetn ratio if not based on _n 
# if ihme_achievement_ratio ==NA

# --------------------------------------
# Senegal TB Analyses

# subset data as appropriate:
DT1 <- DT[loc_name=="sen"]
DT1 <- DT1[disease=="tb"]
DT1 <- DT1[pudr_sheet %in% c('impact_outcome_indicators_main', 'coverage_indicators_main')]
DT1 <- DT1[file_name=="SEN-Z-MOH PUDR (Juil-Dec18) LFA 19Avr19 MAJ 25apr19.xlsx"]

# plot of TB Data in Senegal
ggplot(DT1, aes(x=indicator_code, y=lfa_result_achievement_ratio)) +
  geom_point() +
  labs(title="TB performance in Senegal") +
  geom_hline(yintercept = 1) +
  theme_bw()+
  coord_flip()

# DRC TB Analyses
DT2 <- DT[loc_name=="cod"]
DT2 <- DT2[disease=="tb"]
DT2 <- DT2[pudr_sheet=="coverage_indicators_main"]
DT2 <- DT2[file_name=="LFA_Review_COD-T-MOH_Progress Report_30Jun2018_Sent_02102018_OK.xlsx"] # most recent PUDR

# plot of TB Data in DRC
ggplot(DT2, aes(x=indicator_code, y=lfa_result_achievement_ratio)) +
  geom_point() +
  labs(title="TB performance in DRC") +
  geom_hline(yintercept = 1) +
  coord_flip()

# Uganda TB Analyses
DT3 <- DT[loc_name=="uga"]
DT3 <- DT3[disease=="tb"]
DT3 <- DT3[pudr_sheet=="coverage_indicators_main"]
DT3 <- DT3[file_name=="LFA Reviewed UGA-T-MOFPED PE 31 Dec 2019 (10 May 2019).xlsx"] # most recent PUDR

# plot of TB Data in Uganda
ggplot(DT3, aes(x=indicator_code, y=lfa_result_achievement_ratio)) +
  geom_point() +
  labs(title="TB performance in Uganda") +
  geom_hline(yintercept = 1) +
  coord_flip() +
  labs(caption=paste0('Data sources:', unique(DT$file_name)))


# --------------------------------------
# Cross-country comparisons TB
# -----------------------------------------
DT5 <- DT[disease=="tb"]
DT5 <- DT5[pudr_sheet=="coverage_indicators_main"]
DT5 <- DT5[file_name!="SEN-Z-MOH PU (Jan-Juin 18), LFA 10Dec18.xlsx"] # old senegal PUDR
DT5 <- DT5[file_name!="LFA reviewed UGA_T_MOFPED PUDR PE 30 June 2018.xlsx"] # old UGA PUDR

ggplot(DT5, aes(x=indicator_code, y=lfa_result_achievement_ratio, col=loc_name)) +
  geom_point(aes(shape=loc_name)) +
  scale_shape_manual(values = c(15:17))+
  labs(title="TB performance across countries") +
  geom_hline(yintercept = 1) +
  coord_flip()+
  theme_bw()+
  labs(caption = "Data sources: \n SEN-Z-MOH Jul-Dec 2018 PUDR. \n UGA-T_MOFPED 31 Dec 2019 PUDR. \n COD-T_MOH 30 Jun 2018 Progress Report.")

# --------------------------------------
# Cross-country comparisons HIV
# -----------------------------------------
DT5 <- DT[disease=="tb"]
DT5 <- DT5[pudr_sheet=="coverage_indicators_main"]
DT5 <- DT5[file_name!="SEN-Z-MOH PU (Jan-Juin 18), LFA 10Dec18.xlsx"] # old senegal PUDR
DT5 <- DT5[file_name!="LFA reviewed UGA_T_MOFPED PUDR PE 30 June 2018.xlsx"] # old UGA PUDR

ggplot(DT5, aes(x=indicator_code, y=lfa_result_achievement_ratio, col=loc_name)) +
  geom_point(aes(shape=loc_name)) +
  scale_shape_manual(values = c(15:17))+
  labs(title="TB performance across countries") +
  geom_hline(yintercept = 1) +
  coord_flip()+
  theme_bw()+
  labs(caption = "Data sources: \n SEN-Z-MOH Jul-Dec 2018 PUDR. \n UGA-T_MOFPED 31 Dec 2019 PUDR. \n COD-T_MOH 30 Jun 2018 Progress Report.")

# subset data as appropriate

# re-create last year's figure

# get data in same format
# create new variables


# create plot
#ggplot(DT, aes(x=indicator_code, y=lfa_result_achievement_ratio, color=module_code, size = no_observ)) + theme_bw()+
#  geom_pointrange( aes(ymin= min_ach_ratio, ymax= max_ach_ratio), position= position_jitter(width = 0.40), shape = 21, fill = "white") + guides(color=FALSE) +
#  scale_size_continuous(breaks= c(1, 2, 3, 4, 5), range=c(0.5,1.5), name = "Number of \ncountries reporting") + 
#  ggtitle(paste0("Average achievement ratios of performance indicators by module")) +
#  ylab("Achievement ratio") + xlab("Module")  +
#  labs(caption = "Bars represent the max and min and points are the mean by module-intervention across countries and grants.
#       Note: Where the max, mean, or min for a given module-intervention was higher than 200, it was changed to 200 for clarity in this figure.") +
#  theme(axis.text=element_text(size=14), axis.title=element_text(size=16), legend.title=element_text(size = 14), 
#        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=12),
#        axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
#  geom_hline(yintercept=100, linetype="dashed", color="grey", alpha=0.6, size=2) 
#
#
#
#
#






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

# DThiv <- DT[(disease=="hiv"),.(loc_name, indicator_code, lfa_result_achievement_ratio)]
# DTtb <- DT[(disease=="tb"),.(loc_name, indicator_code, lfa_result_achievement_ratio)]
# DThivtb <- DT[(disease=="hiv/tb"),.(loc_name, indicator_code, lfa_result_achievement_ratio)]
# DTmal <- DT[(disease=="malaria"),.(loc_name, indicator_code, lfa_result_achievement_ratio)]
# 
# a <- ggplot(DTtb, aes(x=indicator_code, y=lfa_result_achievement_ratio, color=loc_name)) +
#   geom_point() +
#   labs(title="TB") +
#   coord_flip()
# 
# b <- ggplot(DThivtb, aes(x=indicator_code, y=lfa_result_achievement_ratio, color=loc_name)) +
#   geom_point() + 
#   labs(title="HIV")+
#   coord_flip()
# 
# c <- ggplot(DTmal, aes(x=indicator_code, y=lfa_result_achievement_ratio, color=loc_name)) +
#   geom_point() + 
#   labs(title="Malaria") +
#   coord_flip()
# 
# d <- ggplot(DThiv, aes(x=indicator_code, y=lfa_result_achievement_ratio, color=loc_name)) +
#   geom_point() + 
#   labs(title="HIV")
#   coord_flip()
# 
# ggplot(DTtb, aes(x=indicator_code, y=lfa_result_achievement_ratio)) +
#    facet_wrap(~loc_name) +
#     geom_point() +
#     labs(title="TB") +
#     coord_flip() 
# 
# ggplot(DThiv, aes(x=indicator_code, y=lfa_result_achievement_ratio)) +
#   facet_wrap(~loc_name) +
#   geom_point() +
#   labs(title="HIV") +
#   coord_flip()
# 
# ggplot(DThivtb, aes(x=indicator_code, y=lfa_result_achievement_ratio)) +
#   facet_wrap(~loc_name) +
#   geom_point() +
#   labs(title="HIV/TB") +
#   coord_flip()
#   
# # with our own calculated ratios
# 
# 
# 
# # --------------------------
# # Save PDF of files
#pdf("J:/Project/Evaluation/GF/special_assessments/pudr_framework_Indicators/sample1.pdf", height=5.5, width=9)
#a
#b
#c
#d
#dev.off()
