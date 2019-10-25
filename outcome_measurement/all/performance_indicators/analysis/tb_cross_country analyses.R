# make TB cross country comparisons

library(data.table)
library(ggplot2)

# ------------------------------------
# read in all_prepped_data.rds
DT <- readRDS("J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/prepped_data/all_prepped_data.rds")
#--------------------------------------

#--------------------------------------
# essential data prep: SEN, COD, UGA
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

# split apart code from indicator file
DT = DT[, c("indicator_code", "indicator_description", "indicator_misc") := tstrsplit(indicator, ": ", fixed=TRUE)]

# subset data to merge with guatemala
tball <- DT[,.(loc_name, indicator_code, indicator_type, indicator,
               disease,
               baseline_value, baseline_year, 
               target_n, target_d, `target_%`, target_year,
               lfa_result_n, lfa_result_d, `lfa_result_%`, lfa_result_achievement_ratio, 
               pudr_sheet, file_name,
               reverse_indicator)]

#--------------------------------------

# prep gtm data
DT2 <- readRDS("J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/prepped_data/gtm_1A.rds")

#--------------------------------------
# essential data prep: GTM
#--------------------------------------

# remove rows that are not necessary
DT2 <- DT2[indicator!="[Impact Indicator Name]"]
DT2 <- DT2[indicator!="[Outcome Indicator Name]"]

# remove duplicated column in DT
DT2 <- DT2[,unique(names(DT2)),with=FALSE]

# split apart the code
DT2 = DT2[, c("indicator_code", "indicator_description", "indicator_misc") := tstrsplit(indicator, ": ", fixed=TRUE)]


# keep only columns of interest
tbgtm <- DT2[,.(loc_name, indicator_code, indicator_type, indicator,
                disease,
                baseline_value, baseline_year,
                target_n, target_d, `target_%`, target_year,
                lfa_result_n, lfa_result_d, `lfa_result_%`,
                pudr_sheet, file_name)]
# subset to only latest year of PUDR Target
tbgtm <- tbgtm[target_year==2018]
  
# merge data together
DTall <- rbind(tball, tbgtm, fill=TRUE)

# subset as appropriate, disease, latest pudrs etc, aggregated only
DTall <- DTall[disease=="tb"]
DTall <- DTall[file_name %in% c("SEN-Z-MOH PUDR (Juil-Dec18) LFA 19Avr19 MAJ 25apr19.xlsx", 
                                "GTM-T-MSPAS_Progress Report_LFA18Mar19.xlsx",
                                "LFA_Review_COD-T-MOH_Progress Report_30Jun2018_Sent_02102018_OK.xlsx",
                                "LFA Reviewed UGA-T-MOFPED PE 31 Dec 2019 (10 May 2019).xlsx")]
DTall <- DTall[pudr_sheet %in% c("coverage_indicators_main", "impact_outcome_indicators_main")]


# clean reverse_indicator
DTall$reverse_indicator[which(DTall$indicator_code=="TB I-2")] <- "Yes"
DTall$reverse_indicator[which(DTall$indicator_code=="TB I-3(M)")] <- "Yes"
DTall$reverse_indicator[which(DTall$indicator_code=="TB/HIV I-1")] <- "Yes"
DTall$reverse_indicator[which(DTall$indicator_code=="TB I-4(M)")] <- "Yes"
DTall$reverse_indicator[which(DTall$indicator_code=="TB O-1a")] <- "No"
DTall$reverse_indicator[which(DTall$indicator_code=="TB O-2a")] <- "No"
DTall$reverse_indicator[which(DTall$indicator_code=="TB O-4(M)")] <- "No"
DTall$reverse_indicator[which(DTall$indicator_code=="TB O-5(M)")] <- "No"
DTall$reverse_indicator[which(DTall$indicator_code=="TB O-6")] <- "No"


# convert variables to numeric
DTall$lfa_result_n <- as.numeric(DTall$lfa_result_n)
DTall$`lfa_result_%` <- as.numeric(DTall$`lfa_result_%`)
DTall$target_n <- as.numeric(DTall$target_n)
DTall$`target_%` <- as.numeric(DTall$`target_%`)

# generate our own ihme target and performance results and ratio
DTall$ihme_target_n <- NA
DTall$ihme_target_n <- ifelse(is.na(DTall$`target_%`), DTall$target_n, DTall$`target_%`)

DTall$ihme_result_n <- NA
DTall$ihme_result_n <- ifelse(is.na(DTall$`lfa_result_%`), DTall$lfa_result_n, DTall$`lfa_result_%`)

# calculate ihme_results_achievement_ratio
DTall$ihme_result_achievement_ratio <-NA
DTall$ihme_target_n <- as.numeric(DTall$ihme_target_n)
DTall$ihme_result_n <- as.numeric(DTall$ihme_result_n)
DTall$ihme_result_achievement_ratio <- DTall$ihme_result_n/DTall$ihme_target_n

# read in tb codebook
tbcodes <- fread("./special_assessments/synthesis/2019_multicountry_analyses/tb_indicators_codebook.csv")
DTall <- merge(DTall, tbcodes, by.x="indicator_code", by.y = "Indicator Code")

gtmvariable <- c('TB I-2', 'TB I-3(M)', 'TB I-4(M)', 'TB O-1a', 'TB O-1b', 'TB O-2a', 'TB O-4(M)', 'TB/HIV I-1')
DTplot <- DTall[indicator_code %in% gtmvariable]
#############
# plot data
#############

# ggplot(DTplot, aes(x=short_name_code, y=ihme_result_achievement_ratio, col=loc_name)) +
#   geom_point(aes(shape=loc_name)) +
#   scale_shape_manual(values = c(15:18)) +
#   labs(title="TB performance across countries") +
#   geom_hline(yintercept = 1) +
#   coord_flip()+
#   theme_bw()+
#   labs(caption = "Data sources: \n SEN-Z-MOH Jul-Dec 2018 PUDR. \n UGA-T_MOFPED 31 Dec 2019 PUDR. \n COD-T_MOH 30 Jun 2018 Progress Report \n GTM LFA Verified Progress Report 18 March 2019.")
# 
# # plot only those in which positive is good
# ggplot(DTall[reverse_indicator=="Yes"], aes(x=short_name_code, y=ihme_result_achievement_ratio, col=loc_name)) +
#   geom_point() +
#   labs(title="TB performance across countries", x="Indicator", y="Result Acheivement Ratio") +
#   geom_hline(yintercept = 1) +
#   coord_flip()+
#   theme_bw()+
#   facet_wrap(~loc_name)+
#   labs(caption = "Data sources: \n SEN-Z-MOH Jul-Dec 2018 PUDR \n UGA-T_MOFPED 31 Dec 2019 PUDR \n COD-T_MOH 30 Jun 2018 Progress Report \n GTM LFA Verified Progress Report 18 March 2019")
# 
#### final plot
DTplot$loc_name <- as.factor(DTplot$loc_name)


a <- ggplot(DTplot[reverse_indicator=="No"], aes(x=short_name_code, y=ihme_result_achievement_ratio)) +
  geom_point(aes(color=loc_name)) +
  labs(title="TB performance across countries", x="Indicator", y="Result Acheivement Ratio") +
  geom_hline(yintercept = 1) +
  ylim(0,2)+
  theme_bw()+
  coord_flip()+
  annotate("segment", x = 0.5, y = 0.0, xend = 0.5, yend = 1.75,
           arrow=arrow())+
  annotate("text", x = 0.6, y = .9, label="Higher performance")+
  labs(caption = "Data sources: \n SEN-Z-MOH Jul-Dec 2018 PUDR \n UGA-T_MOFPED 31 Dec 2019 PUDR \n GTM LFA Verified Progress Report 18 March 2019")


# save file
setwd("J:/Project/Evaluation/GF/special_assessments/pudr_framework_indicators")
ggsave("tb_cc_analysis_09062019.jpeg")

jpeg(filename = "J:/Project/Evaluation/GF/special_assessments/pudr_framework_indicators/pudr_tb_cc.jpeg",
     width = 9, height = 2.5, res= 300, units = "in", pointsize = 12)
a
dev.off()
