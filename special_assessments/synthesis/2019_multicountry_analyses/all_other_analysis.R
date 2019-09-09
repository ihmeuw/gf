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
DT$indicator[which(DT$indicator=='M&amp;E-1: Pourcentage d'entités déclarantes présentant leurs rapports dans les délais selon les directives nationales')] <- "M&E-1: Pourcentage d'entités déclarantes présentant leurs rapports dans les délais selon les directives nationales"
DT$indicator[which(DT$indicator=='M&amp;E-1: Percentage of HMIS or other routine reporting units submitting timely reports according to national guidelines')] <- "M&E-1: Percentage of HMIS or other routine reporting units submitting timely reports according to national guidelines"
DT$indicator[which(DT$indicator=='M&amp;E-2: Proportion of facility reports received over the reports expected during the reporting period')] <- "ME-2: Proportion of facility reports received over the reports expected during the reporting period"
DT$indicator[which(DT$indicator=='Nombre et pourcentage de structures de santé (Centres de Santé intégrés) qui rapporte une rupture de stock des tests de diagnostic rapide (TDR) malaria')] <- "RDT SO: Nombre et pourcentage de structures de santé (Centres de Santé intégrés) qui rapporte une rupture de stock des tests de diagnostic rapide (TDR) malaria"
DT$indicator[which(DT$indicator=='Number and percentage of health facilities (Integrated health facilities) reporting stock out of malaria rapid diagnostic test (RDT)')] <- "RDT SO: Number and percentage of health facilities (Integrated health facilities) reporting stock out of malaria rapid diagnostic test (RDT)"
DT$indicator[which(DT$indicator=='GP other-2 Nombre de SVS ayant recu le kit PEP dans les 72h')] <- "GP other-2: Nombre de SVS ayant recu le kit PEP dans les 72h"

# split apart code from indicator file
DT = DT[, c("indicator_code", "indicator_description", "indicator_misc") := tstrsplit(indicator, ": ", fixed=TRUE)]

# subset data to merge with guatemala
DT <- DT[,.(loc_name, indicator_code, indicator_type, indicator,
               disease,
               baseline_value, baseline_year, 
               target_n, target_d, `target_%`, target_year,
               lfa_result_n, lfa_result_d, `lfa_result_%`, lfa_result_achievement_ratio, 
               pudr_sheet, file_name,
               reverse_indicator,
               gf_result_n, gf_result_d, `gf_result_%`,
            gf_result_value, gf_result_achievement_ratio)]

#--------------------------------------
# essential data prep: GTM
#--------------------------------------
DT2 <- readRDS("J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/prepped_data/gtm_1A.rds")

# remove rows that are not necessary
DT2 <- DT2[indicator!="[Impact Indicator Name]"]
DT2 <- DT2[indicator!="[Outcome Indicator Name]"]

# remove duplicated column in DT
DT2 <- DT2[,unique(names(DT2)),with=FALSE]

# split apart the code
DT2 = DT2[, c("indicator_code", "indicator_description", "indicator_misc") := tstrsplit(indicator, ": ", fixed=TRUE)]


# keep only columns of interest
DT2 <- DT2[,.(loc_name, indicator_code, indicator_type, indicator,
                disease,
                baseline_value, baseline_year,
                target_n, target_d, `target_%`, target_year,
                lfa_result_n, lfa_result_d, `lfa_result_%`,
                pudr_sheet, file_name,
           gf_result_n, gf_result_d, `gf_result_%`, gf_result_verification_method)]

#--------------------------------------
# Merge all data together
#--------------------------------------
DTall <- rbind(DT, DT2, fill=TRUE)

# subset as appropriate, disease, latest pudrs etc, aggregated only, years of relevance
DTall <- DTall[file_name %in% c("SEN-Z-MOH PUDR (Juil-Dec18) LFA 19Avr19 MAJ 25apr19.xlsx", 
                                "GTM-T-MSPAS_Progress Report_LFA18Mar19.xlsx",
                                "LFA_Review_COD-T-MOH_Progress Report_30Jun2018_Sent_02102018_OK.xlsx",
                                "LFA Reviewed UGA-T-MOFPED PE 31 Dec 2019 (10 May 2019).xlsx",
                                # hiv grants
                                "SEN-H-CNLS PUDR (Juil-Dec18) LFA, 9Avr19.xlsx",
                                "SEN-H-ANCS_PUDR (Juil-Dec18) LFA, 15Mar18.xlsx",
                                "LFA Review_COD-H-MOH_Progress  Report_30Jun2018_07092018 ok_Sent 01102018.OK.xlsx",
                                "LFA Reviewed UGA-H-MoFPED PUDR PE 31 Dec 18  (10 May 19).xlsx",
                                # hiv/tb grants
                                "LFA Reviewed UGA-C-TASO PE 31Dec18.xlsx",
                                "COD-C-CORDAID_Progress Report Disbursement_31Dec2018_v18 01032019.xlsx",
                                # malaria grants
                                "SEN M PNLP PUDR (Juil-Dec18) LFA, 10Mai19.xlsx",
                                "Final LFA reviewed UGA-M-MoFPED PUDR.xlsx",
                                "Final LFA reviewed UGA-M-TASO PUDR 2 Oct 2018.xlsx",
                                "COD-M-SANRU PUDR 31122018_LFA5Apr2019_Finance8Apr2019_CTfull.xlsx",
                                "COD-M-MOH_Progress Report Disbursement_31Dec2018_v40_Vf.xlsx")]

DTall <- DTall[pudr_sheet %in% c("coverage_indicators_main", "impact_outcome_indicators_main")]

Dtall <- DTall[year %in% c('2018')]

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

############################################################
# generate our own ihme target and performance results and ratio
############################################################

DTall$ihme_target_n <- NA
DTall$ihme_target_n <- ifelse(is.na(DTall$`target_%`), DTall$target_n, DTall$`target_%`)

DTall$ihme_result_n <- NA
DTall$ihme_result_n <- ifelse(is.na(DTall$`lfa_result_%`), DTall$lfa_result_n, DTall$`lfa_result_%`)

# calculate ihme_results_achievement_ratio
DTall$ihme_result_achievement_ratio <-NA
DTall$ihme_target_n <- as.numeric(DTall$ihme_target_n)
DTall$ihme_result_n <- as.numeric(DTall$ihme_result_n)
DTall$ihme_result_achievement_ratio <- DTall$ihme_result_n/DTall$ihme_target_n

#######################
# Make plots
##########################

setwd('J:/Project/Evaluation/GF/special_assessments/pudr_framework_indicators/RT')
ggplot(DTall[disease=="hiv"], aes(x=indicator_code, y=ihme_result_achievement_ratio)) +
     geom_point(aes(color=loc_name)) +
  geom_line(group=loc_name)+
     labs(title="PUDR Performance") +
     coord_flip()+
  geom_hline(yintercept = 1) +
  theme_minimal()


# specific hivs
h<-ggplot(DTall[disease=="hiv"], aes(x=indicator_code, y=ihme_result_achievement_ratio, group=loc_name)) +
  geom_line(aes(color=loc_name))+
  geom_point(aes(color=loc_name))+
  geom_hline(yintercept = 1) +
  coord_flip()+
  labs(title = "PUDR Performance - HIV")
h
ggsave("hiv.jpeg")

# malaria
m<-ggplot(DTall[disease=="malaria"], aes(x=indicator_code, y=ihme_result_achievement_ratio, group=loc_name)) +
  geom_line(aes(color=loc_name))+
  geom_point(aes(color=loc_name))+
  geom_hline(yintercept = 1) +
  coord_flip()+
  labs(title = "PUDR Performance - malaria")
m
ggsave("malaria.jpeg")

# malaria 2 
m2<-ggplot(DTall[disease=="malaria"], aes(x=indicator_code, y=lfa_result_achievement_ratio, group=loc_name)) +
  geom_line(aes(color=loc_name))+
  geom_point(aes(color=loc_name))+
  geom_hline(yintercept = 1) +
  coord_flip()+
  labs(title = "PUDR Performance - malaria (LFA achievement ratio)")
m2
ggsave("malaria2.jpeg")

# hiv tb
ht<-ggplot(DTall[disease=="hiv/tb"], aes(x=indicator_code, y=ihme_result_achievement_ratio, group=loc_name)) +
  geom_line(aes(color=loc_name))+
  geom_point(aes(color=loc_name))+
  geom_hline(yintercept = 1) +
  coord_flip()+
  labs(title = "PUDR Performance - HIV/TB")
ht
ggsave("ht.jpeg")


