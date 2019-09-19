# --------------------------------------
# AUTHOR: Francisco Rios Casas
# PURPOSE: script that cleans PUDR Indicators Performance Framework Data for analyses
# DATE: Created 11Sept2019. 
# NOTE: this assumes that Guatemala-TB data is kept in a different file than the other indicators for the rest of the countries which are combined
# --------------------------------------

# set up
library(data.table)

# read in COD, SEN, UGA data
DT <- readRDS("J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/prepped_data/all_prepped_data.rds")

# read in GTM data
DT2 <- readRDS("J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/prepped_data/gtm_1A.rds")

# for COD, SEN, UGA code----------------------------------------
# clean typos in the formatting (missing ampersands, missing colons, and missing Indicator Codes)
DT$indicator[which(DT$indicator=='M&amp;E-2: Proportion de rapports reçus des formations sanitaires  par rapport aux rapports attendus attendus au cours de la période de rapportage')] <- "M&E-2: Proportion de rapports reçus des formations sanitaires  par rapport aux rapports attendus attendus au cours de la période de rapportage"
DT$indicator[which(DT$indicator=="M&amp;E-1: Pourcentage d'entités déclarantes présentant leurs rapports dans les délais selon les directives nationales")] <- "M&E-1: Pourcentage d'entités déclarantes présentant leurs rapports dans les délais selon les directives nationales"
DT$indicator[which(DT$indicator=='M&amp;E-1: Percentage of HMIS or other routine reporting units submitting timely reports according to national guidelines')] <- "M&E-1: Percentage of HMIS or other routine reporting units submitting timely reports according to national guidelines"
DT$indicator[which(DT$indicator=='M&amp;E-2: Proportion of facility reports received over the reports expected during the reporting period')] <- "ME-2: Proportion of facility reports received over the reports expected during the reporting period"
DT$indicator[which(DT$indicator=='Nombre et pourcentage de structures de santé (Centres de Santé intégrés) qui rapporte une rupture de stock des tests de diagnostic rapide (TDR) malaria')] <- "RDT SO: Nombre et pourcentage de structures de santé (Centres de Santé intégrés) qui rapporte une rupture de stock des tests de diagnostic rapide (TDR) malaria"
DT$indicator[which(DT$indicator=='Number and percentage of health facilities (Integrated health facilities) reporting stock out of malaria rapid diagnostic test (RDT)')] <- "RDT SO: Number and percentage of health facilities (Integrated health facilities) reporting stock out of malaria rapid diagnostic test (RDT)"
DT$indicator[which(DT$indicator=='GP other-2 Nombre de SVS ayant recu le kit PEP dans les 72h')] <- "GP other-2: Nombre de SVS ayant recu le kit PEP dans les 72h"

# clean special characters, blanks, and NAs
DT$baseline_value[which(DT$baseline_value=="16,121,172")] <- "16121172"
DT$baseline_value[which(DT$baseline_value=="N/A")] <- NA
DT$baseline_value <- gsub(",",".",DT$baseline_value)
DT$baseline_value <- gsub("%","",DT$baseline_value)

DT$`lfa_result_%`[which(DT$`lfa_result_%`==" ")] <- NA
DT$`lfa_result_%` <- gsub(",",".",DT$`lfa_result_%`)
DT$`lfa_result_%` <- gsub("%","",DT$`lfa_result_%`)

DT$pr_result_achievement_ratio[which(DT$pr_result_achievement_ratio==" ")] <- NA
DT$gf_result_achievement_ratio[which(DT$gf_result_achievement_ratio==" ")] <- NA

DT$pr_result_n[which(DT$pr_result_n=="ND")]<- NA
DT$pr_result_d[which(DT$pr_result_d=="ND")]<- NA
DT$`pr_result_%`[which(DT$`pr_result_%`=="ND")]<- NA
DT$lfa_result_n[which(DT$lfa_result_n=="ND")]<- NA
DT$lfa_result_d[which(DT$lfa_result_d=="ND")]<- NA

# fix variables' "class"
DT$baseline_value <- as.numeric(DT$baseline_value)

DT$lfa_result_achievement_ratio <- as.numeric(DT$lfa_result_achievement_ratio)
DT$pr_result_achievement_ratio <- as.numeric(DT$pr_result_achievement_ratio)
DT$gf_result_achievement_ratio <- as.numeric(DT$gf_result_achievement_ratio)

DT$target_n <- as.numeric(DT$target_n)
DT$target_d <- as.numeric(DT$target_d)
DT$`target_%` <- as.numeric(DT$`target_%`)

DT$pr_result_n <- as.numeric(DT$pr_result_n)
DT$pr_result_d <- as.numeric(DT$pr_result_d)
DT$`pr_result_%` <- as.numeric(DT$`pr_result_%`)

DT$lfa_result_n <- as.numeric(DT$lfa_result_n)
DT$lfa_result_d <- as.numeric(DT$lfa_result_d)
DT$`lfa_result_%` <- as.numeric(DT$`lfa_result_%`)

DT$gf_result_n <- as.numeric(DT$gf_result_n)
DT$gf_result_d <- as.numeric(DT$gf_result_d)
DT$`gf_result_%` <- as.numeric(DT$`gf_result_%`)



# fix date variables
# might be done in other stage?

# create variable with indicator code (makes cross--country comparisons easier) can merge short description using this code later
DT = DT[, c("indicator_code", "indicator_description", "indicator_misc") := tstrsplit(indicator, ": ", fixed=TRUE)]
DT = DT[,c("indicator_description", "indicator_misc"):=NULL]

##########################################################
# for GTM - TB --------------------------------
##########################################################

# remove duplicated column titled "pr_result_verification_method" 
DT2 <- DT2[,unique(names(DT2)),with=FALSE]

# remove unnecessary rows in the Guatemala data which are mostly blank
DT2 <- DT2[indicator!="[Impact Indicator Name]"]
DT2 <- DT2[indicator!="[Outcome Indicator Name]"]

# remove percent signs
DT2$baseline_value <- gsub("%","",DT2$baseline_value)
DT2$baseline_value <- gsub("/100,000","",DT2$baseline_value)

# remove special characters and text
DT2$gf_result_n[which(DT2$gf_result_n=="qu ")] <- NA

# fix variables' "class"
DT2$baseline_value <- as.numeric(DT2$baseline_value)

DT2$lfa_result_achievement_ratio <- as.numeric(DT2$lfa_result_achievement_ratio)

DT2$target_n <- as.numeric(DT2$target_n)
DT2$target_d <- as.numeric(DT2$target_d)
DT2$`target_%` <- as.numeric(DT2$`target_%`)

DT2$pr_result_n <- as.numeric(DT2$pr_result_n)
DT2$pr_result_d <- as.numeric(DT2$pr_result_d)
DT2$`pr_result_%` <- as.numeric(DT2$`pr_result_%`)

DT2$lfa_result_n <- as.numeric(DT2$lfa_result_n)
DT2$lfa_result_d <- as.numeric(DT2$lfa_result_d)
DT2$`lfa_result_%` <- as.numeric(DT2$`lfa_result_%`)

DT2$gf_result_n <- as.numeric(DT2$gf_result_n)
DT2$gf_result_d <- as.numeric(DT2$gf_result_d)
DT2$`gf_result_%` <- as.numeric(DT2$`gf_result_%`)

# fix target_year in Guatemala
DT2$target_year <- as.numeric(DT2$target_year)

# Indicate if it's a reverse indicator
DT2$reverse_indicator <- NA
DT2$reverse_indicator[which(DT2$indicator_code=="TB I-2")] <- "Yes"
DT2$reverse_indicator[which(DT2$indicator_code=="TB I-3(M)")] <- "Yes"
DT2$reverse_indicator[which(DT2$indicator_code=="TB/HIV I-1")] <- "Yes"
DT2$reverse_indicator[which(DT2$indicator_code=="TB I-4(M)")] <- "Yes"

# keep only most recent data from 2018 in Guatemala
DT2 <- DT2[target_year==2018]

# fix date variables
class(DT2$end_date_programmatic) <- class(DT$end_date_programmatic)
# this leads to incorrect dates as it is but necessary for rbind to work properly
# might be done in other stage?

# merge on codebook
# pending

# create variable with indicator code (makes cross--country comparisons easier) can merge short description using this code later
DT2 = DT2[, c("indicator_code", "indicator_description", "indicator_misc") := tstrsplit(indicator, ": ", fixed=TRUE)]
DT2 = DT2[,c("indicator_description", "indicator_misc"):=NULL]

#------------------------------------------------------
# SAVE FINAL DATA
# -----------------------------------------------------
l = list(DT, DT2)
merge_file <- rbindlist(l, use.names = TRUE, fill= TRUE)

outputFile4 <- "C:/Users/frc2/Documents/data/pudr/cleaned_data_16Sept2019.rds"
saveRDS(merge_file, outputFile4)

