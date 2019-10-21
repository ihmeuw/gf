# --------------------------------------
# AUTHOR: Francisco Rios Casas
# PURPOSE: script that cleans PUDR Indicators Performance Framework Data for analyses
# DATE: Created 11Sept2019. Updated 14Oct2019.
# NOTE: this version assumes that all country data is stored in same file
# --------------------------------------

#----------------------------------------------
# TO-DO list for this code: 
# - Need to reshape wide based off of the most "final" indicator (PR, LFA, GF)
#----------------------------------------------

# read in COD, SEN, UGA data
DT <- readRDS("J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/prepped_data/all_prepped_data.rds") # EMILY - give data a more descriptive name. 

# for COD, SEN, UGA, GTM code----------------------------------------
# clean typos in the formatting (missing ampersands, missing colons, and missing Indicator Codes) # EMILY - is this all a UTF-8 issue? We shouldn't have any "?" in data.  
DT$indicator[which(DT$indicator=='M&amp;E-2: Proportion de rapports reçus des formations sanitaires  par rapport aux rapports attendus attendus au cours de la période de rapportage')] <- "M&E-2: Proportion de rapports reçus des formations sanitaires  par rapport aux rapports attendus attendus au cours de la période de rapportage"
DT$indicator[which(DT$indicator=="M&amp;E-1: Pourcentage d'entités déclarantes présentant leurs rapports dans les délais selon les directives nationales")] <- "M&E-1: Pourcentage d'entités déclarantes présentant leurs rapports dans les délais selon les directives nationales"
DT$indicator[which(DT$indicator=='M&amp;E-1: Percentage of HMIS or other routine reporting units submitting timely reports according to national guidelines')] <- "M&E-1: Percentage of HMIS or other routine reporting units submitting timely reports according to national guidelines"
DT$indicator[which(DT$indicator=='M&amp;E-2: Proportion of facility reports received over the reports expected during the reporting period')] <- "ME-2: Proportion of facility reports received over the reports expected during the reporting period"
DT$indicator[which(DT$indicator=='Nombre et pourcentage de structures de santé (Centres de Santé intégrés) qui rapporte une rupture de stock des tests de diagnostic rapide (TDR) malaria')] <- "RDT SO: Nombre et pourcentage de structures de santé (Centres de Santé intégrés) qui rapporte une rupture de stock des tests de diagnostic rapide (TDR) malaria"
DT$indicator[which(DT$indicator=='Number and percentage of health facilities (Integrated health facilities) reporting stock out of malaria rapid diagnostic test (RDT)')] <- "RDT SO: Number and percentage of health facilities (Integrated health facilities) reporting stock out of malaria rapid diagnostic test (RDT)"
DT$indicator[which(DT$indicator=='GP other-2 Nombre de SVS ayant recu le kit PEP dans les 72h')] <- "GP other-2: Nombre de SVS ayant recu le kit PEP dans les 72h"
DT$indicator[which(DT$indicator=="M&amp;E-2: Proporción de informes de centros de salud recibidos entre el total de informes previstos durante el período de reporte")] <- "M&E-2: Proporción de informes de centros de salud recibidos entre el total de informes previstos durante el período de reporte"

#EMILY - alternate way to do the section above. 
DT[, indicator:=gsub("&amp;", "&", indicator)]

# remove rows which are not necessary - #EMILY this may be a data extraction issue. 
DT <- DT[indicator!="[Impact Indicator Name]"]
DT <- DT[indicator!="[Outcome Indicator Name]"]
DT <- DT[indicator!="0"]

# clean special characters, blanks, and NAs # EMILY- how are you systematically testing for this? If new data comes in which has these issues, how will you catch it? 
#EMILY - can you make more of this a loop? Systematically replace NAs because you've got the same command at least 8 times here. 
DT$baseline_value[which(DT$baseline_value=="N/A")] <- NA
DT$baseline_value[which(DT$baseline_value=="ND")] <- NA
DT$baseline_value <- gsub("/100,000","",DT$baseline_value)
DT$baseline_value[which(DT$baseline_value=="16,121,172")] <- "16121172"

DT$baseline_value <- gsub(",",".",DT$baseline_value)
DT$baseline_value <- gsub("%","",DT$baseline_value)
DT$baseline_value <- gsub("‰","",DT$baseline_value)
DT$baseline_value <- gsub("???","",DT$baseline_value)

# pr 
DT$pr_result_n[which(DT$pr_result_n=="ND")]<- NA
DT$pr_result_d[which(DT$pr_result_d=="ND")]<- NA

DT$`pr_result_%`[which(DT$`pr_result_%`=="ND")]<- NA
DT$pr_result_achievement_ratio[which(DT$pr_result_achievement_ratio==" ")] <- NA

# lfa
DT$lfa_result_n[which(DT$lfa_result_n=="ND")]<- NA
DT$lfa_result_d[which(DT$lfa_result_d=="ND")]<- NA

DT$`lfa_result_%`[which(DT$`lfa_result_%`==" ")] <- NA
DT$`lfa_result_%` <- gsub(",",".",DT$`lfa_result_%`)
DT$`lfa_result_%` <- gsub("%","",DT$`lfa_result_%`)

# gf
DT$gf_result_n[which(DT$gf_result_n=="qu ")] <- NA
DT$gf_result_n[which(DT$gf_result_n=="i")] <- NA

DT$gf_result_achievement_ratio[which(DT$gf_result_achievement_ratio==" ")] <- NA

# Emily - you can think about rewriting this as a loop. 
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
# EMILY - how will you know that a class is wrong? 
# EMILY - rewrite this as a loop. 
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

# gather any_achievement_ratio
DT$any_achievement_ratio <- NA
DT$any_achievement_ratio <-ifelse(is.na(DT$gf_result_achievement_ratio),
                                  ifelse(is.na(DT$lfa_result_achievement_ratio), 
                                         ifelse(is.na(DT$pr_result_achievement_ratio), NA, 
                                                DT$pr_result_achievement_ratio), 
                                         DT$lfa_result_achievement_ratio),
                                  DT$gf_result_achievement_ratio)

# verify achievement_ratios
DT$ihme_target_n <- NA
DT$ihme_target_n <- ifelse(is.na(DT$`target_%`), DT$target_n, DT$`target_%`) #EMILY does the % symbol not allow you to type it as a variable name without ticks? That's an easy fix

DT$ihme_result_n <- NA
DT$ihme_result_n <- ifelse(is.na(DT$`lfa_result_%`), DT$lfa_result_n, DT$`lfa_result_%`)

# calculate ihme_results_achievement_ratio
DT$ihme_result_achievement_ratio <-NA
DT$ihme_target_n <- as.numeric(DT$ihme_target_n)
DT$ihme_result_n <- as.numeric(DT$ihme_result_n)
DT$ihme_result_achievement_ratio <- DT$ihme_result_n/DT$ihme_target_n

# create variable with indicator code (makes cross--country comparisons easier) can merge short description using this code later
DT = DT[, c("indicator_code", "indicator_description", "indicator_misc", "indicator_misc2") := tstrsplit(indicator, ": ", fixed=TRUE)]
DT = DT[,c("indicator_description", "indicator_misc", "indicator_misc2"):=NULL]

# read and merge codebook to standardize names
codebook <- fread("C:/Users/frc2/Documents/gf/special_assessments/synthesis/2019_multicountry_analyses/indicators_codebook.csv") 
# EMILY use a path built up from global variables. 
DT1 <- merge(DT, codebook, by="indicator_code", all.x = TRUE)

# clean and standardize data source names across PUDRs
# create spreadsheet of data source names to standardize
#DTtemp <- data.table(unique(DT1$baseline_source))
#DTtemp <- rbind(DTtemp, data.table(unique(DT1$pr_result_source)))
#DTtemp <- rbind(DTtemp, data.table(unique(DT1$lfa_result_source)))
#write.csv(DTtemp, "C:/Users/frc2/Documents/gf/special_assessments/synthesis/2019_multicountry_analyses/data_source_codebook_original.csv")

# read and merge codebook to standardize data source names
# pending

# reverse country codes


#------------------------------------------------------
# SAVE FINAL DATA
# -----------------------------------------------------

saveRDS(DT1, "C:/Users/frc2/Documents/Data/cleaned_pfi.RDS") # EMILY - this should be saved on the J:drive - we never want to save data to our local disks. 

