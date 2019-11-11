# plotting data for synthesis report
# Francisco, based on code from Audrey Batzel
# this plots only the most recent 2019 s1 PUDRS
# 11/07/2019

# set-up
library(data.table)
library(ggplot2)

# read data
data <- readRDS("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/cleaned_data/kpi_data_for_analyses2.RDS")

# merge with the correct module code and the correct indicator type
codebook <- fread("C:/Users/frc2/Documents/gf/outcome_measurement/all/performance_indicators/codebooks/module_code_map.csv")
data <- merge(data, codebook, by.x='indicator_code', by.y="indicator_code", all.x = TRUE, all.y=FALSE)

# add reverse indicator variable
#reverse_codebook <- fread("C:/Users/frc2/Documents/gf/outcome_measurement/all/performance_indicators/codebooks/indicators_codebook_reverse.csv")
#reverse_codebook = reverse_codebook[,.(indicator_code, reverse_indicator_final)]
#data <- merge(data, reverse_codebook, by.x="indicator_code", by.y = "indicator_code", all.x = TRUE, all.y = FALSE)

# set parameters of data to be analyzed
main_indicators = c('impact_outcome_indicators_main', 'coverage_indicators_main')
beginning = "2019-01-01"
end = "2019-06-30"

# most_recent_pudrs = c('SEN-Z-MOH_Progress  Report_30Jun2019   02 09 2019.xlsx', 
#                       'SEN-M-PNLP_Progress Report_S1 2019 Version finale du 15 Aout 2019.xlsx', 
#                       'PU-SEN-H-CNLS-S1-2019_15082019_finale.xlsx', 
#                       'SEN H ANCS PU (Jan-Juin19), LFA 5Sept19.xlsm',
#                       'Final LFA reviewed UGA-M-MoFPED PUDR.xlsx',
#                       'UGA-M-TASO PUDR Jul-Dec18.xlsx',
#                       'UGA-H-MoFPED Progress Update Report Jan-Jun 2019.xlsx',
#                       'LFA Reviewed UGA-C-TASO PE 31Dec18.xlsx',
#                       'LFA Reviewed UGA-T-MOFPED PE 31 Dec 2019 (10 May 2019).xlsx',
#                       'Malaria_MOH_PUDR_S1 2019_LFA verified.xlsx',
#                       'Malaria_SANRU_PUDR S1 2019_LFA verified.xlsx',
#                       'CORDAID_PUDR_S1 2019_not verified.xlsx',
#                       'HIV_MOH_PUDR_S1 2019_not verified.xlsx',
#                       'Copy of LFA_COD-T-MOH_Progress Report_30Jun2019_CCF_Final_10092019.xlsx',
#                       'GTM-H-HIVOS_Progress Report_31Dec2018_RV_SO_YAV_120219_LFA REVIEW Reviewed FS.xlsx',
#                       'GTM-M-MSPAS_PU_FINAL_fuentes_indi_corregidos.xlsx',
#                       'GTM-T-MSPAS_Progress Report_LFA18Mar19.xlsx')

# subset as appropriate
DT <- data
DT <- DT[pudr_sheet %in% main_indicators & start_date_programmatic==beginning & end_date_programmatic==end]
DT <- DT[,.(loc_name, grant, disease, module_code, indicator_code, brief_description_code, target_value, any_result_value, ihme_result_achievement_ratio, reverse_indicator_final)]


#####################################################################
# collapse countries/grant where module_code/intervention_code is the same
########################################################################
# re-code variables that are reverse indicators
DT$ihme_result_achievement_ratio <- ifelse(DT$reverse_indicator_final=="yes", DT$target_value/DT$any_result_value, DT$ihme_result_achievement_ratio)

dt_subset <- DT[, .(module_code, indicator_code, ihme_result_achievement_ratio)]
dt_subset <- dt_subset[!is.na(ihme_result_achievement_ratio)]
dt_subset <- dt_subset[, .(avg_ach_ratio= mean(ihme_result_achievement_ratio),
                           max_ach_ratio= max(ihme_result_achievement_ratio),
                           min_ach_ratio= min(ihme_result_achievement_ratio)),
                       by=c("module_code", "indicator_code")]
dt_subset[max_ach_ratio>2, max_ach_ratio := 2]
dt_subset[avg_ach_ratio>2, avg_ach_ratio := 2]
dt_subset[min_ach_ratio>2, min_ach_ratio := 2]

dt_subset[, kpi_code:= paste(module_code, indicator_code)]

no_country_per_indicator <- unique(DT[, .(loc_name, module_code, indicator_code)])
no_country_per_indicator <- no_country_per_indicator[, .(no_observ = .N), by=c("module_code", "indicator_code")]

dt_subset <- merge(dt_subset, no_country_per_indicator, by=c("module_code", "indicator_code"))


ggplot(dt_subset[], aes(x=module_code, y=avg_ach_ratio, color=module_code, size = no_observ)) + theme_bw()+
  geom_pointrange( aes(ymin= min_ach_ratio, ymax= max_ach_ratio), position= position_jitter(width = 0.40), shape = 21, fill = "white") + guides(color=FALSE) +
  scale_size_continuous(breaks= c(1, 2, 3, 4, 5), range=c(0.5,1.5), name = "Number of \ncountries reporting") + 
  ggtitle(paste0("Average achievement ratios of performance indicators by module")) +
  ylab("Achievement ratio") + xlab("Module")  +
  labs(caption = "Bars represent the max and min and points are the mean by module-intervention across countries and grants.
       Note: Where the max, mean, or min for a given module-intervention was higher than 2, it was changed to 2 for clarity in this figure.") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=16), legend.title=element_text(size = 14), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=12),
        axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  geom_hline(yintercept=1, linetype="dashed", color="grey", alpha=0.6, size=2)

outputFile = "J:\\Project\\Evaluation\\GF\\outcome_measurement\\multi_country\\performance_indicators\\pudr_indicator_extraction\\visualizations\\cross_country_comparisons.jpeg"
ggsave(outputFile, height = 8, width = 11)


