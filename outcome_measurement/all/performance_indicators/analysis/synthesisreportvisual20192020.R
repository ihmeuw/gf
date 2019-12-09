# synthesis report visual for SO1

# read in data
library(data.table)
library(ggplot2)

data <-fread("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/data_for_analysis/cross_consortia_for_synthesis_pf_indicators_25Nov2019.csv")

# rename column names
setnames(data, 
         old = names(data),
         new = c('loc_name', 'grant', 'indicator_long', 'achievement_ratio', 'reporting_period'))

# parameters to edit
graphic_name = "cross_country_comparisons_20192020_synthesis_report.png"
codebook_name = "module_code_map.csv"

# split data indicator codes
data = data[, indicator_code:=tstrsplit(indicator_long, ":", keep=1)]

# merge with the correct module code and the correct indicator type
codebook <- fread(paste0("C:/Users/frc2/Documents/gf/outcome_measurement/all/performance_indicators/codebooks/", codebook_name))
data <- merge(data, codebook, by.x='indicator_code', by.y="indicator_code", all.x = TRUE, all.y=FALSE)

# merge on information on whether this is a reverse indicator
reverse_codebook <- fread("C:\\Users\\frc2\\Documents\\gf\\outcome_measurement\\all\\performance_indicators\\codebooks\\indicators_codebook_reverse.csv")
data <- merge(data, reverse_codebook, by.x='indicator_code', by.y='indicator_code', all.x = TRUE, all.y = FALSE)

# set parameters of data to be analyzed
date = c('s1_2019')

# subset as appropriate
DT <- data
DT <- DT[reporting_period==date]
DT <- DT[,.(loc_name, grant, indicator_code, indicator_long, achievement_ratio, reverse_indicator_final, module_code)]

#####################################################################
# collapse countries/grant where module_code/intervention_code is the same
########################################################################
# re-code variables that are reverse indicators
DT$achievement_ratio_final <- ifelse(DT$reverse_indicator_final=="yes", 1/DT$achievement_ratio, DT$achievement_ratio)

dt_subset <- DT[, .(module_code, indicator_code, achievement_ratio_final)]
dt_subset <- dt_subset[!is.na(achievement_ratio_final)]
dt_subset <- dt_subset[, .(avg_ach_ratio= mean(achievement_ratio_final),
                           max_ach_ratio= max(achievement_ratio_final),
                           min_ach_ratio= min(achievement_ratio_final)),
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
  scale_size_continuous(breaks= c(1, 2, 3, 4, 5, 6, 7, 8), range=c(0.5,1.5), name = "Number of \ncountries reporting") + 
  ggtitle(paste0("Average achievement ratios of performance indicators by module")) +
  ylab("Achievement ratio") + xlab("Module")  +
  labs(caption = "Bars represent the max and min and points are the mean by module-intervention across countries and grants.
       Note: Where the max, mean, or min for a given module-intervention was higher than 2, it was changed to 2 for clarity in this figure.") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=16), legend.title=element_text(size = 14), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=12),
        axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  geom_hline(yintercept=1, linetype="dashed", color="grey", alpha=0.6, size=2)

outputFile = paste0("J:\\Project\\Evaluation\\GF\\outcome_measurement\\multi_country\\performance_indicators\\pudr_indicator_extraction\\visualizations\\",graphic_name)
ggsave(outputFile, height = 8, width = 11)


