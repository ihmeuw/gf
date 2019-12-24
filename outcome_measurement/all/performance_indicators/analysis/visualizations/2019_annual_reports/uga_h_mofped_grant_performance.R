# UGA-H-MoFPED Grant Performance

# set-up
library(data.table)
library(ggplot2)
library(knitr)
library(dplyr)
library(kableExtra)
library(scales)

# set parameters of data to be analyzed
country = "uga"
main_indicators = c('impact_outcome_indicators_main', 'coverage_indicators_main')
beginning = "2019-01-01"
end = "2019-06-30"

# load data
data <- readRDS("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/cleaned_data/kpi_data_for_analyses2.RDS")

# subset data
DT <- data
DT = DT[loc_name==country & pudr_sheet %in% main_indicators & start_date_programmatic==beginning & end_date_programmatic==end]

# make two plots: one for impact and one for coverage indicators
DT[ihme_result_achievement_ratio>2.0, ihme_result_achievement_ratio:=2.0]

grants = unique(DT$grant)

plot1 = ggplot(DT[grant=="UGA-H-MoFPED" & pudr_sheet=="impact_outcome_indicators_main"], aes(x=brief_description_code, y=ihme_result_achievement_ratio, col=target_met)) +
  geom_point() +
  labs(col="Target Met", title="", y="Achievement Ratio", x="Indicator", caption=paste0("Source: ", unique(DT[grant=="UGA-H-MoFPED"]$file_name))) +
  geom_hline(yintercept = 1) +
  theme_bw(base_size = 18)+
  coord_flip()+
  ylim(0,1.5)+
  theme(legend.position = "right")

plot2 = ggplot(DT[grant=="UGA-H-MoFPED"], aes(x=brief_description_code, y=ihme_result_achievement_ratio, col=target_met)) +
    geom_point() +
    labs(col="Target Met", title="", y="Achievement Ratio", x="Indicator", caption=paste0("Source: ", unique(DT[grant=="UGA-H-MoFPED"]$file_name))) +
    geom_hline(yintercept = 1) +
    theme_bw(base_size = 18)+
    coord_flip()+
    ylim(0,1.5)+
    theme(legend.position = "right")

plot3 = ggplot(DT[grant=="UGA-H-MoFPED"], aes(x=brief_description_code, y=ihme_result_achievement_ratio, col=target_met)) +
  geom_point() +
  labs(col="Target Met", title="", y="Achievement Ratio", x="Indicator", caption=paste0("Source: ", unique(DT[grant=="UGA-H-MoFPED"]$file_name))) +
  geom_hline(yintercept = 1) +
  theme_bw(base_size = 18)+
  coord_flip()+
  ylim(0,1.5)+
  theme(legend.position = "right")

pdf('J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/visualizations/u_h_mofped_indicators.pdf',
    width = 12, height = 9)
plot1
plot2
dev.off()
