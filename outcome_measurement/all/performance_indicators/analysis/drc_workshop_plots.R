# make visualizations on each of the grant performance indicators
# for DRC
# OCt 28, 2019
# Francisco

library(data.table)
library(ggplot2)
library(knitr)
library(dplyr)
library(kableExtra)
library(scales)

# set parameters of data to be analyzed
country = "cod"
main_indicators = c('impact_outcome_indicators_main', 'coverage_indicators_main')
recent_pudrs = c('Malaria_MOH_PUDR_S1 2019_LFA verified.xlsx',
                 'Malaria_SANRU_PUDR S1 2019_LFA verified.xlsx',
                 'CORDAID_PUDR_S1 2019_not verified.xlsx',
                 'HIV_MOH_PUDR_S1 2019_not verified.xlsx',
                 'Copy of LFA_COD-T-MOH_Progress Report_30Jun2019_CCF_Final_10092019.xlsx')

# load data
data <- readRDS("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/cleaned_data/kpi_data_for_analyses.RDS")

# subset as appropriate
DT <- data
DT = DT[loc_name==country]
DT = DT[pudr_sheet %in% main_indicators]
DT = DT[file_name %in% recent_pudrs]

grants = unique(DT$grant)
plots = list()
i=1
for(g in grants) {
  plots[[i]] = ggplot(DT[grant==g], aes(x=brief_description_code, y=ihme_result_achievement_ratio)) +
    geom_point() +
    labs(title="", y="Achievement Ratio", x="Indicator", caption=paste0("Source: ", unique(DT[grant==g]$file_name))) +
    geom_hline(yintercept = 1) +
    theme_bw()+
    coord_flip()+
    ylim(0,1.5)+
    theme(legend.position = "bottom")+
    theme_bw()
    ggsave(paste0("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/visualizations/drc_wokshop_plots/plot",i,".jpeg"), width=8, height = 4)
  i=i+1
}