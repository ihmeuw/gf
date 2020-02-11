 # make visualizations on each of the grant performance indicators
# for Sen
# while creating a summary visual 
# OCt 28, 2019
# Francisco

library(data.table)
library(ggplot2)
library(knitr)
library(dplyr)
library(kableExtra)
library(scales)
library(colormap)

# load data
data <- readRDS("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/prepped_data/cleaned_pfi.RDS")

# set parameters of data to be analyzed
country = "sen"
main_indicators = c('coverage_indicators_main')

# subset as appropriate
DT <- data
DT = DT[loc_name==country & pudr_sheet %in% main_indicators]

# flip coding on reverse indicators
DT$achievement_ratio_final <- ifelse(DT$reverse_indicator_final=="yes", 1/DT$ihme_result_achievement_ratio, DT$ihme_result_achievement_ratio)

# recode semester period for each variable
DT$semester <- NA
DT$semester[which(DT$start_date_programmatic=="2018-01-01" & DT$end_date_programmatic=="2018-06-30")] <- 1
DT$semester[which(DT$start_date_programmatic=="2018-07-01" & DT$end_date_programmatic=="2018-12-31")] <- 2
DT$semester[which(DT$start_date_programmatic=="2019-01-01" & DT$end_date_programmatic=="2019-06-30")] <- 3

# subset data
dt_subset <- DT[,.(grant, indicator_code, semester, ihme_result_achievement_ratio, achievement_ratio_final)]

# calculate average achievement by grant semester
dt_subset <- dt_subset[, .(avg_ach_ratio= mean(achievement_ratio_final, na.rm = TRUE)),
                       by=c("grant", "semester")]

# reshape wide
table1 <- dcast(dt_subset, grant ~ semester)

# visualization settings
# set colors
colors = c('SEN-H-CNLS'=colormap()[1], 'SEN-H-ANCS'=colormap()[71] , 'SEN-M-PNLP'=colormap()[30], 'SEN-Z-MOH'=colormap()[61])

# make graph of dt_subset
a <- ggplot(dt_subset, aes(y=avg_ach_ratio, x=semester, group=grant))+
  geom_line(size=1.5, aes(color=grant))+
  theme_bw(base_size = 18)+
  labs(y="Average Achievement Ratio",
       x="Semester")+
  scale_x_continuous(limits = c(1,3), breaks = c(1,2,3)) +
  scale_y_continuous(limits = c(0,1.25), breaks = c(0.25, 0.5, 0.75, 1.00, 1.25))+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        plot.title=element_text(size=11), axis.title.y=element_text(size=11), plot.caption=element_text(size=8))+
  scale_color_manual('', values=colors)

outputFile = "J:\\Project\\Evaluation\\GF\\outcome_measurement\\multi_country\\performance_indicators\\pudr_indicator_extraction\\visualizations\\sen_report_table_coverage_indic.csv"
write.csv(table1, outputFile)

pdf("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/analysis/visualizations/2019_annual_reports/sen_report_achievement_cov_indic.pdf", width = 9, height = 6)
a
dev.off()
