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

# load data
data <- readRDS("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/data_for_analysis/kpi_data_for_analyses2.RDS")

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

# make graph of dt_subset
a <- ggplot(dt_subset, aes(y=avg_ach_ratio, x=semester, group=grant))+
  geom_line(aes(color=grant))+
  geom_point(aes(color=grant))+
  theme_bw(base_size = 18)+
  labs(y="Average Achievement Ratio",
       x="Semester")+
  scale_x_continuous(limits = c(1,3), breaks = c(1,2,3))

outputFile = "J:\\Project\\Evaluation\\GF\\outcome_measurement\\multi_country\\performance_indicators\\pudr_indicator_extraction\\visualizations\\sen_report_table_coverage_indic.csv"
write.csv(table1, outputFile)

pdf("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/visualizations/sen_report_achievement_cov_indic.pdf", width = 9, height = 6)
a
dev.off()