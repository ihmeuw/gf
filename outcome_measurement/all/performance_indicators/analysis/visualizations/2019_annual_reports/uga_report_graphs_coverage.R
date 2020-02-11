# make visualizations on each of the grant performance indicators
# for UGA
# while creating a summary visual 
# using lfa verified values
# OCt 28, 2019
# Francisco

library(data.table)
library(ggplot2)
library(knitr)
library(dplyr)
library(kableExtra)
library(scales)

# load data
data <- readRDS("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/prepped_data/cleaned_pfi.rds")

# set parameters of data to be analyzed
country = "uga"
main_indicators = c('coverage_indicators_main')

# subset as appropriate
DT <- data
DT = DT[loc_name==country & pudr_sheet %in% main_indicators]


# recode semester period for each variable
DT$semester <- NA
DT$semester[which(DT$start_date_programmatic=="2018-01-01" & DT$end_date_programmatic=="2018-06-30")] <- 1
DT$semester[which(DT$start_date_programmatic=="2018-07-01" & DT$end_date_programmatic=="2018-12-31")] <- 2
DT$semester[which(DT$start_date_programmatic=="2019-01-01" & DT$end_date_programmatic=="2019-06-30")] <- 3

# flip coding on reverse indicators
#DT$achievement_ratio_final <- ifelse(DT$reverse_indicator_final=="yes", 1/DT$ihme_result_achievement_ratio, DT$ihme_result_achievement_ratio)

# for semester 2 and 3 or the UGA-M-TASO grant replace the achievement_ratio_final with lfa_achievement_ratio
#DT$achievement_ratio_final[which(DT$grant=="UGA-M-TASO" & DT$semester==1)] <- NA
#DT$achievement_ratio_final[which(DT$grant=="UGA-M-TASO" & DT$semester==1)] <- as.numeric(DT$lfa_result_achievement_ratio)

#DT[grant=="UGA-M-TASO" & semester==1,.(achievement_ratio_final=)]

#DT$achievement_ratio_final[which(DT$grant=="UGA-M-TASO" & DT$semester==2)] <- NA
#DT$achievement_ratio_final[which(DT$grant=="UGA-M-TASO" & DT$semester==2)] <- DT$lfa_result_achievement_ratio

# make sure str is of type numeric
DT$lfa_result_achievement_ratio <- as.numeric(DT$lfa_result_achievement_ratio)

# subset data
dt_subset <- DT[,.(grant, indicator_code, semester, lfa_result_achievement_ratio)]


# calculate average achievement by grant semester
dt_subset <- dt_subset[, .(avg_ach_ratio= mean(lfa_result_achievement_ratio, na.rm = TRUE)),
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

outputFile = "J:\\Project\\Evaluation\\GF\\outcome_measurement\\multi_country\\performance_indicators\\pudr_indicator_extraction\\visualizations\\uga_report_table_coverage_indic.csv"
write.csv(table1, outputFile)

pdf("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/visualizations/ugan_report_achievement_cov_indic.pdf", width = 9, height = 6)
a
dev.off()