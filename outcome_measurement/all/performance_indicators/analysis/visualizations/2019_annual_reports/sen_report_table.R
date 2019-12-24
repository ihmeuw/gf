# make visualizations on each of the grant performance indicators
# for DRC
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
data <- readRDS("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/cleaned_data/kpi_data_for_analyses.RDS")

# set parameters of data to be analyzed
country = "sen"
main_indicators = c('impact_outcome_indicators_main', 'coverage_indicators_main')

# subset as appropriate
DT <- data
DT = DT[loc_name==country & pudr_sheet %in% main_indicators]

# factor target_met
DT$target_met[which(DT$target_met=="yes")] <-1
DT$target_met[which(DT$target_met=="no")] <- 0
DT$target_met <- as.numeric(DT$target_met)

# table1 <- DT[,.(percent_met=sum(target_met, na.rm=TRUE)/length(indicator_code), by=c('grant'))]

table2 <-DT[,list(indicators=length(indicator_code), indicators_reported=sum(!is.na(ihme_result_achievement_ratio)), total_met=sum(target_met, na.rm = TRUE)), by=c('grant', 'start_date_programmatic', 'end_date_programmatic')]
table3 <-table2[,.(grant, start_date_programmatic, end_date_programmatic, indicators, indicators_reported, total_met, percent_met_all=total_met/indicators, percent_met_reported=total_met/indicators_reported)]

# add new variable for the PUDR
table3$reporting_period <- NA
table3$reporting_period[which(table3$start_date_programmatic=="2019-01-01" & table3$end_date_programmatic=="2019-06-30")] <- "2019 S1"
table3$reporting_period[which(table3$start_date_programmatic=="2018-07-01" & table3$end_date_programmatic=="2018-12-31")] <- "2018 S2"
table3$reporting_period[which(table3$start_date_programmatic=="2018-01-01" & table3$end_date_programmatic=="2018-06-30")] <- "2018 S1"


table4 <- table3[,.(grant, reporting_period, percent_met_reported)]

table5 <- melt(table4, id.vars = c('grant', 'reporting_period'))
table6 <- dcast(table5, grant ~ reporting_period)

# change names to french
setnames(table6, 
         old = c("grant"),
         new = c("Subvention"))

# round values
cols <- names(table6)[2:4]
table6[,(cols) := round(.SD,2), .SDcols=cols]
         

outputFile = "J:\\Project\\Evaluation\\GF\\outcome_measurement\\multi_country\\performance_indicators\\pudr_indicator_extraction\\visualizations\\sen_report_table.csv"
write.csv(table6, outputFile)
