# make visualizations on each of the grant performance indicators
# for Uganda
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
data <- readRDS("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/prepped_data/cleaned_pfi.RDS")

# set parameters of data to be analyzed
country = "uga"
main_indicators = c('coverage_indicators_main') # updated to only include coverage indicators

# subset as appropriate
DT <- data
DT = DT[loc_name==country & pudr_sheet %in% main_indicators]

# factor target_met
DT$target_met[which(DT$target_met=="yes")] <-1
DT$target_met[which(DT$target_met=="no")] <- 0
DT$target_met <- as.numeric(DT$target_met)

table2 <-DT[,list(indicators=length(indicator_code), indicators_reported=sum(!is.na(ihme_result_achievement_ratio)), total_met=sum(target_met, na.rm = TRUE)), by=c('grant', 'start_date_programmatic', 'end_date_programmatic')]


# tablle3a <- 
  
# table 3 subsets the data only to columns of interest
table3 <-table2[,.(grant, start_date_programmatic, end_date_programmatic, indicators, indicators_reported, total_met, percent_met_all=total_met/indicators, percent_met_reported=total_met/indicators_reported)]

# add new variable indicating which semester the PUDR corresponds to
table3$reporting_period <- NA
table3$reporting_period[which(table3$start_date_programmatic=="2019-01-01" & table3$end_date_programmatic=="2019-06-30")] <- 3
table3$reporting_period[which(table3$start_date_programmatic=="2018-07-01" & table3$end_date_programmatic=="2018-12-31")] <- 2
table3$reporting_period[which(table3$start_date_programmatic=="2018-01-01" & table3$end_date_programmatic=="2018-06-30")] <- 1

# table 4 subsets only the variables: grant, reporting period, and how percent targets were met
table4 <- table3[,.(grant, reporting_period, percent_met_reported)]

# table 5 melts the data long (so each row represents a grant and semester)
table5 <- melt(table4, id.vars = c('grant', 'reporting_period'))

# table 6 casts the data wide (so each colunn represents a semester)
table6 <- dcast(table5, grant ~ reporting_period)


# round values
cols <- names(table6)[2:4]
table6[,(cols) := round(.SD,2), .SDcols=cols]
         
outputFile = "J:\\Project\\Evaluation\\GF\\outcome_measurement\\multi_country\\performance_indicators\\pudr_indicator_extraction\\analysis\\visualizations\\2019_annual_reports\\uga_report_table_27Dec2019.csv"
write.csv(table6, outputFile)
