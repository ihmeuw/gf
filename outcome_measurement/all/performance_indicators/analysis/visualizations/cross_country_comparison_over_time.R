# make visualizations on each of the grant performance indicators 
# over time
# while creating a summary visual 
# 11/8/2019
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
main_indicators = c('impact_outcome_indicators_main', 'coverage_indicators_main')

# subset as appropriate
DT <- data
DT = DT[pudr_sheet %in% main_indicators]

# change names
DT$loc_name[which(DT$loc_name=="sen")]<-"SEN"
DT$loc_name[which(DT$loc_name=="cod")]<-"COD"
DT$loc_name[which(DT$loc_name=="uga")]<-"UGA"
DT$loc_name[which(DT$loc_name=="gtm")]<-"GTM"

# factor target_met
DT$target_met[which(DT$target_met=="yes")] <-1
DT$target_met[which(DT$target_met=="no")] <- 0
DT$target_met <- as.numeric(DT$target_met)

# table1 <- DT[,.(percent_met=sum(target_met, na.rm=TRUE)/length(indicator_code), by=c('grant'))]

table2 <-DT[,list(indicators=length(indicator_code), indicators_reported=sum(!is.na(ihme_result_achievement_ratio)), total_met=sum(target_met, na.rm = TRUE)), by=c('loc_name', 'grant', 'start_date_programmatic', 'end_date_programmatic', 'disease')]
table3 <-table2[,.(loc_name, grant, disease, start_date_programmatic, end_date_programmatic, indicators, indicators_reported, total_met, percent_met_all=total_met/indicators, percent_met_reported=total_met/indicators_reported)]

# add new variable for the PUDR
table3$reporting_period <- NA
table3$reporting_period[which(table3$start_date_programmatic=="2019-01-01" & table3$end_date_programmatic=="2019-06-30")] <- "2019.0"
table3$reporting_period[which(table3$start_date_programmatic=="2018-07-01" & table3$end_date_programmatic=="2018-12-31")] <- "2018.5"
table3$reporting_period[which(table3$start_date_programmatic=="2018-01-01" & table3$end_date_programmatic=="2018-06-30")] <- "2018.0"
table3$reporting_period[which(table3$start_date_programmatic=="2017-01-01" & table3$end_date_programmatic=="2017-06-30")] <- "2017.1"
table3$reporting_period[which(table3$start_date_programmatic=="2017-07-01" & table3$end_date_programmatic=="2017-12-31")] <- "2017.5"
table3$reporting_period[which(table3$start_date_programmatic=="2018-10-01" & table3$end_date_programmatic=="2019-06-30")] <- "2019.0"

table4 <- table3[,.(loc_name, grant, disease, reporting_period, percent_met_reported)]

#table5 <- melt(table4, id.vars = c('loc_name', 'grant', 'disease', 'reporting_period'))
#table6 <- dcast(table5, grant + loc_name + disease~ reporting_period)

# remove data on guatme

# set order of columns
#setorder(table6, c("loc_name", "grant", "disease", ""))

table4 = table4[loc_name!="GTM"]



# make plot to visualize data
p1 = ggplot(data=table4, aes(x=reporting_period, y=percent_met_reported, color=disease))+
  geom_line(aes(group=grant))+
  geom_point()+
  facet_wrap(~loc_name)+
  theme_bw()+
  labs(title="Percentage of Performance Indicators Reaching Targets", x="Reporting Period", y= "%  at or above target goal", 
       caption="Percentage based on reported indicators in each PUDR \n Guatemala removed due to different reporting time periods")+
  theme(axis.text=element_text(size=10), axis.title=element_text(size=16), legend.title=element_text(size = 14), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=12),
        axis.text.x = element_text(vjust=1, hjust=1))

outputFile = "J:\\Project\\Evaluation\\GF\\outcome_measurement\\multi_country\\performance_indicators\\pudr_indicator_extraction\\analysis\\visualizations\\cross_country_comparison_over_time.jpeg"
ggsave(outputFile, plot=p1, height=9, width=11)

# change names to french
#setnames(table6, 
#         old = c("grant"),
#         new = c("Subvention"))

# round values
#cols <- names(table6)[2:4]
#table6[,(cols) := round(.SD,2), .SDcols=cols]
         

#outputFile = "J:\\Project\\Evaluation\\GF\\outcome_measurement\\multi_country\\performance_indicators\\pudr_indicator_extraction\\visualizations\\cod_report_table.csv"
#write.csv(table6, outputFile)
