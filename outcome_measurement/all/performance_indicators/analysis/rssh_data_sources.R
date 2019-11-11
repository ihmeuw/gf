# RSSH, HMIS information in each country

# read data
data <- readRDS("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/cleaned_data/kpi_data_for_analyses.RDS")

# subset data to graph

# set parameters of data to be analyzed
main_indicators = c('impact_outcome_indicators_main', 'coverage_indicators_main')

dt <- data[pudr_sheet %in% main_indicators]

# create visuals of which data sources they are using for each disease and country
# for each country, indicator, data sources
  
dt2 <- dt[,.(loc_name, disease, pr_result_source_code, start_date_programmatic, end_date_programmatic)]

# re-shape dt2 to plot

dt3 <- dt2[, count := uniqueN(pr_result_source_code), by = c("loc_name", "disease", "start_date_programmatic", "end_date_programmatic")]

# add new variable for the PUDR
dt3$date <- NA
dt3$date[which(dt3$start_date_programmatic=="2019-01-01" & dt3$end_date_programmatic=="2019-06-30")] <- "2019.0"
dt3$date[which(dt3$start_date_programmatic=="2018-07-01" & dt3$end_date_programmatic=="2018-12-31")] <- "2018.5"
dt3$date[which(dt3$start_date_programmatic=="2018-01-01" & dt3$end_date_programmatic=="2018-06-30")] <- "2018.0"
dt3$date[which(dt3$start_date_programmatic=="2017-01-01" & dt3$end_date_programmatic=="2017-06-30")] <- "2017.1"
dt3$date[which(dt3$start_date_programmatic=="2017-07-01" & dt3$end_date_programmatic=="2017-12-31")] <- "2017.5"

dt4 <- dcast(dt3, loc_name + disease + start_date_programmatic + end_date_programmatic + date~ pr_result_source_code, fun.aggregate = sum, value.var = count)

ggplot(dt3[loc_name=="uga"], aes(x=date, y=count, group=pr_result_source_code, col=pr_result_source_code))+
  geom_point()+
  geom_path()+
  facet_wrap(~disease)
