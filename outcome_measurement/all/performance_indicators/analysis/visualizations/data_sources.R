# RSSH, HMIS information in each country

# set up
library(data.table)
library(ggplot2)

# read data
data <- readRDS("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/cleaned_data/kpi_data_for_analyses2.RDS")

# subset data to graph

# set parameters of data to be analyzed
main_indicators = c('coverage_indicators_main')

dt <- data[pudr_sheet %in% main_indicators]

# create visuals of which data sources they are using for each disease and country
# for each country, indicator, data sources
  
dt2 <- dt[,.(loc_name, disease, pr_result_source_code, start_date_programmatic, end_date_programmatic)]

# add new variable for the PUDR
dt2$date <- NA
dt2$date[which(dt2$start_date_programmatic=="2019-01-01" & dt2$end_date_programmatic=="2019-06-30")] <- "2019.0"
dt2$date[which(dt2$start_date_programmatic=="2018-07-01" & dt2$end_date_programmatic=="2018-12-31")] <- "2018.5"
dt2$date[which(dt2$start_date_programmatic=="2018-01-01" & dt2$end_date_programmatic=="2018-06-30")] <- "2018.0"
dt2$date[which(dt2$start_date_programmatic=="2017-01-01" & dt2$end_date_programmatic=="2017-06-30")] <- "2017.0"
dt2$date[which(dt2$start_date_programmatic=="2017-07-01" & dt2$end_date_programmatic=="2017-12-31")] <- "2017.5"
dt2$date[which(dt2$start_date_programmatic=="2018-10-01" & dt2$end_date_programmatic=="2019-06-30")] <- "2019.0"

#
dt3 <- dt2[,.(loc_name, disease, pr_result_source_code, date)]

dt4 <- dcast(dt3, loc_name + disease + date ~ pr_result_source_code)
dt5 <- melt(dt4, id.vars=c("loc_name", "disease", "date"))
dt5 <- dt5[variable!="NA"]

# re-shape dt2 to plot
# dttest <- dt2[, Count := unique(pr_result_source_code),by=.(loc_name, disease, date)]

# dt3 <- dt2[, count := uniqueN(pr_result_source_code), by = c("loc_name", "disease", "start_date_programmatic", "end_date_programmatic")]
# dt4 <- dt3[,.(Sum=sum(count)),by=c("loc_name", "disease", "start_date_programmatic", "end_date_programmatic", "pr_result_source_code")]


# dt4 <- dcast(dt3, loc_name + disease + start_date_programmatic + end_date_programmatic + date~ pr_result_source_code, fun.aggregate = sum, value.var = count)

#dt5 <- dt4[,.(loc_name, date, disease, pr_result_source_code, Sum)]

dt5$loc_name[which(dt5$loc_name=="sen")]<-"SEN"
dt5$loc_name[which(dt5$loc_name=="cod")]<-"COD"
dt5$loc_name[which(dt5$loc_name=="uga")]<-"UGA"
dt5$loc_name[which(dt5$loc_name=="gtm")]<-"GTM"

countries = unique(dt5$loc_name)
plots = list()
i=1
for(c in countries) {
plots[[i]] = ggplot(dt5[loc_name==c], aes(x=date, y=value, group=variable, col=variable))+
  geom_point()+
  geom_line()+
  facet_wrap(~disease)+
  theme_bw(base_size = 18)+
  labs(col="Data source", title=paste0("Data Sources in ", c), y="Sum Total", x="Date", caption = "Data Source: Coverage Indicators in PUDRs")+
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
i=i+1
}

pdf("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/visualizations/data_sources.pdf", width=10, height=8)
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
dev.off()
