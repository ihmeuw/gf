# create simple dataset that can be used to create cross-consortium synthesis visuals
# francisco rios casas
# 11/7/2019


library(reshape2)
library(data.table)

# load data
data <- readRDS("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/data_for_analysis/kpi_data_for_analyses2.RDS")

# subset as appropriate
main_indicators = c('impact_outcome_indicators_main', 'coverage_indicators_main')

DT <- data
DT = DT[pudr_sheet %in% main_indicators]

# save certain columns
DT = DT[,.(loc_name, grant,start_date_programmatic, end_date_programmatic, indicator_code, brief_description_code, ihme_result_achievement_ratio)]

# clean reporting period

# add new variable for the PUDR
DT$reporting_period <- NA
DT$reporting_period[which(DT$start_date_programmatic=="2019-01-01" & DT$end_date_programmatic=="2019-06-30")] <- "s1_2019"
DT$reporting_period[which(DT$start_date_programmatic=="2018-07-01" & DT$end_date_programmatic=="2018-12-31")] <- "s2_2018"
DT$reporting_period[which(DT$start_date_programmatic=="2018-01-01" & DT$end_date_programmatic=="2018-06-30")] <- "s1_2018"
DT$reporting_period[which(DT$start_date_programmatic=="2017-01-01" & DT$end_date_programmatic=="2017-06-30")] <- "s1_2017"
DT$reporting_period[which(DT$start_date_programmatic=="2017-07-01" & DT$end_date_programmatic=="2017-12-31")] <- "s2_2017"
DT$reporting_period[which(DT$start_date_programmatic=="2018-10-01" & DT$end_date_programmatic=="2019-06-30")] <- "s1_2019"

DT = DT[,.(loc_name, grant, indicator_code, brief_description_code, ihme_result_achievement_ratio, reporting_period)]

DT$loc_name[which(DT$loc_name=="sen")]<-"SEN"
DT$loc_name[which(DT$loc_name=="cod")]<-"COD"
DT$loc_name[which(DT$loc_name=="uga")]<-"UGA"
DT$loc_name[which(DT$loc_name=="gtm")]<-"GTM"

# only keep for the following time periods

# for Uganda, Senegal, and DRC we have 2019 S1 data for all grants, for guatemala only the GTM-T-MSPAS and GTM-H-INCAP are recent
countries=c("SEN", "UGA", "COD", "GTM")
date = "s1_2019"
DT2 <- DT[loc_name%in%countries & reporting_period==date]

# for guatemala, we only have the S2 2018 Grant for GTM-T-MSPAS and GTM-H-HIVOS 
DT3 <- DT[loc_name=="GTM" & grant%in%c("GTM-M-MSPAS", "GTM-H-HIVOS") & reporting_period=="s2_2018"]

# bind the two together
DT4 <- rbind(DT2, DT3)

#DT2 <- melt(DT, id.vars = c('loc_name', 'grant', 'brief_description_code', 'reporting_period'))

# re-shape to get wide
#DT3 <- dcast(DT2, grant + brief_description_code + loc_name ~ reporting_period, value.var = "value", fun.aggregate = mean)

#DT3 <- data.table(DT3)

# set column order 
# setcolorder(DT3, c("loc_name", "grant", "brief_description_code", "s1_2017", "s2_2017", "s1_2018", "s2_2018", "s1_2019"))
# 
# # this might work if we re-ordered the columsn from oldest to most recent
# DT3[, res := NA]
# DT3$res <- as.double(DT3$res)
# for (v in rev(names(DT3))[-1]) DT3[is.na(res), res := get(v)]
# 
# DT3$s1_2017[is.nan(DT3$s1_2017)] <-NA
# DT3$s2_2017[is.nan(DT3$s2_2017)] <-NA
# DT3$s1_2018[is.nan(DT3$s1_2018)] <-NA
# DT3$s2_2018[is.nan(DT3$s2_2018)] <-NA
# DT3$s1_2019[is.nan(DT3$s1_2019)] <-NA
# 
# # return which time period it's from
# DT3$date <- NA
# DT3$date <- ifelse(!is.na(DT3$s1_2019),paste0("s1_2019"), 
#                               ifelse(!is.na(DT3$s2_2018), paste0("s2_2018"), 
#                                      ifelse(!is.na(DT3$s1_2018), paste0("s1_2018"), 
#                                             ifelse(!is.na(DT3$s2_2017),paste0("s2_2017"), 
#                                                    ifelse(!is.na(DT3$s1_2017),paste0("s1_2017"), paste0(NA))))))

# subset data
#DT4 = DT4[,.(loc_name, grant, indicator brief_description_code, res, date)]

# remove missing values
DT4 = na.omit(DT4)

# save file for export
outputFile = "J:\\Project\\Evaluation\\GF\\outcome_measurement\\multi_country\\performance_indicators\\pudr_indicator_extraction\\visualizations\\cross_consortia_data_9Dec2019.csv"
write.csv(DT4, outputFile)
