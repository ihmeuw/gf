# read in data
library(data.table)
library(ggplot2)

# read in data on cleaned PFi
dir <- "J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/" 
DT <- readRDS(paste0(dir,"prepped_data/cleaned_pfi.RDS"))

# subset data to certain rows
data <- DT[pudr_sheet %in% c("coverage_indicators_main", "impact_outcome_indicators_main")] # subset to main indicators
data <- data[start_date_programmatic=="2019-01-01" & end_date_programmatic=="2019-06-30"] # semester 1 of 2019 only
data <- data[indicator_code %like% "KP" | indicator_code %like% "TCP" | indicator_code %like% "SPI"] # subset to KP only
data$pudr_time_frame <- "S1 2019" # add pudr_time_frame
data$performance_indicator <- paste(data$indicator_code, data$full_description, sep = ": ") # add performance indicator
data$achievement_ratio_reported <- data$pr_result_achievement_ratio 
data$achievement_ratio_verified <- data$lfa_result_achievement_ratio 

# subset data to select columns
data <- data[,.(loc_name,grant, pudr_time_frame, performance_indicator, lfa_result_pct, target_pct, achievement_ratio_reported, achievement_ratio_verified)]

# fix data names
data$loc_name[which(data$loc_name=="sen")]<-"Senegal"
data$loc_name[which(data$loc_name=="uga")]<-"Uganda"
data$loc_name[which(data$loc_name=="cod")]<-"DRC"
data$loc_name[which(data$loc_name=="gtm")]<-"Guatemala"

# View(DT[pudr_sheet%in%c("coverage_indicators_main", "impact_outcome_indicators_main") & loc_name=="uga" & start_date_programmatic=="2018-07-01"])

# use na.omit
data2 <- na.omit(data)

# save as csv
write.csv(data, file=paste0(dir,"analysis/subset_data/synthesis_kvps.csv"))
 

