# This bit of code merges on the cleaned and standardized names for the data sources column
# could be re-added later to the overall cleaning code once that and this is done
# Francisco Rios
# 10/21/2019

# Cleaning code -- attach the standardized data source name to the data
library(data.table)

# load code book and PUDR PFI Database
DT <- readRDS("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/prepped_data/cleaned_pfi.rds") # this is the data that has already been somewhat cleaned based on part 1 of the data_cleaning script
#You should set the "repo root" variable in step 1_master_file.r so it can be run by anyone. 
codebook <- fread(paste0(repo_root, "outcome_measurement/all/performance_indicators/codebooks/data_source_codebook.csv"), header = TRUE) # code book which describes names of the data sources in a standardized way

# fixed one last final mistake in code name
DT$indicator <- gsub("&amp;", "&", DT$indicator)
DT$indicator_code <- gsub("&amp;", "&", DT$indicator_code)

# Merge and replace the lfa result source code
data4 <- merge(DT, codebook, by.x="pr_result_source", by.y = "source_original", all.x = TRUE)
data5 <- data4[,lfa_result_source_code:=source_code]
data5 <- data4[,source_code:=NULL]


# calculate 'value' variables which are either the percent reported or the numerator (if indicator is not a proportion or percent)
# this is done for the one target value reported and the three sources of result values (PR, LFA, and GF)
# data6$target_value <- ifelse(is.na(data6$`target_%`),data6$target_n, data6$`target_%`)
# data6$pr_result_value <- ifelse(is.na(data6$`pr_result_%`), data6$target_n, data6$`pr_result_%`)
# data6$lfa_result_value <- ifelse(is.na(data6$`lfa_result_%`), data6$lfa_result_n, data6$`lfa_result_%`)
# data6$gf_result_value <- ifelse(is.na(data6$`gf_result_%`), data6$gf_result_n, data6$`gf_result_%`)

data6 <- data5

# calculate ihme_results_achievement_ratio
data6$ihme_result_achievement_ratio <-NA

data6$ihme_result_achievement_ratio <- as.numeric(data6$ihme_result_achievement_ratio)
data6$any_result_value <- as.numeric(data6$any_result_value)
data6$target_value <- as.numeric(data6$target_value)
data6$ihme_result_achievement_ratio <- data6$any_result_value/data6$target_value


# calculate if the sources differ between the baseline value and the pr reported value
data6$sources_different <- NA
data6$sources_different[which(data6$baseline_source_code!=data6$pr_result_source_code)] <- 1
data6$sources_different[which(data6$baseline_source_code==data6$pr_result_source_code)] <- 0

# add reverse indicator variable
reverse_codebook <- fread(paste0(repo_root, "outcome_measurement/all/performance_indicators/codebooks/indicators_codebook_reverse.csv"))
reverse_codebook = reverse_codebook[,.(indicator_code, reverse_indicator_final)]
data7 <- merge(data6, reverse_codebook, by.x="indicator_code", by.y = "indicator_code", all.x = TRUE, all.y = FALSE)

# create new variable to indicate whether target is being met
data7$target_met <- NA
data7$target_met[which(data7$reverse_indicator_final=="no" & data7$any_result_value >= data7$target_value)] <- "yes"
data7$target_met[which(data7$reverse_indicator_final=="no" & data7$any_result_value < data7$target_value)] <- "no"
data7$target_met[which(data7$reverse_indicator_final=="yes" & data7$any_result_value <= data7$target_value)] <- "yes"
data7$target_met[which(data7$reverse_indicator_final=="yes" & data7$any_result_value > data7$target_value)] <- "no"

# save output
saveRDS(data7, "J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/data_for_analysis/kpi_data_for_analyses2.RDS")
