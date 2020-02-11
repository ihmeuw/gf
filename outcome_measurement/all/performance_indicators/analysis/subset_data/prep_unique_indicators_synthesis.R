# Francisco Rios CAsas
# January 7 2020
# This file creates a unique datatable of all unique indicators in the synthesis indicators data

# load data
dir = "J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/"
indicators = readRDS(paste0(dir, 'pudr_indicator_extraction/analysis/subset_data/prepped_cross_consortia_pfi_2019.RDS'))

# subset to only unique indicator_code and indicator name
temp <- as.data.table(unique(indicators[,.(indicator_code, indicator_long)]))

# save unique values in the synthesis data as a spreadsheet
write.csv(temp, file=paste0(dir, 'pudr_indicator_extraction/analysis/subset_data/unique_synthesis_indicators.csv'))
