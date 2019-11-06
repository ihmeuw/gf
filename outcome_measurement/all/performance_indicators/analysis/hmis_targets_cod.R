# pull table of all targets set in DRC for HMIS and M&E -2

library(data.table)

data <- readRDS("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/cleaned_data/kpi_data_for_analyses.RDS")

# set subsetting parameters
country = "cod"
reporting_targets = "M&E-2"

DT <- data[loc_name==country & indicator_code==reporting_targets]

# indicators to keep
DT <- DT[,.(indicator_code,full_description, loc_name, grant, disease, start_date_programmatic, end_date_programmatic,
            baseline_value, baseline_source, baseline_year, 
            target_n, target_d,`target_%`, target_value, 
            pr_result_n, pr_result_d, pr_result_value, pr_result_value,
            lfa_result_n, lfa_result_d, `lfa_result_%` ,lfa_result_value, 
            gf_result_n, gf_result_d, `gf_result_%`, gf_result_value,
            file_name)]


outFile <- "J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/prepped_data/hmis_targets_cod.RDS"
saveRDS(DT, outFile)
