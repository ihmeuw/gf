# readme.txt file for prep code in gf/outcome_measurement/all/performance_indicators/prep

### Files included in main prep pipeline:
- 1_master_file: lists and controls all scripts in the performance indicator pipelin
- 2_read_filelist: reads in master_file_list and sources scripts to extract indicator data
- 3_aggregate_data: combines data from all 4 countries into one file.
- 4_clean_merge_data: cleans special characters, corrects errors, standardizes indicator names and saves data used for plotting or analyses.

### Additional files:
- prep_qualitative_data: extracts the qualitative data from PUDRs
- prep_wptms: extracts data from Workplan Tracking Measure Sheets
- set_up_r: lists directories, and libraries necessary to prep Performance Indicator data.
