# script which downloads data from all EHG countries from google online spreadsheet


library(googledrive)
library(readxl)

#drive_find(n_max=30)

ccdata <- drive_get(as_id("1yIO1dNH_OppKlVNB9ceNoJBklHCLwH7K"))
local_file = "J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/analysis/subset_data/cross_consortia_financial_analyses_for_synthesis.xlsx"
drive_download(file=ccdata, 
               path=local_file,
               overwrite = TRUE)


# seperate out the data on performance indicators
data <- read_excel(local_file, sheet=4)
write.csv(data, "J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/analysis/subset_data/cross_consortia_data_13Dec2019.csv")
