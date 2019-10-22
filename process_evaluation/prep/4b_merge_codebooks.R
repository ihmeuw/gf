# This bit of code merges on the cleaned and standardized names for the data sources column
# could be re-added later to the overall cleaning code once that and this is done
# Francisco Rios
# 10/21/2019

# Cleaning code -- attach the standardized data source name to the data
library(data.table)

# load code book and PUDR PFI Database
codebook <- fread("C:/Users/frc2/Documents/gf/special_assessments/synthesis/2019_multicountry_analyses/data_source_codebook.csv", header = TRUE)
DT <- readRDS("J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/cleaned_data/cleaned_pfi.RDS")

# Merge and replace the Baselice source code 
data <- merge(DT, codebook, by.x="baseline_source", by.y = "source_original", all.x=TRUE)
data1 <- data[,baseline_source_code:=source_code]
data1 <- data1[,c("source_code"):=NULL]

# Merge and replace the pr result source code
data2 <- merge(data1, codebook, by.x="pr_result_source", by.y = "source_original", all.x = TRUE)
data3 <- data2[,pr_result_source_code:=source_code]
data3 <- data3[,source_code:=NULL]

# merge on new codebook with all indicator names
# read and merge codebook to standardize names
codebook_names <- fread("C:/Users/frc2/Documents/gf/special_assessments/synthesis/2019_multicountry_analyses/indicators_codebook_full.csv") 
data4 <- merge(data3, codebook_names, by="indicator_code", all.x = TRUE)

# save merged_pfi.RDS
saveRDS(data4, "J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/cleaned_data/merged_pfi_data.RDS")
