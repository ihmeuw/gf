# Code to map absorption vs performance

# set up
library(data.table)

# load codebook and PUDR data
codebook <- fread("C:/Users/frc2/Documents/gf/outcome_measurement/all/performance_indicators/codebooks/indicators_codebook.csv")
data <- readRDS("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/cleaned_data/kpi_data_for_analyses.RDS")

# data to be subset
s1y2019pudrs = data[start_date_programmatic >= "2019-01-01" & end_date_programmatic <= "2019-06-30",.(indicator_code, in2019s1=1)]
s2y2018pudrs = data[start_date_programmatic >= "2018-01-01" & end_date_programmatic <= "2018-12-31",.(indicator_code, in2018s2=1)]
s1y2018pudrs = data[start_date_programmatic >= "2018-01-01" & end_date_programmatic <= "2018-06-30",.(indicator_code, in2018s1=1)]

# mapping unique variables that appear in 2019 S1 PUDRS
setkey(s1y2019pudrs, "indicator_code")
s1y2019pudrs = unique(s1y2019pudrs)

# merge two files
first_file <- merge(codebook, s1y2019pudrs, by="indicator_code", all.x = TRUE, all.y = FALSE)

# mapping unique variables that appear in 2019 S1 PUDRS
setkey(s2y2018pudrs, "indicator_code")
s2y2018pudrs = unique(s2y2018pudrs)

second_file <- merge(codebook, s2y2018pudrs, by="indicator_code", all.x = TRUE, all.y = FALSE)

# look within semester 1 of 2018 data
setkey(s1y2018pudrs, "indicator_code")
s1y2018pudrs = unique(s1y2018pudrs)

third_file <- merge(codebook, s1y2018pudrs, by="indicator_code", all.x = TRUE, all.y = FALSE)

# subset final colums together
second_file <- second_file[,.(indicator_code, in2018s2)]
third_file <- third_file[,.(indicator_code, in2018s1)]

# merge
final_file1 <- merge(first_file, second_file, all = FALSE)
final_file2 <- merge(final_file1, third_file, all = FALSE)

# replace NAs with zero
final_file2$in2019s1[which(is.na(final_file2$in2019s1))]<-0
final_file2$in2018s2[which(is.na(final_file2$in2018s2))]<-0
final_file2$in2018s1[which(is.na(final_file2$in2018s1))]<-0

# included
final_file2$included <- 0
final_file2$included[which(final_file2$in2019s1==1 | 
                             final_file2$in2018s2==1|
                             final_file2$in2018s1==1)] <- 1

# save outfile
outFile = "C:/Users/frc2/Documents/gf/outcome_measurement/all/performance_indicators/codebooks/indic_absrp_cb.RDS"
saveRDS(final_file2, outFile)
outFile2 = "C:/Users/frc2/Documents/gf/outcome_measurement/all/performance_indicators/codebooks/indic_absrp_cb.csv"
write.csv(final_file2, outFile2)
