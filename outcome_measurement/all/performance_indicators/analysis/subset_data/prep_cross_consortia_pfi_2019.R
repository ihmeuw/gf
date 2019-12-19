# prep prepped_cross_consortia_pfi_2019.csv 
# used for SO1
# difference between this code and SO3 is that this removes duplicates (cases where different PRs report the same value of achievement)

# synthesis report visual for SO1

# read in data
library(data.table)
library(ggplot2)

data <-fread("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/analysis/subset_data/cross_consortia_data_13Dec2019.csv")

# delete first column
data[,V1:=NULL]

# rename column names
setnames(data, 
         old = names(data),
         new = c('loc_name', 'grant', 'indicator_long', 'achievement_ratio', 'reporting_period'))

# parameters to edit
codebook_name = "module_code_map.csv"

# split data indicator codes
data = data[, indicator_code:=tstrsplit(indicator_long, ":", keep=1)]

# merge with the correct module code and the correct indicator type
codebook <- fread(paste0("C:/Users/frc2/Documents/gf/outcome_measurement/all/performance_indicators/codebooks/", codebook_name))
data <- merge(data, codebook, by.x='indicator_code', by.y="indicator_code", all.x = TRUE, all.y=FALSE)

# merge on information on whether this is a reverse indicator
reverse_codebook <- fread("C:\\Users\\frc2\\Documents\\gf\\outcome_measurement\\all\\performance_indicators\\codebooks\\indicators_codebook_reverse.csv")
data <- merge(data, reverse_codebook, by.x='indicator_code', by.y='indicator_code', all.x = TRUE, all.y = FALSE)


# merge properly cleaned name
clean_codebook <- fread("C:\\Users\\frc2\\Documents\\gf\\outcome_measurement\\all\\performance_indicators\\codebooks\\indicators_codebook.csv")
data <- merge(data, clean_codebook, by.x='indicator_code', by.y='indicator_code', all.x = TRUE, all.y = FALSE)

# set parameters of data to be analyzed
#date = c('s1_2019')

# subset as appropriate
DT <- data
#DT <- DT[reporting_period==date]
DT <- DT[,.(loc_name, grant, indicator_code, full_description, achievement_ratio, reverse_indicator_final, module_code, type_desc, reporting_period)]
# change name of full_description to indicator_long
setnames(DT, old=c('full_description'),
         new = "indicator_long")

############################
#### Data cleaning
#############################

# delete specific rows
DT$achievement_ratio[which(DT$grant=="COD-C-CORDAID" & DT$indicator_code=="TB O-6")] <- 2.229 # removing the cordaid value which is incorrect and duplicated in the COD-T-MOH grant PUDR --verified by reding comments in the PUDR

######## find duplicated values that are the same ############
DT <- unique.data.frame(DT) # there are three values that are duplicated across grant, PRs, and achievement ratio

### instances where different PRs report the same value--keep the first
DT <- unique(DT, by=c("loc_name", "indicator_code", "achievement_ratio")) # there are 47 rows which have duplicated values (same value reported by both PRs in the country)
# View(unique(DT, by=c("loc_name", "indicator_code"))) # there are 43 indicators which the two PRs reported different values

####### fixing errors in the data--would be better to create a script for this in the data prep stage

# save dataset
write.csv(DT, "J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/analysis/subset_data/prepped_cross_consortia_pfi_2019.csv" )
saveRDS(DT, "J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/analysis/subset_data/prepped_cross_consortia_pfi_2019.RDS" )
