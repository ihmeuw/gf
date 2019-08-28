# Francisco and Audrey
# data prep for Drivers of mortality analysis plan
# August 8, 2019

# set up
library(data.table)
library(ggplot2)

j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')
dir = paste0(j, "/Project/Evaluation/GF/impact_evaluation/mortality/")

inFile = paste0(dir, "gbd_estimates_tb_pce_countries.csv")
outFile = paste0(dir, "prepped_data/tb_pce_data.RDS")

# read data
TB = fread(inFile)

# subset data to include:
if ( length(unique(TB$metric_name))!= 1 & unique(TB$metric_name) != "Rate" ) TB = TB[metric_name=="Rate"]
if ( length(unique(TB$age_group_name))!= 1 & unique(TB$age_group_name) != "Age-standardized" ) TB = TB[age_group_name=="Age-standardized"]

# keep columns of interest
TB = TB[,.(year_id, location_name, cause_name, measure_name, val)]
names(TB) = c('year', 'country', 'disease', 'measure', 'val')

# sum across TB types
TB = TB[, .(val = sum(val)), by = .(year, country, measure)]
TB[, disease := "tb_all_types"]

# reshape the data to be wide
data = dcast(TB,year + country + disease ~ measure, value.var = "val")

# remove 2018 because right now 2018 not avail
data = data[year != 2018]

#  compute MI ration
data[, mi_ratio := Deaths/Incidence]

# save data for further analysis
saveRDS(data, outFile)


