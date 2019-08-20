# Francisco and Audrey
# data prep for Drivers of mortality analysis plan
# August 8, 2019

# Limitation 1 :TB data is disaggregated by HIV status, so that would need to be combined to get an overall value

# set up
library(data.table)
library(ggplot2)
library(GGally)
outFile1 = 'J:/Project/Evaluation/GF/impact_evaluation/mortality/prepped_data/tb_pce_data.RDS'

# read data
TB <- fread("J:/Project/Evaluation/GF/impact_evaluation/mortality/tb_all_gbd.csv")

# subset data to include:
TB = TB[cause_name=="Drug-susceptible tuberculosis"]
TB = TB[metric_name=="Rate"]
TB = TB[age_name=="Age-standardized"]

# keep columns of interest
TB = TB[,.(measure_name, cause_name, location_name, year,val)]

# reshape the data to be wide
data = dcast(TB, cause_name + location_name + year ~ measure_name, value.var = "val")

#  compute MI ration
data$mi_ratio <- data$Deaths/data$Incidence

# save data for further analysis
saveRDS(data, outFile1)