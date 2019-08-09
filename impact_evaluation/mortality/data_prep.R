# Francisco and Audrey
# data prep for Drivers of mortality analysis plan
# August 8, 2019

# Data step that is pending: the TB data is disaggregated by HIV status, so that would need to be combined to get an overall value

# set up
library(data.table)
library(ggplot2)
library(GGally)

# read data
TB <- fread("J:/Project/Evaluation/GF/impact_evaluation/mortality/tb_all_gbd.csv")

# subset data to include:
TB = TB[cause_name=="Drug-susceptible tuberculosis"]
TB = TB[metric_name=="Rate"]
TB = TB[age_name=="Age-standardized"]

#  compute MI ration
TB = TB[]
TB$mi_ratio <- T