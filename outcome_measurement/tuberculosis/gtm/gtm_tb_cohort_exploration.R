##########################################################
# GTM mortality cohort data exploration
# J. Ross 8-22-2018
#
#########################################################

rm(list=ls())
library(data.table)

#Read the prepped cohort data. This is broken up since it is part of a script that runs on the cluster.
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j') 
cohort_dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/prepped_data/')
cohort <- data.table(fread(paste0(cohort_dir, "GTM - Tx cohort data 2012-2016.csv")))

#subset to annual totals (drop trimester reports) and deaths (drop other outcomes)
cohort1 <- cohort[col_name=="TOTAL" & row_name_B=="DEATHS"]
cohort1 <- cohort1[order(year, deptocode, table)]

#Number of reporting categories varies by year
cohort1[, .N, by = year]

#See that Departments 1, 14 and 17 consistently have more reporting categories than other departments
#I haven't figured out how to code the two-way frequency table in data table
table(cohort1$year, cohort1$deptocode)

#It looks like Dept 1 has 4 reports per category of "table" plus some additonal single categories and that
#Depts 14 and 17 each have 3 reports per category, plus some additional single categories