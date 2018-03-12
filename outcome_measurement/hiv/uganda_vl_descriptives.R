# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 3/11/2018
# Run descriptive statistics from the Uganda VL Dashboard data, disaggregated only by sex (no tb or age)
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)

# --------------------

# ----------------------------------------------
# Files and directories

# upload the data with month, year, sex
uganda_vl <- readRDS("J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape_agg/sex_data.rds")

# set output directory
# ----------------------------------------------
setwd("J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard")

# ----------------------------------------------

# view the data set and variable names
class(uganda_vl) # check that it is a data table
str(uganda_vl)
names(uganda_vl)

# check that 2014 and 2018 data only includes appropriate months
uganda_vl[year==2014, sum(samples_received), by=month]
uganda_vl[year==2018, sum(samples_received), by=month]

# ----------------------------------------------
# add useful variables and prep the data 

# destring sex
uganda_vl[ , .(class(sex)) ]

uganda_vl[sex == "m", female := 0]
uganda_vl[sex == "f", female := 1]
uganda_vl[sex == "x", female := NA]

uganda_vl[, .(sum(female, na.rm=T)), by=year] # tells you the # of entries, not females
uganda_vl [, .(sum(samples_received)), by=.(year, female)] # samples from females by year


# ----------------------------------------------
# summary tables of patients, samples, valid results, suppressed, and % suppressed over time

# table 1: patients, samples, valid results, suppressed, %suppressed over time
table_1 <- uganda_vl[ ,
                      .(total_patients = sum(patients_received), samples = sum(samples_received), 
                        test_results = sum(valid_results), total_suppressed = sum(suppressed),
                        suppression_ratio=100*(sum(suppressed)/sum(valid_results))),
                      by=year]

# ggplot of suppression ratio oer time
ggplot(table_1, aes(x = year, y = suppression_ratio )) +
  geom_point() + theme_bw()

table_1 # to compare values with the scatter plot


# --------------------

# table 1 by year

table_1_2014 <- uganda_vl[ year==2014,
                           .(total_patients = sum(patients_received), samples = sum(samples_received), 
                             test_results = sum(valid_results), total_suppressed = sum(suppressed),
                             suppression_ratio=100*(sum(suppressed)/sum(valid_results))),
                           by=month]

# scatter of plot % suppressed over time
ggplot(table_1_2014, aes(x = month, y = suppression_ratio )) +
  geom_point() + theme_bw()

# --------------------

table_1_2015 <- uganda_vl[ year==2015,
                           .(total_patients = sum(patients_received), samples = sum(samples_received), 
                             test_results = sum(valid_results), total_suppressed = sum(suppressed),
                             suppression_ratio=100*(sum(suppressed)/sum(valid_results))),
                           by=month]

# scatter of plot % suppressed over time
ggplot(table_1_2015, aes(x = month, y = suppression_ratio )) +
  geom_point() + theme_bw()


# --------------------

table_1_2016 <- uganda_vl[ year==2016,
                           .(total_patients = sum(patients_received), samples = sum(samples_received), 
                             test_results = sum(valid_results), total_suppressed = sum(suppressed),
                             suppression_ratio=100*(sum(suppressed)/sum(valid_results))),
                           by=month]

# scatter of plot % suppressed over time
ggplot(table_1_2016, aes(x = month, y = suppression_ratio )) +
  geom_point() + theme_bw()


# --------------------

table_1_2017 <- uganda_vl[ year==2017,
                           .(total_patients = sum(patients_received), samples = sum(samples_received), 
                             test_results = sum(valid_results), total_suppressed = sum(suppressed),
                             suppression_ratio=100*(sum(suppressed)/sum(valid_results))),
                           by=month]

# scatter of plot % suppressed over time
ggplot(table_1_2017, aes(x = month, y = suppression_ratio, color="clarity" )) +
  geom_point() + theme_bw()

#patients
ggplot(table_1_2017, aes(x = month, y = total_patients, color="clarity" )) +
  geom_point() + theme_bw()

ggplot(table_1_2017, aes(x = month, y = samples, color="clarity" )) +
  geom_point() + theme_bw()

ggplot(table_1_2017, aes(x = month, y = test_results, color="clarity" )) +
  geom_point() + theme_bw()

ggplot(table_1_2017, aes(x = month, y = total_suppressed, color="clarity" )) +
  geom_point() + theme_bw()

# --------------------

table_1_2018 <- uganda_vl[ year==2018,
                           .(total_patients = sum(patients_received), samples = sum(samples_received), 
                             test_results = sum(valid_results), total_suppressed = sum(suppressed),
                             suppression_ratio=100*(sum(suppressed)/sum(valid_results))),
                           by=month]

# scatter of plot % suppressed over time
ggplot(table_1_2018, aes(x = month, y = suppression_ratio )) +
  geom_point() + theme_bw()



# ----------------------------------------------

uganda_vl[year==2014, sum(patients_received), by = month]
uganda_vl[year==2015, sum(tb_status, na.rm=TRUE), by = month]


# females, % suppressed over time
#scatter plot
ggplot(uganda_vl[sex==1], aes(x = month, y = sup_ratio, color = year )) +
  geom_point() + theme_bw()

ggplot(uganda_vl, aes)

ggplot(uganda_vl[sex==1], aes(x = month, y = sup_ratio, color = )) +
  geom_jitter() +
  facet_grid(. ~ Species)


# table : patients, samples, valid results, %suppressed, % not suppressed, % active TB

tab1 <- uganda_vl[ ,
                   .(total_patients=sum(patients_received), samples=sum(samples_received), valid_results=sum(valid_results), total_suppressed=sum(suppressed), active_tb=sum(tb_status, na.rm=TRUE), 
                     suppression_ratio=100*(sum(suppressed)/sum(valid_results)), not_suppressed=100*(1-(sum(suppressed)/sum(valid_results))), 
                     suppressed_w_tb=100*(sum(sup_tb, na.rm=TRUE)/sum(suppressed)), suppressed_no_tb=100*(sum(sup_no_tb, na.rm=TRUE)/sum(suppressed))),
                   
                   by=.(month, year)]

# export table 1 as a .csv
write.csv(tab1, "J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/tab1.csv")

