# ----------------------------------------------
# Caitlin O'Brien- Carelli
# Visualize the SIGPRO testing data sets

# ----------------------------------------------

#-----------------------------------------------
# Install packages 
# ----------------------------------------------
rm(list=ls())
library(lubridate)
library(data.table)
library(ggplot2)
library(stringr)
#---------------------------------------
# Set up directories 
#----------------------------------------

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set working and output directories
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/hiv/')

#----------------------------------------
# load sigpro testing data 

dt = readRDS(paste0(dir, 'prepped/sigpro_testing_transfer_prepped.RDS'))

# factor the test results for visualization
dt$result = factor(dt$result, c("indeterminate", "nonreactive",
                                "reactive", "test not done"),
                   c("Indeterminate result", "Non-reactive", "Reactive", 
                     "Test not done"))

pdf(paste0(dir, 'outputs/sigpro_data_quality_visuals.pdf'), height=8, width=12)

#----------------------------------------
# tests performed by data set

set = dt[ ,.(value=sum(value)), by=.(set, date)]

ggplot(set, aes(x=date, y=value, color=set)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x="Date", y="Count", color="Excel sheet", 
       title="Number of screenings and tests performed by data set*",
       subtitle="*sigpro_f1 includes pregnant women, while other sheets do not") +
       theme(text = element_text(size=16))

#----------------------------------------
# number of people screened and tested by gender

tests = dt[ ,.(value=sum(value)), by=.(gender, result, date)]

# may indicate pregnant women were not captured in subsequent data sets
ggplot(tests, aes(x=date, y=value, color=result)) +
  geom_point() +
  geom_line() +
  facet_wrap(~gender, scales='free_y') +
  theme_bw() +
  labs(x="Date", y="Count", color="HIV test result",
       title="HIV screening and test results by gender identity") +
      theme(text = element_text(size=18))

# subset test results 
ggplot(tests[result=='Reactive' | result=='Non-reactive'], aes(x=date, y=value, color=gender)) +
  geom_point() +
  geom_line() +
  facet_wrap(~result, scales='free_y') +
  theme_bw() +
  labs(x="Date", y="Count", color="Gender", 
       title="Test results by gender identity", 
       subtitle="Excludes indeterminate results") +
        theme(text = element_text(size=16))

#----------------------------------------
# by sr code and sr code/data set 

# sr code
code = dt[ ,.(value=sum(value)), by=.(sr_code, date)]

ggplot(code, aes(x=date, y=value, color=sr_code)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x="Date", y="Count", color="SR code", 
       title="Number of screenings and tests performed by SR code") +
      theme(text = element_text(size=18))

# SR codes by data set
code_set = dt[ ,.(value=sum(value)), by=.(sr_code, set, date)]

ggplot(code_set, aes(x=date, y=value, color=sr_code)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~set, scales='free_y') +
  labs(x="Date", y="Count", color="SR code", 
       title="Number of screenings and tests performed by data set and SR code") +
  theme(text = element_text(size=16))

#----------------------------------------
# screenings and tests performed by department

dep = dt[ ,.(value=sum(value)), by=.(department, date)]

ggplot(dep, aes(x=date, y=value, color=department)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x="Date", y="Count", color="Department", 
       title="Number of screenings and tests performed by Department",
       subtitle="First data set in the time series is missing geographic information") +
  theme(text = element_text(size=16))

#----------------------------------------

dev.off()
  