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

# check that 2014 and 2018 data only includes appropriate months (8/14 - 12/14, 1/18 - present)
uganda_vl[year==2014, sum(samples_received), by=month]
uganda_vl[year==2018, sum(samples_received), by=month]

# ----------------------------------------------
# add useful variables and prep the data 

# destring sex
uganda_vl[ , .(class(sex)) ]
View(uganda_vl)
# rename sex, sex1 so you can use sex as a variable later
names(uganda_vl)[names(uganda_vl)=="sex"] <- "sex1"

# destring sex
uganda_vl[sex1 == "m", female := 0]
uganda_vl[sex1 == "f", female := 1]
uganda_vl[sex1 == "x", female := NA]

uganda_vl[, .(sum(female, na.rm=T)), by=year] # tells you the # of entries with female filter, not females
uganda_vl [, .(sum(patients_received)), by=.(year, female)] # patients by sex by year


# ----------------------------------------------
# summary tables of patients, samples, valid results, suppressed, and % suppressed over time
# all months and years

# table 1: patients, samples, valid results, suppressed, %suppressed over time
table_1 <- uganda_vl[ ,
                      .(total_patients = sum(patients_received), samples = sum(samples_received), 
                        test_results = sum(valid_results), total_suppressed = sum(suppressed),
                        suppression_ratio=100*(sum(suppressed)/sum(valid_results))),
                        by=.(month,year)]
table_1 <- table_1[order(year, month)]

# total suppressed persons by month, year
ggplot(table_1, aes(x=factor(month), y=total_suppressed, col=factor(year), group=year)) + 
  geom_point(size=2.5, alpha=0.8) + geom_line(alpha=0.4) + theme_bw() +
  xlab("Month") + ylab("Total suppressed patients") + 
  labs(title = "Virally suppressed patients by month, year", caption="Source: Uganda VL Dashboard", colour="Year")

  # add a second graph to the suppressed persons graph for valid results (combine this graph with first graph)
  ggplot(table_1, aes(x=factor(month), y=test_results, col=factor(year), group=year)) + 
  geom_point(size=2.5, alpha=0.8) + geom_line(alpha=0.4) + theme_bw() +
  xlab("Month") + ylab("Valid viral load test results") + 
  labs(title = "Valid viral load tests by month, year", caption="Source: Uganda VL Dashboard", colour="Year")

# suppression ratio by month, year
ggplot(table_1, aes(x=factor(month), y=suppression_ratio, col = factor(year), group=year)) + 
  geom_point(size=2.5) + geom_line(alpha=0.4) + theme_bw() + ylim(0,100) +
   xlab("Month") + ylab("Percent suppressed of valid test results (%)") + 
  labs(title = "Percentage virally suppressed by month, year", caption="Source: Uganda VL Dashboard", colour="Year")

# zoom in on suppression ratio by month, year (reduce size of y axis)
ggplot(table_1, aes(x=factor(month), y=suppression_ratio, col = factor(year), group=year)) + 
  geom_point(size=2.5) + geom_line(alpha=0.4) + theme_bw() + 
  xlab("Month") + ylab("Percent suppressed of valid test results (%)") + 
  labs(title = "Percentage virally suppressed by month, year", caption="Source: Uganda VL Dashboard", colour="Year")


# ----------------------------------------------
# tables and graphs by sex, all years

# females only
table_1_f <- uganda_vl[female==1 ,
                      .(total_patients = sum(patients_received), samples = sum(samples_received), 
                        test_results = sum(valid_results), total_suppressed = sum(suppressed),
                        suppression_ratio=100*(sum(suppressed)/sum(valid_results))),
                      by=.(month,year)]

table_1_f <- table_1_f[order(year, month)]
table_1_f

# males only
table_1_m <- uganda_vl[female==0 ,
                       .(total_patients = sum(patients_received), samples = sum(samples_received), 
                         test_results = sum(valid_results), total_suppressed = sum(suppressed),
                         suppression_ratio=100*(sum(suppressed)/sum(valid_results))),
                       by=.(month,year)]

table_1_m <- table_1_m[order(year, month)]
table_1_m

# --------------------
# females only graphs

# total suppressed females/males by month, year
ggplot(table_1_f, aes(x=factor(month), y=total_suppressed, col=factor(year), group=year)) + 
  geom_point(size=2.5, alpha=0.8) + geom_line(alpha=0.4) + theme_bw() +
  xlab("Month") + ylab("Total suppressed female patients") + 
  labs(title = "Virally suppressed female patients by month, year", caption="Source: Uganda VL Dashboard", colour="Year")

# suppression ratio by month, year
ggplot(table_1_f, aes(x=factor(month), y=suppression_ratio, col = factor(year), group=year)) + 
  geom_point(size=2.5) + geom_line(alpha=0.4) + theme_bw() + ylim(0,100) +
  xlab("Month") + ylab("Percent suppressed of valid test results (%)") + 
  labs(title = "Percentage virally suppressed females by month, year", caption="Source: Uganda VL Dashboard", colour="Year")

# zoom in on suppression ratio by month, year (reduce size of y axis)
ggplot(table_1_f, aes(x=factor(month), y=suppression_ratio, col = factor(year), group=year)) + 
  geom_point(size=2.5) + geom_line(alpha=0.4) + theme_bw() + 
  xlab("Month") + ylab("Percent suppressed of valid test results (%)") + 
  labs(title = "Percentage virally suppressed females by month, year", caption="Source: Uganda VL Dashboard", colour="Year")

# --------------------
# bar plots by sex




# ----------------------------------------------
# tb status




# ----------------------------------------------
# annual tables and plots

# --------------------

# 2014

table_1_2014 <- uganda_vl[ year==2014,
                           .(total_patients = sum(patients_received), samples = sum(samples_received), 
                             test_results = sum(valid_results), total_suppressed = sum(suppressed),
                             suppression_ratio=100*(sum(suppressed)/sum(valid_results))),
                           by=month]

# scatter of plot % suppressed over time
ggplot(table_1_2014, aes(x = month, y = suppression_ratio )) +
  geom_point() + theme_bw()


# geom_point(shape = 21, size = 4, alpha=0.6) + theme_bw()

# change merge file to import entire year for ease of interpretation
# drop march of 2018?

ggplot(plot_1, aes(x=total_patients, fill=factor(female))) + geom_bar(position="dodge")

ggplot(plot_1, aes(x=month, y=total_patients, fill=factor(female))) + geom_bar(stat="identity", position="fill")

ggplot(mtcars, aes(x = factor(cyl), fill = factor(am))) + geom_bar()

# used stacked bar for tb status



# --------------------

table_1_2015 <- uganda_vl[ year==2015,
                           .(total_patients = sum(patients_received), samples = sum(samples_received), 
                             test_results = sum(valid_results), total_suppressed = sum(suppressed),
                             suppression_ratio=100*(sum(suppressed)/sum(valid_results))),
                           by=month]

# scatter of plot % suppressed over time
ggplot(table_1_2015, aes(x = month, y = suppression_ratio )) +
  geom_point() + theme_bw()



table_2015 <- uganda_vl[year==2015 ,
                        .(total_patients = sum(patients_received), samples = sum(samples_received), 
                          test_results = sum(valid_results), total_suppressed = sum(suppressed),
                          suppression_ratio=100*(sum(suppressed)/sum(valid_results))),
                        by=.(month,year)]
table_2015 <- table_2015[order(month)]

ggplot(table_2015, aes(x=month, y=total_suppressed, col = factor(year), group=year)) + 
  geom_point() + geom_line() + theme_bw() 

ggplot(table_2015, aes(x=month, y=test_results)) + 
  geom_point() + geom_line() + theme_bw() 

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

