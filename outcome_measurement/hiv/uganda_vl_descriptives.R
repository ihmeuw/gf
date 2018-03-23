# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 3/23/2018
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

# set input/output directory
# ----------------------------------------------
dir <- 'J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard'

# upload the data with month, year, sex
uganda_vl <- readRDS(paste0(dir, "/webscrape_agg/sex_data.rds"))

# ----------------------------------------------

# view the data set and variable names
class(uganda_vl) # check that it is a data table
str(uganda_vl)
names(uganda_vl)

# list unique values for district and facility ids
uganda_vl[ , district_id, by=district_id] # 123 unique values
uganda_vl[ , unique(district_id)] # returns all unique values
uganda_vl[, facility_id, by=facility_id] # 2042 unique values


# check that 2014 and 2018 data only includes appropriate months (8/14 - 12/14, 1/18 - present)
uganda_vl[year==2014, sum(samples_received), by=month]
uganda_vl[year==2018, sum(samples_received), by=month]

# ----------------------------------------------
# add useful variables and prep the data 

# destring sex
uganda_vl[ , .(class(sex)) ]

# rename sex, sex1 so that you can work with the sex variable
setnames(uganda_vl, "sex", "sex1")
names(uganda_vl)

# destring sex1 into sex for use
uganda_vl[sex1 == "m", sex := 0]
uganda_vl[sex1 == "f", sex := 1]
uganda_vl[sex1 == "x", sex := NA]

uganda_vl[, .(sum(sex, na.rm=T)), by=year] # tells you the # of entries with female filter, not females
uganda_vl [, .(sum(patients_received)), by=.(year, sex)] # patients received by sex by year

# make date variable
uganda_vl[, date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]

# -----------

# Reshape indicators long
idVars <- c("_id", "facility_id", "district_id", "hub_id", "year", "month", "sex")
uganda_vl_long <- melt(uganda_vl, id.vars=idVars)

# make date variable
uganda_vl_long[, date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]


# ----------------------------------------------
## COUNTRY LEVEL SUMMARY STATISTICS AND GRAPHS

# summary tables and graphs of patients, samples, valid results, suppressed, and % suppressed over time
# all months and years

uganda_vl[,
          .(total_patients = sum(patients_received), samples = sum(samples_received), 
            total_results = sum(valid_results), total_suppressed = sum(suppressed),
            suppression_ratio=100*(sum(suppressed)/sum(valid_results))),
          by=.(month,year)]

# table 1: patients, samples, valid results, suppressed, %suppressed over time
# excludes March 2018 - change to present month
table_1 <- uganda_vl[!(month==3 & year==2018),
                      .(total_patients = sum(patients_received), total_samples = sum(samples_received), 
                        total_results = sum(valid_results), total_suppressed = sum(suppressed),
                        suppression_ratio=100*(sum(suppressed)/sum(valid_results))),
                        by=.(month,year)]
table_1 <- table_1[order(year, month)]
table_1

# add table 1 to uganda_vl 
# these values will repeat to match the number of values in the data table
uganda_vl <- merge(uganda_vl, table_1, by=c('month','year'))


# -----------
# graphs of counts 

# total patients received by month, year
ggplot(table_1, aes(x=factor(month), y=total_patients, col=factor(year), group=year)) + 
  geom_point(size=2.5, alpha=0.8) + geom_line(alpha=0.4) + theme_bw() +
  xlab("Month") + ylab("Patients received") + 
  labs(title = "Patients received by month, year", caption="Source: Uganda VL Dashboard", colour="Year")

# total valid viral load test results by month, year
ggplot(table_1, aes(x=factor(month), y=total_results, col=factor(year), group=year)) + 
  geom_point(size=2.5, alpha=0.8) + geom_line(alpha=0.4) + theme_bw() +
  xlab("Month") + ylab("Viral load test results") + 
  labs(title = "Viral load test results by month, year", caption="Source: Uganda VL Dashboard", colour="Year")

# total suppressed persons by month, year
ggplot(table_1, aes(x=factor(month), y=total_suppressed, col=factor(year), group=year)) + 
  geom_point(size=2.5, alpha=0.8) + geom_line(alpha=0.4) + theme_bw() +
  xlab("Month") + ylab("Total suppressed patients") + 
  labs(title = "Total virally suppressed patients by month, year", caption="Source: Uganda VL Dashboard", colour="Year")


# red colors for bar graph
colors <- c('Not Suppressed'='#de2d26', 'Suppressed'='#fc9272')

# stacked bar showing suppressed/not suppressed of valid test results by year 
ggplot(table_1, aes(x=factor(year), y=total_results, fill='Not Suppressed')) + 
    geom_bar(stat="identity") + 
    geom_bar(aes(y=total_suppressed, fill='Suppressed'), stat='identity') + 
    scale_fill_manual(name='', values=colors) + theme_bw() +
    xlab("Year") + ylab("Total valid test results") +
    labs(title = "Virally suppressed patients", caption="Source: Uganda VL Dashboard")


# -------------------------
# ratios 

# suppression ratio by month, year
ggplot(table_1, aes(x=factor(month), y=suppression_ratio, col = factor(year), group=year)) + 
  geom_point(size=2.5) + geom_line(alpha=0.4) + theme_bw() + ylim(0,100) +
   xlab("Month") + ylab("Percent suppressed of valid test results (%)") + 
  labs(title = "Percent of patients that are virally suppressed by month, year", caption="Source: Uganda VL Dashboard", colour="Year")

# zoom in on suppression ratio by month, year (reduce size of y axis)
ggplot(table_1, aes(x=factor(month), y=suppression_ratio, col = factor(year), group=year)) + 
  geom_point(size=2.5) + geom_line(alpha=0.4) + theme_bw() + 
  xlab("Month") + ylab("Percent suppressed of valid test results (%)") + 
  labs(title = "Percent of patients that are virally suppressed by month, year", caption="Source: Uganda VL Dashboard", colour="Year")


# ----------------------------------------------
# check for missing data by district and facility
# use a loop to graph data reporting by month, year for each district, facility

uganda_vl[, district_id, by=district_id] # 123 districts

# check if every district reported in every year, or some began later

dist_10 <- uganda_vl[district_id<=10,
                    .( total_pts = sum(patients_received), samples = sum(samples_received), 
                      results = sum(valid_results), suppressed = sum(suppressed), district_id, year),
                    by=.(district_id, year)]
dist_10 <- dist_10[order(district_id, year)]


# total patients received by year
ggplot(dist_10, aes(x=factor(year), y=total_pts, col=factor(district_id), group=(district_id))) + 
  geom_point (size=2.5, alpha=0.8) + geom_line(alpha=0.4) + theme_bw() +
xlab("Year") + ylab("Patients received") + 
  labs(title = "Annual patients received", caption="Source: Uganda VL Dashboard")

# print a list of the missing data 
dist_10[year==2014, district_id]
dist_10[year==2015, total_pts, by=district_id]
dist_10[year==2016, total_pts, by=district_id]
dist_10[year==2017, total_pts, by=district_id]
dist_10[year==2018, total_pts, by=district_id]


# check valid results
# total valid results by year
ggplot(dist_10, aes(x=factor(year), y=results, col=factor(district_id), group=(district_id))) + 
  geom_point (size=2.5, alpha=0.8) + geom_line(alpha=0.4) + theme_bw() +
  xlab("Year") + ylab("Patients received") + 
  labs(title = "Annual patients received", caption="Source: Uganda VL Dashboard")

# print a list of the missing data 
dist_10[, results, by=.(year, district_id)]

dist_10[year==2014, results, by=district_id]
dist_10[year==2015, results, by=district_id]
dist_10[year==2016, results, by=district_id]
dist_10[year==2017, results, by=district_id]
dist_10[year==2018, results, by=district_id]




dist_10 <- uganda_vl[district_id<=10,
                     .( total_pts = sum(patients_received), samples = sum(samples_received), 
                        results = sum(valid_results), suppressed = sum(suppressed), district_id, year),
                     by=.(district_id, year)]
dist_10 <- dist_10[order(district_id, year)]


# check valid results
# total valid results by year
ggplot(dist_10, aes(x=factor(year), y=results, col=factor(district_id), group=(district_id))) + 
  geom_point (size=2.5, alpha=0.8) + geom_line(alpha=0.4) + theme_bw() +
  xlab("Year") + ylab("Patients received") + 
  labs(title = "Annual patients received", caption="Source: Uganda VL Dashboard")



#---

ggplot(uganda_vl_long[facility_id==1101], aes(y=value, x=date, color=sex)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~variable, scales='free_y')



list_of_plots = NULL
i=1
for(f in unique(uganda_vl_long$facility_id)) {
  # make your graph
  
  list_of_plots[[i]] <- ggplot(uganda_vl_long[facility_id==f], aes(y=value, x=date, color=sex)) + 
    geom_point() + 
    geom_line() + 
    facet_wrap(~variable, scales='free_y')
  
  i=i+1
}



pdf('C:/Users/ccarelli/graphs.pdf', height=6, width=9)
for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
}
dev.off()
















# ----------------------------------------------
# tables and graphs by sex, all years

# females only
table_1_f <- uganda_vl[,
                      .(total_patients = sum(patients_received), samples = sum(samples_received), 
                        test_results = sum(valid_results), total_suppressed = sum(suppressed),
                        suppression_ratio=100*(sum(suppressed)/sum(valid_results))),
                      by=.(month,year, female)]

table_1_f <- table_1_f[order(year, month, female)]
table_1_f

write.csv(table_1_f, file="J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/tb_compare.csv", row.names=F)

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
plot_1_sex <- uganda_vl[ ,
                      .(total_patients = sum(patients_received), samples = sum(samples_received), 
                        test_results = sum(valid_results), total_suppressed = sum(suppressed),
                        suppression_ratio=100*(sum(suppressed)/sum(valid_results)), female),
                      by=.(year)]
plot_1_sex <- table_1[order(year, month)]


ggplot(plot_1_sex, aes(x=total_patients, fill=factor(female))) + geom_histogram(position="dodge", binwidth=100000)

ggplot(uganda_vl, aes(x=patients_received, y=))



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

