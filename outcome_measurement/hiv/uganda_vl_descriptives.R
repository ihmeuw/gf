# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 3/11/2018
# Run descriptive statistics from the Uganda VL Dashboard data
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
uganda_vl <- readRDS("J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape_agg/sex/full_data.rds")

# set output directory
# ----------------------------------------------
setwd("J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard")

# ----------------------------------------------

# view the data set and variable names
View(uganda_vl) 
class(uganda_vl) # check that it is a data table
str(uganda_vl)
names(uganda_vl)

# ----------------------------------------------

# destring sex, active tb status
uganda_vl[ , .(class(tb), class(sex)), ]

uganda_vl[tb == "n", tb_status := 0]
uganda_vl[tb == "y", tb_status := 1]
uganda_vl[tb == "x", tb_status := NA]

uganda_vl[sex == "m", female := 0]
uganda_vl[sex == "f", female := 1]
uganda_vl[sex == "x", female := NA]

# create a suppression ratio and add to the data table
uganda_vl[ , (sup_ratio = 100*(suppressed/valid_results))]

uganda_vl[, .(sum(tb_status, na.rm=TRUE)), by = year] # people w active tb by year
uganda_vl[, .(sum(female, na.rm=TRUE)), by = year] # females by year
#uganda_vl[, .(sum(sup_ratio, na.rm=TRUE)), by = year] # % suppressed by year


uganda_vl[year==2014, sum(patients_received), by = month]
uganda_vl[year==2015, sum(tb_status, na.rm=TRUE), by = month]

# ----------------------------------------------

# create a variable for suppressed persons with active tb for ease of manipulation
uganda_vl[tb_status == 0 & suppressed==1, sup_tb := 0]
uganda_vl[tb_status == 1 & suppressed==1, sup_tb := 1]
uganda_vl[, .(sum(sup_tb, na.rm=TRUE)), by=year ]

# create a variable for suppressed persons without active tb 
uganda_vl[tb_status == 0 & suppressed==1, sup_no_tb := 1]
uganda_vl[tb_status == 1 & suppressed==1, sup_no_tb := 0]
uganda_vl[, .(sum(sup_no_tb, na.rm=TRUE)), ]

# ----------------------------------------------

# general look at major descriptives
uganda_vl[year==2014, suppressed, by = month ]


# ----------------------------------------------
# tables

# table 1: patients, samples, valid results, suppressed, %suppressed over time
tab1 <- uganda_vl[ ,
                  .(total_patients = sum(patients_received), samples = sum(samples_received), 
                    valid_results = sum(valid_results), total_suppressed = sum(suppressed),
                    suppression_ratio=100*(sum(suppressed)/sum(valid_results))), 
                    by=.(month, year)]

# scatter of plot % suppressed over time
ggplot(uganda_vl[year==2014], aes(x = month, y = sum(sup_ratio))) +
  geom_point() + theme_bw()



# females only
tab2 <- uganda_vl[female==1,
        .(total_patients = sum(patients_received), samples = sum(samples_received), 
         valid_results = sum(valid_results), total_suppressed = sum(suppressed),
         suppression_ratio=100*(sum(suppressed)/sum(valid_results))), 
         by=.(month, year)]


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

#tables by sex 
f_subset <- uganda_vl[uganda_vl$female==1, ]
tab2 <- f_subset[ ,.(total_patients=sum(patients_received), samples=sum(samples_received), valid_results=sum(valid_results), total_suppressed=sum(suppressed), active_tb=sum(tb_status, na.rm=TRUE), 
           suppression_ratio=100*(sum(suppressed)/sum(valid_results)), not_suppressed=100*(1-(sum(suppressed)/sum(valid_results))), 
            suppressed_w_tb=100*(sum(sup_tb, na.rm=TRUE)/sum(suppressed)), suppressed_no_tb=100*(sum(sup_no_tb, na.rm=TRUE)/sum(suppressed))),
           by=.(month, year)]

# export table 2 as a .csv
write.csv(tab1, "J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/tab1.csv")

m_subset <- uganda_vl[which(uganda_vl$female==1), ]
    tab3 <- m_subset[ ,.(total_patients=sum(patients_received), samples=sum(samples_received), valid_results=sum(valid_results), total_suppressed=sum(suppressed), active_tb=sum(tb_status, na.rm=TRUE), 
                     suppression_ratio=100*(sum(suppressed)/sum(valid_results)), not_suppressed=100*(1-(sum(suppressed)/sum(valid_results))), 
                     suppressed_w_tb=100*(sum(sup_tb, na.rm=TRUE)/sum(suppressed)), suppressed_no_tb=100*(sum(sup_no_tb, na.rm=TRUE)/sum(suppressed))),
                     by=.(month, year)]
    
# export table 3 as a .csv
    write.csv(tab1, "J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/tab1.csv")




# by district



# to get total patients by sex, month, year, #add district after bug fix





# 2 graphs on one plot, males female

ggplot(mtcars, aes(x = wt, y = mpg, col = cyl, fill = am)) +
  geom_point(shape = 21, size = 4, alpha = 0.6) + theme_classic()
  facet_grid(. ~ sex)



#trash bin
  
  
  ggplot(uganda_vl, aes(x = month, y = sup_2014)) +
    geom_point() + theme_bw()
  
  ggplot(uganda_vl, aes(suppressed)) + geom_histogram()
  
  
  ggplot(uganda_vl, aes)
  
  uganda_vl[ year==2014, .(sup_2014 = sum(suppressed)), by=month]
  
  ggplot(uganda_vl, aes(x = month, y = sup_2014 )) + geom_smooth() + theme_bw()
  tab_fem <- uganda_vl[ ,
                        .(total_patients=sum(patients_received), samples=sum(samples_received), valid_results=sum(valid_results), total_suppressed=sum(suppressed), active_tb=sum(tb_status, na.rm=TRUE), 
                          suppression_ratio=100*(sum(suppressed)/sum(valid_results)), not_suppressed=100*(1-(sum(suppressed)/sum(valid_results))), 
                          suppressed_w_tb=100*(sum(sup_tb, na.rm=TRUE)/sum(suppressed)), suppressed_no_tb=100*(sum(sup_no_tb, na.rm=TRUE)/sum(suppressed))),
                        
                        by=.(sex, month, year)]

uganda_vl[, tb_status:=factor(tb, levels = c("n", "y", "x"), labels = c("No", "Yes", "Unknown") )]
uganda_vl[ , tb_status:=replace(tb, list=c("n", "y", "x"), values=c(0,1,NA) ), ]
uganda_vl[ , .(replace(tb, list=c("n"=0, "y"=1, "x"=NA))) ]
#replace(tb_status, c("n", "y", "x"), c("n"=0, "y"=1, "x"=NA))

uganda_vl[, .(monthly_patients = sum(patients_received)), by = .(sex,month,year)]


uganda_frame <- data.frame(uganda_vl[, .(monthly_patients = sum(patients_received)), by = .(sex,month, year)])
write.csv(uganda_frame, "J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/uganda_frame.csv")



uganda_vl[, .(suppression_ratio = sum(suppressed)/sum(valid_results)), ] #all time suppression ratio
uganda_vl[, .(suppression_ratio = sum(suppressed)/sum(valid_results)), by=year] #annual ratios
out <- uganda_vl[, .(suppression_ratio = sum(suppressed)/sum(valid_results)), by=.(year, month)] 




uga_stat <- uganda_vl[, .(patients=sum(patients_received), test_results=sum(valid_results), total_suppressed=sum(suppressed), suppression_ratio=(sum(suppressed)/sum(valid_results))), by=.(month, year)]

write.csv(uga_stat, "J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/uga_stat.csv")





# to check or see initial ratios


#suppression ratios
uganda_vl[, sum(patients_received), ]
uganda_vl[, sum(valid_results), ]
uganda_vl[, sum(suppressed), ]

#check this using data frames 
x <- data.frame(uganda_vl[, sum(suppressed), by=year ])
y <- data.frame(uganda_vl[, sum(valid_results), by=year ])
x/y

#trash bin

uganda_vl[, .(total_patients = sum(patients_received)), by = .(year)]
uganda_vl[, .(monthly_patients = sum(patients_received)), by = .(month, year)]

summary(uganda_vl$patients_received, digits=4)


uganda_vl[, .(total_patients = sum(patients_received)), by = .(month, year, district_id.y)]
uganda_vl[, .(total_patients = sum(patients_received)), by = .(month, year, district_id.x)]
# these don't match - why??
# district names didn't make the merge...


# graphs of patients received
uganda_vl[, plot(year, sum(patients_received)), by=.(year)]
uganda_vl[, hist(patients_received), by=.(year)]



#trash bin
 sapply(uganda_vl$patients_received, mean, na.rm=TRUE)

#write.csv(uganda_vl, file = "J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/uganda_vl.csv", na="")






