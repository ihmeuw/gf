######################################################
# Summarizing Guatemala and regional incidence estimates from GBD
# J. Ross
# July 17, 2018
# Updated March 2019 with GBD 2017 results
#
#########################################################

rm(list=ls())

library(ggplot2)
library(data.table)

gbd_tb<-fread("J:/Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/gbd_2017/GBD results tool 2015-2017 region cent America.csv")

gbd_tb<- gbd_tb[,c('location_id', 'location_name', 'cause_id', 'year', 'val')]

annual <- gbd_tb[, list(inc_sum=sum(val ,na.rm=TRUE)), by=c('year', 'location_id', 'location_name')]

annual <- annual[order(-year)]

#The following section is older code that refers to GBD 2016


tb<-read.csv("J:/Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/gbd_2016/tb_no_hiv_bothsex_inc_count.csv")
tb<-tb[tb$Year>1999 & complete.cases(tb),]

hiv_tb<-read.csv("J:/Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/gbd_2016/hiv_tb_incidence_counts.csv")
hiv_tb<-hiv_tb[hiv_tb$sex_id==3,]

#reshape long to wide by cause
w <- reshape(hiv_tb, 
             timevar = "cause_id",
             idvar = c("sex","year_id"),
             direction = "wide")


w$hiv_tb_count <-w$inc_count.948 + w$inc_count.949 + w$inc_count.950

w_hiv_tb<-w[,c("year_id","hiv_tb_count")]

df<-merge(tb, w_hiv_tb, by.x=c("Year"), by.y=c("year_id"))

df$tb_all<-df$Value+df$hiv_tb_count

gbd2016_inc<-df[,c("Year", "Location", "Age", "Sex", "Measure", "tb_all")]

write.csv(gbd2016_inc, file="J:/Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/gbd_2016/gbd_tb_all_forms.csv")

