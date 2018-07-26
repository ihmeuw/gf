######################################################
# Treatment seeking adjustment for GTM TB
# Authors: J. Ross (Adjuster function D. Phillips)
# July 23, 2018
#
#
######################################################

library(data.table)
library(ggplot2)

rm(list=ls())

seek<-read.csv("J:/Project/Evaluation/GF/outcome_measurement/gtm/treatment_seeking/ENSMI_2014/GT_cough and fever tx seeking.csv")

#Begins with 851 cluster values
hist(seek$seektx_ch, breaks=50)
#>500 0's range 0-17
hist(seek$seektx_wm, breaks=50)
#>500 0's range 0-13

seek$name<-NULL

muni<-aggregate(seek, by=list(seek$municode), FUN=sum)
#muni <- seek[, lapply(.SD, sum), by='municode', .SDcols=names(seek)[!names(seek)=='municode']]
#Data table version

#Now down to 278 muni values
hist(muni$seektx_ch, breaks=50)
hist(muni$seektx_wm, breaks=50)

muni$seek_p_ch<-muni$seektx_ch/muni$cough_ch
muni$seek_p_wm<-muni$seektx_wm/muni$cough_wm

hist(muni$seek_p_ch, breaks=50)
hist(muni$seek_p_wm, breaks=50)

p1<-ggplot(muni, aes(x=seek_p_ch, y=seek_p_wm)) + geom_point()
p1
#Enjoy the beautiful correlation in this scatter, quite meaningless, unfortunately, as they are asking the women about treatment seeking for the child.



#-----------------------------------------------------------------------------------------------------
#Experimental adjuster function

data = data.table(muni=c('a','b','c','d'), cases=c(200,240,160,210), tx_seeking=c(.99,.8,.6,.5))
total=1000

adjuster = function(beta, data, total) { 
  adjustedCases = data$cases/(data$tx_seeking*beta*(1-data$tx_seeking))
  return(sqrt((sum(adjustedCases)-total)^2))
}

est = optim(.1, adjuster, gr=NULL, data, total)

adjustedCases =  data$cases/(data$tx_seeking*est$par*(1-data$tx_seeking))
