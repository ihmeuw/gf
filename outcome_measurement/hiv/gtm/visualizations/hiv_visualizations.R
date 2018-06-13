# ----------------------------------------------
# Irena Chen
#
# 3/1/2018
# ### Create visualizations for the GTM anti-malarial drug information 
# ----------------------------------------------

# Set up R
rm(list=ls())
library(ggplot2)
library(zoo)
library(dplyr)
library(tools)
library(data.table)
library(lubridate)
library(grDevices)
library(RColorBrewer)
library(readxl)
library(reshape)
library(scales)
library(stringr)

# ----------------------------------------------
##load data 
sc_database <- data.table(read.csv("J:/Project/Evaluation/GF/outcome_measurement/gtm/HIV/prepped_data/condom_prepped_data.csv"))



#sum up the data by the metrics that you want
#(I am doing it by department, drug type and start date)
byVars = names(sc_database)[names(sc_database)%in%c('period', 'start_date')]
hivData =sc_database[, list(condom_consumption=sum(na.omit(condom_consumption)),
                            condom_stockage=sum(na.omit(condom_stockage))), by=byVars]

hivData[, end_date:=start_date + period-1]

# "melt" long
tmp = copy(hivData)
tmp$start_date = NULL
setnames(tmp, 'end_date', 'start_date')
hivData$end_date = NULL
hivData = rbind(hivData, tmp)


# ---------------------------------------------
##we want to introduce NAs for the months that we don't have data, so that the lines will break when there are NAs

##create a complete range of months from the start and end years 
start_date <- data.table(seq(as.Date("2015-01-01"), as.Date("2017-12-01"), by="months"))
setnames(start_date, "start_date")
##now join the data and create NAs when we have months that we don't have data for 
graphData <- merge(hivData, start_date, all.y=TRUE, by="start_date",
                   allow.cartesian = TRUE)


graphData <- melt(graphData, id.vars = c("start_date", "period"), variable.name = "condom_metric")
# ----------------------------------------------
##facet the graphs by certain groupings (e.g. put the demand metrics in one group, delivered drugs in another group)

# ----------------------------------------------
##histograms: 
prog_plots <- list()
for (k in unique(antimal_database$antimalarial_input)){
  subdata <- antimal_database[antimalarial_input==k]
  plot <- (ggplot(data=subdata, aes(x=start_date)) + geom_bar()+
             labs(title=paste(k, "histogram")))
  prog_plots[[k]] <- plot
}

pdf("histograms_overtime.pdf", height=6, width=9)
invisible(lapply(prog_plots, print))
dev.off()
# ----------------------------------------------

##time series (lines with points)
prog_plots <- list()
plot <- (ggplot(data=graphData, aes(x=start_date, y=value/1000000,colour=condom_metric))
       + geom_point()
       + geom_line()
       +  scale_fill_discrete(name = "Condom Metric")
       +labs(title="GTM Condoms at National Level", x = "Start Date", y = "# of Condoms (in hundred thousands)"))

prog_plots[[1]] <- plot
           

pdf("J:/Project/Evaluation/GF/outcome_measurement/gtm/HIV/visualizations/national_level_condoms_overtime.pdf", height=6, width=9)
invisible(lapply(prog_plots, print))
dev.off()

