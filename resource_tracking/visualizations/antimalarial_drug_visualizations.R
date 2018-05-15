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

antimal_database <- data.table(read.csv("J:/Project/Evaluation/GF/outcome_measurement/gtm/prepped_data/antimalarial_prepped_data.csv"))


##if you want to group by quarters instead of months: 
antimal_database$quarter <- as.yearqtr(antimal_database$start_date)


antimal_database$department_drug <- paste(antimal_database$department, antimal_database$drug_type)

#sum up the data by the metrics that you want
#(I am doing it by department, drug type and start date)
byVars = names(antimal_database)[names(antimal_database)%in%c('department', 'drug_type',
                                                              'antimalarial_input','period', 'start_date')]
malData =antimal_database[, list(amount=sum(na.omit(amount))), by=byVars]

malData <- malData[drug_type!="Antimoniato"]
malData$department_drug <- paste(malData$department, malData$drug_type)
malData[, end_date:=start_date + period-1]

# "melt" long
tmp = copy(malData)
tmp$start_date = NULL
setnames(tmp, 'end_date', 'start_date')
malData$end_date = NULL
malData = rbind(malData, tmp)


byVars = names(malData)[names(malData)%in%c('department_drug','antimalarial_input','start_date')]
malData =malData[, list(amount=sum(na.omit(amount))), by=byVars]

# ---------------------------------------------
##we want to introduce NAs for the months that we don't have data, so that the lines will break when there are NAs

##create a complete range of months from the start and end years 
time_range <- seq(as.Date("2012-01-01"), as.Date("2017-12-01"), by="months")

##bind these into another dataset with the drug types and the drug metrics: 
create_quarter_range <- data.table(malData$antimalarial_input, malData$department_drug, time_range)
create_quarter_range <- unique(create_quarter_range)

setnames(create_quarter_range, c("antimalarial_input", "department_drug","start_date"))

##now join the data and create NAs when we have months that we don't have data for 
graphData <- merge(malData, create_quarter_range, all.y=TRUE, by=c("antimalarial_input","department_drug", "start_date"),
                   allow.cartesian = TRUE)

# ----------------------------------------------
##facet the graphs by certain groupings (e.g. put the demand metrics in one group, delivered drugs in another group)

facet_drug_metrics <- function(antimalaria_drug){
  x <- "Other"
  if(grepl("deliver", tolower(antimalaria_drug))){
    x <- "Drug Delivery"
  } else if (grepl("demand",tolower(antimalaria_drug))){
    x <- "Demand"
  } else if (grepl(paste0(c("exist","balance"),collapse="|"), tolower(antimalaria_drug))) {
    x <- "Drug Stock"
  } else {
    x <- x
  }
  return(x)
}

graphData$facet <- mapply(facet_drug_metrics, graphData$antimalarial_input)


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
for (k in unique(graphData$department_drug)){
  subdata <- graphData[department_drug==k]
  plot <- (ggplot(data=subdata, aes(x=start_date, y=amount, group=antimalarial_input, colour=antimalarial_input))
           + geom_point()
           + geom_line()
           +facet_wrap(~facet, scales = "free") 
           +  scale_fill_discrete(name = "Antimalarial Metric")
           +labs(title=paste(k, "over time"), x = "Start Date", y = "Antimalarial Drug"))
  prog_plots[[k]] <- plot
}

pdf("department_drug_metrics_overtime.pdf", height=6, width=9)
invisible(lapply(prog_plots, print))
dev.off()

