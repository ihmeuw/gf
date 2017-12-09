

# ----------------------------------------------
# Irena Chen
#
# 11/27/2017
# ### GOS DATA graphs 

# TO DO
# fix time series graph so that there are gaps where appropriate (use `group` aesthetic)

# ----------------------------------------------
# Set up R
rm(list=ls())
library(ggplot2)
library(dplyr)
library(tools)
library(data.table)
library(lubridate)
library(grDevices)
library(RColorBrewer)
library(readxl)

# ----------------------------------------------
## prep data 

read_gos_data <- function(){
  gos_data  <- data.table(read_excel("J:/Project/Evaluation/GF/resource_tracking/gtm/gf/Expenditures from GMS and GOS for PCE IHME countries.xlsx", sheet = "GMS SDAs - extract", col_types = c("text", "text", "date", "date", "date", "date", "numeric", "text", "text", "numeric", "numeric", "text")))
  colnames(gos_data)[2] <- "grant_number"
  colnames(gos_data)[5] <- "start_date"
  colnames(gos_data)[6] <- "end_date"
  colnames(gos_data)[8] <- "gf_program"
  colnames(gos_data)[9] <- "cost_category"
  colnames(gos_data)[10] <- "budget"
  colnames(gos_data)[11] <- "expenditure"
  colnames(gos_data)[12] <- "gos_disease"
  
  map_disease <- unique(gos_data$gos_disease)
  names(map_disease) <- c("tb", "malaria", "hiv", "hss", "hiv/tb")
  
  kDT = data.table(map_disease = names(map_disease), value = TRUE, gos_disease = unname(map_disease))
  gos_data[kDT, on=.(gos_disease), disease := i.map_disease]
  gos_data$gos_disease <- NULL
  gos_data[disease=='hiv/tb', disease:='hiv']
  return(gos_data)
}


## graph budgets vs. expenditures: 

# ---------------------------------------------
graphData <- copy(gos_data)
graphData[disease=='hiv', disease:='HIV/Aids']
graphData[disease=='malaria', disease:='Malaria']
graphData[disease=='tb', disease:='Tuberculosis']
graphData[disease=='hss', disease:='HSS']




gos_nat_plots <- list()
for (k in unique(na.omit(graphData$Country))){
  fit <- lm(expenditure ~ budget, data = na.omit(graphData[Country==k]))
  range = c(min(na.omit(graphData[Country==k]$expenditure/1000000)), max(na.omit(graphData[Country==k]$budget/1000000)))
  plot <- (ggplot(na.omit(graphData[Country==k]), aes(x=budget/1000000, y=expenditure/1000000)) + 
             geom_point(aes(color=Year, shape=disease)) +
             geom_abline(intercept=0, slope=1) + 
             xlim(range) + 
             ylim(range)+
             geom_smooth(method='lm') + 
             coord_trans(x="log10") +
             scale_colour_gradient(low = "red", high = "blue",
                                   space = "Lab", na.value = "grey50", guide = "colourbar") +
             #ylim(0, 9) + 
             labs(x = "Budget USD (Millions)", y = "Expenditure USD (Millions)", caption="Source: GOS",
                  title=paste(k, "Budget vs Expenditure Data"),
                  subtitle = (paste0("reg. slope: ", round(coefficients(fit)[2], digits=3))),
                  colour="Year", shape="Disease") +
             theme_bw(base_size=16) +
             theme(plot.title=element_text(hjust=.5), 
                   plot.subtitle=element_text(size=10, hjust=0.5, face="bold", color="dark green"))) 
  gos_nat_plots[[k]] <- plot
}


pdf("country_gos_budget_expenditures.pdf", height=6, width=9)
invisible(lapply(gos_nat_plots, print))
dev.off()


logData <- copy(graphData)
logData[logData  <= 0] <- NA

gos_log_plots <- list()
for (k in unique(na.omit(logData$Country))){
  fit <- lm(expenditure ~ budget, data = na.omit(graphData[Country==k]))
  range = c(min(na.omit(graphData[Country==k]$expenditure/1000000)), max(na.omit(graphData[Country==k]$budget/1000000)))
  plot <- (ggplot(na.omit(graphData[Country==k]), aes(x=budget/1000000, y=expenditure/1000000)) + 
             geom_point(aes(color=Year, shape=disease)) +
             geom_abline(intercept=0, slope=1) + 
             # xlim(range) + 
             # ylim(range)+
             geom_smooth(method='glm',formula=y~x) + 
             scale_x_log10(breaks=c(1,5,30), limits= c(1, max(na.omit(graphData[Country==k]$budget/1000000)))) +
             scale_y_log10(breaks=c(1,5,30), limits= c(1, max(na.omit(graphData[Country==k]$budget/1000000)))) +
             scale_colour_gradient(low = "red", high = "blue",
                                   space = "Lab", na.value = "grey50", guide = "colourbar") +
             #ylim(0, 9) + 
             labs(x = "log(Budget USD (Millions))", y = "log(Expenditure USD (Millions))", caption="Source: GOS",
                  title=paste(k, "Budget vs Expenditure Data"),
                  subtitle = (paste0("reg. slope: ", round(coefficients(fit)[2], digits=3))),
                  colour="Year", shape="Disease") +
             theme_bw(base_size=16) +
             theme(plot.title=element_text(hjust=.5), 
                   plot.subtitle=element_text(size=10, hjust=0.5, face="bold", color="dark green"))) 
  gos_log_plots[[k]] <- plot
}


