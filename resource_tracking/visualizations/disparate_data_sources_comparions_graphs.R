
# ----------------------------------------------
# Irena Chen
#
# 3/1/2018
# ### Disparate Data comparison graphs 
# ----------------------------------------------
# Set up R
rm(list=ls())
library(ggplot2)
library(tools)
library(data.table)
library(lubridate)
library(grDevices)
library(RColorBrewer)
library(reshape)
library(scales)
library(ggrepel)

# ----------------------------------------------
##load the data: 

totalData <- data.table(read.csv('J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/total_mapped_data.csv',
                                 fileEncoding = "latin1"))
fghData <- data.table(read.csv('J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/fgh_data_prepped.csv',
                               fileEncoding = "latin1"))
# ----------------------------------------------
## prep data 

## functions to be able to plot FGH and GOS data:



get_fgh_amount <- function(indicator, amount){
  x <- 0 
  if(indicator=="fgh"){
    x <- amount
  } else {
    x <- x
  }
  return(x)
}



get_gos_amount <- function(indicator, amount){
  x <- 0 
  if(indicator=="gos"){
    x <- amount
  } else {
    x <- x
  }
  return(x)
}


get_fpm_amount <- function(indicator, amount){
  x <- 0 
  if(indicator=="fpm"){
    x <- amount
  } else {
    x <- x
  }
  return(x)
}

### ----------------------------------------------
##time series plots: 

##FGH vs All others 


byVars = names(totalData)[names(totalData)%in%c('country', 'year', 'disease', 'data_source')]
graphData = totalData[, list(budget=sum(na.omit(budget))), by=byVars]


fghData <- fghData[, list(fgh_budget=sum(na.omit(disbursement))),by=byVars]
fghData$data_source <- NULL

##get the FGH "variable" column to repeat over for all of the other data sources: 
graphData <- merge(graphData, fghData, by=c("year", "country", "disease"), all.x = TRUE)

graphData$fpm_ind <- mapply(get_fpm_amount, graphData$data_source, graphData$variable)
graphData$fgh_ind <- mapply(get_fgh_amount, graphData$data_source, graphData$variable)
graphData$gos_ind <- mapply(get_gos_amount, graphData$data_source, graphData$variable)

byVars = names(graphData)[names(graphData)%in%c('year', 'country', 'disease')]
graphData = graphData[, list(fpm_ind = sum(na.omit(fpm_ind)), fgh_ind=sum(na.omit(fgh_ind)), gos_ind=sum(na.omit(gos_ind))), by=byVars]



graphData <- disease_names_for_plots(graphData)

##create color gradient for years: 
graphCols <- c('#F18500',
               '#EFC901',
               '#CDEC02',
               '#88E903',
               '#45E704',
               '#05E406',
               '#06E147',
               '#08DE85',
               '#09DCC2',
               '#045a8d',
               '#0AB6D9',
               '#0570b0',
               '#0B79D6',
               '#0B3ED4'
)

names(graphCols) <- unique(graphData$year)

colScale <- scale_fill_manual(name="Year", values =graphCols) 
## graph budgets vs. expenditures: 

gos_nat_plots <- list()
for (k in unique(na.omit(graphData$country))){
  subdata <- graphData[country==k]
  fit <- lm( gos_ind~ fgh_ind, data = na.omit(subdata))
  range = c(min(na.omit(subdata$fgh_ind/1000000)), max(na.omit(subdata$fgh_ind/1000000)))
  plot <- (ggplot(na.omit(subdata), aes(x=fgh_ind/1000000, y=gos_ind/1000000)) + 
             geom_point(aes(color=year, shape=disease), size=3) +
             geom_abline(intercept=0, slope=1) + 
             xlim(range) + 
             ylim(range)+
             geom_smooth(method='lm') +
             scale_color_gradient2(low='#e6e600', mid="#ed4c9d", high='#0000a1', midpoint=2010,
                                  space = "Lab",guide = "colourbar") +
             #ylim(0, 9) + 
             labs(y = "GOS Budget (USD millions)", x = "FGH Disbursements (USD millions)", 
                  caption="Source: GOS, FGH",
                  title=paste(k, "FGH vs GOS"),
                  subtitle = (paste0("reg. slope: ", round(coefficients(fit)[2], digits=3))),
                  colour="Year", shape="Disease") +
             theme_bw(base_size=12) +
             theme(plot.title=element_text(hjust=.5), 
                   plot.subtitle=element_text(size=8.5, hjust=0.5, face="bold", color="dark green")))
  gos_nat_plots[[k]] <- plot
}


pdf("J:/Project/Evaluation/GF/resource_tracking/multi_country/visualizations/fgh_vs_gos_graphs.pdf", height=6, width=9)
invisible(lapply(gos_nat_plots, print))
dev.off()



# ----------------------------------------------
##sum up budget and expenditures by country, year, etc. and DATA SOURCE: 

byVars = names(totalData)[names(totalData)%in%c('country', 'year', 'disease', 'data_source', 'grant_number','source')]
graphData = totalData[, list(budget=sum(na.omit(budget)), expenditure=sum(na.omit(expenditure))), by=byVars]

## GOS VS FPM for now: 
graphData$fpm_ind <- mapply(get_fpm_amount, graphData$data_source, graphData$budget)
graphData$gos_ind <- mapply(get_gos_amount, graphData$data_source, graphData$budget)


byVars = names(graphData)[names(graphData)%in%c('country', 'disease','grant_number')]
graphData = graphData[, list(budget=sum(budget), fpm_ind = sum(na.omit(fpm_ind)),
                             gos_ind=sum(na.omit(gos_ind))), by=byVars]

graphData$fpm_ind[graphData$fpm_ind==0] <- NA
## only plot grants where we have both GOS and FPM data (or else will just be on an axis)

gos_nat_plots <- list()
for (k in unique(na.omit(graphData$country))){
  subdata <- graphData[country==k]
  range_max <- max(na.omit(subdata$gos_ind/1000000), na.omit(subdata$fpm_ind/1000000))
  range = c(0, range_max)
  plot <- (ggplot(na.omit(subdata), aes(x=gos_ind/1000000, y=fpm_ind/1000000)) + 
             geom_point(aes(color=disease), size=3) +
             geom_abline(intercept=0, slope=1) + 
             xlim(range) + 
             ylim(range)+
             geom_text_repel(aes(gos_ind/1000000, fpm_ind/1000000, label=grant_number)) +
             #ylim(0, 9) + 
             labs(x = "GOS Budget (USD millions)", y = "FPM Disbursements (USD millions)", 
                  caption="Source: GOS, FGH",
                  title=paste(k, "FGH vs GOS")) +
             theme_bw(base_size=12) +
             theme(plot.title=element_text(hjust=.5)))
  gos_nat_plots[[k]] <- plot
}

pdf("J:/Project/Evaluation/GF/resource_tracking/multi_country/visualizations/fpm_vs_gos_graphs.pdf", height=6, width=9)
invisible(lapply(gos_nat_plots, print))
dev.off()
