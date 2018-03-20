
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

totalData <- data.table(read.csv('J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/total_resource_tracking_data.csv',
                                 fileEncoding = "latin1"))
fghData <- data.table(read.csv('J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/fgh_data_prepped.csv',
                               fileEncoding = "latin1"))

totalData$budget <- as.numeric(totalData$budget)

fgh_gf <- fghData[source=="gf"]
totalData <- totalData[source=="gf"]

##sum all of the municipalities to national for now: 
totalData[data_source=="sicoin", country:="Guatemala"]
# ----------------------------------------------
## prep data 

### ----------------------------------------------
##FGH vs All others - time series 

##sum up budget (as "variable") by year, disease, and data source 
byVars = names(totalData)[names(totalData)%in%c('year', 'disease', 'country','data_source')]
graphData = totalData[, list(variable=sum(na.omit(budget))), by=byVars]

##edit Sicoin Data: 
graphData[data_source=="sicoin", country:="Guatemala"]
graphData$country <- factor(graphData$country, levels=c("Congo (Democratic Republic)","Guatemala","Uganda"))


##rename FGH disburesment to "variable"
fgh_gf <- fgh_gf[, list(variable=sum(na.omit(disbursement))),by=byVars]

##rbind the two datasets together 
graphData <- rbind(graphData,fgh_gf)

#clean up the disease names and set colors for each data source: 
graphData <- disease_names_for_plots(graphData)
graphData = graphData[, list(variable=sum(na.omit(variable))), by=byVars]


sourceColors <- c("#000080",
                  "#ff7f00",
                  "#006400",
                  "#6a3d9a",
                  "#33a02c",
                  "#fb9a99",
                  "#1f78b4",
                  "#b20059")

names(sourceColors) <- levels(graphData$data_source)

countryData =graphData[, list(variable=sum(na.omit(variable))), by=c("country", "year","data_source")]
diseaseData = graphData[, list(variable=sum(na.omit(variable))), by=c("disease", "year","data_source")]
##PLOTS:
gos_nat_plots <- list()
for (k in unique(graphData$country)){
  subdata <- graphData[country==k]
  plot <- (ggplot(na.omit(subdata), aes(x=year, y=variable/1000000, group=data_source, color=data_source)) + 
             geom_line(size=1) +
             geom_point()+ 
             scale_color_manual(name="Data Source", values =sourceColors) +
             labs(y = "USD (mil.)", x = "Year", 
                  caption="Source: GOS, FGH, FPM, SICOIN",
                  title=paste(k, "FGH vs. Other Sources Comparisons"),
                  subtitle=("FGH disbursement, all others budget")) +
             theme_bw(base_size=12) +
             theme(plot.title=element_text(hjust=.5)))
  gos_nat_plots[[k]] <- plot
}

pdf("J:/Project/Evaluation/GF/resource_tracking/multi_country/visualizations/time_series/data_sources_over_time_by_disease.pdf", height=6, width=9)
invisible(lapply(gos_nat_plots, print))
dev.off()



### ----------------------------------------------
##FGH vs All others - scatterplot of disease: 


byVars = names(totalData)[names(totalData)%in%c('disease', 'data_source')]
scatterData = totalData[, list(budget=sum(na.omit(budget))), by=byVars]

##create a "FGH" specific variable for disbursement: 
fgh_disease<- fgh_gf[, list(fgh_disb=sum(na.omit(disbursement))),by="disease"]
fgh_disease$data_source <- NULL
##DISEASES: 

##get the FGH "variable" column to repeat over for all of the other data sources: 
graphData <- merge(scatterData, fgh_disease, by="disease", all.x = TRUE)

### ----------------------------------------------
##PLOTS



graphData <- disease_names_for_plots(graphData)


gos_nat_plots <- list()
for (k in unique(na.omit(graphData$disease))){
  subdata <- graphData[disease==k]
  max_fgh = max(subdata$fgh_disb)
  max_others <- max(subdata$budget)
  range = c(0, max(max_fgh, max_others)/10000000)
  plot <- (ggplot(na.omit(subdata), aes(x=fgh_disb/10000000, y=budget/10000000)) + 
             geom_point(aes(color=data_source), size=3) +
             scale_color_manual(name="Data Source", values =sourceColors) +
             geom_abline(intercept=0, slope=1) + 
             xlim(range)+
             ylim(range) +
             labs(y = "Other Sources - Budget (USD 10 mil.)", x = "FGH Disbursements (USD 10 mil.)", 
                  caption="Source: GOS, FGH, FPM",
                  title=paste(k, "FGH vs. Other Sources Comparisons"),
                  colour="Data Source") +
             theme_bw(base_size=12) +
             theme(plot.title=element_text(hjust=.5)))
  gos_nat_plots[[k]] <- plot
}



pdf("J:/Project/Evaluation/GF/resource_tracking/multi_country/visualizations/scatterplots/fgh_vs_all_by_disease.pdf", height=6, width=9)
invisible(lapply(gos_nat_plots, print))
dev.off()



##COUNTRY: 
byVars = names(totalData)[names(totalData)%in%c('country', 'data_source')]
scatterData = totalData[, list(budget=sum(na.omit(budget))), by=byVars]

##create a "FGH" specific variable for disbursement: 
fgh_id<- fgh_gf[, list(fgh_disb=sum(na.omit(disbursement))),by="country"]
fgh_id$data_source <- NULL
##DISEASES: 

##get the FGH "variable" column to repeat over for all of the other data sources: 
graphData <- merge(scatterData, fgh_id, by="country", all.x = TRUE)

graphData = graphData[, list(fgh_disb=sum(na.omit(fgh_disb)), budget=sum(na.omit(budget))), by=c("country", "data_source")]

gos_nat_plots <- list()
for (k in unique(na.omit(graphData$country))){
  subdata <-graphData[country==k]
  max_fgh = max(subdata$fgh_disb)
  max_others <- max(subdata$budget)
  range = c(0, max(max_fgh, max_others)/10000000)
  plot <- (ggplot(na.omit(subdata), aes(x=fgh_disb/10000000, y=budget/10000000)) + 
             geom_point(aes(color=data_source), size=3) +
             scale_color_manual(name="Data Source", values =sourceColors) +
             geom_abline(intercept=0, slope=1) + 
             xlim(range)+
             ylim(range) +
             labs(y = "Other Sources - Budget (USD 10 mil.)", x = "FGH Disbursements (USD 10 mil.)", 
                  caption="Source: GOS, FGH, FPM",
                  title=paste(k, "FGH vs. Other Sources Comparisons"),
                  colour="Data Source") +
             theme_bw(base_size=12) +
             theme(plot.title=element_text(hjust=.5)))
  gos_nat_plots[[k]] <- plot
}



pdf("J:/Project/Evaluation/GF/resource_tracking/multi_country/visualizations/scatterplots/fgh_vs_all_by_country.pdf", height=6, width=9)
invisible(lapply(gos_nat_plots, print))
dev.off()

### ----------------------------------------------
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

graphData$fpm_ind <- mapply(get_fpm_amount, graphData$data_source, graphData$variable)
graphData$fgh_ind <- mapply(get_fgh_amount, graphData$data_source, graphData$variable)
graphData$gos_ind <- mapply(get_gos_amount, graphData$data_source, graphData$variable)

byVars = names(graphData)[names(graphData)%in%c('year', 'country', 'disease')]
graphData = graphData[, list(fpm_ind = sum(na.omit(fpm_ind)), fgh_ind=sum(na.omit(fgh_ind)), gos_ind=sum(na.omit(gos_ind))), by=byVars]

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
