

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


gos_data  <- data.table(read_excel('J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/Expenditures from GMS and GOS for PCE IHME countries.xlsx', sheet=as.character('GOS Mod-Interv - Extract')))

setnames(gos_data, c("Country","Grant Number", "Year", "Financial Reporting Period Start Date",
                     "Financial Reporting Period End Date", "Intervention", "Total Budget Amount (in budget currency)", 
                     "Total Expenditure Amount (in Budget currency)", "Component"), 
         c("country","grant_number", "year","start_date","end_date","intervention", "budget", "expenditure", "disease"))

mapping_for_R <- data.table(read.csv('J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/mapping_for_R.csv', fileEncoding="latin1"))


##clean the activity descriptions: 

gos_data$sda_orig <-gsub(paste(c(" ", "[\u2018\u2019\u201A\u201B\u2032\u2035]", "\\\\", "[\r\n]"), collapse="|"), "", gos_data$intervention)
gos_data$sda_orig <-tolower(gos_data$sda_orig)
gos_data$sda_orig <- gsub("[[:punct:]]", "", gos_data$sda_orig)


# test for missing SDAs from map
sdas_in_map = unique(mapping_for_R$sda_orig)
sdas_in_data = unique(gos_data$sda_orig)
if (any(!sdas_in_data %in% sdas_in_map)) { 
  stop('Map doesn\'t include cost categories that are in this data file!')
}
#unmapped_values <- gos_data[sda_orig%in%sdas_in_data[!sdas_in_data %in% sdas_in_map]]
#View(unique(unmapped_values$sda_orig))

gos_intervention <- copy(gos_data)

byVars = names(gos_intervention)[names(gos_intervention)%in%c('country', 'year', 'start_date', 'end_date','grant_number', 'disease', 'intervention')]
gos_intervention = gos_intervention[, list(budget=sum(na.omit(budget)), expenditure=sum(na.omit(expenditure))), by=byVars]

gos_intervention$budget[gos_intervention$budget<=0] <- NA
gos_intervention$expenditure[gos_intervention$expenditure<=0] <- NA


gos_intervention$disease <- as.factor(gos_intervention$disease)

gos_data  <- data.table(read_excel('J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/Expenditures from GMS and GOS for PCE IHME countries.xlsx',
                                   sheet=as.character('GMS SDAs - extract')))




read_gos_data <- function(dir, fileName, sheet_name){
  gos_data  <- data.table(read_excel(paste(dir, fileName, sep=""), sheet=as.character(sheet_name)))
  colnames(gos_data)[1] <- "country"
  colnames(gos_data)[2] <- "grant_number"
  colnames(gos_data)[5] <- "start_date"
  colnames(gos_data)[6] <- "end_date"
  colnames(gos_data)[7] <- "year"
  colnames(gos_data)[8] <- "sda_orig"
  colnames(gos_data)[9] <- "intervention"
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

og_gos <- copy(gos_data)



byVars = names(gos_data)[names(gos_data)%in%c('country', 'year', 'start_date', 'end_date','grant_number', 'disease', 'intervention')]
gos_data = gos_data[, list(budget=sum(na.omit(budget)), expenditure=sum(na.omit(expenditure))), by=byVars]

total_gos <- rbind(gos_data, gos_intervention)

## graph budgets vs. expenditures: 

# ---------------------------------------------
graphData <- copy(total_gos)
graphData[disease=='hiv', disease:='HIV/AIDS']
graphData[disease=='malaria', disease:='Malaria']
graphData[disease=='tb', disease:='Tuberculosis']
graphData[disease=='hss', disease:='Health Systems Strengthening']




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





gf_portfolio <- data.table(read.csv('H:/rt_data/ihme_dah_cod_uga_gtm_1990_2016.csv'))

setnames(gf_portfolio, "iso3_rc", "country")

get_dah_channels <- function(dah_channel){
  x <- dah_channel
  if(x=="GFATM"){
    x <- "GFATM"
  } else if (x=="BIL_USA"){
    x <- "BIL_USA"
  } else {
    x <- "other_dah"
  }
  return(x)
}


gf_portfolio$dah_channel <- mapply(get_dah_channels, gf_portfolio$channel)
gf_portfolio$dah_channel <- factor(gf_portfolio$dah_channel, levels=c("GFATM", "BIL_USA", "other_dah"))


swap_hss_dah_17

disease_columns <- unique(colnames(gf_portfolio)[(grep(paste(c("mal", "hiv", "hss", "tb"), collapse="|"), names(gf_portfolio)))])

gf_port_melted <- gf_portfolio[,c("year", "country", "source", "dah_channel",disease_columns), with=FALSE]
gf_port_melted <- gf_port_melted[dah_channel=="GFATM"]


gf_port_melted<- melt(gf_port_melted,id=c("year", "country", "source", "dah_channel"), 
                variable.name = "sda_orig", value.name="disbursement")



get_disease <- function(sda_orig){
  x <- sda_orig
  if(grepl("hiv", x)){
    x <- "HIV/AIDS"
  } else if (grepl("mal", x)){
    x <- "Malaria"
  } else if (grepl("tb", x)){
    x <- "Tuberculosis"
  } else {
    x <- "Health Systems Strengthening"
  }
  return(x)
}

gf_port_melted$disease<- mapply(get_disease, gf_port_melted$sda_orig)
gf_port_melted$budget <- gf_port_melted$disbursement



gos_comparison_data <- total_gos[,c("year", "country", "disease", "intervention", "budget"), with=FALSE]
gos_comparison_data$data_source <- "gos"
setnames(gos_comparison_data, "intervention", "sda_orig")
fgh_comparison_data <- gf_port_melted[,c("year", "country", "disease", "sda_orig", "budget"), with=FALSE]
fgh_comparison_data$data_source <- "fgh"


dah_data <- rbind(fgh_comparison_data, gos_comparison_data)

dah_data[disease=='hiv', disease:='HIV/AIDS']
dah_data[disease=='malaria', disease:='Malaria']
dah_data[disease=='tb', disease:='Tuberculosis']

dah_data[country=='COD', country:='Congo (Democratic Republic)']
dah_data[country=='UGA', country:='Uganda']
dah_data[country=='GTM', country:='Guatemala']

dah_data[disease=='hss', disease:='Health Systems Strengthening']
dah_data[data_source=='fgh', data_source:='fgh_portfolio']

gos_ind <- function(data_source,budget){
  x <- budget
  if(data_source=="gos"){
    x <- x
  } else {
    x <- 0 
  }
  return(x)
}

fgh_ind <- function(data_source,budget){
  x <- budget
  if(data_source=="fgh_portfolio"){
    x <- x
  } else {
    x <- 0 
  }
  return(x)
}


dah_data$gos_ind<- mapply(gos_ind, dah_data$data_source, dah_data$budget)
dah_data$fgh_ind <- mapply(fgh_ind,dah_data$data_source, dah_data$budget)

byVars = names(dah_data)[names(dah_data)%in%c("disease", "country", "year")]
graphData= dah_data[, list(fgh_ind=sum(na.omit((fgh_ind))), gos_ind=sum(na.omit((gos_ind)))), by=byVars]


graphData$fgh_ind[graphData$fgh_ind<=0] <- NA
graphData$gos_ind[graphData$gos_ind<=0] <- NA



gos_nat_plots <- list()
for (k in unique(na.omit(graphData$country))){
  subdata <- na.omit(graphData[country==k])
  plot <- (ggplot(subdata, aes(x=fgh_ind/1000000, y=gos_ind/1000000)) + 
             geom_point(aes(color=year, shape=disease)) +
             geom_abline(intercept=0, slope=1) + 
             #xlim(range) + 
             #ylim(range)+
             #ylim(0, 9) + 
             scale_colour_gradient(low = "red", high = "blue",
                                   space = "Lab", na.value = "grey50", guide = "colourbar") + 
             labs(x = "FGH Disbursement ($USD Millions)", y = "GOS Budget ($USD Millions)",
                  caption="Source: GOS, FGH",
                  title=paste(k, "GOS vs FGH Data"),
                  colour="year", shape="disease") +
             theme_bw(base_size=10) +
             theme(plot.title=element_text(hjust=.5))) 
  gos_nat_plots[[k]] <- plot
}

pdf("H:/rt_data/gos_vs_fgh_data.pdf", height=6, width=9)
invisible(lapply(gos_nat_plots, print))
dev.off()




