
# ----------------------------------------------
# Irena Chen
#
# 2/1/2018
## product SDAs over time faceted by grants:   
# ----------------------------------------------
# Set up R
library(ggplot2)
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

# ---------------------------------------------
##load the dataset: 

totalData <- data.table(read.csv('J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/cleaned_total_data.csv',
                                 fileEncoding = "latin1"))

# ---------------------------------------------
##subset the country you want from the aggregate data: 

# graphData <- totalData[country=="Uganda"]
# graphData <- totalData[country=="Guatemala"]
# graphData <- totalData[country=="Congo (Democratic Republic)"]


# ---------------------------------------------

## sum budget and exp. by the variables of interest 
#here, I'm doing SDA, grant, disease, and data source (gos, fpm etc.) by year 
byVars = names(graphData)[names(graphData)%in%c('program_activity', 'year', 'grant_number', 'disease', 'data_source')]
graphData = graphData[, list(budget=sum(na.omit(budget)), expenditure=sum(na.omit(expenditure))), by=byVars]

graphData$budget[graphData$budget<=0] <- NA
graphData$expenditure[graphData$expenditure<=0] <- NA


##apply the grant facet (Past/Upcoming/Rejected)
graphData$facet <- as.factor(mapply(appr_rej_indicators, graphData$year, graphData$data_source))
graphData$facet <- factor(graphData$facet ,levels=c("Past/Active", "In Iteration", "Initial", "Upcoming", "Iteration 2"))

##make the disease text nicer for the graphs: 
graphData <- disease_names_for_plots(graphData)


graphData[program_activity=="HIV/AIDS care and support", program_activity:="HIV/AIDS care, support and outreach"]
graphData[program_activity=="PBF", program_activity:="Performance Based Financing"]





### ---------------------------------------------------------------------------- 
##SDA over time faceted by grants


## we might need to reorder the grants based on start date so that they graph nicely: 

##order the grants first by year, and then change the levels so that they match up according to start year: 

## for DRC: 
##graphData <- graphData[!(data_source=="init_fpm")]
graphData[grant=="COD-H-CORDAID-810", grant:="COD-H-CORDAID"]
graphData[grant=="UGD-T-MoFPED", grant:="UGA-T-MoFPED"]
graphData <- graphData[with(graphData, order(year))]
graphData$grant <- factor(graphData$grant_number, levels=unique(graphData$grant_number))
graphData$grant <- str_wrap(graphData$grant, width=6)



##depending on how large the data is, you might want to split up the data by disease: 
## <- graphData[disease=="HIV/AIDS"]
##malData <- graphData[disease=="Malaria"]
##tbData <- graphData[disease=="Tuberculosis"]


### ---------------------------------------------------------------------------- 
##FOR UGANDA: HIV grants fit on one page: 

hivData$grant <- factor(hivData$grant, levels=unique(hivData$grant))


sda_plots <- list()
plot <- (ggplot(data=hivData, aes(x = year, y= budget/1000000, fill=program_activity)) + 
           geom_bar(# if you want 100% stacked, uncomment: position = "fill",
            stat="identity") + 
           scale_fill_manual(name="SDA", values =primColors) +
           theme_bw(base_size=14) +
           theme(strip.text.x = element_text(size = 8.5, colour = "black"), plot.caption=element_text(size=10)) +
           facet_grid(~grant,scales = "free_x", space="free_x") + 
           # if you want 100% stacked, uncomment:  scale_y_continuous(labels = percent_format()) +
           scale_x_continuous(name ="Year", breaks = seq(2003, 2020,2)) +
           labs(title=paste("HIV/AIDS", "Data at National Level"),
                x = "", y = "$ USD (Millions)", caption="Data Source: GOS, FPM"))

## do plot2 as the 100% bar graphs: 


sda_plots[[1]] <- plot
sda_plots[[2]] <- plot2


pdf("hiv_grant_sdas_overtime.pdf", height=6, width=9)
invisible(lapply(sda_plots, print))
dev.off()


### ---------------------------------------------------------------------------- 
##For Malaria data, we can facet by pre 2015 and post 2015: 
malData$grant <- factor(malData$grant, levels=unique(malData$grant))

mal_past_active_ind <- function(year){
  x <- "Past"
  if(year < 2015){
    x <- x
  } else {
    x <- "Active"
  }
  return(x)
}

malData$facet <- mapply(mal_past_active_ind, malData$year)
malData$facet<-factor(malData$facet,
                      levels=c("Past", "Active"))

sda_plots <- list()
for (k in unique(malData$facet)){
  subdata <- malData[facet==k]
  colScale <- scale_fill_manual(name="SDA", values =primColors) 
  plot <- (ggplot(data=subdata, aes(x = year, y= budget/1000000, fill=program_activity)) + 
             geom_bar(# position = "fill",
                      stat="identity") + 
             colScale +
             theme_bw(base_size=14) +
             theme(strip.text.x = element_text(size = 8.5, colour = "black"), plot.caption=element_text(size=10)) +
             facet_grid(~grant,scales = "free_x", space="free_x") + 
             # scale_y_continuous(labels = percent_format()) +
             scale_x_continuous(name ="Year", breaks = seq(2003, 2020,2)) +
             labs(title=paste(k, "Data at National Level"),
                  x = "", y = "$ USD (Millions)", caption="Data Source: GOS, FPM"))
  sda_plots[[k]] <- plot
}

pdf("malaria_grant_sdas_overtime.pdf", height=6, width=9)
invisible(lapply(sda_plots, print))
dev.off()


### ---------------------------------------------------------------------------- 
##TB data also fits on one page, so basically do the same as for the HIV data: 
tbData$grant <- factor( tbData$grant, levels=unique(tbData$grant))
sda_plots <- list()
plot <- (ggplot(data=tbData, aes(x = year, y= budget/1000000, fill=program_activity)) + 
           geom_bar(# if you want 100% stacked, uncomment: position = "fill",
             stat="identity") + 
           scale_fill_manual(name="SDA", values =primColors) +
           theme_bw(base_size=14) +
           theme(strip.text.x = element_text(size = 8.5, colour = "black"), plot.caption=element_text(size=10)) +
           facet_grid(~grant,scales = "free_x", space="free_x") + 
           # if you want 100% stacked, uncomment:  scale_y_continuous(labels = percent_format()) +
           scale_x_continuous(name ="Year", breaks = seq(2003, 2020,2)) +
           labs(title=paste("TB", "Data at National Level"),
                x = "", y = "$ USD (Millions)", caption="Data Source: GOS, FPM"))

## do plot2 as the 100% bar graphs: 

sda_plots[[1]] <- plot
sda_plots[[2]] <- plot2



