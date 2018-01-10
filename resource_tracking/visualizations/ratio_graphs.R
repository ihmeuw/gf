# ----------------------------------------------
# Set up R
library(ggplot2)
library(dplyr)
library(data.table)
library(lubridate)
gos_data  <- data.table(read_excel("J:/Project/Evaluation/GF/resource_tracking/gtm/gf/Expenditures from GMS and GOS for PCE IHME countries.xlsx", sheet = "GMS SDAs - extract", col_types = c("text", "text", "date", "date", "date", "date", "numeric", "text", "text", "numeric", "numeric", "text")))

gtm_gos <- subset(gos_data, Country=="Guatemala")


## get differences and ratios: 

histData <- copy(graphData)
histData<- histData[,-c(3:6, 8:9)]

##stuff for grants: 

grantData <- copy(histData)
grantData = grantData[, list(budget=sum(na.omit((budget))), expenditure=sum(na.omit(expenditure))), by=c("Country", "grant_number", "Year", "disease")]
grantData = grantData[order(Country, Year, grant_number, disease)]
grantData[, cumulative_budget:=cumsum(na.omit(budget)),by=c("Country",'disease','grant_number' )]
grantData[, cumulative_exp:=cumsum(na.omit(expenditure)),by=c("Country",'disease','grant_number')]
grantData[,cum_ratio:=cumulative_exp/cumulative_budget]


codData <- grantData[Country=="Congo (Democratic Republic)"]
gtmData <- grantData[Country=="Guatemala"]
ugaData <- grantData[Country=="Uganda"]

gos_grant_ratio_plots <- list()
for (k in unique(na.omit(ugaData$disease))){
  # range = c(min(na.omit(graphData[Country==k]$expenditure/1000000)), max(na.omit(graphData[Country==k]$budget/1000000)))
  plot <- (ggplot(na.omit(ugaData[disease==k]), aes(x=Year,y=cum_ratio)) + 
             geom_point(aes(color=grant_number,  size=budget/1000000)) +
             geom_line(aes(color=grant_number))+
            # facet_wrap(~disease, drop=T, scales='free') +
             geom_abline(intercept=0, slope=1) + 
             # xlim(range) + 
             # geom_smooth(method='auto',formula=log(y)~log(x)) +
             #ylim(0, 9) + 
             labs(x = "Year", y = "Ratio (Cum. Exp./Cum.Budget)", caption="Source: GOS",
                  title=paste(k, "Cumulative Ratio over Time"), shape="Disease", size="Budget (in mil)", group="Grant") +
             theme_bw(base_size=16) +
             scale_x_discrete(name ="Year", 
                              limits=c(2004,2008,2012, 2016)) +
             theme(plot.title=element_text(hjust=.5))) 
  gos_grant_ratio_plots[[k]] <- plot
}

pdf("uga_grant_cumul_ratios_over_time.pdf", height=6, width=9)
invisible(lapply(gos_grant_ratio_plots, print))
dev.off()







##stuff for ratios 
histData = histData[, list(budget=sum(na.omit((budget))), expenditure=sum(na.omit(expenditure))), by=c("Country", "Year", "disease")]
histData[, ratio:=expenditure/budget]
histData[, difference:=budget-expenditure]
histData = histData[order(Country, Year, disease)]
histData[, cumulative_budget:=cumsum(na.omit(budget)),by=c("Country",'disease')]
histData[, cumulative_exp:=cumsum(na.omit(expenditure)),by=c("Country",'disease')]
histData[,cum_ratio:=cumulative_exp/cumulative_budget]

## create cumulative values 

histData= melt(histData, id.vars=c("Country", "disease", "Year"))


##make histograms and time series plots: 

gos_hist_diff_plots <- list()
for (k in unique(na.omit(histData$Country))){
  plot <- (ggplot(data = histData[Country==k], aes(x = difference/1000000)) + 
             geom_histogram(bins=5, col="black", fill="cornflower blue") +
             labs(x = "Budget - Expenditure Difference", y = "Count", caption="Source: GOS",
                  title=paste(k, "Histogram of Budget & Expenditure Difference")) +
             ylim(0, 5.5) +
             facet_wrap(~disease, scales = "free_y"))
  gos_hist_diff_plots[[k]] <- plot
}


pdf("gos_hist_difference.pdf", height=6, width=9)
invisible(lapply(gos_hist_diff_plots, print))
dev.off()

for (k in unique(na.omit(graphData$Country))){
  hist(graphData[Country=="Uganda"]$ratio/100,  main="Expenditure/Budget Ratio (USD Millions)",
       xlab=paste("Uganda Budget Ratio"), col="green", xlim=c(-10, 600), ylim=c(0, 350))
}

### 


gos_ratio_plots <- list()
for (k in unique(na.omit(histData$Country))){
  # range = c(min(na.omit(graphData[Country==k]$expenditure/1000000)), max(na.omit(graphData[Country==k]$budget/1000000)))
  plot <- (ggplot(data = na.omit(histData[Country==k]), aes(x =ratio)) + 
             geom_histogram(bins=15, col="black", fill="cornflower blue") +
             labs(x = "Expenditure/Budget Ratio", y = "Count", caption="Source: GOS",
                  title=paste(k, "Histogram of Expenditure/Budget Ratio")) +
             facet_wrap(~disease, scales = "free_y"))
  gos_ratio_plots[[k]] <- plot
}

pdf("gos_hist_ratios.pdf", height=6, width=9)
invisible(lapply(gos_ratio_plots, print))
dev.off()



gos_ratio_plots <- list()
for (k in unique(na.omit(histData$Country))){
  # range = c(min(na.omit(graphData[Country==k]$expenditure/1000000)), max(na.omit(graphData[Country==k]$budget/1000000)))
  plot <- (ggplot(na.omit(histData[Country==k]), aes(x=Year,y=cum_ratio)) + 
             geom_point(aes(color=disease,  size=budget/1000000)) +
             geom_line(aes(color=disease))+
             # facet_wrap(~disease, drop=T, scales='free') +
             geom_abline(intercept=0, slope=1) + 
             # xlim(range) + 
             # geom_smooth(method='auto',formula=log(y)~log(x)) +
             #ylim(0, 9) + 
             labs(x = "Year", y = "Ratio (Cum. Exp./Cum.Budget)", caption="Source: GOS",
                  title=paste(k, "Cumulative Ratio over Time"), shape="Disease", size="Budget (in mil)") +
             theme_bw(base_size=16) +
             scale_x_discrete(name ="Year", 
                              limits=c(2004,2008,2012, 2016)) +
             theme(plot.title=element_text(hjust=.5))) 
  gos_ratio_plots[[k]] <- plot
}

pdf("gos_cumul_ratios_over_time.pdf", height=6, width=9)
invisible(lapply(gos_ratio_plots, print))
dev.off()
