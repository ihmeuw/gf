# ----------------------------------------------
# Irena Chen
#
# 3/1/2018
# ### Map FPM SDAs to Sicoin $$ by municipality
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
library(dplyr)
# ----------------------------------------------

##load the data: 

totalData <- data.table(read.csv('J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/total_resource_tracking_data.csv',
                                 fileEncoding = "latin1"))

gtmData <- totalData[(!country%in%c("Uganda", "Congo (Democratic Republic)"))&(data_source%in%c("sicoin", "fpm"))]
gtmData <- gtmData[source=="gf"]


##sum up budget (as "variable") by year, disease, and data source 
byVars = names(gtmData)[names(gtmData)%in%c('year', 'disease', 'data_source','module')]
gtmData = gtmData[, list(budget=sum(na.omit(budget)), expenditure=sum(na.omit(expenditure))
                          , disbursement=sum(na.omit(disbursement))), by=byVars]


gtmData  <- disease_names_for_plots(gtmData)
gtmData  <- data_source_names_for_plots(gtmData)

##PLOTS:
gos_nat_plots <- list()
for (k in unique(gtmData$disease)){
  subdata <- gtmData[disease==k]
  subdata$module <- factor(subdata$module, levels=unique(subdata$module))
  plot <- (ggplot(na.omit(subdata), aes(x=year, y=budget/1000000,color=module)) + 
             geom_line(aes(linetype=data_source), size=1)+
             geom_point()+ 
            # scale_color_manual(name="Data Source", values =sourceColors) +
             labs(y = "USD (mil.)", x = "Year", 
                  caption="Source: FPM, SICOIN",
                  title=paste(k, "module comparisons"))+
             theme_bw(base_size=10.5) +
             theme(plot.title=element_text(hjust=.5)))
  gos_nat_plots[[k]] <- plot
}



pdf("J:/Project/Evaluation/GF/resource_tracking/multi_country/sicoin_fpm_graphs.pdf", height=9, width=12)
invisible(lapply(gos_nat_plots, print))
dev.off()





