
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
library(readxl)

# ----------------------------------------------
## prep data 

totalData <- data.table(read.csv('J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/cleaned_total_data.csv',
                                 fileEncoding = "latin1"))
fghData <- data.table(read.csv('J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/fgh_data_prepped.csv',
                                 fileEncoding = "latin1"))


gosData <- totalData[data_source=="gos"]
byVars = names(gosData)[names(gosData)%in%c('country', 'year', 'disease', 'data_source')]
gosData = gosData[, list(variable=sum(na.omit(budget))), by=byVars]

fghData <- fghData[, list(variable=sum(na.omit(disbursement))),by=byVars]



graphData <- rbind(gosData, fghData)


# ----------------------------------------------
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


graphData$fgh_ind <- mapply(get_fgh_amount, graphData$data_source, graphData$variable)
graphData$gos_ind <- mapply(get_gos_amount, graphData$data_source, graphData$variable)

byVars = names(graphData)[names(graphData)%in%c('year', 'country', 'disease')]
graphData = graphData[, list(fgh_ind=sum(na.omit(fgh_ind)), gos_ind=sum(na.omit(gos_ind))), by=byVars]



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
