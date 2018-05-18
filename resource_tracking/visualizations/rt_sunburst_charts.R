
# ----------------------------------------------
# Irena Chen
#
# 5/15/2018
# ### General Visualizations for Bar Graphs over time:  
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
library(reshape)
library(scales)
library(ggrepel)
library(stringr)

# ----------------------------------------------
#########load the dataset:  ########
# ----------------------------------------------
totalData <- data.table(read.csv('J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/cleaned_total_data.csv',
                                 fileEncoding = "latin1"))

# ---------------------------------------------
######### subset the country you want from the aggregate data  #######
# ----------------------------------------------

# graphData <- totalData[country=="Uganda"]
# graphData <- totalData[country=="Guatemala"]
# graphData <- totalData[country=="Congo (Democratic Republic)"]

# ---------------------------------------------
######### sum budget and exp. by the variables of interest ############
# ---------------------------------------------
##for upcoming grants, do this:
#graphData = graphData[data_source=="fpm"&year>2017]

#sum up the budget/exp by the variables of interest:
##if you want to plot this over year, add in the temporal variable 
byVars = names(graphData)[names(graphData)%in%c('gf_module', 'gf_intervention',"code", 'grant_number', 'disease')]
graphData = graphData[, list(budget=sum(na.omit(budget)), expenditure=sum(na.omit(expenditure))), by=byVars]

graphData$budget[graphData$budget<=0] <- NA
graphData$expenditure[graphData$expenditure<=0] <- NA
graphData[gf_module=="Health management information system and monitoring and evaluation"
        , gf_module:="Health management information system and M&E"]

# ---------------------------------------------
######### Create the "module" dataset ############
# ---------------------------------------------

modData<-graphData[with(graphData, order(disease, grant_number, gf_module,gf_intervention, budget)), ]

intData <- copy(modData)

byVars = names(modData)[names(modData)%in%c('gf_module','grant_number')]
modData=modData[, list(budget=sum(na.omit(budget)), expenditure=sum(na.omit(expenditure))), by=byVars]

modData[,budget_sum:=sum(na.omit(budget)), by="grant_number"]
modData[,fraction:=budget/budget_sum, by="grant_number"]
modData[, ymax := cumsum(fraction),by="grant_number"]
modData[, ymin := c(0, head(ymax, n=-1)),by="grant_number"]
modData[, pos := cumsum(fraction)-fraction/2,by="grant_number"]

# --------------------------------------------
######### Make the pie charts of  ############
# ---------------------------------------------




prog_plots <- list()
for (k in unique(modData$grant_number)){
  subdata <- modData[grant_number==k]
  plot <- (ggplot(data=subdata,aes(x=1,y=fraction, fill=gf_module,ymax=ymax,ymin=ymin)) +
             coord_polar(theta="y") + 
             geom_col(position = 'stack')+
             theme_void() +
             theme(plot.title = element_text(hjust = 0.5),
                   legend.title=element_text(size=16) ,legend.text=element_text(size=8.5))+ 
             theme(strip.text.x = element_text(size = 12, face="bold")) +
             scale_fill_manual(name="GF Module", values =primColors) +
             guides(linetype=guide_legend(nrow=2))+
             ggtitle(paste(k, ": 2018-2020 Modules")))
  
  prog_plots[[k]]<- plot
}

pdf("J:/Project/Evaluation/GF/resource_tracking/uga/visualizations/piecharts/uga_upcoming_modules_piecharts.pdf", height=6, width=9)
invisible(lapply(prog_plots, print))
dev.off()


# ---------------------------------------------
######### Create the "intervention" dataset ############
# ---------------------------------------------

intData[,budget_sum:=sum(na.omit(budget)), by="grant_number"]
intData[,fraction:=budget/budget_sum, by="grant_number"]
intData[, ymax := cumsum(fraction),by="grant_number"]
intData[, ymin := c(0, head(ymax, n=-1)),by="grant_number"]
intData[, pos := cumsum(fraction)-fraction/2,by="grant_number"]

graphData <- graphData[order(-graphData$budget), ]
d <- data.table(graphData, key=c("grant_number"))
int_subset <- d[, head(.SD, 4), by=grant_number]

interventions <- c(as.character(unique(intData$gf_intervention))
                   ,as.character(unique(intData$gf_module)))##change datasets when necessary

cols <- rep(primColors, length.out=length(interventions))
names(cols) <- interventions

# ---------------------------------------------
######### Make the sunburst charts ############
# ---------------------------------------------

modData[modData$budget<=0]$budget <- NA
intData[intData$budget<=0]$budget <- NA


sunburst_plots <- list()
for (k in unique(intData$grant_number)){
  mod_data <- modData[grant_number==k]
  int_data <-intData[grant_number==k]
  subdata <- int_data[gf_intervention%in%int_subset[grant_number==k]$gf_intervention]
  plot <- (ggplot()+
             geom_rect(data=int_data,aes(fill=gf_intervention,xmax=6, xmin=5, ymax=ymax,ymin=ymin)) + 
             geom_rect(data=mod_data, aes(xmin=0, xmax=5, ymax=ymax,ymin=ymin, fill=gf_module)) +
             coord_polar(theta="y") + 
             geom_text_repel(data =subdata,
                         aes(label = str_wrap(gf_intervention, 20), x =6, y=pos, size=1),
                         size=3.5,force=3,nudge_y=1,nudge_x=2) +
             geom_col(position = 'stack')+
            scale_fill_manual(name="Modules",values=cols, breaks = unique(int_data$gf_module)) + 
             theme_void() +
             theme(plot.title = element_text(hjust = 0.5),
                   legend.title = element_text(size=14),legend.text=element_text(size=8)) +
             labs(title=paste("Grant Modules and Interventions", k))
  )
  sunburst_plots[[k]]<- plot
}


pdf("J:/Project/Evaluation/GF/resource_tracking/uga/visualizations/pie_charts/uga_upcoming_modules_sunburst.pdf", height=6, width=9)
invisible(lapply(sunburst_plots, print))
dev.off()


