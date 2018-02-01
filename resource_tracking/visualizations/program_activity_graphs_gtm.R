
# ----------------------------------------------
# Irena Chen
#
# 11/27/2017
# Make graphs of program activities 

# TO DO
# fix time series graph so that there are gaps where appropriate (use `group` aesthetic)

# ----------------------------------------------
# Set up R
rm(list=ls())
library(gsubfn)
library(ggplot2)
library(dplyr)
library(tools)
library(data.table)
library(lubridate)
library(grDevices)
library(RColorBrewer)
library(readxl)

# ----------------------------------------------
## load the sicoin data 
sicoin <- data.table(read.csv("prepped_sicoin_data_1201_ic.csv", fileEncoding="UTF-8"))
sicoin$start_date <- as.Date(sicoin$start_date, '%m/%d/%Y') 

if (!is.numeric(sicoin$budget)) sicoin[,budget:=as.numeric(budget)]
if (!is.numeric(sicoin$disbursement)) sicoin[,disbursement:=as.numeric(disbursement)]
if (!is.numeric(sicoin$expenditure)) sicoin[,expenditure:=as.numeric(expenditure)]

# collapse the data to national level - we're ignoring municipalities for now: 
byVars = names(sicoin)[!names(sicoin)%in%c('budget','disbursement','expenditure','grant_number', 'cost_category','loc_id', 'data_source')]
nat_level = sicoin[, list(budget=sum(na.omit(budget)), disbursement=sum(na.omit(disbursement)), expenditure=sum(na.omit(expenditure))), by=byVars]


# remap some of the values so that they are "prettier" for the graphs: 
graphData = copy(nat_level)
graphData[source=='gf', source:='Global Fund']
graphData[source=='ghe', source:='Government']
graphData[disease=='hiv', disease:='HIV/Aids']
graphData[disease=='malaria', disease:='Malaria']
graphData[disease=='tb', disease:='Tuberculosis']

# ----------------------------------------------
## This creates a scatterplot of budget & disbursement data w/ respect to disease and time (year)
nat_plots <- list()
for (k in unique(graphData$source)){ ##plotting GF and GHE differently for now: 
  plot <- (ggplot(graphData[source==k], aes(x=budget/1000000, y=disbursement/1000000)) + 
    geom_point(aes(color=year(start_date), shape=disease)) +
    geom_abline(intercept=0, slope=1) + 
    geom_smooth(method='lm') + 
    scale_colour_gradient(low = "red", high = "blue",
                          space = "Lab", na.value = "grey50", guide = "colourbar") +
    ggtitle(paste(k, "Budget vs Disbursement Data")) +
    #ylim(0, 9) + 
    labs(x = "Budget USD (Millions)", y = "Disbursement USD (Millions)", caption="Data Source: SICOIN", 
         colour="Year", shape="Disease") +
    theme_bw(base_size=16) +
    ylim(0, max(graphData[source==k]$budget/1000000)) +
    theme(plot.title=element_text(hjust=.5)))
  nat_plots[[k]] <- plot
}


# save the graphs to your desktop 
pdf("sicoin_budget_disbursement_over_time.pdf", height=6, width=9)
invisible(lapply(nat_plots, print))
dev.off()

# ----------------------------------------------
##This code produces Budget and Disbursement time series (lines over time) comparing GF and GHE data
# melt quantities long
graphData = melt(graphData, id.vars=c('source', 'start_date', 'disease', 'period'))
graphData$value[graphData$value == 0] <- NA

graphData[, variable:=toTitleCase(as.character(variable))] ##create a title 
graphData = graphData[!is.na(value)] ##remove NAs 

# add an "end of time series" observation so the step-line doesn't suddenly end
tmp = graphData[year(start_date)==max(year(start_date))]
tmp[, start_date:=start_date+period-1]
graphData = rbind(graphData, tmp)

# graph time series per disease
nat_plots <- list()
for (k in unique(graphData$disease)){ ##plotting each disease separately for now 
  plot <- (ggplot(graphData[disease==k & variable=='Budget'], aes(x=start_date, y=value/1000000, color=source)) + 
             geom_step(aes(linetype='Budget'), size=1.25, alpha=.5) +
             geom_step(data=graphData[disease==k & variable=='Disbursement'], aes(linetype='Disbursement'), size=1.25) +
             labs(title=paste(k, "data at national level"), x = "", y = "USD (Millions)",
                  caption="Data Source: SICOIN", color='Source') +
             scale_linetype_manual('Quantity', values=c('Budget'=1,'Disbursement'=4)) +
             theme_bw(base_size=16) + 
             theme(plot.title=element_text(hjust=.5)))
  nat_plots[[k]] <- plot
}

# save graphs
pdf("ghe_vs_gf_data_over_time.pdf", height=6, width=9)
invisible(lapply(nat_plots, print))
dev.off()


# ----------------------------------------------
# compute cumulative totals
graphData = graphData[order(start_date)]
graphData[, cumulative:=cumsum(value), by=c('source', 'disease', 'variable')]



# graph cumulative time series per disease
for (k in unique(graphData$disease)){
  plot <-  (ggplot(graphData[disease==k & variable=='Budget'], 
                   aes(x=start_date, y=cumulative/1000000, color=source)) + 
              geom_step(aes(linetype='Budget'), size=1.25, alpha=.5) +
              geom_step(data=graphData[disease==k & variable=='Disbursement'], aes(linetype='Disbursement'), size=1.25) +
              labs(title=paste(k, "cumulative data at national level")
                   , x = "",y = "USD (Millions)", caption="Data Source: SICOIN", color='Source') +
              scale_linetype_manual('Quantity', values=c('Budget'=1,'Disbursement'=4)) +
              theme_bw(base_size=16) +
              theme(plot.title=element_text(hjust=.5)))
  nat_plots[[paste0(k,'cumulative')]] <- plot
}

# save graphs
pdf("cumgraph_ghe_vs_gf_overtime.pdf", height=6, width=9)
invisible(lapply(nat_plots, print))
dev.off()

# ----------------------------------------------
## BUDGET vs DISBURSEMENT - municipality level 

## aggregate by disease and start date
byVars = names(muni_sicoin)[names(muni_sicoin)%in%c('loc_id', 'source', 'start_date')]
muni_sicoin = muni_sicoin[, list(budget=sum(budget), expenditure=sum(expenditure), disbursement=sum(disbursement)), 
                          by=byVars]

##list of plots 
muni_plots <- list()

## loop through gf/ghe to plot budgets vs. disbursements (w/ respect to disease and year)
for(k in unique(muni_sicoin$source)){
  muni_subset <- subset(muni_sicoin, source==k)
  muni_lm <- lm(disbursement~budget, data=muni_subset)
  muni_plot <- (ggplot(muni_subset, aes(x = budget/1000, y=disbursement/1000)) + 
                  geom_point(aes(color=year(start_date), shape=disease)) +
                  geom_abline(intercept=0, slope=1) + 
                  geom_smooth(method='lm') + 
                  scale_colour_gradient(low = "#73D487", high = "#FF66CC",
                                        space = "Lab", na.value = "grey50", guide = "colourbar") +
                  ggtitle(paste(k, "Budget vs Disbursement Data, regression slope =",signif(muni_lm$coef[[2]],5))) +
                  #ylim(0, 9) + 
                  labs(x = "Budget $$ (thousands)", y = "Disbursement  $$ (thousands)") +
                  theme_bw())
  muni_plots[[k]] <- muni_plot
}


# ----------------------------------------------
##plot muni level sicoin data 

# collapse cost categories
byVars = names(sicoin)[!names(sicoin)%in%c('budget','disbursement','expenditure','cost_category', 'coeff', 'code')]
muni_level = sicoin[, list(budget=sum(budget), disbursement=sum(disbursement), expenditure=sum(expenditure)), by=byVars]


# "melt" long
tmp = copy(muni_level)
tmp$start_date = NULL
setnames(tmp, 'end_date', 'start_date')
muni_level$end_date = NULL
muni_level = rbind(muni_level, tmp)

muni_melt = melt(muni_level, id.vars=c('loc_id', 'source', 'start_date', 'period', 'disease',
                                       'data_source', 'grant_number'))
muni_melt <- muni_melt[, list(start_date, disease, variable, value),by="loc_id"]
muni_melt$value[muni_melt$value == 0] <- NA
muni_melt$loc_id <- as.factor(muni_melt$loc_id)


muni_mapping <- cbind(unique(levels(muni_melt$loc_id)), as.numeric(1:length(unique(levels((muni_melt$loc_id))))))

colnames(muni_mapping) <- c("loc_id", "muni_code")

muni_merge <- merge(muni_melt, muni_mapping, by="loc_id")
muni_merge$muni_code <- as.numeric(muni_merge$muni_code)

### use a loop to create plots and store them into a pdf 
plot_list = list()
for (i in 1:48){
  subdata <- subset(muni_merge, muni_code%%48==i)
  plot <- ggplot(subdata, aes(x = start_date, y = value)) + 
    geom_line(aes(color=variable, linetype=disease)) +
    facet_wrap(~loc_id, scales='free') +
    theme(text = element_text(size=5),axis.text.x = element_text(angle=90, hjust=1)) +
    geom_point() +
    ggtitle("Municipality Level Resources by Disease") +
    labs(x = "Start Date", y = "Resource $$", caption="data source:SICOIN")
  plot_list[[i]] <- plot
  
}

pdf("gtm municipalities by disease.pdf", height=6, width=9)
invisible(lapply(plot_list, print))
dev.off()





