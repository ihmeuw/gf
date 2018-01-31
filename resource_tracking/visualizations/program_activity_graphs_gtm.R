
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
pdf("gf vs ghe over time.pdf", height=6, width=9)
invisible(lapply(nat_plots, print))
dev.off()



# ----------------------------------------------
## BUDGET vs DISBURSEMENT - municipality level 

sicoin <- data.table(read.csv("prepped_sicoin_data_1130.csv", fileEncoding="UTF-8"))

toMatch <- c("gtm", "guat")
muni_sicoin <- sicoin[ !grepl(paste(toMatch, collapse="|"), tolower(sicoin$loc_id)),]

## aggregate by disease, start_date 
byVars = names(muni_sicoin)[!names(muni_sicoin)%in%c('budget','expenditure', 'disbursement', 'cost_category', 'period', 'data_source', 'grant_number')]
muni_sicoin = muni_sicoin[, list(budget=sum(budget), expenditure=sum(expenditure), disbursement=sum(disbursement)), by=byVars]

##list of plots 
muni_plots <- list()

## loop through gf/ghe to plot budgets vs. disbursements (w/ respect to disease and year)
for(k in unique(muni_sicoin$source)){
  muni_subset <- subset(muni_sicoin, source==k)
  muni_lm <- lm(disbursement ~budget, data=muni_subset)
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
#### get FPM vs sicoin, etc. 
byVars = names(total_data)[!names(total_data)%in%c('budget','disbursement','expenditure','cost_category', 'program_activity', 'loc_id', 'code', 'coeff')]
nat_level = total_data[, list(budget=sum(budget), disbursement=sum(disbursement), expenditure=sum(expenditure)), by=byVars]


## reshape data to plot it better 
nat_level = melt(nat_level, id.vars=c('source', 'disease', 'start_date', 'end_date', 'period', 'grant_number', 'data_source'))
nat_level$value[nat_level$value == 0] <- NA


#subset to remove pudr data 
fpm_sicoin <- nat_level[data_source != "pudr"]


## create fpm/sicoin indicators so we can plot one on each axis 
fpm_sicoin$sicoin_ind <- 0
fpm_sicoin$fpm_ind <- 0
for(i in 1: length(fpm_sicoin$sicoin_ind)){
  if (fpm_sicoin$data_source[i]=="SICOIN"){
    fpm_sicoin$sicoin_ind[i] <- fpm_sicoin$value[i]
    fpm_sicoin$fpm_ind[i] <- 0
  } else if (fpm_sicoin$data_source[i]!="SICOIN"){
    fpm_sicoin$sicoin_ind[i] = fpm_sicoin$sicoin_ind[i]
    fpm_sicoin$fpm_ind[i] <- fpm_sicoin$value[i]
  }
}

##plot
ggplot(fpm_sicoin, aes(x = sicoin_ind/1000000, y=fpm_ind/1000000)) + 
  geom_point(aes(color=year, shape=disease)) +
  geom_abline(intercept=0, slope=1) + 
  geom_smooth(method='lm') + 
  scale_colour_gradient(low = "#73D487", high = "#FF66CC",
                        space = "Lab", na.value = "grey50", guide = "colourbar") +
  ggtitle("SICOIN vs FPM Budget Data") +
  #ylim(0, 9) + 
  labs(x = "SICOIN $$ in mil", y = "FPM $$ in mil") 


ggsave("fpm vs sicoin budget data.pdf", 
       plot = last_plot(), # or give ggplot object name as in myPlot,
       height = 6, width=9,
       units = "in", # other options c("in", "cm", "mm"), 
       dpi = 300)

# ----------------------------------------------
## mapping to get program activity 

mapping_for_R <- read.csv("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/mapping_for_R.csv", fileEncoding="latin1")
mapping_for_graphs <- read.csv("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/mapping_for_graphs.csv")

gtm_total_data <- merge(sicoin, mapping_for_R,  by=c("disease", "cost_category"))

gtm_total_data$budget <- gtm_total_data$budget*gtm_total_data$coeff
gtm_total_data$disbursement <- gtm_total_data$disbursement*gtm_total_data$coeff
gtm_total_data$expenditure <- gtm_total_data$expenditure*gtm_total_data$coeff

gtm_total <- merge(gtm_total_data, mapping_for_graphs, by="code")

