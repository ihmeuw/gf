
# ----------------------------------------------
# Irena Chen
#
# 12/08/2017
# ### General Visualizations for Bar Graphs over time:  
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

# ---------------------------------------------
# stacked bar charts over time 

prog_plots <- list()
for (k in unique(graphData$disease)){
  subdata <- graphData[disease==k]
  colScale <- scale_fill_manual(name="SDA", values =primColors) 
  plot <- (ggplot(data=subdata, aes(x = year, y= budget/1000000, fill=program_activity)) + 
    geom_bar(## if you want 100% stacked graphs, uncomment: position = "fill",
      stat="identity") + 
    colScale +
    theme_bw(base_size=14) +
    theme(strip.text.x = element_text(size = 8, colour = "black")) +
    facet_grid(~facet,scales = "free_x", space="free_x") + 
    ## if you want 100% stacked graphs, uncomment:scale_y_continuous(labels = percent_format()) +
    scale_x_continuous(name ="Year", breaks = seq(2005, 2020,5)) +
    labs(title=paste(k, "Data at National Level"), 
         x = "", y = "$ USD (Millions)", caption="Data Source: GOS, FPM"))
  prog_plots[[k]] <- plot
}

pdf("sdas_overtime.pdf", height=6, width=9)
invisible(lapply(prog_plots, print))
dev.off()


# ---------------------------------------------
## bar charts over time for HSS/key pop: 

hssData <- get_hss_ind(graphData)
keyData <- get_keypop_ind(graphData)

##-----------------------------------------------------------
# HSS CHARTS: 

##set the colors - if HSS: 
indColors <- c('#c0c0c0','#0000ff','#008000',
               '#ffef00', '#4ca6a6') 
names(indColors) <- c('Non RSSH', 'RSSH: information system',
                      'RSSH: health workforce', 'RSSH: service delivery', 'RSSH: other')

##aggregate the data by the variables of interest (disease, year, HSS type)
byVars = names(hssData)[names(hssData)%in%c('hss_ind', 'disease', "facet", "year")]
hssData = hssData[, list(budget=sum(na.omit(budget))), by=byVars]

##HSS bar charts over time 
hss_plots <- list()
for (k in unique(hssData$disease)){
  subdata <- hssData[disease==k]
  colScale <- scale_fill_manual(name="RSSH Activities", values =indColors) 
  plot <- ggplot(data=subdata, aes(x = year, y= budget/1000000, fill=hss_ind)) + 
    geom_bar(position = "fill",
             stat="identity") + 
    colScale +
    # facet_grid(~facet,scales = "free_x", space="free_x") + 
    theme_bw(base_size=10.5) +
    scale_y_continuous(labels = percent_format()) +
    scale_x_continuous(name ="Year", breaks = seq(2005, 2020,5)) +
    labs(title=paste(k, "RSSH Activity over Time"), fill='HSS Ind.',
         x = "", y = "% of Budget", caption="Data Source: GOS, FPM")
  hss_plots[[k]] <- plot
}


pdf("hss_overtime_perc.pdf", height=6, width=9)
invisible(lapply(hss_plots, print))
dev.off()

##-----------------------------------------------------------
##KEY POPULATIONS CHARTS: 

##set the key population colors: 
indColors <- c('#c0c0c0', '#4292c6', '#ff748c') 
names(indColors) <- unique(keyData$key_pop) 

# ---------------------------------------------
## sum budget and exp. by country, grant #, key categories, and disease

byVars = names(keyData)[names(keyData)%in%c('key_pop', 'disease', "facet", "year")]
keyData = keyData[, list(budget=sum(na.omit(budget))), by=byVars]

# ---------------------------------------------
##key Populations charts
key_plots <- list()
for (k in unique(keyData$disease)){
  colScale <- scale_fill_manual(name="Key Pop. Activities", values =indColors) 
  subdata <- keyData[disease==k]
  plot <- (ggplot(data=subdata, aes(x = year, y= budget/1000000, fill=key_pop)) + 
             geom_bar(position = "fill",
                      stat="identity") + 
             colScale +
             theme_bw(base_size=14) +
             theme(strip.text.x = element_text(size = 8, colour = "black")) +
             facet_grid(~facet,scales = "free_x", space="free_x") + 
             scale_y_continuous(labels = percent_format()) +
             scale_x_continuous(name ="Year", breaks = seq(2005, 2020,5)) +
             labs(title=paste(k, "Data at National Level"), 
                  x = "", y = "% of Budget", caption="Data Source: GOS, FPM"))
  #+scale_fill_manual('Program Activity', values=set3, levels(subdata$program_activity))
  key_plots[[k]] <- plot
}

pdf("keypop_perc.pdf", height=6, width=9)
invisible(lapply(key_plots, print))
dev.off()



