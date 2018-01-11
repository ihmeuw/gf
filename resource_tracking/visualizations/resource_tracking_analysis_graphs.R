
# ----------------------------------------------
# Irena Chen
#
# 12/08/2017
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

# ---------------------------------------------

##function to create indicator between current, upcoming, and rejected budgets: 
appr_rej_indicators <- function(year, data_source){
  if(year < 2018){
    x <- "Past/Active"
  } else if (data_source=="rej_fpm"){
    x <- "In Iteration"
  }else {
    x <- "Upcoming"
  }
  return(x)
}
# ---------------------------------------------
##load the dataset: 

totalData <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/uga/prepped/mapped_total_uga_data.csv",
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


##colors to map: 
primColors <- c('#b20000', '#660000', ##reds
                '#f6ae8f', '#EE5D1F', ##oranges
                '#3DCC3D', '#008000',##greens
                '#004c4c', '#00D9D9',##blues
                "#35978f", "#80cdc1", ##teals
                '#3786b0', '#0097f1',##ocean 
                '#ffd700', '#d3a308',#yellows
                '#3d017a','#b200b2', ##magenta
                '#660066', '#bf7fbf',#purples
                '#ff748c', "#e00222", #pinks 
                '#a6a6a6', '#ffc0cb', ##grey and millennial pink 
                '#4169e1', '#00007f') ##royal blue


names(primColors) <- c('HIV/AIDS care and support', 'Community care and outreach', ##reds
                       'Case detection and diagnosis', 'Case Diagnosis',##oranges
                       'Treatment','HIV/AIDS counseling and testing',##greens
                       'HSS: health workforce', 'HSS: information system',##blues
                       'HSS: service delivery', 'HSS: other', ##teals
                       'Key and vulnerable populations', 'Community care and outreach',##ocean
                        'Prevention', 'Malaria other control and prevention', ##yellows
                       'MDR-TB case detection and diagnosis', 'MDR-TB prevention',   ##magenta
                       'HIV/TB collaborative interventions', 'MDR-TB treatment', ##purples
                       'Monitoring and evaluation', 'Malaria bed nets', #pinks
                       'Other/Unidentified', 'HIV/AIDS PMTCT', ##greys
                       'PBF', 'Malaria indoor residual spraying' #royal blue and purple
                       )


hssColors <- c('#a6cee3', '#1f78b4',##blues
                '#b2df8a', "#33a02c", ##teals
                '#fb9a99', '#e31a1c',##ocean 
               '#fdbf6f', '#ff7f00')  ##grey and millennial pink 

names(hssColors) <- c('HSS - Procurement supply chain management (PSCM)',
                      'Removing legal barriers to access',
                      'HSS: service delivery',
                      'HSS - Health information systems and M&E',
                      'HSS: Human resources',
                      'HSS: Community Systems Strengthening',
                      'HSS: Information system & Operational research',
                      'Program management')


graphData$facet <- as.factor(mapply(appr_rej_indicators, graphData$year, graphData$data_source))
graphData$facet <- factor(graphData$facet ,levels=c("Past/Active", "In Iteration", "Upcoming"))

graphData <- disease_names_for_plots(graphData)


prog_plots <- list()
for (k in unique(graphData$disease)){
  subdata <- graphData[disease==k]
  if(k=="HSS"){
    colScale <- scale_fill_brewer(type="qual", palette = "Paired")
  } else {
    colScale <- scale_fill_manual(name="SDA", values =primColors) 
  }
  plot <- (ggplot(data=subdata, aes(x = year, y= budget/1000000, fill=program_activity)) + 
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
  prog_plots[[k]] <- plot
}

pdf("H:/visualizations/uga_overtime_perc.pdf", height=6, width=9)
invisible(lapply(prog_plots, print))
dev.off()



prog_hss_plots <- list()
for (k in unique(keyData$disease)){
  subdata <- keyData[disease==k]
  colScale <- scale_fill_manual(name="key_pop", values =indColors) 
  plot <- ggplot(data=subdata, aes(x = year, y= budget/1000000, fill=key_pop)) + 
    geom_bar(position = "fill",
      stat="identity") + 
    colScale +
    theme_bw(base_size=16) +
   scale_y_continuous(labels = percent_format()) +
    scale_x_discrete(name ="Year", 
                     limits=c(2005, 2010, 2015, 2020)) +
    labs(title=paste(k, "Key Pop. Activity over Time"), fill='Key Pop.',
         x = "", y = "% of Budget", caption="Data Source: GOS, FPM")
  prog_hss_plots[[k]] <- plot
}


pdf("J:/Project/Evaluation/GF/resource_tracking/cod/visualizations/2018_budgets/cod_key_overtime_perc.pdf", height=6, width=9)
invisible(lapply(prog_hss_plots, print))
dev.off()



# ---------------------------------------------
## pie charts for HSS/key pop: 

hssData <- get_hss_ind(graphData)
keyData <- get_keypop_ind(graphData)


##set the colors - if HSS: 
indColors <- c('#00FFFF','#bf7fbf',
               '#0000ff', '#1f78b4',
               '#660066', '#bf7fbf',
               '#ff748c', "#e00222",
               '#c0c0c0','#ff3333') 
names(indColors) <- c('HSS: information system', 'HSS - Procurement supply chain management (PSCM)',
                      'HSS - Health information systems and M&E', 'HSS: health workforce',
                      'HSS: other', 'HSS: Human resources',
                      'HSS: Community Systems Strengthening', 'HSS: Information system & Operational research',
                      'Other categories', 'HSS: service delivery')


##if key pop: 
indColors <- c('#c0c0c0', '#4292c6', '#ff748c') 
names(indColors) <- unique(keyData$key_pop) 


# ---------------------------------------------
## sum budget and exp. by country, grant #, key categories, and disease

byVars = names(keyData)[names(keyData)%in%c('key_pop', 'disease', "facet", "year")]
keyData = keyData[, list(budget=sum(na.omit(budget))), by=byVars]

# ---------------------------------------------
##HSS/Key Populations charts

prog_plots <- list()
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
  prog_plots[[k]] <- plot
}

pdf("H:/visualizations/uga_keypop_perc.pdf", height=6, width=9)
invisible(lapply(prog_plots, print))
dev.off()


