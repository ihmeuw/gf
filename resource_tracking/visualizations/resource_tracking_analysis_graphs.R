
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
byVars = names(graphData)[names(graphData)%in%c('gf_module', 'gf_intervention',"code", 'year', 'grant_number', 'disease', 'data_source')]
graphData = graphData[, list(budget=sum(na.omit(budget)), expenditure=sum(na.omit(expenditure))), by=byVars]

graphData$budget[graphData$budget<=0] <- NA
graphData$expenditure[graphData$expenditure<=0] <- NA

##apply the grant facet:
graphData$facet <- as.factor(mapply(data_sources_facet, graphData$year, graphData$data_source))
graphData$facet <- factor(graphData$facet, levels=c("Past/Active", "Initial",  "Upcoming"))

graphData[gf_module=="Health management information system and monitoring and evaluation"
          , gf_module:="Health management information system and M&E"]
##make the disease text nicer for the graphs: 
graphData <- disease_names_for_plots(graphData)
graphData$gf_module <- factor(graphData$gf_module, levels=names(primColors))
# ---------------------------------------------
# stacked bar charts over time 

prog_plots <- list()
for (k in unique(graphData$disease)){
  subdata <- graphData[disease==k]
  colScale <- scale_fill_manual(name="GF Module", values =primColors) 
  plot <- (ggplot(data=subdata, aes(x = year, y= budget/1000000, fill=gf_module)) + 
    geom_bar(## if you want 100% stacked graphs, uncomment: position = "fill",
      stat="identity") + 
    colScale +
    theme_bw(base_size=14) +
      theme(legend.title = element_text(size=10),legend.text=element_text(size=8),
          strip.text.x = element_text(size = 7, colour = "black")) +
    facet_grid(~facet,scales = "free_x", space="free_x") + 
    ## if you want 100% stacked graphs, uncomment:scale_y_continuous(labels = percent_format()) +
    scale_x_continuous(name ="Year", breaks = seq(2005, 2020,3)) +
    labs(title=paste(k, "Data at National Level"), 
         x = "", y = "$ USD (Millions)", caption="Data Source: GOS, FPM"))
  prog_plots[[k]] <- plot
}

pdf("sdas_overtime.pdf", height=6, width=9)
invisible(lapply(prog_plots, print))
dev.off()


# ---------------------------------------------
## bar charts over time for HSS/key pop: 



##Fill in year gaps where modules and years might be missing:

year_range <- seq(2005, 2020, 1)
gf_mods <- data.table(graphData$gf_module, graphData$disease)

create_na_mods <- merge(year_range, gf_mods)

create_na_mods <- unique(create_na_mods)

setnames(create_na_mods,c("year", "gf_module","disease"))


graphData <- merge(graphData, create_na_mods, all.y=TRUE, by=c("year","gf_module","disease"))

graphData[is.na(gf_module), gf_module:="Module Not Included"]
graphData[is.na(gf_intervention), gf_intervention:="Not Included"]
graphData[gf_intervention=="No Data", budget:=0]

graphData$str_wrap <- mapply(get_summary_level,as.character(graphData$gf_module), as.character(graphData$gf_intervention))

graphData$str_wrap <- str_wrap(graphData$str_wrap, 45)

graphData$str_wrap <- factor(graphData$str_wrap, levels=c("No Data","Summary Level Only",
                                                          unique(graphData[!str_wrap%in%c("No Data", "Summary Level Only")]$str_wrap)))

hivData <- graphData[disease=="HIV/AIDS"]
malData <- graphData[disease=="Malaria"]
tbData <- graphData[disease=="Tuberculosis"]
hssData <- graphData[disease=="RSSH"]
##-----------------------------------------------------------
# Intervention charts: 

colors <- brewer.pal(12, "Paired")
colors[1] <- "grey50"
colors[2] <- "grey30"

#### bar charts over time 
int_plots <- list()
for (k in unique(hivData$gf_module)){
  subdata <- hivData[gf_module==k]
  if((any(as.character(subdata$str_wrap)%in%"No Data")&any(as.character(subdata$str_wrap)%in%"Summary Level Only"))){
    cols = colors
  } else if(any(as.character(subdata$str_wrap)%in%"No Data")){
    cols = c(colors[1], colors[3:12])
  } else if(any(as.character(subdata$str_wrap)%in%"Summary Level Only")){
    cols = colors[2:12]
  } else {
    cols = colors[3:12]
  }
  plot <- (ggplot(data=subdata, aes(x = year, y= budget/1000000, fill=str_wrap)) + 
             geom_bar(position = "fill",
                      stat="identity") + 
             scale_fill_manual(name="Interventions", values =cols) +
             # facet_grid(~facet,scales = "free_x", space="free_x") + 
             theme_bw(base_size=10.5) +
             scale_y_continuous(labels = percent_format()) +
             scale_x_continuous(name ="Year", breaks = seq(2005, 2020,3), limits=c(2005, 2020)) +
             labs(title=paste("GF Module:", k),
                  x = "", y = "% of Budget", caption="Data Source: GOS, FPM"))
  int_plots[[k]] <- plot
}


pdf("interventions_overtime_perc.pdf", height=6, width=9)
invisible(lapply(int_plots, print))
dev.off()


# 

mod_names <- as.character(unique(hivData$gf_module))

cols=sample(countryColors$color, length(unique(hivData$str_wrap)), replace = FALSE)
names(cols) <- unique(hivData$str_wrap)


cols=sample(countryColors$color, length(unique(hivData$str_wrap)), replace = FALSE)


int_plots <- list()
for (k in 1:length(mod_names)){
  subdata <- hivData[gf_module==mod_names[k]]
  cols[names(cols)=="Summary Level Only"]="grey50"
  cols[names(cols)=="No Data"]="#FFFFFF"
  plot = (ggplot(data=subdata, aes(x = year, y= budget/1000000, fill=str_wrap)) + 
             geom_bar(position = "fill",
                      stat="identity") + 
             scale_fill_manual(name="Interventions", values =cols) +
             # facet_grid(~facet,scales = "free_x", space="free_x") + 
             theme_bw(base_size=10.5) +
             scale_y_continuous(labels = percent_format()) +
             scale_x_continuous(name ="Year", breaks = seq(2005, 2020,3), limits=c(2005, 2020)) +
             labs(title=paste("GF Module:", mod_names[k]),
                  x = "", y = "% of Budget", caption="Data Source: GOS, FPM"))
  int_plots[[k]] = plot
}
