
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


mapping_list <- load_mapping_list("J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/intervention_and_indicator_list.xlsx")


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
gf_mods <- data.table(graphData$disease)

create_na_mods <- merge(year_range, gf_mods)

create_na_mods <- unique(create_na_mods)

setnames(create_na_mods,c("year","disease"))

##years that we have no data: 
mapping_list <- disease_names_for_plots(mapping_list)
mapping_list$code <- NULL
mapping_list$intervention <- NULL
setnames(mapping_list, "module", "gf_module")

create_na_mods <- merge(create_na_mods, mapping_list, by="disease")
create_na_mods <- unique(create_na_mods)

modData <- merge(graphData, create_na_mods,all.y=TRUE, by=c("year","disease", "gf_module"), allow.cartesian = TRUE)

modData[is.na(gf_intervention), gf_intervention:="Module Not Included"]
modData[gf_intervention=="Module Not Included", budget:=100]

modData$str_wrap <- mapply(get_summary_level,as.character(modData$gf_module), as.character(modData$gf_intervention))

modData$str_wrap <- str_wrap(modData$str_wrap, 45)

#here, I'm doing SDA, grant, disease, and data source (gos, fpm etc.) by year 
byVars = names(modData)[names(modData)%in%c('gf_module', 'str_wrap','year', 'disease')]
modData = modData[, list(budget=sum(na.omit(budget)), expenditure=sum(na.omit(expenditure))), by=byVars]



hivData <- modData[disease=="HIV/AIDS"]
malData <- modData[disease=="Malaria"]
tbData <- modData[disease=="Tuberculosis"]
hssData <- modData[disease=="RSSH"]
##-----------------------------------------------------------
# Intervention charts: 

colors <- c('#a6cee3',
  '#1f78b4',
  '#b2df8a',
  '#4affd4',
  '#fb9a99',
  '#9e82ba',
  '#fdbf6f',
  '#ff7f00',
  '#cab2d6',
  '#93b500',
  '#db0645',
  '#42090a',
  '#f4c7d4',
  '#a977f4',
  '#01a004', 
  '#0c5768',
  '#6a3d9a',
  '#33a02c',
  '#ffff99',
  '#b15928'
  )
interventions <- unique(na.omit(modData$str_wrap))

cols <- rep(colors, length.out=length(interventions))
names(cols) <- interventions

cols[names(cols)=="Summary Level Only"]="grey50"
cols[names(cols)=="No Data"]="#FFFFFF"
cols[names(cols)=="Module Not Included"]="#FFFFFF"


#### bar charts over time 
int_plots <- list()
for (k in unique(hivData$gf_module)){
  subdata <- hivData[gf_module==k]
  plot <- (ggplot(data=subdata, aes(x = year, y= budget/1000000, fill=str_wrap)) + 
             geom_bar(colour="black", position = "fill",
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


