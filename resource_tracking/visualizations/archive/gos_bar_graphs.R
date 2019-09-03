
# ----------------------------------------------
# Irena Chen
#
# 12/08/2017
# ### GOS BAR CHART GRAPHS 
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
##load the data 
dir <- "J:/Project/Evaluation/GF/resource_tracking/gtm/gf/"
fileName <- "Expenditures from GMS and GOS for PCE IHME countries.xlsx"
sheet_name <- "GMS SDAs - extract"
gos_data <- read_gos_data(dir, fileName, sheet_name)

### make program activity graphs - first without grants, just by disease 

byVars = names(gos_data)[!names(gos_data)%in%c('budget','expenditure','gf_program')]
program_level = gos_data[, list(budget=sum(na.omit(budget)), expenditure=sum(na.omit(expenditure))), by=byVars]

program_level <- program_level[, -c(3:4)]

## prep for mapping
mapping_for_R <- read.csv("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/mapping_for_R.csv", fileEncoding="latin1")
mapping_for_graphs <- read.csv("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/mapping_for_graphs.csv")


# test for missing SDAs from map
sdas_in_map = unique(mapping_for_R$cost_category)
sdas_in_data = unique(program_level$cost_category)
if (any(!sdas_in_data %in% sdas_in_map)) { 
  stop('Map doesn\'t include cost categories that are in this data file!')
}
program_level[cost_category%in%sdas_in_data[!sdas_in_data %in% sdas_in_map]]


# test to make sure map doesn't contain duplicates
d1 = nrow(mapping_for_R)
d2 = nrow(unique(mapping_for_R))
if (d1!=d2) stop('Map contains duplicates!') 

# ---------------------------------------------
##map program activities from GOS data to our standard categories:

program_level_mapped <- merge(program_level, mapping_for_R, by=c("disease","cost_category"))
program_level <- merge(program_level_mapped, mapping_for_graphs, by="code")

program_level$budget <- program_level$budget*program_level$coeff
program_level$expenditure <- program_level$expenditure*program_level$coeff

## sum budget and exp. by country, year, program activity, disease (no grants) 
byVars = names(program_level)[!names(program_level)%in%c('budget','expenditure', 'coeff', 'code', 'cost_category', "grant_number")]
program_level = program_level[, list(budget=sum(na.omit(budget)), expenditure=sum(na.omit(expenditure))), by=byVars]



##melt to get program activity 
program_level <- program_level[,-c(3:4)]
program_level= melt(program_level, id.vars=c("program_activity", "disease","Year", "Country"))
program_level$value[program_level$value<=0] <- NA



##set up to graph nicely
program_level[disease=='hiv', disease:='HIV/Aids']
program_level[disease=='malaria', disease:='Malaria']
program_level[disease=='tb', disease:='Tuberculosis']

gtm_gos <- program_level[Country=="Guatemala"]
uga_gos <- program_level[Country=="Uganda"]
cod_gos <- program_level[Country=="Congo (Democratic Republic)"]


### percent bar charts: 
progColors <- c('#b20000', '#660000', ##reds
               '#f6ae8f', '#EE5D1F', ##oranges
               '#256325', '#92b192',##greens
               '#004c4c', '#00FFFF',##blues
               '#660066', '#bf7fbf',#purples
               '#ff748c', "#ffc0cb", #pinks
               '#a6a6a6', '#d8d8d8', ##greys
               '#ffd700', '#d3a308',#yellows
               "#35978f", "#80cdc1", ##teals
               '#9ecae1', '#4292c6',##ocean 
               '#7a0177') #magenta
names(progColors) <- unique(program_level$program_activity)

prog_plots <- list()
for (k in unique(uga_gos$disease)){
  subdata <- uga_gos[disease==k & variable=='budget']
  colScale <- scale_fill_manual(name="program_activity", values =progColors) 
  plot <- ggplot(data=subdata, aes(x = as.integer(Year), y= value/1000000, fill=program_activity)) + 
    geom_bar(position="fill", stat="identity") + 
    colScale +
    theme_bw(base_size=16) +
    scale_y_continuous(labels = percent_format()) +
    labs(title=k, x="Year", y = "% of Budget", caption="Data Source: GOS") + 
    theme(legend.text=element_text(size=8)) +
    scale_x_discrete(name ="Year", 
                     limits=c(2004, 2008, 2012, 2016))
  #+scale_fill_manual('Program Activity', values=set3, levels(subdata$program_activity))
  prog_plots[[k]] <- plot
  }

pdf("uga_gos_percent_bars.pdf", height=6, width=9)
invisible(lapply(prog_plots, print))
dev.off()




for (k in unique(gtm_gos$disease)){
  subdata <- gtm_gos[disease==k & variable=='budget']
  plot <- ggplot(data=subdata, aes(x = as.integer(Year), y= value/1000000, fill=program_activity)) + 
    geom_col() + 
    theme_bw(base_size=16) +
    labs(title=paste(k, "Data at National Level"), 
         x = "", y = "USD (Millions)", caption="Data Source: GOS and GMS") +
    scale_fill_manual('Program Activity', values=set3, levels(subdata$program_activity))
  prog_plots[[k]] <- plot
}

pdf("gtm_gos_activity_bars.pdf", height=6, width=9)
invisible(lapply(prog_plots, print))
dev.off()

for (k in unique(uga_gos$disease)){
  subdata <- uga_gos[disease==k & variable=='budget']
  plot <- ggplot(data=subdata, aes(x = as.integer(Year), y= value/1000000, fill=program_activity)) + 
    geom_col() + 
    theme_bw(base_size=16) +
    labs(title=paste(k, "Data at National Level"), 
         x = "", y = "USD (Millions)", caption="Data Source: GOS and GMS") +
    scale_fill_manual('Program Activity', values=set3, levels(subdata$program_activity)) 
  prog_plots[[k]] <- plot
}

pdf("uga_gos_activity_bars.pdf", height=6, width=9)
invisible(lapply(prog_plots, print))
dev.off()

for (k in unique(cod_gos$disease)){
  subdata <- cod_gos[disease==k & variable=='budget']
  plot <- ggplot(data=subdata, aes(x = as.integer(Year), y= value/1000000, fill=program_activity)) + 
    geom_col() + 
    theme_bw(base_size=16) +
    labs(title=paste(k, "Data at National Level"), 
         x = "", y = "USD (Millions)", caption="Data Source: GOS and GMS") +
    scale_fill_manual('Program Activity', values=set3, levels(subdata$program_activity)) +
    scale_x_discrete(name ="Year", 
                     limits=c(2005,2007,2009,2011, 2013, 2015))
  prog_plots[[k]] <- plot
}

pdf("cod_gos_activity_bars.pdf", height=6, width=9)
invisible(lapply(prog_plots, print))
dev.off()

## program only - no disease (with grants)
byVars = names(program_level)[!names(program_level)%in%c('budget','expenditure', 'coeff', 'code', 'cost_category', 'disease')]
grant_level = program_level[, list(budget=sum(budget), expenditure=sum(expenditure)), by=byVars]


colnames(grant_level)[1] <- "grant_number"
tmp = copy(grant_level)
tmp$start_date = NULL
setnames(tmp, 'end_date', 'start_date')
grant_level$end_date = NULL
grant_level = rbind(grant_level , tmp)


grant_level= melt(grant_level, id.vars=c("program_activity", "grant_number", "start_date"))
grant_level$value[grant_level$value==0] <- NA


grant_plots <- list()
for (k in unique(grant_level$grant_number)){
  subdata <- subset(grant_level, grant_number==k)
  plot <- (ggplot() + geom_col(aes(x = year(start_date), y= value/1000000, fill=program_activity), data=subdata) + 
             theme_bw(base_size=16) +
             facet_wrap(~variable, drop=T, scales='free') +
             labs(x = "Year", y = "$$ in mil", caption="Source: The Global Fund")+
             scale_fill_brewer(palette = "Set3") +
             # scale_color_manual(values = setNames(set3(13), levels(subdata$program_activity)))+
             ggtitle(paste(k, "data at the national level"))) 
  grant_plots[[k]] <- plot
}

pdf("gos_activity_grant_bars.pdf", height=6, width=9)
invisible(lapply(grant_plots, print))
dev.off()

