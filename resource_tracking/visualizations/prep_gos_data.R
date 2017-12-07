

# ----------------------------------------------
# Irena Chen
#
# 11/27/2017
# ### GOS DATA graphs 

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
## prep data 

gos_data  <- data.table(read_excel("J:/Project/Evaluation/GF/resource_tracking/gtm/gf/Expenditures from GMS and GOS for PCE IHME countries.xlsx", sheet = "GMS SDAs - extract", col_types = c("text", "text", "date", "date", "date", "date", "numeric", "text", "text", "numeric", "numeric", "text")))

colnames(gos_data)[2] <- "grant_number"
colnames(gos_data)[5] <- "start_date"
colnames(gos_data)[6] <- "end_date"
colnames(gos_data)[8] <- "gf_program"
colnames(gos_data)[9] <- "cost_category"
colnames(gos_data)[10] <- "budget"
colnames(gos_data)[11] <- "expenditure"
colnames(gos_data)[12] <- "gos_disease"

map_disease <- unique(gos_data$gos_disease)
names(map_disease) <- c("tb", "malaria", "hiv", "hss", "hiv/tb")

kDT = data.table(map_disease = names(map_disease), value = TRUE, gos_disease = unname(map_disease))
gos_data[kDT, on=.(gos_disease), disease := i.map_disease]
gos_data$gos_disease <- NULL
gos_data[disease=='hiv/tb', disease:='hiv']

## graph budgets vs. expenditures: 

# ---------------------------------------------
graphData <- copy(gos_data)
graphData[disease=='hiv', disease:='HIV/Aids']
graphData[disease=='malaria', disease:='Malaria']
graphData[disease=='tb', disease:='Tuberculosis']
graphData[disease=='hss', disease:='HSS']




gos_nat_plots <- list()
for (k in unique(na.omit(graphData$Country))){
  fit <- lm(expenditure ~ budget, data = na.omit(graphData[Country==k]))
  range = c(min(na.omit(graphData[Country==k]$expenditure/1000000)), max(na.omit(graphData[Country==k]$budget/1000000)))
  plot <- (ggplot(na.omit(graphData[Country==k]), aes(x=budget/1000000, y=expenditure/1000000)) + 
             geom_point(aes(color=Year, shape=disease)) +
             geom_abline(intercept=0, slope=1) + 
             xlim(range) + 
             ylim(range)+
             geom_smooth(method='lm') + 
             coord_trans(x="log10") +
             scale_colour_gradient(low = "red", high = "blue",
                                   space = "Lab", na.value = "grey50", guide = "colourbar") +
             #ylim(0, 9) + 
             labs(x = "Budget USD (Millions)", y = "Expenditure USD (Millions)", caption="Source: GOS",
                  title=paste(k, "Budget vs Expenditure Data"),
                  subtitle = (paste0("reg. slope: ", round(coefficients(fit)[2], digits=3))),
                  colour="Year", shape="Disease") +
             theme_bw(base_size=16) +
             theme(plot.title=element_text(hjust=.5), 
                   plot.subtitle=element_text(size=10, hjust=0.5, face="bold", color="dark green"))) 
  gos_nat_plots[[k]] <- plot
}


pdf("country_gos_budget_expenditures.pdf", height=6, width=9)
invisible(lapply(gos_nat_plots, print))
dev.off()


logData <- copy(graphData)
logData[logData  <= 0] <- NA

gos_log_plots <- list()
for (k in unique(na.omit(logData$Country))){
  fit <- lm(expenditure ~ budget, data = na.omit(graphData[Country==k]))
  range = c(min(na.omit(graphData[Country==k]$expenditure/1000000)), max(na.omit(graphData[Country==k]$budget/1000000)))
  plot <- (ggplot(na.omit(graphData[Country==k]), aes(x=budget/1000000, y=expenditure/1000000)) + 
             geom_point(aes(color=Year, shape=disease)) +
             # geom_abline(intercept=0, slope=1) + 
             # xlim(range) + 
             # ylim(range)+
             geom_smooth(method='glm',formula=y~x) + 
             scale_x_log10(limits= c(1, max(na.omit(graphData[Country==k]$budget/1000000)))) +
             scale_y_log10(limits= c(0.5, max(na.omit(graphData[Country==k]$budget/1000000)))) +
             scale_colour_gradient(low = "red", high = "blue",
                                   space = "Lab", na.value = "grey50", guide = "colourbar") +
             #ylim(0, 9) + 
             labs(x = "log(Budget USD (Millions))", y = "log(Expenditure USD (Millions))", caption="Source: GOS",
                  title=paste(k, "Budget vs Expenditure Data"),
                  subtitle = (paste0("reg. slope: ", round(coefficients(fit)[2], digits=3))),
                  colour="Year", shape="Disease") +
             theme_bw(base_size=16) +
             theme(plot.title=element_text(hjust=.5), 
                   plot.subtitle=element_text(size=10, hjust=0.5, face="bold", color="dark green"))) 
  gos_nat_plots[[k]] <- plot
}


# ---------------------------------------------
### make program activity graphs - first without grants, just by disease 

byVars = names(gos_data)[!names(gos_data)%in%c('budget','expenditure','gf_program')]
program_level = gos_data[, list(budget=sum(budget), expenditure=sum(expenditure)), by=byVars]

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

## program only - no grants 
byVars = names(program_level)[!names(program_level)%in%c('budget','expenditure', 'coeff', 'code', 'cost_category', 'Grant Number')]
program_level = program_level[, list(budget=sum(na.omit(budget)), expenditure=sum(na.omit(expenditure))), by=byVars]


program_level= melt(program_level, id.vars=c("program_activity", "disease", "start_date", "end_date","Year", "Country", "Grant Number"))
program_level$value[program_level$value==0] <- NA


gtm_gos <- program_level[Country=="Guatemala"]
uga_gos <- program_level[Country=="Uganda"]
cod_gos <- program_level[Country=="Congo (Democratic Republic)"]

prog_plots <- list()
set3 <- colorRampPalette(brewer.pal('Set3',n=12))
set3 = set3(15)

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


