
# ----------------------------------------------
# Irena Chen
#
# 12/08/2017
# ### General Visualizations for Bar Graphs over time:  
# ----------------------------------------------
# Set up R
rm(list=ls())
library(ggplot2)
library(tools)
library(data.table)
library(lubridate)
library(readxl)
library(reshape)
library(scales)
library(stringr)

repo <- "your local repository where these files live:"
source(paste0(repo,"visualizations/functions_for_rt_analysis_graphs.R"))
source(paste0(repo,"prep/map_modules_and_interventions.R"))
# ----------------------------------------------
#########load the dataset:  ########
# ----------------------------------------------
totalData <- data.table(read.csv('J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/cleaned_total_data.csv',
                                 fileEncoding = "latin1"))


gf_mapping_list <- load_mapping_list("J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/intervention_and_indicator_list.xlsx", include_rssh_by_disease = TRUE)
# ---------------------------------------------
######### Run the function to paste "RSSH:" in front of the modules #######
# ----------------------------------------------

define_rssh_modules <- function(code, disease, module){
  x <- module
  if(grepl(tolower(code), "r")&disease!="hss"){
    x <- paste0("RSSH: ", module)
  } else {
    x <- x
  }
  return(x)
}

# ---------------------------------------------
######### subset the country you want from the aggregate data  #######
# ----------------------------------------------


# graphData <- totalData[country=="Uganda"]
# graphData <- totalData[country=="Guatemala"]
# graphData <- totalData[country=="Congo (Democratic Republic)"]


# ---------------------------------------------

######### sum budget and exp. by the variables of interest ############
# ---------------------------------------------

#here, I'm doing SDA, grant, disease, and data source (gos, fpm etc.) by year 
byVars = names(graphData)[names(graphData)%in%c('abbrev_module', 'abbrev_intervention',"code", 'year', 'grant_number', 'disease', 'data_source')]
graphData = graphData[, list(budget=sum(na.omit(budget)), expenditure=sum(na.omit(expenditure))), by=byVars]

graphData$budget[graphData$budget<=0] <- NA
graphData$expenditure[graphData$expenditure<=0] <- NA

##drop FGH as a data source since no module-level information available 
graphData <- graphData[data_source!="fgh"]

##apply the grant facet:
graphData$facet <- as.factor(mapply(data_sources_facet, graphData$year, graphData$data_source))
graphData$facet <- factor(graphData$facet, levels=c("Past/Active", "Initial",  "Upcoming"))

##make the disease text nicer for the graphs: 
graphData <- disease_names_for_plots(graphData)
graphData$abbrev_module <- factor(graphData$abbrev_module, levels=names(primColors))
                 
# ---------------------------------------------
# stacked bar charts over time 
# ---------------------------------------------
prog_plots <- list()
for (k in unique(graphData$disease)){
  subdata <- graphData[disease==k]
  colScale <- scale_fill_manual(name="GF Module", values =primColors) 
  plot <- (ggplot(data=subdata, aes(x = year, y= budget/1000000, fill=abbrev_module)) + 
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

pdf("modules_overtime.pdf", height=6, width=9)
invisible(lapply(prog_plots, print))
dev.off()


# ---------------------------------------------
######### Create graphs of interventions per module over time ############
# ---------------------------------------------

##Fill in year gaps where modules and years might be missing:
gf_mods <- data.table(graphData$disease, graphData$grant_number, graphData$year) ##the diseases that are present in the data

setnames(gf_mods,c("disease", "grant_number", "year"))
create_na_mods <- unique(gf_mods)
##bring in the module list from the GF framework
mapping_list <- disease_names_for_plots(gf_mapping_list)
mapping_list$code <- NULL
mapping_list$intervention <- NULL
mapping_list$module <- NULL
mapping_list$abbrev_intervention <- NULL

##make a list of all possible modules for each year and disease that we have: 
create_na_mods <- merge(create_na_mods, mapping_list, by="disease", allow.cartesian = TRUE)
create_na_mods <- unique(create_na_mods)

modData <- merge(graphData, create_na_mods,all.y=TRUE, by=c("year","disease", "grant_number","abbrev_module"), allow.cartesian = TRUE)

##sometimes modules won't be present in the data
modData[is.na(abbrev_intervention), abbrev_intervention:="Module Not Included"]

## figure out which values only have summary level data: 
modData$graph_module <- mapply(get_summary_level,as.character(modData$abbrev_module), 
                           as.character(modData$abbrev_intervention))


#sum up the budget and expenditure by variables of interest
byVars = names(modData)[names(modData)%in%c('abbrev_module', 'graph_module','year', 'disease', 'grant_number')]
modData = modData[, list(budget=sum(na.omit(budget)), expenditure=sum(na.omit(expenditure))), by=byVars]

modData[,sum_budget:=sum(na.omit(budget)), by=c("grant_number", "year", "disease")]

## if you want bars with dollar amounts: 
modData[budget<=0]$budget <- NA

##if you want 100% stacked bar graphs: 
modData[budget==0, graph_module:="Module Not Included"]
modData[graph_module=="Module Not Included", budget:=100]


##subset by disease: 
hivData <- modData[disease=="HIV/AIDS"]
malData <- modData[disease=="Malaria"]
tbData <- modData[disease=="Tuberculosis"]
hssData <- modData[disease=="RSSH"]
# ---------------------------------------------
######### set up color schemes for graphs ############
# ---------------------------------------------

##list of colors to apply to the interventions: 
colors <- c('#a6cee3', ##periwinkle
  '#1f78b4', ##summer lake
  '#b2df8a', ##muted lime 
  '#B9006E',##magenta
  '#4affd4', # bright mint
  '#fb9a99', ##peach blossom 
  '#9e82ba', #dark lilac
  '#fdbf6f', ##goldenrod
  '#ff7f00', ##pumpkin spice
  '#cab2d6', ##lilac
  '#00CCD6', ##caribbean beach
  '#93b500', ##neutralized chartreuse 
  '#db0645', ##cherry flavoring
  '#42090a', ##cherry coke
  '#f4c7d4', ##blush pink
  '#e2725b',##terracotta
  '#a977f4', ##80s purple
  '#01a004', ## kelly green
  '#0c5768', ##dark teal 
  '#6a3d9a', ## UW purple
  '#487a06', ## jungle olive
  '#f2f22e', ##daffodil
  '#94FFFC', ##icy blue
  '#b15928' ##umber 
  )

##sometimes there are more interventions that colors, so this just repeats the colors 
##until all of the interventions are assigned a color
interventions <- unique(na.omit(hivData$abbrev_interventions)) ##change datasets when necessary
cols <- rep(colors, length.out=length(interventions))
names(cols) <- interventions

##manually assign "No Data", "Summary Level Only", etc. their own colors: 
cols[names(cols)=="Summary Level Only"]="grey50"
cols[names(cols)=="No Data"]="#FFFFFF"
cols[names(cols)=="Module Not Included"]="#FFFFFF"

# ---------------------------------------------
######### Produce the graphs and export as PDF ############
# ---------------------------------------------

#### bar charts over time 
int_plots <- list()
for (k in unique(hivData$graph_module)){
  subdata <- hivData[graph_module==k]
  plot <- (ggplot(data=subdata, aes(x = year, y= budget/1000000, fill=abbrev_intervention)) + 
             geom_bar(colour="black", #uncomment if you want 100% bars:  position = "fill",
                      stat="identity") + 
             scale_fill_manual(name="Interventions", values =cols) +
             # facet_grid(~facet,scales = "free_x", space="free_x") + 
             theme_bw(base_size=10.5) +
            ##uncomment if you want 100% bars:  scale_y_continuous(labels = percent_format()) +
             scale_x_continuous(name ="Year", breaks = seq(2005, 2020,3), limits=c(2005, 2020)) +
             labs(title=paste("GF Module:", k),
                  x = "", y = "USD (millions)", caption="Data Source: GOS, FPM"))
  int_plots[[k]] <- plot
}


##export the list of graphs as a PDF: 
pdf("interventions_overtime_perc.pdf", height=6, width=9)
invisible(lapply(int_plots, print))
dev.off()




