#----------------------------------------------------------------------
# AUTHOR: Emily Linebarger
# PURPOSE: Make malaria Other DAH and GHE visualizations for Uganda
# DATE: July 2019 
#----------------------------------------------------------------------

rm(list=ls())
options(scipen=100)
library(data.table)
library(ggplot2)
library(googlesheets)
library(RColorBrewer)

user <- "elineb" #Replace with your username
repo <- paste0("C:/Users/", user, "/Documents/gf/results_chains/") #Modify to fit your repo location

source(paste0(repo, "mapping_functions.r")) 

# ---------------------------------------
# Set filepaths 
# ---------------------------------------
main = "J:/Project/Evaluation/GF/results_chains/"

gtm_save <- paste0(main, "gtm/")
cod_save <- paste0(main, "cod/")
uga_save <- paste0(main, "uga/")
sen_save <- paste0(main, "sen/")

# ---------------------------------------
# Prep key datasets.   
# ---------------------------------------
#allRT <- fread("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/total_resource_tracking_data.csv")

other_dah = readRDS("J:/Project/Evaluation/GF/resource_tracking/_fgh/prepped_data/other_dah_actuals_all.rds")
other_dah[loc_name=='COD', country:='Congo (Democratic Republic)']
other_dah[loc_name=='GTM', country:='Guatemala']
other_dah[loc_name=='SEN', country:='Senegal']
other_dah[loc_name=='UGA', country:='Uganda']

ghe = readRDS("J:/Project/Evaluation/GF/resource_tracking/_fgh/prepped_data/ghe_actuals_malaria.rds") 
setDT(ghe)
ghe = ghe[loc_name=="uga"]

#You have overlap in reporting sources for UGA - this needs to be addressed more generally, but for these graphs just subset to NHAs where you have 2 sources in 2012-2014. . 
ghe[duplicated(ghe, by=c('year')), dup:=TRUE]
ghe 
ghe$dup = NULL

ghe = ghe[!(year>=2012 & year <= 2014 & source_type!="nha")]
ghe

#--------------------------------
# PLOT 1 - MALARIA DAH 
#--------------------------------
plot_data = other_dah[loc_name=="UGA" & disease == "malaria" & year>=2010 & year <= 2017] #Use actual numbers. 

#Add in a line for DFID - assuming this is Great Britain bilateral aid. 
plot_data[channel=="BIL_GBR", channel_agg:="DFID"]

plot_data = plot_data[, .(disbursement=sum(disbursement)), by = .(channel_agg, year)]

#Format variables 
plot_data$disbursement <- as.numeric(plot_data$disbursement)

#Wrap text for expecially long labels
plot_data[channel_agg == "UN agencies, The World Bank and other regional development banks", 
          channel_agg:= "UN agencies, The World Bank \nand other regional development banks"]

#Order plot so global fund is on the bottom. 
plot_data$channel_agg <- as.factor(plot_data$channel_agg)
funders <- as.vector(unique(plot_data$channel_agg))
funders = funders[!funders %in% c("The Global Fund")]
funders = c(funders, "The Global Fund")
plot_data$channel_agg <- factor(plot_data$channel_agg, levels = funders)
plot_data = plot_data[order(channel_agg)]

#Generate plot 
funding_landscape = ggplot(plot_data, aes(x = year, y = disbursement, fill = channel_agg)) + 
  geom_ribbon(aes(ymin = 0, ymax= 100000000), position = "stack") + 
  theme_bw(base_size = 18) + theme(legend.title = element_blank())+
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 150000000, by = 25000000), labels = scales::dollar) +
  scale_fill_brewer(palette = "RdYlBu") +
  labs(x = "Year", y = "Disbursement ($)", title = paste0("International funding landscape in Uganda for malaria, 2010-2017"))

ggsave("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/UGA Malaria September 2019/funding_landscape.png", plot = funding_landscape, height=6, width=14)


#--------------------------------
# PLOT 2- case management only 
#--------------------------------
plot_data = other_dah[loc_name=="UGA" & disease == "malaria" & gf_module == "Case management" & year>=2010 & year <= 2017] #Use actual numbers. 

#Add in a line for DFID - assuming this is Great Britain bilateral aid. 
plot_data[channel=="BIL_GBR", channel_agg:="DFID"]

plot_data = plot_data[, .(disbursement=sum(disbursement)), by = .(channel_agg, year)]

#Format variables 
plot_data$disbursement <- as.numeric(plot_data$disbursement)

#Wrap text for expecially long labels
plot_data[channel_agg == "UN agencies, The World Bank and other regional development banks", 
          channel_agg:= "UN agencies, The World Bank \nand other regional development banks"]

#Order plot so global fund is on the bottom. 
plot_data$channel_agg <- as.factor(plot_data$channel_agg)
funders <- as.vector(unique(plot_data$channel_agg))
funders = funders[!funders %in% c("The Global Fund")]
funders = c(funders, "The Global Fund")
plot_data$channel_agg <- factor(plot_data$channel_agg, levels = funders)
plot_data = plot_data[order(channel_agg)]

#Generate plot 
funding_landscape = ggplot(plot_data, aes(x = year, y = disbursement, fill = channel_agg)) + 
  geom_ribbon(aes(ymin = 0, ymax= 60000000), position = "stack") + 
  theme_bw(base_size = 18) + theme(legend.title = element_blank())+
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 60000000, by = 10000000), labels = scales::dollar) +
  scale_fill_brewer(palette = "RdYlBu") +
  labs(x = "Year", y = "Disbursement ($)", title = paste0("International funding landscape in Uganda for\nmalaria case management, 2010-2017"))

ggsave("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/UGA Malaria September 2019/funding_landscape_cm.png", plot = funding_landscape, height=6, width=14)

#--------------------------------
# PLOT 3 - ADD GHE
#--------------------------------
# Prep data 
{
  plot_data = other_dah[loc_name=="UGA" & disease == "malaria" & year>=2010 & year <= 2017] #Use actual numbers. 
  
  #Add in a line for DFID - assuming this is Great Britain bilateral aid. 
  plot_data[channel=="BIL_GBR", channel_agg:="DFID"]
  
  plot_data = plot_data[, .(disbursement=sum(disbursement)), by = .(channel_agg, year)]
  
  ghe = ghe[, .(disbursement=sum(value)), by='year'] #I'm not sure if we can assume this is the same as disbursement? 
  ghe[, channel_agg:='GHE']
  
  plot_data = rbind(plot_data, ghe)
} 


#Format variables 
plot_data$disbursement <- as.numeric(plot_data$disbursement)

#Wrap text for expecially long labels
plot_data[channel_agg == "UN agencies, The World Bank and other regional development banks", 
          channel_agg:= "UN agencies, The World Bank \nand other regional development banks"]

#Order plot so global fund is on the bottom. 
plot_data$channel_agg <- as.factor(plot_data$channel_agg)
funders <- as.vector(unique(plot_data$channel_agg))
funders = funders[!funders %in% c("GHE", "The Global Fund")]
funders = c("GHE", funders, "The Global Fund")
plot_data$channel_agg <- factor(plot_data$channel_agg, levels = funders)
plot_data = plot_data[order(channel_agg)]

#Generate plot 
funding_landscape = ggplot(plot_data[year>=2010], aes(x = year, y = disbursement, fill = channel_agg)) + 
  geom_ribbon(aes(ymin = 0, ymax= 200000000), position = "stack") + 
  theme_bw(base_size = 18) + theme(legend.title = element_blank())+
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 200000000, by = 25000000), labels = scales::dollar) +
  scale_fill_brewer(palette = "RdYlBu") +
  labs(x = "Year", y = "Disbursement ($)", title = paste0("Funding landscape in Uganda for malaria, 2010-2017"))

ggsave("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/UGA Malaria September 2019/funding_landscape_ghe.png", plot = funding_landscape, height=6, width=14)

