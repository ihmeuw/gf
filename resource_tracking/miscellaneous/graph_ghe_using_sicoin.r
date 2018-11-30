#---------------------------------------------------------------
#AUTHOR: Emily Linebarger 
#DATE: November 2018 
#PURPOSE: Generate a graph showing government health expenditure 
#         over time by disease using SICOIN data. To be used 
#         for 2018 annual reports. 
#---------------------------------------------------------------

# ------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(scales)
# ------------------

# -----------------------------------------------------------------
# Files and directories
# -----------------------------------------------------------------
# root directory for input/average_absorptionput
dir = 'J:/Project/Evaluation/GF/resource_tracking/multi_country/'

# input data
inFile = paste0(dir, 'mapping/total_resource_tracking_data.csv')


# -----------------------------------------------------------------------
# Load/prep data - subset to government health expenditure in SICOIN data 
# -----------------------------------------------------------------------
allData = fread(inFile)
sicoin = allData[data_source == 'sicoin']
sicoin = sicoin[financing_source == 'ghe']

#Extrapolate 2018 budget numbers from first 6 months 
sicoin_2018 <- sicoin[year == 2018]
sicoin_2018 <- sicoin_2018[, .('budget' = sum(budget, na.rm = T)), by = c("start_date", "disease")]
sicoin_2018 = sicoin_2018[, .(budget=mean(budget)), by='disease']
sicoin_2018[, budget:=budget*12]
sicoin_2018[, year:=2018]
setnames(sicoin_2018, 'budget', 'extrapolated_budget')

sicoin <- sicoin[, .('budget' = sum(budget, na.rm = T)), by = c('disease', 'year')]
sicoin = merge(sicoin, sicoin_2018, by=c('disease','year'), all.x=TRUE)

sicoin[year==2018, budget:=extrapolated_budget]

#-------------------------
# Make the graph 
#-------------------------

graph = ggplot(data = sicoin, aes(x = year, y = budget, color = disease)) + geom_line(lwd = 3) + ggtitle("Government Health Expenditure by disease, using SICOIN data") + 
  labs(x = "Year", y = "Budget", caption = "2018 numbers extrapolated based on first 6 months") + 
  theme_bw(base_size = 18) + theme(legend.title = element_blank()) + scale_colour_discrete(labels = c("HIV", "Malaria", "TB")) + 
  scale_y_continuous(breaks = seq(0,14000000,1000000), labels = dollar)
graph

ggsave("J:/Project/Evaluation/GF/resource_tracking/gtm/visualizations/ghe_using_sicoin_by_year.pdf", graph)
ggsave("J:/Project/Evaluation/GF/resource_tracking/gtm/visualizations/ghe_using_sicoin_by_year.png", graph)


#------------------------------------------------------------------------------------------
#Can you do another graph as well showing what share of total program spend this represents?
#------------------------------------------------------------------------------------------

total_gtm <- allData[country == "Guatemala"]
total_gtm <- total_gtm[, is_ghe:=ifelse(financing_source == "ghe", "Global Health Expenditure", "Other")]

#Option 1, not faceted by diease 
plot_data <- total_gtm[, .(budget = sum(budget)), by = c("is_ghe", "year")]
plot_data[, budget_total:=sum(budget), by = c("year")]
plot_data[is_ghe == "Other", budget:=budget_total] #Replace "other" with the total of other and ghe funding. 
plot_data[, .(is_ghe, year, budget)]
plot_data = plot_data[budget_total != 0] #Don't have information for these years.

plot_data = plot_data[year < 2019] #Removing years in the future. 
plot_data = plot_data[, budget:=budget/1000000] #Scale budget down so we can graph by millions. 

graph = ggplot(data = plot_data, aes(year, budget)) + geom_bar(stat = "identity", aes(fill = is_ghe)) + 
  labs(x = "Year", y = "Budget, Millions USD") + 
  theme_bw(base_size = 18) + theme(legend.title = element_blank()) +
  scale_y_continuous(breaks = seq(0, 100, 10))
graph

#Option 1, not faceted by diease 
ggsave("J:/Project/Evaluation/GF/resource_tracking/gtm/visualizations/ghe_percentage1.pdf", graph)
ggsave("J:/Project/Evaluation/GF/resource_tracking/gtm/visualizations/ghe_percentage1.png", graph)

#Option 2, faceted by diease. 
plot_data <- total_gtm[, .(budget = sum(budget)), by = c("is_ghe", "year", "disease")]
plot_data[, budget_total:=sum(budget), by = c("year", "disease")]
plot_data[is_ghe == "Other", budget:=budget_total] #Replace "other" with the total of other and ghe funding. 
plot_data[, .(is_ghe, year, budget, disease)]
plot_data = plot_data[budget_total != 0] #Don't have information for these years.

plot_data = plot_data[year < 2019] #Removing years in the future. 
plot_data = plot_data[, budget:=budget/1000000] #Scale budget down so we can graph by millions.

graph = ggplot(data = plot_data, aes(year, budget)) + geom_bar(stat = "identity", aes(fill = is_ghe)) + 
  labs(x = "Year", y = "Budget, Millions USD") + 
  theme_bw(base_size = 18) + theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 50, 10)) + 
  facet_wrap(vars(disease))
graph

ggsave("J:/Project/Evaluation/GF/resource_tracking/gtm/visualizations/ghe_percentage2.pdf", graph)
ggsave("J:/Project/Evaluation/GF/resource_tracking/gtm/visualizations/ghe_percentage2.png", graph)
