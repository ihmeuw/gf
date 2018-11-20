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

ggsave("J:/Project/Evaluation/GF/resource_tracking/gtm/visualizations/ghe_using_sicoin_by_year.png", graph)

