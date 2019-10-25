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
dir = 'J:/Project/Evaluation/GF/resource_tracking/gtm/prepped'

# input data
inFile = paste0(dir, '/prepped_sicoin_data.csv')


# -----------------------------------------------------------------------
# Load/prep data - subset to government health expenditure in SICOIN data 
# -----------------------------------------------------------------------
#Grab SICOIN for Guatemala funding graph 
sicoin <- fread(inFile)

# drop first/last year of the series because they appear to be incomplete
#sicoin = sicoin[year!=min(year) & year!=max(year)]
sicoin = sicoin[financing_source=='ghe'] #Make sure you subset to the correct data source too! 

#Convert SICOIN financial data into numeric 
sicoin[, budget:=as.numeric(budget)]
sicoin[, expenditure:=as.numeric(expenditure)]
sicoin[, disbursement:=as.numeric(disbursement)]

#Extrapolate 2018 budget numbers from first 6 months 
sicoin_2018 <- sicoin[year == 2018]
sicoin_2018 <- sicoin_2018[, .('budget' = sum(budget, na.rm = T)), by = c("start_date", "disease")]
sicoin_2018 = sicoin_2018[, .(budget=mean(budget), na.rm = T), by='disease'] #Make sure you use na.rm = T for all algebraic functions. 
sicoin_2018[, budget:=budget*12]
sicoin_2018[, year:=2018]
setnames(sicoin_2018, 'budget', 'extrapolated_budget')

sicoin <- sicoin[, .('budget' = sum(budget, na.rm = T)), by = c('disease', 'year')]
sicoin = merge(sicoin, sicoin_2018, by=c('disease','year'), all.x=TRUE)

sicoin[year==2018, budget:=extrapolated_budget]

#-------------------------
# Format graph in millions 
# ------------------------
sicoin[, budget:=budget/1000000]

#-------------------------
# Make the graph 
#-------------------------

graph = ggplot(data = sicoin, aes(x = year, y = budget, color = disease)) + geom_line(lwd = 3) + ggtitle("Government Health Expenditure by disease, using SICOIN data") + 
  labs(x = "Year", y = "Budget (Millions USD)", caption = "2018 numbers extrapolated based on first 6 months") + 
  theme_bw(base_size = 18) + theme(legend.title = element_blank()) + scale_colour_discrete(labels = c("HIV", "Malaria", "TB")) + 
  scale_y_continuous(breaks = seq(0,14,1), labels = scales::dollar)
graph

ggsave("J:/Project/Evaluation/GF/resource_tracking/gtm/visualizations/ghe_using_sicoin_by_year_corrected.pdf", graph)
ggsave("J:/Project/Evaluation/GF/resource_tracking/gtm/visualizations/ghe_using_sicoin_by_year_corrected.png", graph)


#------------------------------------------------------------------------------------------
#Can you do another graph as well showing what share of total program spend this represents?
#------------------------------------------------------------------------------------------

fgh = fread("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/prepped_current_fgh.csv")
fgh_actual = fgh[fin_data_type == "actual"]#Split FGH between actual numbers and model estimates. 
fgh_actual = fgh_actual[, .(year, budget, disease, financing_source)]
sicoin <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/prepped_sicoin_data.csv"
                              ,fileEncoding="latin1"))
sicoin_merge <- sicoin[financing_source == 'ghe', .(financing_source, year, budget, disease)]
total_gtm <- rbind(sicoin_merge, fgh_actual)
total_gtm <- total_gtm[, is_ghe:=ifelse(financing_source == "ghe", "Government Health Expenditure", "Other")]
total_gtm$disease <- factor(total_gtm$disease, c('hiv', 'tb', 'malaria'), c('HIV', 'TB', 'Malaria'))

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
  theme_bw(base_size = 18) + theme(legend.title = element_blank()) +
  facet_wrap(vars(disease), scales = 'free_y')
graph

ggsave("J:/Project/Evaluation/GF/resource_tracking/gtm/visualizations/ghe_percentage2.pdf", graph)
ggsave("J:/Project/Evaluation/GF/resource_tracking/gtm/visualizations/ghe_percentage2.png", graph)
