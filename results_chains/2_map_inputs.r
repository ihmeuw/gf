# ----------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Map inputs as part of results chain. 
#       
# DATE: Last updated November 2018. 
#-----------------------------------------------------

#Take milestones table, and graph 5 categories over time.

#----------
# Inputs 
#----------

#1. Funding landscape- think about a stacked area plot for this. Especially global fund, GHE, other. Split graph by data source and financing source. 
plot_data = fgh[disbursement != 0, .(disbursement, year, financing_source)] #Do we want to subset to disbursement == 0 here? Do we want to subset year? 
plot_data$year <- as.Date(as.character(plot_data$year), format = "%Y")
funding_landscape = ggplot(data = plot_data, aes(x = year, y = disbursement, color = financing_source, fill = financing_source)) + 
  geom_area(position = "stack")
funding_landscape

