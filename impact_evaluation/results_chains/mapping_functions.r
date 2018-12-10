# ----------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Standard functions for mapping results chains
#       
# DATE: Last updated November 2018. 
#-----------------------------------------------------

#
budget_over_time = function(country_name, disease_name, start_year, end_year, y_var){
  plot_data = gf_budgets[country == country_name & disease == disease_name]
  
  #Subset to the years and columns we want. 
  desired_cols <- c("year", "budget", y_var)
  plot_data = plot_data[(year >=start_year & year <=end_year), desired_cols, with = FALSE]
  plot_data$budget <- as.numeric(plot_data$budget)
  
  #Aggregate budget by activity/year
  plot_data <- plot_data[, budget:=sum(budget), c("year", y_var)]
  plot_data = plot_data[!duplicated(plot_data)]
  
  #Format variables 
  plot_data$year <- as.Date(as.character(plot_data$year), format = "%Y")
  
  #Generate plot - How can we make this general for any y_var? 
  budget_over_time = ggplot(data = plot_data, aes(x = year, y = budget, color = gf_module)) + 
    geom_line()  + 
    theme_bw(base_size = 16) +
    labs(x = "Year", y = y_var, title = paste0(y_var, " in ", country_name, " ", start_year, "-", end_year))
  
  return(budget_over_time)
}

#Accepts a country, a disease, and a time period (start and end year, inclusive)
#Return a graph of the funding landscape for the disease in the country over the time period using Financing Global Health actuals. 
funding_landscape = function(country_name, disease_name, start_year, end_year){
  plot_data = fgh_actual[country == country_name & disease == disease_name] #Use actual numbers. 
  
  #Subset to the years we want, and remove cases where disbursement is less than 0 
  plot_data = plot_data[(year >=start_year & year <=end_year) & disbursement > 0, .(disbursement, year, financing_source)]
  
  #Format variables 
  plot_data$year <- as.Date(as.character(plot_data$year), format = "%Y")
  plot_data$disbursement <- as.numeric(plot_data$disbursement)
  plot_data = plot_data[, disbursement:=disbursement/1000000] #Format graph in millions
  max_db <- (max(plot_data$disbursement)) #Somehow use this to set axes. 
  y_max <- round(max_db*10, 0) #Scale y-axis 
  
  #Generate plot 
  funding_landscape = ggplot(data = plot_data, aes(x = year, y = disbursement, color = financing_source, fill = financing_source)) + 
    #geom_bar(stat = "identity")  + #Might also view this as a bar plot
    geom_ribbon(aes(ymin = 0, ymax = disbursement), alpha = 0.5) + 
    theme_bw(base_size = 16) + scale_y_continuous(breaks = seq(0, y_max, round(y_max/10, 0))) +
    labs(x = "Year", y = "Disbursement (Millions USD)", title = paste0("Funding landscape in ", country_name, " ", start_year, "-", end_year))
  
  return(funding_landscape)
  
}