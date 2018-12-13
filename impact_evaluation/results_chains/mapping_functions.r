# ----------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Standard functions for mapping results chains
#       
# DATE: Last updated December 2018. 
#-----------------------------------------------------

#
modules_over_time = function(country_name, disease_name, start_year, end_year){
  plot_data = gf_budgets[country == country_name & disease == disease_name & (year >=start_year-1 & year <=end_year),
                        .(country, disease, year, start_date, end_date, budget, gf_module)] #Use actual numbers. 
  
  plot_data$budget <- as.numeric(plot_data$budget)
  
  map_mod_groups <- fread("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/category_codebook.csv")
  map_mod_groups = map_mod_groups[, .(gf_module, category)]
  map_mod_groups = map_mod_groups[!duplicated(map_mod_groups)]
  
  plot_data =  merge(plot_data, map_mod_groups, allow.cartesian = TRUE)
  
  #Aggregate budget by activity/year
  plot_data <- plot_data[, budget:=sum(budget), .(year, category)]
  plot_data = plot_data[!duplicated(plot_data) & !is.na(budget)]
  
  #Format variables 
  plot_data$year <- as.Date(as.character(plot_data$year), format = "%Y")
  
  max_db <- (max(plot_data$budget)) #Somehow use this to set axes. 
  scale_group = 1 #Group 1 is less than 5 Mil
  if(max_db >= 5000000 & max_db < 25000000){
    scale_group = 2  #Group 2 is between 5mil and 25mil
  }else if(max_db >= 25000000 & max_db < 100000000){
    scale_group = 3 #Group 3 is between 25 mil and 100 mil
  } else if (max_db >=100000000){
    scale_group = 4 #Group 4 is greater than 100 mil
  }
  plot_scale = ifelse(scale_group == 3 | scale_group == 4, 1000000, 1) #If max disbursement is greater than 25 million divide by 1 million. 
  scale_label = ifelse(scale_group == 1 | scale_group == 2, "Budget (USD)", "Budget (Millions USD)")
  plot_ticks = as.numeric(100000)
  if (scale_group == 2){
    plot_ticks = 1000000
  } else if (scale_group == 3){
    plot_ticks = 5
  } else if (scale_group == 4){
    plot_ticks = 50
  }
  
  plot_data = plot_data[, budget:=budget/plot_scale] #Format graph in millions
  max_db <- (max(plot_data$budget)) #Somehow use this to set axes. 
  y_max <- ifelse(scale_group == 1 | scale_group == 2, round(max_db, 0), round(max_db*10, 0)) #Scale y-axis 
  
  disease_label = "HIV"
  if(disease_name == "tb"){
    disease_label = "TB"
  } else if (disease_name == "malaria"){
    disease_label = "Malaria"
  }
  
  #Remove rows where budget is 0. 
  plot_data = plot_data[budget != 0]
  
  #Format year for labels, for some reason R is moving everything back one tic. 
  plot_data$year = plot_data$year-365
  
  #Generate plot
  budget_over_time = ggplot(data = plot_data, aes(x = year, y = budget, color = category)) + 
    geom_line(size = 2, alpha = 0.25)  + 
    geom_point() + 
    theme_bw(base_size = 16) + theme(legend.title = element_blank()) +
    scale_y_continuous(breaks = seq(0, y_max, by = plot_ticks), labels = dollar) +
    scale_x_date(date_breaks = "1 year", labels = date_format("%Y")) + 
    labs(x = "Year", y = scale_label, title = paste0("Budget by module in ", country_name, ", for ", disease_label, " ", start_year, "-", end_year))
  
  return(budget_over_time)
}

#Accepts a country, a disease, a time period (start and end year, inclusive), and a boolean on whether to include national health expenditure. 
#Return a graph of the funding landscape for the disease in the country over the time period using Financing Global Health actuals. 
funding_landscape = function(country_name, disease_name, start_year, end_year, include_ghe){
  plot_data = fgh_actual[country == country_name & disease == disease_name & (year >=start_year& year <=end_year),
                         .(country, disease, year, financing_source, start_date, end_date, disbursement)] #Use actual numbers. 
  if (include_ghe == TRUE){ #Need to discuss with David. 
    if(country_name == "Guatemala"){
      sicoin <- allRT[data_source == "sicoin"]
      plot_data <- merge(plot_data, sicoin)
    } else {
      plot_data <- merge(plot_data, gf_budgets)
    }
  }
  
  plot_data[, disbursement:=sum(disbursement), by = .(financing_source, start_date, end_date)]
  plot_data = plot_data[!duplicated(plot_data)]
  
  #Format variables 
  plot_data$year <- as.Date(as.character(plot_data$year), format = "%Y")
  plot_data$disbursement <- as.numeric(plot_data$disbursement)
  
  max_db <- (max(plot_data$disbursement)) #Somehow use this to set axes. 
  scale_group = 1 #Group 1 is less than 5 Mil
  if(max_db >= 5000000 & max_db < 25000000){
    scale_group = 2  #Group 2 is between 5mil and 25mil
  }else if(max_db >= 25000000 & max_db < 100000000){
    scale_group = 3 #Group 3 is between 25 mil and 100 mil
  } else if (max_db >=100000000){
    scale_group = 4 #Group 4 is greater than 100 mil
  }
  plot_scale = ifelse(scale_group == 3 | scale_group == 4, 1000000, 1) #If max disbursement is greater than 25 million divide by 1 million. 
  scale_label = ifelse(scale_group == 1 | scale_group == 2, "Disbursement (USD)", "Disbursement (Millions USD)")
  plot_ticks = as.numeric(50000)
  if (scale_group == 2){
    plot_ticks = 1000000
  } else if (scale_group == 3){
    plot_ticks = 5
  } else if (scale_group == 4){
    plot_ticks = 50
  }
  
  plot_data = plot_data[, disbursement:=disbursement/plot_scale] #Format graph in millions
  max_db <- (max(plot_data$disbursement)) #Somehow use this to set axes. 
  y_max <- ifelse(scale_group == 1 | scale_group == 2, round(max_db, 0), round(max_db*10, 0)) #Scale y-axis 
  
  disease_label = "HIV"
  if(disease_name == "tb"){
    disease_label = "TB"
  } else if (disease_name == "malaria"){
    disease_label = "Malaria"
  }
  
  plot_data = plot_data[disbursement != 0] #Don't plot funders with 0 disbursement. 
  
  plot_data$financing_source <- as.factor(plot_data$financing_source)
  funders <- as.vector(unique(plot_data$financing_source))
  funders = funders[!funders %in% "Global Fund"]
  funders = c(funders, "Global Fund")
  plot_data$financing_source <- factor(plot_data$financing_source, levels = funders)
  
  #Format year for labels, for some reason R is moving everything back one tic. 
  plot_data$year = plot_data$year-365
  
  #Generate plot 
  funding_landscape = ggplot(data = plot_data, aes(x = year, y = disbursement, color = financing_source, fill = financing_source)) + 
    geom_ribbon(aes(ymin = 0, ymax = disbursement), position = "stack") + 
    theme_bw(base_size = 16) + theme(legend.title = element_blank())+
    scale_y_continuous(breaks = seq(0, y_max, by = plot_ticks), labels = dollar) +
    scale_x_date(date_breaks = "1 year", labels = date_format("%Y")) + 
    labs(x = "Year", y = scale_label, title = paste0("Funding landscape in ", country_name, " for ", disease_label, ", ", start_year, "-", end_year))
  
  return(funding_landscape)
  
}