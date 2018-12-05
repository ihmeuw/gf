# ----------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Standard functions for mapping results chains
#       
# DATE: Last updated November 2018. 
#-----------------------------------------------------

#----------
# Outputs 
#----------

#time on x axis and outputs on y axis

#----------
# Activities 
#----------

#Time on x axis and activities on y axis 

#Make a standard theme? 
std_theme = theme_bw(base_size = 18)

#Plot given y-var over time for dataset dt. 
budget_over_time = function(dt, y_var){
  plot = ggplot(data = dt, aes(x = year, y = y_var)) + 
    std_theme
  return(plot)
}

count_over_time = function(dt, y_var){
  
  
}

#Return a stacked area plot for the given x, y, and fill variables. 
stacked_area_plot = function(data, x_var, y_var, fill_var){
  print(length(dt)) 
  dt = copy(data)
  print(length(dt$x_var)) 
  print(length(dt$y_var))
  print(length(dt$fill_var))
  plot = ggplot(data = dt, aes(x = dt$x_var, y = dt$y_var)) + 
    geom_area(aes(color = dt$fill_var, fill = dt$fill_var), position = "stack") + 
    std_theme
  return(plot)
  
}