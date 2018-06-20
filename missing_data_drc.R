# ----------------------------------------------
  # Audrey Batzel
  #
  # 6/6/18
  # PNLP data for 2010-2017 all provinces
  # Missing Data Analysis for DRC
  
  # Set your working directory (this will be wherever your local repository is located)
    # Note: this might not be necessary since there are no external functions sourced in this code,
    # however, it is good practice to do so. 
    setwd('C:/local/gf/')
# ----------------------------------------------


# --------------------
# Set up R / install packages
  rm(list=ls())
  library(data.table)
  library(reshape2)
  library(stringr)
  library(ggplot2)
  library(lubridate)
  library(stats)
  library(tidyr)
# --------------------


# ----------------------------------------------
# Overview - Files and Directories

  # data directory
    dir_prepped <- "J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/"

  # input file:
    fullDataDPSstandardized <- "fullData_dps_standardized.csv"
  # csv file was initially produced by prep_COD_Malaria_data_function.R within ./gf/outcome_measurement/malaria/cod/
    # and then further prepped (to standardize DPS) by reshape_data.R and standardize_dps.R also within ./gf/outcome_measurement/malaria/cod/
  
  # read in the data
    dt_wide <- fread(paste0(dir_prepped, fullDataDPSstandardized))
# ----------------------------------------------
    
    
# ----------------------------------------------
  # Set up: 
    # make sure date variable is Date class in R
    dt_wide[, date := as.Date(date)]
    
    # remove variable "V1" which is added when the file is read into RStudio
    dt_wide <- dt_wide[,-c("V1")]
    
    # use colnames() to grab all of the variable names in the data table and store in a vector
    all_vars <- c(colnames(dt_wide))
    
    # make a vector of id variables (the variables not measured)
    id_vars <- c("province", "dps", "health_zone", "donor", "operational_support_partner", "population", "quarter", "month",
                 "year", "stringdate", "date", "natl", "natl_name", "province11", "province11_name", "province26", "province26_name", 
                 "dps_name_2015", "dps_name_2014", "dps_name_2013", "dps_name_2012", "dps_name_2010to2011", "dps_in_original_data") 
    
    # using the all_vars and id_vars vectors, get all of the measured variables by grabbing all of the 
    # variables that are in all_vars that aren't in id_vars
    num_vars <- all_vars[!all_vars %in% id_vars]
    
  # Use melt() function to convert the wide data table into long format, save this as a data table called "dt"
    dt <- melt(dt_wide, id.vars=id_vars, measure.vars=num_vars, variable.name="indicator", value.name= "value")

    # split the indicator values into "indicator" and "subpopulation variables on the character, "_"
    dt[, c("indicator", "subpopulation") := tstrsplit(indicator, "_", fixed=TRUE)]
    
    # make sure that the date variable is Date class in R, and the value variable is numeric
    dt[, date := as.Date(date)]
    dt[, value := as.numeric(value)] 
# ---------------------------------------------- 


# ----------------------------------------------
# Use the function is.na() 
    # Note, this is using syntax specific to data.table, which is saying to loop over all rows,
    # and create a new variable called "missing_value," and assign a value of 1 to this variable
    # if the variable "value" (which contains the data), is missing, and a 0 if there is data for it,
    # and do this by the vector of variables listed in the "by" section (effectively one value assessed row by row).
    
    dt_missing <- dt[, 
                     .(missing_value = ifelse(is.na(value), 1, 0)), 
                     by=c('date', 'province', 'dps', 'health_zone', 'indicator', 'subpopulation', 'value')]

# ----------------------------------------------
    

# ----------------------------------------------
# Make a histogram for total counts (sum) of missing data by each month
  # Note: there is a lot more missing data in earlier years because some variables
  # were only added in later years, and therefore are completely missing in earlier years
  # This gives an overview of missing data, but we want to look more specifically at variables
  # that are present in all years to see the trends. (See code and graph generated below).
    
    
    # This code will create a new variable called "sum_missing" which is the sum of missing values
    # by date, or effectively the number of missing data points when compared to the most complete data set over time
    
    dt_total_missing_byDate <- dt_missing[,
                                          .(sum_missing = sum(missing_value)),
                                          by=c('date')]
    
    # Make a histogram to display this data:
    
    g <- ggplot(data=dt_total_missing_byDate, aes(x = date, y = sum_missing)) + geom_bar(stat="identity")
    g <- g + ggtitle("Missing Data by Date") + xlab("Date (Month and Year)") + ylab("Count of Missing Values")
    print(g)
    
# ----------------------------------------------
    
    
# ----------------------------------------------
# ------HERE IS AN EXAMPLE OF THE GRAPHS IN THE SLIDES------
# Make a histogram for counts of missing data for a specfic indicator over time
  # Note: you can replace the value for indicator or subpopulation to generate different graphs
    
    
    # This code subsets the data table to just data for the indicator and subpopulation specified,
    # and creates a new variable called "percent_missing" which is effectively the percent of health zones
    # with data missing at each point in time for the specified indicator/subpopulation. (This is because 
    # there is one observation of each variable/subpopulation at each time point for each health zone)
    
    dt_missing_byIndicator <- dt_missing[indicator == "malariaDeaths" & subpopulation == "under5",
                                        .(percent_missing = (mean(missing_value)*100)), 
                                        by=c('date')]
    
    # Make a histogram to display this data:
    
    g <- ggplot(data=dt_missing_byIndicator, aes(x = date, y = percent_missing)) + geom_bar(stat="identity")
    g <- g + ggtitle("Missing Data for 1st ANC visit") + ylim(0, 100) + labs(x= "Date (Month and Year)", y= "Percent Missing Data") 
    print(g)
    
