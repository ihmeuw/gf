
# ----------------------------------------------
# Irena Chen 
#
# 4/5/18
# Function that cleans the GTM malaria supply chain excel book and returns a data frame

# ----------------------------------------------
###### Set up the function  ###### 
# ----------------------------------------------

prep_malaria_data <- function(dir, inFile, sheet_name,start_date, period){
  sc_data <-data.table(read_excel(paste0(dir,inFile), sheet=as.character(sheet_name)))

  ##drop columns that contain only NAs: 
  sc_data <-Filter(function(x)!all(is.na(x)), sc_data)
  
  ## set column names according to the supply data they contain: 
  if(ncol(sc_data)>32&year(start_date)==2012){
    col_names <- c("das_code", "das", "Cloroquina250_previousBalance", "Cloroquina250_topLevelEntries", "Cloroquina250_deliveredToUser", 
                   "Cloroquina250_notDeliveredToUser", "Cloroquina250_realDemand", "Cloroquina250_readjustments", "Cloroquina250_nextMonthBalance", 
                   "Cloroquina250_existenceInBodega", "Cloroquina250_avgMonthlyDemand", "Cloroquina250_monthsAvailable", "Primaquina15_previousBalance", 
                   "Primaquina15_topLevelEntries", "Primaquina15_deliveredToUser", "Primaquina15_notDeliveredToUser", "Primaquina15_realDemand", 
                   "Primaquina15_readjustments", "Primaquina15_nextMonthBalance", "Primaquina15_existenceInBodega", "Primaquina15_avgMonthlyDemand", 
                   "Primaquina15_monthsAvailable", "Primaquina5_previousBalance", "Primaquina5_topLevelEntries", "Primaquina5_deliveredToUser", 
                   "Primaquina5_notDeliveredToUser", "Primaquina5_realDemand", "Primaquina5_readjustments", "Primaquina5_nextMonthBalance", 
                   "Primaquina5_existenceInBodega", "Primaquina5_avgMonthlyDemand", "Primaquina5_monthsAvailable",
                   "antimoniato_10ml_prevbalance", "antimoniato_10ml_toplevel", "antimoniato_10ml_delivered_to_user"
                   ,"antimoniato_10ml_not_deliveredtouser", "antimoniato_10ml_realdemand", "antimoniato_10ml_readj"
                   ,"antimoniato_10ml_nextmobal", "antimoniato_10ml_phys_exist",
                   "antimoniato_10ml_avg_mo_demand", "antimoniato_10ml_mo_avail")
    
  } else if (ncol(sc_data)>32){
    col_names <- c("das_code", "das", "Cloroquina250_previousBalance", "Cloroquina250_topLevelEntries", "Cloroquina250_deliveredToUser", 
                   "Cloroquina250_notDeliveredToUser", "Cloroquina250_realDemand", "Cloroquina250_readjustments", "Cloroquina250_nextMonthBalance", 
                   "Cloroquina250_existenceInBodega", "Cloroquina250_avgMonthlyDemand", "Cloroquina250_monthsAvailable", "Primaquina15_previousBalance", 
                   "Primaquina15_topLevelEntries", "Primaquina15_deliveredToUser", "Primaquina15_notDeliveredToUser", "Primaquina15_realDemand", 
                   "Primaquina15_readjustments", "Primaquina15_nextMonthBalance", "Primaquina15_existenceInBodega", "Primaquina15_avgMonthlyDemand", 
                   "Primaquina15_monthsAvailable", "Primaquina5_previousBalance", "Primaquina5_topLevelEntries", "Primaquina5_deliveredToUser", 
                   "Primaquina5_notDeliveredToUser", "Primaquina5_realDemand", "Primaquina5_readjustments", "Primaquina5_nextMonthBalance", 
                   "Primaquina5_existenceInBodega", "Primaquina5_avgMonthlyDemand", "Primaquina5_monthsAvailable",
                   "antimoniato_10ml_prevbalance", "antimoniato_10ml_toplevel", "antimoniato_10ml_delivered_to_user"
                   ,"antimoniato_10ml_not_deliveredtouser", "antimoniato_10ml_realdemand", "antimoniato_10ml_readj"
                   ,"antimoniato_10ml_nextmobal", "antimoniato_10ml_phys_exist",
                   "antimoniato_10ml_avg_mo_demand", "antimoniato_10ml_mo_avail")
  } else if(month(start_date)<5&year(start_date)==2012){
    # January 2012 - May 2012 column names
    col_names <-  c("das_code", "das", "Primaquina15_previousBalance", "Primaquina15_topLevelEntries", "Primaquina15_deliveredToUser", "Primaquina15_notDeliveredToUser",
                    "Primaquina15_realDemand", "Primaquina15_readjustments", "Primaquina15_nextMonthBalance", "Primaquina15_existenceInBodega", "Primaquina15_avgMonthlyDemand", 
                    "Primaquina15_monthsAvailable", "Primaquina5_previousBalance", "Primaquina5_topLevelEntries", "Primaquina5_deliveredToUser", "Primaquina5_notDeliveredToUser", 
                    "Primaquina5_realDemand", "Primaquina5_readjustments", "Primaquina5_nextMonthBalance", "Primaquina5_existenceInBodega", "Primaquina5_avgMonthlyDemand", 
                    "Primaquina5_monthsAvailable", "Cloroquina250_previousBalance", "Cloroquina250_topLevelEntries", "Cloroquina250_deliveredToUser", "Cloroquina250_notDeliveredToUser",
                    "Cloroquina250_realDemand", "Cloroquina250_readjustments", "Cloroquina250_nextMonthBalance", "Cloroquina250_existenceInBodega", "Cloroquina250_avgMonthlyDemand", 
                    "Cloroquina250_monthsAvailable" )
    
  } else { 
    # July 2015 - December 2012 column names (some order is reversed, names the same)
    col_names <- c("das_code", "das", "Cloroquina250_previousBalance", "Cloroquina250_topLevelEntries", "Cloroquina250_deliveredToUser", 
                   "Cloroquina250_notDeliveredToUser", "Cloroquina250_realDemand", "Cloroquina250_readjustments", "Cloroquina250_nextMonthBalance", 
                   "Cloroquina250_existenceInBodega", "Cloroquina250_avgMonthlyDemand", "Cloroquina250_monthsAvailable", "Primaquina15_previousBalance", 
                   "Primaquina15_topLevelEntries", "Primaquina15_deliveredToUser", "Primaquina15_notDeliveredToUser", "Primaquina15_realDemand", 
                   "Primaquina15_readjustments", "Primaquina15_nextMonthBalance", "Primaquina15_existenceInBodega", "Primaquina15_avgMonthlyDemand", 
                   "Primaquina15_monthsAvailable", "Primaquina5_previousBalance", "Primaquina5_topLevelEntries", "Primaquina5_deliveredToUser", 
                   "Primaquina5_notDeliveredToUser", "Primaquina5_realDemand", "Primaquina5_readjustments", "Primaquina5_nextMonthBalance", 
                   "Primaquina5_existenceInBodega", "Primaquina5_avgMonthlyDemand", "Primaquina5_monthsAvailable")
  }
   
   colnames(sc_data) <- col_names
   sc_data <- sc_data[-c(1:3),] ##the first three rows have no use to us 
   sc_data<- melt(sc_data, id.vars = c("das_code", "das"), variable.name = "sc_input", value.name = "amount")
   
   sc_data <- na.omit(sc_data, cols=1, invert=FALSE)
   
   sc_data$start_date <- start_date
   sc_data$period <- period
   
   return(sc_data)

}


