
# ----------------------------------------------
# Irena Chen 
# 4/5/18
# Function that cleans the GTM malaria supply chain excel book and returns a data frame

# ----------------------------------------------
###### Set up the function  ###### 
# ----------------------------------------------

prep_malaria_data <- function(dir, inFile, sheet_name,start_date, period){
  sc_data <-data.table(read_excel(paste0(dir,inFile), sheet=as.character(sheet_name)))

  ##drop columns that contain only NAs: 
  sc_data <-Filter(function(x)!all(is.na(x)), sc_data)
  # ----------------------------------------------
  ## set column names according to the supply data they contain: 
  # ----------------------------------------------
  
  if(ncol(sc_data)>32&year(start_date)==2012){
    col_names <- c("dept_code", "department", "Cloroquina250_previousBalance", "Cloroquina250_topLevelEntries", "Cloroquina250_deliveredToUser", 
                   "Cloroquina250_notDeliveredToUser", "Cloroquina250_realDemand", "Cloroquina250_readjustments", "Cloroquina250_nextMonthBalance", 
                   "Cloroquina250_existenceInBodega", "Cloroquina250_avgMonthlyDemand", "Cloroquina250_monthsAvailable", "Primaquina15_previousBalance", 
                   "Primaquina15_topLevelEntries", "Primaquina15_deliveredToUser", "Primaquina15_notDeliveredToUser", "Primaquina15_realDemand", 
                   "Primaquina15_readjustments", "Primaquina15_nextMonthBalance", "Primaquina15_existenceInBodega", "Primaquina15_avgMonthlyDemand", 
                   "Primaquina15_monthsAvailable", "Primaquina5_previousBalance", "Primaquina5_topLevelEntries", "Primaquina5_deliveredToUser", 
                   "Primaquina5_notDeliveredToUser", "Primaquina5_realDemand", "Primaquina5_readjustments", "Primaquina5_nextMonthBalance", 
                   "Primaquina5_existenceInBodega", "Primaquina5_avgMonthlyDemand", "Primaquina5_monthsAvailable",
                   "Antimoniato10_previousBalance", "Antimoniato10_TopLevel", "Antimoniato10_deliveredtoUser"
                   ,"Antimoniato10_notDeliveredtoUser", "Antimoniato10_realDemand", "Antimoniato10_readjustments"
                   ,"Antimoniato10_nextMonthBalance", "Antimoniato10_physicalExistence",
                   "Antimoniato10_avgMonthlyDemand", "Antimoniato10_monthsAvailable")
    
  } else if (ncol(sc_data)>32){
    col_names <- c("dept_code", "department","Cloroquina250_previousBalance", "Cloroquina250_topLevelEntries", "Cloroquina250_deliveredToUser", 
                   "Cloroquina250_notDeliveredToUser", "Cloroquina250_realDemand", "Cloroquina250_readjustments", "Cloroquina250_nextMonthBalance", 
                   "Cloroquina250_existenceInBodega", "Cloroquina250_avgMonthlyDemand", "Cloroquina250_monthsAvailable", "Primaquina15_previousBalance", 
                   "Primaquina15_topLevelEntries", "Primaquina15_deliveredToUser", "Primaquina15_notDeliveredToUser", "Primaquina15_realDemand", 
                   "Primaquina15_readjustments", "Primaquina15_nextMonthBalance", "Primaquina15_existenceInBodega", "Primaquina15_avgMonthlyDemand", 
                   "Primaquina15_monthsAvailable", "Primaquina5_previousBalance", "Primaquina5_topLevelEntries", "Primaquina5_deliveredToUser", 
                   "Primaquina5_notDeliveredToUser", "Primaquina5_realDemand", "Primaquina5_readjustments", "Primaquina5_nextMonthBalance", 
                   "Primaquina5_existenceInBodega", "Primaquina5_avgMonthlyDemand", "Primaquina5_monthsAvailable",
                   "Antimoniato10_previousBalance", "Antimoniato10_TopLevel", "Antimoniato10_deliveredtoUser"
                   ,"Antimoniato10_notDeliveredtoUser", "Antimoniato10_realDemand", "Antimoniato10_readjustments"
                   ,"Antimoniato10_nextMonthBalance", "Antimoniato10_physicalExistence",
                   "Antimoniato10_avgMonthlyDemand", "Antimoniato10_monthsAvailable")
  } else if(month(start_date)<5&year(start_date)==2012){
    # January 2012 - May 2012 column names
    col_names <-  c("dept_code", "department", "Primaquina15_previousBalance", "Primaquina15_topLevelEntries", "Primaquina15_deliveredToUser", "Primaquina15_notDeliveredToUser",
                    "Primaquina15_realDemand", "Primaquina15_readjustments", "Primaquina15_nextMonthBalance", "Primaquina15_existenceInBodega", "Primaquina15_avgMonthlyDemand", 
                    "Primaquina15_monthsAvailable", "Primaquina5_previousBalance", "Primaquina5_topLevelEntries", "Primaquina5_deliveredToUser", "Primaquina5_notDeliveredToUser", 
                    "Primaquina5_realDemand", "Primaquina5_readjustments", "Primaquina5_nextMonthBalance", "Primaquina5_existenceInBodega", "Primaquina5_avgMonthlyDemand", 
                    "Primaquina5_monthsAvailable", "Cloroquina250_previousBalance", "Cloroquina250_topLevelEntries", "Cloroquina250_deliveredToUser", "Cloroquina250_notDeliveredToUser",
                    "Cloroquina250_realDemand", "Cloroquina250_readjustments", "Cloroquina250_nextMonthBalance", "Cloroquina250_existenceInBodega", "Cloroquina250_avgMonthlyDemand", 
                    "Cloroquina250_monthsAvailable" )
    
  } else { 
    # July 2015 - December 2012 column names (some order is reversed, names the same)
    col_names <- c("dept_code", "department","Cloroquina250_previousBalance", "Cloroquina250_topLevelEntries", "Cloroquina250_deliveredToUser", 
                   "Cloroquina250_notDeliveredToUser", "Cloroquina250_realDemand", "Cloroquina250_readjustments", "Cloroquina250_nextMonthBalance", 
                   "Cloroquina250_existenceInBodega", "Cloroquina250_avgMonthlyDemand", "Cloroquina250_monthsAvailable", "Primaquina15_previousBalance", 
                   "Primaquina15_topLevelEntries", "Primaquina15_deliveredToUser", "Primaquina15_notDeliveredToUser", "Primaquina15_realDemand", 
                   "Primaquina15_readjustments", "Primaquina15_nextMonthBalance", "Primaquina15_existenceInBodega", "Primaquina15_avgMonthlyDemand", 
                   "Primaquina15_monthsAvailable", "Primaquina5_previousBalance", "Primaquina5_topLevelEntries", "Primaquina5_deliveredToUser", 
                   "Primaquina5_notDeliveredToUser", "Primaquina5_realDemand", "Primaquina5_readjustments", "Primaquina5_nextMonthBalance", 
                   "Primaquina5_existenceInBodega", "Primaquina5_avgMonthlyDemand", "Primaquina5_monthsAvailable")
  }
   
   colnames(sc_data) <- col_names
  
   # --------------------------------------------
   ###  Optional calculations that you can include if you want ### 
   # --------------------------------------------
   # 
   #  sc_data$Cloroquina250_sumDelivered <- as.numeric(sc_data$Cloroquina250_notDeliveredToUser)
   # +as.numeric(sc_data$Cloroquina250_deliveredToUser)
   # sc_data$Primaquina15_sumDelivered <- as.numeric(sc_data$Primaquina15_notDeliveredToUser)
   #                                                 +as.numeric(sc_data$Primaquina15_deliveredToUser)
   # sc_data$Primaquina5_sumDelivered <- as.numeric(sc_data$Primaquina5_notDeliveredToUser)
   #                                                +as.numeric(sc_data$Primaquina5_deliveredToUser)
   # if(!is.null(sc_data$Antimoniato10_realDemand)){
   #   sc_data$Antimoniato10_sumDelivered <- as.numeric(sc_data$Antimoniato10_notDeliveredToUser)
   #   +as.numeric(sc_data$Antimoniato10_deliveredToUser)
   # }
   # sc_data$Cloroquina250_sumExistence <- as.numeric(sc_data$Cloroquina250_previousBalance)
   # +as.numeric(sc_data$Cloroquina250_deliveredToUser)
   # sc_data$Primaquina15_sumExistence <- as.numeric(sc_data$Primaquina15_previousBalance)
   # +as.numeric(sc_data$Primaquina15_deliveredToUser)
   # sc_data$Primaquina5_sumExistence <- as.numeric(sc_data$Primaquina5_previousBalance)
   # +as.numeric(sc_data$Primaquina5_deliveredToUser)
   # if(!is.null(sc_data$Antimoniato10_realDemand)){
   #   sc_data$Antimoniato10_sumExistence <- as.numeric(sc_data$Antimoniato10_previousBalance)
   #   +as.numeric(sc_data$Antimoniato10_deliveredToUser)
   # }
   
   
   # --------------------------------------------
   ### Reshape the data so that all of the variables are in one column ("input") ### 
   # --------------------------------------------
  
   sc_data <- sc_data[-c(1:3),] ##the first three rows have no use to us 
   sc_data<- melt(sc_data, id.vars = c("dept_code", "department"), variable.name = "antimalarial_input", value.name = "amount")
   
   sc_data <- na.omit(sc_data, cols=1, invert=FALSE)
   
   ##add some useful tracking variables: 
   sc_data$start_date <- start_date
   sc_data$period <- period
   
   return(sc_data)

}


