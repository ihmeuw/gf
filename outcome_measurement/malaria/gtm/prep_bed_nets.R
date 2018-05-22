# ----------------------------------------------
# Irena Chen
#
# 1/5/2018

### This code is to AGGREGATE all data sources from all countries: 

# ----------------------------------------------
###### Function to prep the bed net data ###### 
# ----------------------------------------------

prep_gtm_bed_nets <- function(inFile, start_date, period){
  bn_data <- data.table(read_csv(inFile, 
                                 col_names = FALSE))
  
  colnames(bn_data) <- as.character(bn_data[1,])
  bn_data <- bn_data[-1,]
  
  col_names <- c("CodReg", "CodDepto", "CodMuni","Personas","Casa", "CodEnt", "MEmbarazada", "Menores5a", "Camas",	"Pabellones")
  
  bn_data <- bn_data[,col_names, with=FALSE]
  
  setnames(bn_data, c("regional_code","adm1", "adm2", "num_persons", "casa", "codent","num_pregnant", "num_minors"
                      , "household_beds", "bed_nets"))
  
  bn_data[,1:3] <- lapply(bn_data[,1:3], as.integer)
  bn_data[,4:10] <- lapply(bn_data[,4:10], as.numeric)
  
  ## replace any values of 0 in region, dept, and muni columns with corresponding values, matching on "casa"
  ##and "codent" 
  
  bn_data <- setDT(bn_data)[, regional_code:= regional_code[regional_code!=0][1L] , by =c("casa", "codent")]
  bn_data <- setDT(bn_data)[,adm1:= adm1[adm1!=0][1L], by =c("casa", "codent")]
  bn_data <- setDT(bn_data)[, adm2:= adm2[adm2!=0][1L] , by =c("casa", "codent")]

  ##sum by municipality: 
  
  byVars = c('regional_code', 'adm1', 'adm2')
  bn_data=  bn_data[, list(num_persons=sum(na.omit(num_persons)),num_pregnant = sum(na.omit(num_pregnant)),
                          num_minors=sum(na.omit(num_minors)),household_beds=sum(na.omit(household_beds))
                          , bed_nets = sum(na.omit(bed_nets))), by=byVars]
  
  ##add variables that might be useful:
  bn_data$start_date <- start_date
  bn_data$period <- period
  return(bn_data)
}
