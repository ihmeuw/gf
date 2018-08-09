# ----------------------------------------------
# Irena Chen
#
# 1/5/2018

### This code is to AGGREGATE all data sources from all countries: 

# ----------------------------------------------
###### Function to prep the bed net data ###### 
# ----------------------------------------------

prep_gtm_bed_nets <- function(dir, start_date, period){
  #read in the CSVS
  
  bednets_detailed <- data.table(read.csv(dir))
  bednet_dir = paste0(substr(dir, 1, nchar(dir)-11), ".csv")
  bednets <- data.table(read.csv(bednet_dir))

  # Format dates to be correct, and only keep year, month  
  bednets$FechaEnt = ifelse(grepl("/", as.character(bednets$FechaEnt)), format(as.Date(as.character(bednets$FechaEnt), format = "%d/%m/%Y"), "%Y-%m-%d"), as.character(bednets$FechaEnt))
  bednets$FechaEnt = substring(as.character(bednets$FechaEnt), 1, 7)
  
  # Merge details and bednets data based on code_boleta
  bn_data = data.table(merge(bednets_detailed[,c("CodReg", "CodDepto", "CodMuni", "Personas","Casa","CodEnt", "MEmbarazada", "Menores5a", "Camas", "Pabellones","CodBoleta")],
                              bednets[,c("CodReg", "CodDepto", "CodMuni","CodBoleta", "FechaEnt")], by=c("CodReg", "CodDepto", "CodMuni","CodBoleta"), allow.cartesian=TRUE))

  
  ## what's wrong
  #yearly_count = bn_data[, sum(Pabellones), by= .(YearMonth = str_sub(FechaEnt, 1, 4)) ]
  #yearly_count[,sum(V1), by = str_sub(YearMonth, 1, 4) ] [order(str_sub)]
  
  
  #bn_data$FechaEnt = str_sub(bn_data$FechaEnt), 1, 4)
  #specified_year = str_sub(start_date, 1, 4) 
  #bn_data = bn_data[FechaEnt == specified_year]
   
  setnames(bn_data,c("CodReg", "CodDepto", "CodMuni", "Personas","Casa","CodEnt", "MEmbarazada", "Menores5a", "Camas", "Pabellones","CodBoleta", "FechaEnt"),
           c("regional_code","adm1", "adm2", "num_persons", "casa", "codent","num_pregnant", "num_minors"
             , "household_beds", "bed_nets", "code_boleta","year"))

  
  #totalNets_prep[,1:3] <- lapply(bn_data[,1:3], as.integer)
  #totalNets_prep[,4:10] <- lapply(bn_data[,4:10], as.numeric)
  
  
  ## replace any values of 0 in region, dept, and muni columns with corresponding values, matching on "casa"
  ##and "codent" 
  
  bn_data <- setDT(bn_data)[, regional_code:= regional_code[regional_code!=0][1L] , by =c("casa", "codent")]
  bn_data <- setDT(bn_data)[,adm1:= adm1[adm1!=0][1L], by =c("casa", "codent")]
  bn_data <- setDT(bn_data)[, adm2:= adm2[adm2!=0][1L] , by =c("casa", "codent")]

  #bn_data$year = ymd(bn_data$year)
  ##sum by municipality: 
  
  byVars = c('regional_code', 'adm1', 'adm2', 'year')
  bn_data=  bn_data[, list(num_persons=sum(na.omit(num_persons)),num_pregnant = sum(na.omit(num_pregnant)),
                          num_minors=sum(na.omit(num_minors)),household_beds=sum(na.omit(household_beds))
                          , bed_nets = sum(na.omit(bed_nets))), by=byVars]
  
  bn_data = unique(bn_data)
  
  ##add variables that might be useful:
  #bn_data$db_year <- start_date
  bn_data$period <- period
  #bn_data$csv <- dir
  return(bn_data)
}
