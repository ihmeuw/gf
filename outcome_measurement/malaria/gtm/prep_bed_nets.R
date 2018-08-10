# ----------------------------------------------
# Irena Chen
#
# 1/5/2018

### This code is to AGGREGATE all data sources from all countries: 

# ----------------------------------------------
###### Function to prep the bed net data ###### 
# ----------------------------------------------

prep_gtm_bed_nets <- function(dir, start_date, period){
  bednets_detailed <- data.table(read.csv(paste0(dir, "EntregaPIIsDetalle.csv")))
  bednets <- data.table(read.csv(paste0(dir, "EntregaPIIs.csv")))
  
  totalNets = data.table(merge(bednets_detailed[,c("CodDepto", "CodMuni","Casa","Pabellones", "CodBoleta", "Camas")], 
                               bednets[,c("CodDepto", "CodMuni","CodBoleta", "FechaEnt")], by="CodBoleta", allow.cartesian=TRUE))
  
  
  
  setnames(bn_data,c("CodDepto", "CodMuni","Casa","Pabellones", "CodBoleta", "Camas"),
           c("regional_code","adm1", "adm2", "casa", "bed_nets", "code_boleta"
                      "household_beds"))
  
  bn_data[,1:3] <- lapply(bn_data[,1:3], as.integer)
  bn_data[,4:10] <- lapply(bn_data[,4:10], as.numeric)
  
  ## replace any values of 0 in region, dept, and muni columns with corresponding values, matching on "casa"
  ##and "codent" 
  
  bn_data <- setDT(bn_data)[, regional_code:= regional_code[regional_code!=0][1L] , by =c("casa", "codent")]
  bn_data <- setDT(bn_data)[,adm1:= adm1[adm1!=0][1L], by =c("casa", "codent")]
  bn_data <- setDT(bn_data)[, adm2:= adm2[adm2!=0][1L] , by =c("casa", "codent")]

  ##sum by municipality: 
  
  byVars = c('adm1', 'adm2')
  bn_data=  bn_data[, list(num_persons=sum(na.omit(num_persons)),num_pregnant = sum(na.omit(num_pregnant)),
                          num_minors=sum(na.omit(num_minors)),household_beds=sum(na.omit(household_beds))
                          , bed_nets = sum(na.omit(bed_nets))), by=byVars]
  
  ##add variables that might be useful:
  bn_data$start_date <- start_date
  bn_data$period <- period
  return(bn_data)
}
