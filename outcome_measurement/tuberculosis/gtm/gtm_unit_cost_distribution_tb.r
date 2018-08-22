# ----------------------------------------------

# Naomi Provost
# Code for adding 
# ----------------------------------------------
###### Set up R / install packages  ###### 
# ----------------------------------------------

rm(list=ls())
library(lubridate)
library(data.table)
library(readxl)
library(stats)
library(stringr)
library(rlang)
library(zoo)
library(dplyr)

# ----------------------------------------------
## STEP 1: Download the prep_budget_data folder from UGA Basecamp and save somewhere on your local drive: 
##this has all of the files we will be using: 
## Notes: running this will throw a warning: 
#Warning messages:
#In read_fun(path = path, sheet = sheet, limits = limits, shim = shim,  :
#              NA inserted for impossible 1900-02-29 datetime

#But this shouldn't affect the final output. 
# ----------------------------------------------
#2013
# inFile = "Distribucion de medicamentos 2013.xlsx"
# sheet_name = "repmenconpre"

#2014
# inFile = "Distribucion de medicamentos 2014.xlsx"
# sheet_name = "repmenconpre 2014"

#2015
# inFile = "Distribucion de medicamentos 2015.xlsx"
# sheet_name = "repmenconpre 2015"

#2016
# inFile = "Distribucion de medicamentos 2016.xls"
# sheet_name = "Saldo Bodega"

#2016
# inFile = "Distribucion de medicamentos ene- oct 2017.xls"
# sheet_name = "Saldo Bodeg Informe"

#2018
#inFile = 'Egreso de medicamentos hasta junio 2018.xls'

### FUNCTION TO CLEAN THE UNIT COST DATA ###
prep_unit_cost = function(dir, inFile, sheet_name, start_year){
  # Clean data and only keep important columns
  dist_data <-data.table(read_excel(paste0(dir,inFile), sheet=sheet_name))
  dist_data <- dist_data[, c(1:3)]
  colnames(dist_data)[1] <- "Product"
  colnames(dist_data)[2] <- "unit_cost"
  colnames(dist_data)[3] <- "total_month"
  
  # Remove NAs to only keep valid values
  dist_data$Product = na.locf(dist_data$Product)
  dist_data$unit_cost = as.numeric(dist_data$unit_cost)
  dist_data$total_stock = as.numeric(dist_data$total_month)
  dist_data = na.omit(dist_data)
  
  
  # format Product to match file
  dist_data$Product = toupper(dist_data$Product)

  # trim white space
  dist_data[, Product:= trimws(Product)]
  dist_data[, Product := gsub('Á','A', Product)]
  dist_data[, Product := gsub('É','E', Product)]
  dist_data[, Product := gsub('Í','I', Product)]
  dist_data[, Product := gsub('Ó','O', Product)]
  dist_data[, Product := gsub('Ñ','N', Product)]

  dist_data[, Product:=gsub('CICLOCERINA','CICLOSERINA',Product)]
  
  # Fix mispellings
  dist_data[Product == 'RIFAMPICINA, CAPSULA DE 300 MG.', Product:='RIFAMPICINA, TABLETA DE 300 MG.']
  dist_data[Product == 'RIFAMPICINA 100MG/5ML SUSPENSION FRASCO 120ML' , Product := 'RIFAMPICINA SUSPENSION 100MG/5ML., FRASCO DE 120 ML.']
  dist_data[Product == 'ISONIAZIDA, TAB LETA DE 300 MG.', Product:='ISONIAZIDA, TABLETA DE 300 MG.']
  dist_data[Product == 'PIRAZINAMIDA, TABLET DE 500 MG.', Product:='PIRAZINAMIDA, TABLETA DE 500 MG.']
  dist_data[Product == 'ESTREPTOMICINA, VIAL/FRSCO DE 1 GRAMO', Product:= 'ESTREPTOMICINA, VIAL/FRASCO DE 1 GRAMO']
  dist_data[Product == 'ESTREPTOMICINA, VIAL/FRSCO DE 1 GRAMO', Product:= 'ESTREPTOMICINA, VIAL/FRASCO DE 1 GRAMO']
  dist_data[Product == 'AMOXICILINA/ACIDO CLOVULANICO, COMPRIMIDO DE 875/125 MG.', Product:= 'AMOXICILINA/ACIDO CLAVULANICO, COMPRIMIDO DE 875MG/125MG.']
  dist_data[Product == 'AMOXICILINA 875MG + ACIDO CLAVULANICO 125 MG TABLETA', Product:= 'AMOXICILINA/ACIDO CLAVULANICO, COMPRIMIDO DE 875MG/125MG.']
  dist_data[Product == 'CAPREOMICINA VIAL 1 G POLVO PARA INYECTABLE', Product:= 'CAPREOMICINA VIAL 1 G']
  dist_data[Product == 'CICLOSERINA, CAPSULAS DE 250 MG.', Product:= 'CICLOSERINA, CAPSULA DE 250 MG.']
  dist_data[Product == 'CLOFAZIMINE 50 MG., FRASCO DE 100 CAPSULAS', Product:= 'CLOFAZIMINE, CAPSULA DE 50 MG.']
  dist_data[Product == 'CLOFAZIMINE DE 50 MG., FRASCO DE 100 CAPSULAS', Product:= 'CLOFAZIMINE, CAPSULA DE 50 MG.']
  dist_data[Product == 'ESTREPTOMICINA, VIAL/FCO DE 1 G.', Product:= 'ESTREPTOMICINA, VIAL/FRASCO DE 1 GRAMO']
  dist_data[Product == 'IMIPENEM 500MG+ CILASTATIN 500MG VIALES', Product:= 'IMIPENEM/CILASTATINA 1 GRAMOS']
  dist_data[Product == 'KANAMICINA, VIAL/FCO DE 1 GRAMO', Product:= 'KANAMICINA, VIAL DE 1 GRAMO']
  dist_data[Product == 'KANAMICINA, VIAL/FCO DE 1 GRAMO SOLUCION INYECTABLE', Product:= 'ANAMICINA, VIAL DE 1 GRAMO']
  dist_data[Product == 'LINEZOLID 600 MG TABLETAS', Product:= 'LINEZOLID 600 MG TABLETA']
  dist_data[Product == 'MOXIFLOXACINA 400 MG TABLETA', Product:= 'MOXIFLOXACINA, TABLETA DE 400 MG.']
  dist_data[Product == 'RIFAMPICINA CAPSULA 300 MG', Product:= 'RIFAMPICINA, TABLETA DE 300 MG.']
  dist_data[Product == 'RIFAMPICINA SUSPENSION', Product:= 'RIFAMPICINA SUSPENSION 100MG/5ML., FRASCO DE 120 ML.']
  dist_data[Product == 'RIFAMPICINA SUSPENSION, 100MG/5ML., FCO DE 120 ML.', Product:= 'RIFAMPICINA SUSPENSION 100MG/5ML., FRASCO DE 120 ML.']
  dist_data[Product == 'RIFAMPICINA SUSPENSION, 100MG/5ML., FRASCO DE 120 ML.', Product:= 'RIFAMPICINA SUSPENSION 100MG/5ML., FRASCO DE 120 ML.']
  dist_data[Product == 'TUBERCULINA VIAL PPD 2 T.U 1 ML.', Product:= 'TUBERCULINA, VIAL PPD 2 T.U. 1 ML.']
  dist_data[Product == 'TUBERCULINA VIAL PPD 2 T.U 1.5 ML.', Product:= 'TUBERCULINA, VIAL PPD 2 T.U. 1.5 ML.']
  dist_data[Product == 'TUBERCULINA, VIAL/FCO DE 1 ML. (10 DOSIS).', Product:= 'TUBERCULINA, VIAL PPD 5 T.U. 1 ML.']
  dist_data[Product == 'PAS ACID DE 4G SOBRES', Product:= 'ACIDO PARAAMINOSALICILICO SOBRE DE 4 GRAMOS']

  
  
  
  # Find weighted average based on total cost
  dist_data <- 
    dist_data %>% 
    group_by(Product) %>% 
    summarise(weighted_unit_cost = weighted.mean(unit_cost, total_month))
  
  dist_data$Year = as.numeric(substring(start_year, 5, 8))
  # Remove invalid columns, add start_year
  dist_data$weighted_unit_cost = as.numeric(dist_data$weighted_unit_cost)
  dist_data = na.omit(dist_data)

  
  return (dist_data)
  
}

### FUNCTION TO CLEAN THE DST DATA ###
cleanDST = function (tbdistr){
  
  # trim white space
  tbdistr[, Product:= trimws(Product)]
  
  # Fix mispellings
  tbdistr[Product == 'RIFAMPICINA, CAPSULA DE 300 MG.', Product:='RIFAMPICINA, TABLETA DE 300 MG.']
  tbdistr[Product == 'RIFAMPICINA 100MG/5ML SUSPENSION FRASCO 120ML' , Product := 'RIFAMPICINA SUSPENSION 100MG/5ML., FRASCO DE 120 ML.']
  tbdistr[Product == 'ISONIAZIDA, TAB LETA DE 300 MG.', Product:='ISONIAZIDA, TABLETA DE 300 MG.']
  tbdistr[Product == 'PIRAZINAMIDA, TABLET DE 500 MG.', Product:='PIRAZINAMIDA, TABLETA DE 500 MG.']
  tbdistr[Product == 'ESTREPTOMICINA, VIAL/FRSCO DE 1 GRAMO', Product:= 'ESTREPTOMICINA, VIAL/FRASCO DE 1 GRAMO']
  tbdistr[Product == 'ESTREPTOMICINA, VIAL/FRSCO DE 1 GRAMO', Product:= 'ESTREPTOMICINA, VIAL/FRASCO DE 1 GRAMO']
  tbdistr[, Product:=gsub('CICLOCERINA','CICLOSERINA',Product)]
  tbdistr[, Medicine:=gsub('CICLOCERINA','CICLOSERINA',Medicine)]
  
  # fix alternate name of pharma company
  tbdistr[, Supplier:=gsub(', S.A.', '', Supplier)]
  return(tbdistr)
}

# ----------------------------------------------
###### source the functions that we need 
# ----------------------------------------------
dist_dir = "J:/Project/Evaluation/GF/outcome_measurement/gtm/"
cost_dir = paste0(dist_dir, "distribution/")
file_list <- read.csv(paste0(dist_dir, "tb_unitcost_distrub.csv"), na.strings=c("","NA"),
                      stringsAsFactors = FALSE) 
tbdistr = cleanDST(fread(paste0(dist_dir,"prepped_data/GTM-TB-distribution-2013-2018.csv")))

for(i in 1:length(file_list$file_name)){ 
  if(file_list$disease[i]=="tb"){
    tmpData <- prep_unit_cost(cost_dir, file_list$file_name[i], as.character(file_list$sheet[i]), file_list$start_date[i])
  }
  if(i==1){
    cost_database = tmpData 
  } 
  if(i>1){
    cost_database = rbind(cost_database, tmpData, use.names=TRUE)
  }
  print(i)
}


# Convert from Quitzal to USD
conversion_table = data.table("Year" = c("2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"), 
                              "conversion" = c(7.74022,	7.5532,	7.3301,	7.45875,	7.42153,	8.01039,	7.92282,	7.64965,	7.68406,	7.71407,	7.59794,	7.49704,	7.43533,	7.18309,	7.31697))

cost_database = merge(cost_database, conversion_table, by = "Year", allow.cartesian = TRUE)
cost_database$weighted_unit_cost = cost_database$weighted_unit_cost / cost_database$conversion
cost_database$conversion = NULL

# test1 = unique(tbdistr[,c('Product','Year')])
# test2 = unique(cost_database[,c('Product','Year')])
# test1$source = "db cost"
# test2$source = "cost_unit"
# test = rbind(test1, test2)
# 
# write.csv(test, "J:/temp/ninip/yearNAmes.csv")

# Merge tbdistr data with unit cost by year and product
total_db = merge(tbdistr, cost_database, by = c("Product", "Year"), all.x = TRUE, all.y = FALSE)
total_db$weighted_unit_cost[is.na(total_db$weighted_unit_cost)]<-0 
#total_db$weighted_unit_cost = ifelse(total_db$Amount > 0 & total_db$weighted_unit_cost == 0, )