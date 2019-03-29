
# ----------------------------------------------
# AUTHOR: Emily Linebarger, based on code written by Irena Chen.
# PURPOSE: Master file for prepping Grant Operating System (GOS) data
#           from the Global Fund. 
# DATE: Last updated February 2019. 
# ----------------------------------------------

# TO DO
# fix time series graph so that there are gaps where appropriate (use `group` aesthetic)
#Note that we're renaming service delivery area as module for the old data - we should fix this.
# ----------------------------------------------
# Load the GOS tab from the Excel book  
# ----------------------------------------------

gos_data  <- data.table(read_excel(paste0(gos_raw, 'Expenditures from GMS and GOS for PCE IHME countries.xlsx'),
                                   sheet=as.character('GOS Mod-Interv - Extract')))
#Drop the columns you don't need 
gos_data = gos_data[, -c('Budget currency')]

## reset column names
oldNames <- names(gos_data)
newNames <-  c("country","grant", "grant_period_start", "grant_period_end",
               "start_date","end_date", "year", "module","intervention", 
               "budget", "expenditure", "disease")

setnames(gos_data, oldNames, newNames)

gos_data$grant_period = paste0(year(as.Date(gos_data$grant_period_start)), "-",year(as.Date(gos_data$grant_period_end)))
gos_data = gos_data[, -c('grant_period_start', 'grant_period_end')]

stopifnot(nrow(gos_data[is.na(year)])==0)

# ----------------------------------------------
# Load the GMS tab from the Excel book  # Need to rework this as we're thinking about NLP. 
# ----------------------------------------------
gms_data  <- data.table(read_excel(paste0(gos_raw, 'Expenditures from GMS and GOS for PCE IHME countries.xlsx'),
                                   sheet=as.character('GMS SDAs - extract')))

##repeat the subsetting that we did above (grabbing only the columns we want)
gmsOld <- names(gms_data)
gmsNew <-  c("country","grant", "grant_period_start", "grant_period_end",
             "start_date","end_date", "year", "module","standard_sda", 
             "budget", "expenditure", "disease")
setnames(gms_data, gmsOld, gmsNew) 

gms_data = gms_data[, -c('standard_sda')]

gms_data$grant_period = paste0(year(as.Date(gms_data$grant_period_start)), "-",year(as.Date(gms_data$grant_period_end)))
gms_data = gms_data[, -c('grant_period_start', 'grant_period_end')]
stopifnot(nrow(gms_data[is.na(year)])==0)

##combine both GOS and GMS datasets into one dataset, and add final variables. 
totalGos <- rbind(gms_data, gos_data, fill = TRUE)

totalGos$data_source <- "gos"
totalGos$file_name = "Expenditures from GMS and GOS for PCE IHME countries.xlsx"

totalGos[is.na(module), module:='unspecified']
totalGos[is.na(intervention), intervention:='unspecified']

for (i in 1:nrow(code_lookup_tables)){
  totalGos[country==code_lookup_tables$country[[i]], loc_name:=code_lookup_tables$iso_code[[i]]]
}

totalGos[, start_date:=as.Date(start_date)]
totalGos[, end_date:=as.Date(end_date)]
