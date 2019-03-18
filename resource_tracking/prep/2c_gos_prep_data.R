
# ----------------------------------------------
# AUTHOR: Emily Linebarger, based on code written by Irena Chen.
# PURPOSE: Master file for prepping Grant Operating System (GOS) data
#           from the Global Fund. 
# DATE: Last updated February 2019. 
# ----------------------------------------------

# TO DO
# fix time series graph so that there are gaps where appropriate (use `group` aesthetic)
# ----------------------------------------------
# Load the GOS tab from the Excel book  
# ----------------------------------------------

gos_data  <- data.table(read_excel(paste0(gos_raw, 'Expenditures from GMS and GOS for PCE IHME countries.xlsx'),
                                   sheet=as.character('GOS Mod-Interv - Extract')))

## reset column names
oldNames <- names(gos_data)
newNames <-  c("country","grant", "year","start_date","end_date","module","intervention", 
               "budget", "expenditure", "disease", "grant_period_start", "grant_period_end")

setnames(gos_data,oldNames, newNames)

#gos_sum <- gos_data[, sum(budget), by=.(year, country, disease)][order(country, disease, year)]

##subset the columns that we want 
gos_clean <- gos_data[, newNames, with=FALSE]

gos_clean$grant_period = paste0(year(as.Date(gos_clean$grant_period_start)), "-",year(as.Date(gos_clean$grant_period_end)))
gos_clean$grant_period_start = NULL
gos_clean$grant_period_end = NULL

# ----------------------------------------------
# Load the GMS tab from the Excel book  # Need to rework this as we're thinking about NLP. 
# ----------------------------------------------
gms_data  <- data.table(read_excel(paste0(gos_raw, 'Expenditures from GMS and GOS for PCE IHME countries.xlsx'),
                                   sheet=as.character('GMS SDAs - extract')))

##repeat the subsetting that we did above (grabbing only the columns we want)
gmsOld <- c(oldNames[1:5], "Service Delivery Area", "Total Budget Amount (USD equ)", "Total Expenditure Amount (USD equ)", "Component", "Program Start Date", "Program End Date")
gmsNew <- c(newNames[1:6], newNames[8:10], "grant_period_start", "grant_period_end")
setnames(gms_data, gmsOld, gmsNew)
gms_clean <- gms_data[, gmsNew, with=FALSE]

gms_clean$grant_period = paste0(year(as.Date(gms_clean$grant_period_start)), "-",year(as.Date(gms_clean$grant_period_end)))
gms_clean$grant_period_start = NULL
gms_clean$grant_period_end = NULL

##combine both GOS and GMS datasets into one dataset
totalGos <- rbind(gms_clean, gos_clean, fill = TRUE)

# ----------------------------------------------
###### Load the GMS tab from the Excel book  ###### 
# ----------------------------------------------
map_disease <- unique(totalGos$disease)
names(map_disease) <- c("tb", "malaria", "hiv", "hss", "hiv/tb")
  
kDT = data.table(map_disease = names(map_disease), value = TRUE, disease = unname(map_disease))
totalGos[kDT, on=.(disease), disease := i.map_disease]

totalGos$data_source <- "gos"
totalGos$file_name = "Expenditures from GMS and GOS for PCE IHME countries.xlsx"


totalGos[is.na(module), module:='unspecified']
totalGos[is.na(intervention), intervention:='unspecified']

