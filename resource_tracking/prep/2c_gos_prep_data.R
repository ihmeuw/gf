
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

#This is now the archived file - reading it in for 
# gos_data_old  <- data.table(read.xlsx(paste0(gos_raw, 'Expenditures from GMS and GOS for PCE IHME countries.xlsx'),
#                                    sheet=as.character('GOS Mod-Interv - Extract'), detectDates=TRUE))

gos_data = data.table(read.xlsx(paste0(gos_raw, "By_Cost_Category_data .xlsx"), detectDates = TRUE))

## reset column names
oldNames <- names(gos_data)
newNames <- gsub("\\.", "_", oldNames)
newNames = tolower(newNames)

setnames(gos_data, oldNames, newNames)

setnames(gos_data, old=c('calendar_year', 'component_name', 'intervention_name', 'module_name', 'expenditure_startdate', 'expenditure_enddate', 'ip_name'),
         new = c('year', 'disease', 'intervention', 'module', 'start_date', 'end_date', 'grant'))

#Only keep the countries we care about 
gos_data = gos_data[country%in%c("Congo (Democratic Republic)", "Guatemala", "Uganda", "Senegal")]

#Standardize disease column 
gos_data[, disease:=tolower(disease)]
gos_data[disease == "hiv/aids", disease:="hiv"]
gos_data[disease == "tuberculosis", disease:="tb"]

#--------------------------------------------------------------------------
#What does the 'expenditure_aggregation_type' column mean?? 
# NEED TO REVIEW THIS
unique(gos_data$expenditure_aggregation_type)
gos_data = gos_data[, -c("cost_category", "expenditure_aggregation_type", "implementing_entity")]
#--------------------------------------------------------------------------

#Standardize 'budget' and 'expenditure' columns, and melt. 
gos_data[measure_names == "Prorated Cumulative Budget USD Equ", measure_names:='budget']
gos_data[measure_names == "Prorated Cumulative Expenditure USD Equ", measure_names:="expenditure"]
gos_data = dcast(gos_data, year+country+disease+grant+start_date+end_date+module+intervention~measure_names, value.var ='measure_values', fun.aggregate = sum_na_rm)

gos_data = gos_data[order(country, disease, grant, start_date, end_date, year, module, intervention, budget, expenditure)]
sort(names(gos_data))

unique(gos_data$grant)
#Get rid of the P01, P02 etc. at the end of the string. 
substrEnd <- function(x, n){
  substr(x, 1, nchar(x)-n+1)
}
gos_data[, grant:=substrEnd(grant, 4)]
stopifnot(nrow(gos_data[is.na(year)])==0)

gos_data$file_name = "By_Cost_Category_data .xlsx"

# ----------------------------------------------
# Load the GMS tab from the Excel book  # Need to rework this as we're thinking about NLP. 
# ----------------------------------------------
gms_data  <- data.table(read.xlsx(paste0(gos_raw, 'Expenditures from GMS and GOS for PCE IHME countries.xlsx'),
                                   sheet=as.character('GMS SDAs - extract'), detectDates = TRUE))

##repeat the subsetting that we did above (grabbing only the columns we want)
gmsOld <- names(gms_data)
gmsNew <-  c("country","grant", "grant_period_start", "grant_period_end",
             "start_date","end_date", "year", "module","standard_sda", 
             "budget", "expenditure", "disease") #Would be good to change 'module' to 'service delivery area' some day!! 
setnames(gms_data, gmsOld, gmsNew) 

gms_data = gms_data[, -c('standard_sda')]

gms_data$grant_period = paste0(year(as.Date(gms_data$grant_period_start)), "-",year(as.Date(gms_data$grant_period_end)))
gms_data = gms_data[, -c('grant_period_start', 'grant_period_end')]
stopifnot(nrow(gms_data[is.na(year)])==0)

gms_data[, disease:=tolower(disease)]
<<<<<<< HEAD
gms_data[disease == "hiv/aids", disease:='hiv']
gms_data[disease == "health systems strengthening", disease:='rssh']
gms_data[disease == 'tuberculosis', disease:='tb']
=======
gms_data[disease=="hiv/aids", disease:='hiv']
gms_data[disease=='health systems strengthening', disease:='rssh']
gms_data[disease=='tuberculosis', disease:='tb']
>>>>>>> 67a1bf6baf04f125aa7a843fb5565ae3ab2e5ece

gms_data = gms_data[order(country, disease, grant, grant_period, start_date, end_date, year, module, budget, expenditure)]

gms_data$file_name = "Expenditures from GMS and GOS for PCE IHME countries.xlsx"
#-------------------------------------------------
# Compare two datasets to each other, and merge 
#-------------------------------------------------

range(gms_data$start_date)
range(gms_data$end_date)

#Want to keep the new data for as much as we have it for, and then back-fill with the old data. 
date_range = gos_data[, .(start_date = min(start_date)), by='grant']
for (i in 1:nrow(date_range)){
  drop = gms_data[(grant==date_range$grant[i]&start_date>date_range$start_date[i])]

  if (nrow(drop)!=0){
    print(paste0("Dropping rows based on start date, because we have new data for ",  date_range$grant[i], 
                 " from ", date_range$start_date[i]))
    print(drop[, .(grant, start_date, end_date)])
    gms_data = gms_data[!(grant==date_range$grant[i]&start_date>date_range$start_date[i])]
  }
  drop2 = gms_data[(grant==date_range$grant[i]&end_date>date_range$start_date[i])]
  if (nrow(drop2)!=0){
    print(paste0("Dropping rows based on start date, because we have new data for ",  date_range$grant[i], 
                 " from ", date_range$start_date[i]))
    print(drop2[, .(grant, start_date, end_date)])
    gms_data = gms_data[!(grant==date_range$grant[i]&end_date>date_range$start_date[i])]
  }
}

#Are we catching all grant names in this check? 
unique(gos_data[!grant%in%gms_data$grant, .(grant)])

##combine both GOS and GMS datasets into one dataset, and add final variables. 
sort(names(gms_data))
sort(names(gos_data))

#Review the start and end dates for these files in one last check. 
gos_dates = unique(gos_data[, .(mf_start = min(start_date)), by='grant'])
gms_dates = unique(gms_data[, .(sda_end =max(end_date)), by='grant'])
check_dates = merge(gos_dates, gms_dates, by='grant')

#Secondary check to make sure that all grants that don't merge aren't typos 
gms_dates[!grant%in%gos_dates$grant, .(grant)]
gos_dates[!grant%in%gms_dates$grant, .(grant)]


#Bind final datasets together
totalGos <- rbind(gms_data, gos_data, fill=TRUE)

totalGos$data_source <- "gos"

totalGos[is.na(module), module:='unspecified']
totalGos[is.na(intervention), intervention:='unspecified']
totalGos[module=='Not Defined', module:='unspecified']
totalGos[intervention=='Not Defined', intervention:='unspecified']

for (i in 1:nrow(code_lookup_tables)){
  totalGos[country==code_lookup_tables$country[[i]], loc_name:=code_lookup_tables$iso_code[[i]]]
}

totalGos[, start_date:=as.Date(start_date)]
totalGos[, end_date:=as.Date(end_date)]


