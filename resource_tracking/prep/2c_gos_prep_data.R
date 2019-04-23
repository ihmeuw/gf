
# ----------------------------------------------
# AUTHOR: Emily Linebarger, based on code written by Irena Chen.
# PURPOSE: Master file for prepping Grant Operating System (GOS) data
#           from the Global Fund. 
# DATE: Last updated April 2019. 
# ----------------------------------------------

# TO DO
# fix time series graph so that there are gaps where appropriate (use `group` aesthetic)
#Note that we're renaming service delivery area as module for the old data - we should fix this.


# ----------------------------------------------
# Output files other than the essential ones defined in set_up_r.R
# ----------------------------------------------
checkFile = paste0(gos_raw, "Grants missing intervention information in new GOS file.csv")


# ----------------------------------------------
# Load the GOS tab from the Excel book  
# ----------------------------------------------

#This is now the archived file - reading it in for comparison. 
# gos_data_old  <- data.table(read.xlsx(paste0(gos_raw, 'Expenditures from GMS and GOS for PCE IHME countries.xlsx'),
#                                    sheet=as.character('GOS Mod-Interv - Extract'), detectDates=TRUE))

#GOS Data from 2015-2017
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

#Standardize 'budget' and 'expenditure' columns 
gos_data[measure_names == "Prorated Cumulative Budget USD Equ", measure_names:='budget']
gos_data[measure_names == "Prorated Cumulative Expenditure USD Equ", measure_names:="expenditure"]

#Investigate the 'expenditure_aggregation_type' category
check = gos_data[, sum(measure_values, na.rm=TRUE), by=c('grant','measure_names','expenditure_aggregation_type')]
check = dcast(check, grant+measure_names~expenditure_aggregation_type)
missing_intervention = check[Intervention==0]
write.csv(missing_intervention, checkFile, row.names=FALSE)

#Drop everything but "Intervention" aggregation column.
unique(gos_data[expenditure_aggregation_type=="Intervention", .(module, intervention)])
gos_data = gos_data[expenditure_aggregation_type=="Intervention"]

#Drop columns before reshape
gos_data = gos_data[, -c("cost_category", "implementing_entity", "expenditure_aggregation_type")]

#Standardize 'budget' and 'expenditure' columns, and melt. 
gos_data[measure_names == "Prorated Cumulative Budget USD Equ", measure_names:='budget']
gos_data[measure_names == "Prorated Cumulative Expenditure USD Equ", measure_names:="expenditure"]
gos_data = dcast(gos_data, year+country+disease+grant+start_date+end_date+module+intervention~measure_names, value.var ='measure_values', fun.aggregate = sum_na_rm)

gos_data = gos_data[order(country, disease, grant, start_date, end_date, year, module, intervention, budget, expenditure)]
sort(names(gos_data))

unique(gos_data$grant)
#Get rid of the P01, P02 etc. at the end of the string. #David - this is something else we should confirm with Sylvie! 
substrEnd <- function(x, n){
  substr(x, 1, nchar(x)-n+1)
}
gos_data[, grant:=substrEnd(grant, 4)]
stopifnot(nrow(gos_data[is.na(year)])==0)

gos_data$file_name = "By_Cost_Category_data .xlsx"

# ----------------------------------------------
# Load the GMS tab from the Excel book  
# ----------------------------------------------
# GOS Data from 2003-2016
gms_data  <- data.table(read.xlsx(paste0(gos_raw, 'Expenditures from GMS and GOS for PCE IHME countries.xlsx'),
                                   sheet=as.character('GMS SDAs - extract'), detectDates = TRUE))

##repeat the subsetting that we did above (grabbing only the columns we want)
gmsOld <- names(gms_data)
gmsNew <-  c("country","grant", "grant_period_start", "grant_period_end",
             "start_date","end_date", "year", "module","standard_sda", 
             "budget", "expenditure", "disease") #Would be good to change 'module' to 'service delivery area' some day!! 
setnames(gms_data, gmsOld, gmsNew) 

gms_data = gms_data[, -c('standard_sda')]

#Add grant period variable
gms_data$grant_period = paste0(year(as.Date(gms_data$grant_period_start)), "-",year(as.Date(gms_data$grant_period_end)))
gms_data = gms_data[, -c('grant_period_start', 'grant_period_end')]
stopifnot(nrow(gms_data[is.na(year)])==0)

#Relabel disease
gms_data[, disease:=tolower(disease)]
gms_data[disease == "hiv/aids", disease:='hiv']
gms_data[disease == "health systems strengthening", disease:='rssh']
gms_data[disease == 'tuberculosis', disease:='tb']

gms_data = gms_data[order(country, disease, grant, grant_period, start_date, end_date, year, module, budget, expenditure)]

gms_data$file_name = "Expenditures from GMS and GOS for PCE IHME countries.xlsx"

#-------------------------------------------------
# Compare two datasets to each other, and merge 
#-------------------------------------------------

range(gms_data$start_date)
range(gms_data$end_date)
range(gos_data$start_date)
range(gos_data$end_date)

#Want to keep the new data for as much as we have it for, and then back-fill with the old data. 
# What dates do we have the new GOS data for? 
date_range = gos_data[, .(start_date = min(start_date)), by='grant'] #What's the earliest start date for each grant?
for (i in 1:nrow(date_range)){
  #Want to drop rows in old data where the start date in the old data is after the earliest start date in the new data. 
  drop = gms_data[(grant==date_range$grant[i]&start_date>date_range$start_date[i])] #See what rows you'll be dropping. 

  if (nrow(drop)!=0){
    print(paste0("Dropping rows based on start date, because we have new data for ",  date_range$grant[i], 
                 " from ", date_range$start_date[i]))
    print(drop[, .(grant, start_date, end_date)])
    gms_data = gms_data[!(grant==date_range$grant[i]&start_date>date_range$start_date[i])]
  }
  drop2 = gms_data[(grant==date_range$grant[i]&end_date>date_range$start_date[i])] #Fencepost problem - had to rerun one more time if N=1
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

#Reformat modules/interventions for remapping
totalGos[is.na(module), module:='unspecified']
totalGos[is.na(intervention), intervention:='unspecified']
totalGos[module=='Not Defined', module:='unspecified']
totalGos[intervention=='Not Defined', intervention:='unspecified']

#Add in variable names 
totalGos$data_source <- "gos"
for (i in 1:nrow(code_lookup_tables)){
  totalGos[country==code_lookup_tables$country[[i]], loc_name:=code_lookup_tables$iso_code[[i]]]
}

totalGos[, start_date:=as.Date(start_date)]
totalGos[, end_date:=as.Date(end_date)]

#--------------------------------------------------------------------------------------------
# Split data into quarters 
# Because data is not evenly split into quarters (sometimes aggregated to 10 months, etc.)
# First split into months, then aggregate into quarters. 
# -------------------------------------------------------------------------------------------
#Calculate the total by grant as a check that the reshape worked, and keep track of your original start and end dates. 
pretest = totalGos[, .(pre_budget=sum(budget, na.rm=T)), by=c('grant')]
totalGos[, orig_start_date:=start_date]
totalGos[, orig_end_date:=end_date]

#Review some dates that will cause issues later on. 
totalGos[end_date=="2009-07-30", end_date:=as.Date("2009-07-31")] #There are actually 31 days in August. 
totalGos[end_date=="2010-01-01", end_date:=as.Date("2009-12-31")] #This actually ended on the 31st of December of 2009. 

#Generate the variables you need to split
totalGos[, end_date:=end_date+1] #Increment end date by one day, so you can grab the full month range it covers. 
#Find the difference in months, split by month, and then aggregate up to the quarter-level. 
totalGos[, months_reported:=(as.yearmon(end_date)-as.yearmon(start_date))*12]

#Expand data by the number of months, and then split 
nrow(totalGos)
totalGos[, split:=months_reported] #Generate this here, because you'll want to divide the budget by this number. 
totalGos[, sum(split)]
totalGos <- expandRows(totalGos, "months_reported")
nrow(totalGos) 

#Divide budget and expenditure by the months each date range represents
totalGos[, budget:=budget/split]
totalGos[, expenditure:=expenditure/split]

#Reformat date variable, and generate 'quarter' variable
byVars = colnames(totalGos)
totalGos[, seq:=sequence(.N), by=byVars] #But this indexes at 1, so...
totalGos[, seq:=seq-1] #Subtract 1 here. 

#Get the starting month and year variables, and then increment them. 
totalGos[, month:=month(start_date)]
totalGos[, year:=year(start_date)]

#While seq is not 0, go through the loop below.
#If seq is greater than or equal to 4, add 1 to year and divide everything by 4. Continue this loop while max(seq) > 4.
# If month + seq + 1 equals 12, than
totalGos[, new_month:=month+seq]
max_month = max(totalGos$new_month)
print(max_month)
while (max_month>12){
  totalGos[new_month>12, year:=year+1]
  totalGos[new_month>12, new_month:=new_month-12]
  max_month = max(totalGos$new_month)
  print(max_month)
}
#View(totalGos[1:300, .(start_date, end_date, seq, month, new_month, year)])

#Now, add a quarter variable. 
totalGos[new_month<4, quarter:=1]
totalGos[new_month>=4 & new_month<7, quarter:=2]
totalGos[new_month>=7 & new_month<10, quarter:=3]
totalGos[new_month>=10, quarter:=4]
unique(totalGos[, .(new_month, quarter)][order(quarter, new_month)])

#See if you have any duplicate quarters 
date_check = unique(totalGos[, .(quarter, year, grant, orig_start_date, orig_end_date)])
date_check = date_check[order(grant, year, quarter)]
date_check2 = date_check[, .(grant, year, quarter)]
date_check2 = date_check2[duplicated(date_check2)]

date_check = merge(date_check2, date_check, all.x=T, by=c('grant', 'year', 'quarter'))
write.xlsx(date_check, paste0(dir, "_gf_files_gos/gos/Overlaps within Intervention Category.xlsx"))
stopifnot(nrow(date_check)==0) #David please review this. 

#For places where we do have duplicate quarters, what's going on with budget/expenditure? 
overlap = unique(totalGos[, .(grant, year, quarter, orig_start_date, orig_end_date, budget, expenditure)])
overlap = merge(date_check2, overlap, by=c('grant', 'year', 'quarter'), all.x=T)
overlap = overlap[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('grant', 'year', 'quarter', 'orig_start_date', 'orig_end_date')]
overlap[, month_diff:=(as.yearmon(orig_end_date)-as.yearmon(orig_start_date))*12] #David please review this. 
overlap = overlap[order(grant, year, quarter, month_diff)] #Make the smaller date range always be on the top
overlap[, seq:=sequence(.N), by=c('grant', 'year', 'quarter')]

#Cast the data wide so you can compare budget/expenditure
overlap = dcast(overlap, grant+year+quarter~seq, value.var=c('budget', 'expenditure'), fun.aggregate=sum_na_rm) #Here, '1' represents the shorter time period, and '2' represents the longer time period
overlap[, budget_diff:=budget_2-budget_1]
overlap[, exp_diff:=expenditure_2-expenditure_1] #David please review this. 

#Check that the days within a month start and end at the same time. 
days = totalGos[, .(start_day=day(start_date), end_day=day(end_date), start_date, end_date)]
days_check = days[start_day!=end_day]
days_check = unique(days_check) #There are some dates that start in the middle of the month - do we just want to start on the first no matter what?  David. 

#Aggregate to the quarter level. 
totalGos_qtr = totalGos[, .(budget=sum(budget, na.rm=TRUE), expenditure=sum(expenditure, na.rm=TRUE)), by=c(
  'quarter', 'year', 'country', 'loc_name', 'grant', 'module', 'intervention', 'disease', 'grant_period', 'file_name', 'data_source')]

#Check that the days within a month start and end at the same time. 
days = totalGos[, .(start_day=day(start_date), end_day=day(end_date), start_date, end_date)]
days_check = days[start_day!=end_day]
days_check = unique(days_check) #There are some dates that start in the middle of the month - do we just want to start on the first no matter what?  David. 

#Make sure pre- and post-totals match 
posttest = totalGos_qtr[, .(post_budget=sum(budget, na.rm=T)), by=c('grant')]
totals_check = merge(pretest, posttest, by=c('grant'), all=T)
totals_error = totals_check[pre_budget!=post_budget]
stopifnot(nrow(totals_error)==0) #David we need to review these. 




