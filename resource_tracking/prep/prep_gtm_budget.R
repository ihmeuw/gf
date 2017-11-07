# ----------------------------------------------
# Irena Chen
#
# 11/2/2017
# Template for prepping budget data by cost category
# Inputs:
# inFile - name of the file to be prepped
# Outputs:
# budget_dataset - prepped data.table object
# ----------------------------------------------
library(lubridate)
library(data.table)
library(readxl)

dir <- 'J:/Project/Evaluation/GF/resource_tracking/gtm/gf/'
inFile <- 'GUA-610-G04-T_SB_Year_6_7_ENG'
start_quarter <- ymd("2012-08-01")
loc_id <- "gtm"
period <- 90
qtr_number <- 18
disease <- "malaria"
source <- "gf"
file_list <- read.csv("")


prep_gtm_budget = function(dir, inFile, start_quarter, qtr_number, loc_id, period, disease, source) {

col_names <- rep(0, qtr_number+1)
for(i in 1: length(col_names)){
  if (i==1){
    col_names[i] <- "category"
  } else {
  col_names[i] <- paste("Q", i-1, sep="")
  }
}


##pull all rows from between columns that have "FONDO MUNDIAL" in them 

ghe_data <- data.table(read_excel(paste0(dir, inFile, '.xls'), sheet="Budget summary GF"))

## the 1st column isn't always the same, so just call it something: 
colnames(ghe_data)[1] <- "first_column"

## this type of budget data should always have 13 cost categories 
ghe_data <- ghe_data[c(grep("Category", ghe_data$X__1):(grep(13, ghe_data$first_column))),]
# drop first row (it's blank)
ghe_data = ghe_data[-1,]

# drop 1st column (unnecessary)
ghe_data[[1]] <- NULL  

ghe_data$X__1[1] <- "category"


## also drop columns containing only NA's
ghe_data<- Filter(function(x) !all(is.na(x)), ghe_data)


##we only want the data for each quarter, so remove extraneous values: 
toMatch <- c("Year", "RCC", "%", "TOTAL*", "Phase", "X__")
ghe_data <- Filter(function(x) !any(grepl(paste(toMatch, collapse="|"), x)), ghe_data)

##hack to remove columns w/ NA value in 1st row  

colnames(ghe_data) <- as.character(ghe_data[1,])
drop.cols <- grep("X__", colnames(ghe_data))
ghe_data <- ghe_data[, (drop.cols) := NULL]

## rename the columns
colnames(ghe_data) <- col_names
## drop the first row now that we renamed the columns 
ghe_data <- ghe_data[-1,]

## invert the dataset so that budget expenses and quarters are grouped by category
setDT(ghe_data)
budget_dataset<- melt(ghe_data,id="category", variable.name = "qtr", value.name="budget")

##create quarter start dates based on value 

dates <- rep(start_quarter, length(col_names)-1) # -1 because col_names includes "category" as the first value

for (i in 1:length(dates)){
  if (i==1){
    dates[i] <- dates[i]
  } else {
    dates[i] <- dates[i-1]%m+% months(3)
  }
}
##turn the list of dates into a dictionary (but only for quarters!) : 
names(dates) <- col_names[-1]

## now match quarters with start dates 
kDT = data.table(qtr = names(dates), value = TRUE, start_date = unname(dates))
budget_dataset <-budget_dataset[kDT, on=.(qtr), start_date := i.start_date ]

budget_dataset$period <- period
budget_dataset$disease <- disease 

return(budget_dataset)
}
