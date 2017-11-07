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

dir <- 'J:/Project/Evaluation/GF/resource_tracking/gtm/gf/'
inFile <- 'GUA-M-MSPAS_SB_Y4-6a_IL8'
start_quarter <- ymd("2014-06-01")
period <- 90
qtr_number <- 10

col_names <- rep(0, qtr_number+1)
for(i in 1: length(col_names)){
  if (i==1){
    col_names[i] <- "category"
  } else {
  col_names[i] <- paste("Q", i, sep="")
  }
}

file_list <- read.csv("")

##pull all rows from between columns that have "FONDO MUNDIAL" in them 

ghe_data <- data.table(read_excel(paste0(dir, inFile, '.xlsx'), sheet="Summary Budget revised"))

colnames(ghe_data)[1] <- "first_column"


ghe_data <- ghe_data[c(grep("Category", ghe_data$X__1):(grep(13, ghe_data$first_column))),]
# drop first row 
ghe_data = ghe_data[-1,]
# drop 1st column 
ghe_data[[1]] <- NULL  

ghe_data$X__1[1] <- "category"

##we only want the data for each quarter, so remove extraneous values: 
toMatch <- c("Year", "RCC", "%", "TOTAL*")
ghe_data <- Filter(function(x) !any(grepl(paste(toMatch, collapse="|"), x)), ghe_data)

## finally, drop columns containing only NA's
ghe_data<- Filter(function(x)!all(is.na(x)), ghe_data)


## drop the first and last row since we only want the cost categories
ghe_data <- ghe_data[-nrow(ghe_data),]

ghe_data <- ghe_data[-1,]

colnames(ghe_data) <- col_names

## invert the dataset so that budget expenses and quarters are grouped by category
setDT(ghe_data)
melted<- melt(ghe_data,id="category")

##create quarter start dates based on value 

dates <- rep(start_quarter, length(qtr_vector))

for (i in 1:length(qtr_vector)){
  if (i==1){
    dates[i] <- dates[i]
  } else {
    dates[i] <- dates[i-1]%m+% months(3)
  }
}
##turn the list of dates into a dictionary: 
names(dates) <- qtr_vector

## now 
kDT = data.table(variable = names(dates), value = TRUE, v = unname(dates))
melted1 <-melted[kDT, on=.(variable), v := i.v ]

melted1$period <- period

