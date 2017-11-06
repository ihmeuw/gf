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
start_quarter <- ymd("2014-04-01")
period <- 90

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

colnames(ghe_data) <- as.character(unlist(ghe_data[1,]))

qtr_vector <- as.character(ghe_data[1])
qtr_vector <- qtr_vector[-1]

## drop the first and last row since we only want the cost categories
## I changed this so comment this out: ghe_data <- ghe_data[-nrow(ghe_data),]

ghe_data <- ghe_data[-1,]

## invert the dataset so that budget expenses and quarters are grouped by category
setDT(ghe_filter)
melted<- melt(ghe_filter,id="category")

##create quarter start dates based on value 

d <- rep(start_quarter, length(qtr_vector))

for (i in 1:length(qtr_vector)){
  if (i==1){
    d[i] <- d[i]
  } else {
    d[i] <- d[i-1]%m+% months(3)
  }
}
##turn the list of dates into a dictionary: 
names(d) <- qtr_vector

## now 
kDT = data.table(variable = names(d), value = TRUE, v = unname(d))
melted1 <-melted[kDT, on=.(variable), v := i.v ]

melted1$period <- period

