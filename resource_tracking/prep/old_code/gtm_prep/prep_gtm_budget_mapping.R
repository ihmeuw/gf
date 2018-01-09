# ----------------------------------------------
# Irena Chen
#
# 11/2/2017
# Template for prepping GF budget data
# Inputs:
# inFile - name of the file to be prepped
# Outputs:
# budget_dataset - prepped data.table object
# ----------------------------------------------

map_quarters = function(tmpData, start_date, qtr_num, loc_id, period, disease, source, grant){
  
## create a vector of dates that correspond to the quarters in the budget files   
dates <- rep(start_date, qtr_num) # 
for (i in 1:length(dates)){
  if (i==1){
    dates[i] <- dates[i]
  } else {
    dates[i] <- dates[i-1]%m+% months(3)
  }
}


##turn the list of dates into a dictionary (but only for quarters!) : 
dates <- setNames(dates,unique(tmpData$qtr))

## now match quarters with start dates 
kDT = data.table(qtr = names(dates), value = TRUE, start_date = unname(dates))
budget_dataset <-tmpData[kDT, on=.(qtr), start_date := i.start_date ]

##add in other variables to the dataset 
budget_dataset$qtr <- NULL
budget_dataset$loc_id <- loc_id
budget_dataset$period <- period
budget_dataset$disease <- disease 
budget_dataset$source <- source
budget_dataset$grant_number <- grant

return(budget_dataset)
} 

