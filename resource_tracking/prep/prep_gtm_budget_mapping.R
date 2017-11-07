
##create quarter start dates based on value 

map_quarters = function(tmpData, start_date,qtr_num, loc_id, period, disease, source){
  
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

budget_dataset$qtr <- NULL
budget_dataset$loc_id <- loc_id
budget_dataset$period <- period
budget_dataset$disease <- disease 
budget_dataset$source <- source

return(budget_dataset)
} 

