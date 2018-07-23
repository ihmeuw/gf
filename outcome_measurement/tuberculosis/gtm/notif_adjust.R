library(data.table)

data = data.table(muni=c('a','b','c','d'), cases=c(200,240,160,210), tx_seeking=c(.99,.8,.6,.5))
total=1000

adjuster = function(beta, data, total) { 
  adjustedCases = data$cases/(data$tx_seeking*beta*(1-data$tx_seeking))
  return(sqrt((sum(adjustedCases)-total)^2))
}

est = optim(.1, adjuster, gr=NULL, data, total)

adjustedCases =  data$cases/(data$tx_seeking*est$par*(1-data$tx_seeking))