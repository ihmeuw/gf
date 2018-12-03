#----------------------------------------------
#AUTHOR: Emily Linebarger
#DATE: November 2018
#PURPOSE: Calculate average absorption of PEPFAR funding 
#--------------------------------------------------------

#Set up R
rm(list=ls())
library(data.table)

#Read in PEPFAR data; found at this link: https://data.pepfar.gov/country/funding?country=Uganda&year=2016 
pepfar_exp <- fread("C:/Users/davidp6/Downloads/Expenditures by Cost Category 2012-2016.csv")
pepfar_plan <- fread("C:/Users/davidp6/Downloads/Planned Funding 2004-2017.csv")

pepfar_plan[country == "Democratic Republic of the Congo", country:="DRC"]

pepfar_exp = pepfar_exp[, .(expenditure=sum(expenditures)), by=c('country','year')]
pepfar_plan = pepfar_plan[, .(budget=sum(amount)), by=c('country','year')]

data = merge(pepfar_exp, pepfar_plan, by=c('country','year'))

pceCountries = c('Cambodia','Central America Region','DRC','Uganda','Mozambique','Senedal','Sudan','Myanmar')
pceCountries = c('Central America Region','DRC','Uganda')
data = data[country %in% pceCountries]

data[, sum(expenditure)/sum(budget)]
