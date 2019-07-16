# Francisco Rios Casas
# Absorption in Senegal in 2018

# set up
library(data.table)

# read in data 
absorption <- readRDS("C:/Users/frc2/Documents/data/finances/absorption_sen.rds")

# calculate absorption in Senegal (TB/RSSH Grant)
absorption = absorption[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('grant', 'semester')] # Collapse and recalculate absorption 
absorption[, absorption:=(expenditure/budget)*100]
absorption = absorption[order(gf_module, gf_intervention, semester)] #Order your dataset nicely
absorption[order(-absorption)]


# calculate absorption in Senegal (TB/RSSH Grant)
absorption = absorption[grant=="SEN-Z-MOH"]
absorption = absorption[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('grant')] # Collapse and recalculate absorption 
absorption[, absorption:=(expenditure/budget)*100]
absorption = absorption[order(gf_module, gf_intervention, semester)] #Order your dataset nicely
absorption[order(-absorption)]

# 