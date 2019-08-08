# Francisco Rios Casas
# Budget analyses for Senegal
# July 5 2019

# ------
# set-up r
library(data.table)
library(ggplot2)

# ------
# load data
sen <- fread("C:/Users/frc2/Documents/data/finances/final_budgets.csv")

# -----
# analysis for TB/RSSH Grant
# -----

# subset into the TB/RSSH grant only
sen = sen[grant=="SEN-Z-MOH"]

# How much has been spent on MDR-TB each year?
sen[gf_module=="Multidrug-resistant TB", .(budget=sum(budget, na.rm=T)), by=c('year')]

#How much of the budget is for TB vs. RSSH? 
sen[, .(budget=sum(budget, na.rm=T)), by=c('disease')]

#What is the breakdown of RSSH spending by module? 
sen[disease=="rssh", .(budget=sum(budget, na.rm=T)), by=c('gf_module')][order(-budget)]

#How much money is given to each implementer? 
sen[, .(budget=sum(budget, na.rm=T)), by=c('implementer')][order(-budget)]

#Sum of module by implementer
DT1 <- sen[, .(budget=sum(budget)), by=c('implementer', 'gf_module', 'year')][order(implementer, -budget)]

ggplot(data=DT1, aes(x=gf_module, y=budget)) +
  geom_bar(stat="identity") +
  facet_wrap(~year)


