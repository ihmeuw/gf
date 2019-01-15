

library(data.table)
library(ggplot2)

rm(list=ls())

notif<-fread("J:/Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/GTM - TB notifications 2012-2017 deidentified.csv")

table(notif$CONTACTOS)
table(notif$CLASIFICACION, useNA = "always")
table(notif$METODODX, useNA = "always")
table(notif[,c('METODODX','CLASIFICACION')], useNA = "always")
table(notif[,c('PACIENTEPRIVADOLIBERTAD', 'YEAR')], useNA = "always")


notif<-notif[,c("MUNICIPIO","DEPARTAMENTO", "SEXO", "EDAD", "CONDICIONINGRESO", "CLASIFICACION", "YEAR", "VIH")]

table(notif$YEAR)
table(notif$CONDICIONINGRESO)

#Subset to new or relapsed cases or missing classification
#Need data cleaning step to deal with the odd few numeric categories
new<-notif[notif$CONDICIONINGRESO=="nuevo"|notif$CONDICIONINGRESO=="recaida",]

table(new$YEAR)

new$count<-1
counter<-new[,c("YEAR","count")]
#counter$YEAR <-as.factor(counter$YEAR)
agg_years<-aggregate(counter, by=list(counter$YEAR), FUN=sum, na.rm=TRUE)

n_2012<-new[new$YEAR==2012,]
n_2013<-new[new$YEAR==2013,]

