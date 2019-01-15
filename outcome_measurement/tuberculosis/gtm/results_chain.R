library(data.table)
library(ggplot2)


rm(list=ls())
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j') # have to declare the J Drive differently on the cluster 
out_dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/prepped_data/')
gbd_dir <- paste0(j, 'Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/gbd_2017/')
who_dir <- paste0(j, 'Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/WHO/')
notif<-fread("C:/Users/jross3/Desktop/GTM - TB notifications 2012-Jun2018.csv")
head(notif)

notif<-notif[,c("MUNICIPIO","DEPARTAMENTO", "SEXO", "EDAD", "CONDICIONINGRESO", "CLASIFICACION", "YEAR", "VIH", "METODODX", "GENEXPERT")]
notif<-notif[notif$CONDICIONINGRESO=="nuevo"|notif$CONDICIONINGRESO=="recaida",]

table(notif$GENEXPERT)
#Looks like only about 300 results have GENEXPERT filled in, so using METODODX instead

table(notif$METODODX, exclude = NULL)
notif$res_det<-0
notif$res_det[is.na(notif$METODODX)]<-NA

#These are all of the labels where rifampin resistance would be detected. This includes "PCR", which is not a very specific term, but likely refers to Xpert.
notif$res_det[notif$METODODX=="GENXPERT"|notif$METODODX=="MONORESISTENTE"|
                notif$METODODX=="BACILOSCOPIA/GENEXPERT"|notif$METODODX=="CULTIVO/GENEXPERT"|
                notif$METODODX=="G-XPERT/BACILOS."|notif$METODODX=="RAYOS X/GENEXPERT"|
                notif$METODODX=="GENEXPERT"|notif$METODODX=="BIOPSIA/GENEXPERT"|
                notif$METODODX=="GENEXPERTE"|notif$METODODX=="PCR/GENEXPERT"|
                notif$METODODX=="PCR"]<-1

table(notif$res_det, notif$YEAR, exclude = NULL)
res_yr<-table(notif$res_det, notif$YEAR, exclude = NULL)
prop.table(res_yr,2)

#If wanting to look at single year
#notif_2015<-notif[notif$YEAR==2015,]
#table(notif_2015$res_det)

#######Mortality graphic------------------------------------------------------------------------------------------

#Data management GBD. GBD estimates are in separate tables for TB and TB/HIV.
gbd_tb <- fread(paste0(gbd_dir, 'tb_mort_rates.csv'))
gbd_tb <- gbd_tb[sex_id==3]
cols <- c('year_id', 'val')
gbd_tb <- gbd_tb[, .SD, .SDcols=cols]
gbd_tb[,mort_rate:=val*100000]
gbd_tb[,source:='GBD 2017']
gbd_tb[,cause:='TB']
gbd_tb[,val:=NULL]
setcolorder(gbd_tb, c('year_id', 'cause', 'mort_rate', 'source'))


gbd_tbhiv <- fread(paste0(gbd_dir, 'hiv_tb_mort_rates.csv'))
gbd_tbhiv <- gbd_tbhiv[sex_id==3]
gbd_tbhiv_tot = gbd_tbhiv[, list(mort_rate=sum(na.omit(val))), by='year_id']#HIV/TB comes in with the drug resistance categories separately
gbd_tbhiv_tot[,mort_rate:=mort_rate*100000]
gbd_tbhiv_tot[,source:='GBD 2017']
gbd_tbhiv_tot[,cause:='TB/HIV']
setcolorder(gbd_tbhiv_tot, c('year_id', 'cause', 'mort_rate', 'source'))


#Data management for WHO data
who <- fread(paste0(who_dir, 'WHO_gtm_burden_2018.csv'))
cols <- c('year','e_mort_100k', 'e_mort_exc_tbhiv_100k', 'e_mort_tbhiv_100k')
who <- who[, .SD, .SDcols = cols]
colnames(newprice)<- c("premium","change","newprice")
colnames(who) <- c("year_id", 'All Forms', 'TB', 'TB/HIV')

long_who <- melt(who, measure.vars=c('All Forms', 'TB', 'TB/HIV'),
                    variable.name = "cause", value.name = "mort_rate")
long_who[,source:='WHO 2018']
  
#Cohort and VR (raw and re-distributed)
mort <- fread(paste0(out_dir,'mort_vr_cohort.csv'))
mort[,V1:=NULL]

#Bind it all together
table_list <- list(mort, long_who, gbd_tb, gbd_tbhiv_tot)
mort_all<-rbindlist(table_list)
mort_simple<-mort_all[cause!='All Forms'& source!='Redistributed VR']

write.csv(mort_simple, file=paste0(out_dir,'gtm_mortality_rate_compare.csv'))

p1 <- ggplot (data=mort_simple, aes(x=year_id, y=mort_rate, colour=source, linetype=cause))+
  geom_line(size=1)+
  theme_bw()+
  xlim(2007, 2017)+
  ylim(0,5)+
  labs(x="Year", y="Mortality rate", title="TB and TB/HIV mortality rates per 100,000 population in Guatemala", size=10)
p1
