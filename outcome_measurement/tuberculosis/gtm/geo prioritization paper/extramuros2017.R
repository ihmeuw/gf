#####################################################
# Extramuros analysis for geo prioritization paper
# J Ross
# February 2019
####################################################

rm(list=ls())
library(data.table)
library(ggplot2)

indir <- ("H:/PCE analysis/GTM TB prioritization/data/")
outdir <- ("H:/PCE analysis/GTM TB prioritization/")

dt <- fread(paste0(indir, "Extramuros2017.csv"))
colnames(dt) <- c("department", "muni", "pas_sr", "ext_sr", "contrib_sr", "pas_u5", "ext_u5", "contrib_u5", "pas_bacter", "ext_bacter", "contrib_bacter", "year")
dt<-na.omit(dt, cols=c("year"))

#For results. Show ranges by municipality. Sum by department and overall
summary(dt$ext_sr)
summary(dt$ext_bacter)
summary(dt$ext_u5)

tots_dept <- dt[, lapply(.SD, sum, na.rm=TRUE), by=department, .SDcols=c("pas_sr", "ext_sr", "pas_u5", "ext_u5", "pas_bacter", "ext_bacter")]

tots  <- dt[, lapply(.SD, sum, na.rm=TRUE), .SDcols=c("pas_sr", "ext_sr", "pas_u5", "ext_u5", "pas_bacter", "ext_bacter")]
tots[,tot_sr:=pas_sr+ext_sr]
tots[,tot_u5:=pas_u5+ext_u5]
tots[,tot_bacter:=pas_bacter+ext_bacter]
tots[,contrib_sr:=ext_sr/tot_sr*100]
tots[,contrib_u5:=ext_u5/tot_u5*100]
tots[,contrib_bacter:=ext_bacter/tot_bacter*100]

#Descriptive plots
#reshape long for plotting
long_dt <- melt(dt, measure.vars=c("pas_sr", "ext_sr", "contrib_sr", "pas_u5", "ext_u5", "contrib_u5", "pas_bacter", "ext_bacter", "contrib_bacter"),
                    variable.name = "measure", value.name = "value")

#Subset to bacteriologically confirmed for plots for analysis 2
long_bacter <- long_dt[measure=="pas_bacter"|measure=="ext_bacter",]
#Marker here to re-label
long_bacter[(measure=="pas_bacter"), measure:="All cases"]
long_bacter[(measure=="ext_bacter"), measure:="Outreach cases"]

p<-ggplot(long_bacter, aes(x=muni, y=value, fill=measure))+
  geom_bar (stat="identity", position = "stack")+
  theme_bw()+
  theme(legend.title=element_blank())+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  labs(x = "Municipality", y = "Cases", title = "Bacteriologically-confirmed TB cases identified by outreach versus passive surveillance")
  #+facet_grid(~ department) #This doesn't work with this structure, but could be tweaked
p
