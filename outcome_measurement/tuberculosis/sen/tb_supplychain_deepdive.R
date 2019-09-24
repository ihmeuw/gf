# plotting stock-outs and commodities

# set-up
library(data.table)
library(ggplot2)

DT <- readRDS("J:/Project/Evaluation/GF/impact_evaluation/sen/prepped_data/prepped_outcomes_data.RDS")

# get stockout data
DT1 <- DT[,.(region, centre, type, annee, trimestre, struct_tbmedprem_pc, struct_tbstrepto_pc, dur_rupt1, dur_rupt2, dur_rupt3, exstock1, exstock2)]

# add date variable
DT1 = DT1[, date:=annee+((trimestre-1)/4)]

# plot by region and year
# VALUES WHICH MUST BE AVERAGED (percents or rates)
DT2 = DT1[, lapply(.SD, mean, na.rm=TRUE), by=c('region','annee'), .SDcols=c('struct_tbmedprem_pc')]

DT3 = melt(DT2, id.var=c('region', 'annee'))

# melt data long
ggplot(data=DT3, aes(x=annee, y=value, col=region)) +
  geom_line()
