library(ggplot2)
library(dplyr)

# make end date
sicoin_data[, end_date:=start_date + period - 1]

# collapse cost categories
byVars = names(sicoin_data)[!names(sicoin_data)%in%c('budget','disbursement','expenditures','cost_category')]
muni_level = sicoin_data[, list(budget=sum(budget), disbursement=sum(disbursement), expenditures=sum(expenditures)), by=byVars]


# "melt" long
tmp = copy(muni_level)
tmp$start_date = NULL
setnames(tmp, 'end_date', 'start_date')
muni_level$end_date = NULL
muni_level = rbind(muni_level, tmp)

muni_melt = melt(muni_level, id.vars=c('loc_id', 'source', 'start_date', 'period', 'disease', 'file_origin'))

muni_melted <- muni_melt[-grep(paste(c("gtm", "GUAT"),  collapse="|"), muni_melt$loc_id),]

muni_grouped <- muni_melted[, list(start_date, variable, value),by="loc_id"]

for (k in unique(muni_melted$loc_id)){
  subdata <- subset(muni_melted, loc_id == k)
  print(ggplot(subdata, aes(x = start_date, y = value, group=1)) + 
  geom_line(aes(color=variable)) +
  geom_point() +
  ggtitle(k)+
  labs(x = "Start Date", y = "$$ in mil") +
  expand_limits(y=0))
}

print(ggplot(subdata, aes(x = start_date, y = value)) +
        geom_line(aes(color=variable, linetype=variable)) +
        geom_point() +
        ggtitle("MIXCO") +
  labs(x = "Start Date", y = "$$ in mil"))


ggplot(muni_melted, aes(x = start_date, y= value/1000000)) + 
  geom_point() +
  geom_line(aes(color=variable, linetype=variable)) +
  facet_wrap(~loc_id) +
  labs(x = "Start Date", y = "$$ in mil") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


# "melt" long


byVars = names(sicoin_data)[!names(sicoin_data)%in%c('budget','disbursement','expenditures','cost_category','loc_id')]
nat_level = sicoin_data[, list(budget=sum(budget), disbursement=sum(disbursement), expenditures=sum(expenditures)), by=byVars]



tmp = copy(nat_level)
tmp$start_date = NULL
setnames(tmp, 'end_date', 'start_date')
nat_level$end_date = NULL
nat_level = rbind(nat_level, tmp)

nat_level = melt(nat_level, id.vars=c('source', 'start_date', 'period', 'disease', 'file_origin'))

ggplot(nat_level, aes(x = start_date, y= value/1000000)) + 
geom_point() +
geom_line(aes(color = source, linetype=variable)) +
facet_wrap(~disease) +
labs(x = "Start Date", y = "$$ in mil")



nat_level$value[nat_level$value == 0] <- NA

