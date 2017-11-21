# ----------------------------------------------
# Irena Chen
#
# 11/16/2017
# Make preliminary graphs of sicoin data 

## the cleaned up version of this code can be found in gtm_budget_graphs
# ----------------------------------------------
# Set up R

library(ggplot2)
library(dplyr)
library(data.table)

# make end date
sicoin_data[, end_date:=start_date + period-1]
fpm_budget[,end_date:=start_date+period-1]
# ----------------------------------------------

# Get gf and ghe data by disease type at the national level 

byVars = names(sicoin_data)[!names(sicoin_data)%in%c('budget','disbursement','expenditure','cost_category','loc_id')]
nat_level = sicoin_data[, list(budget=sum(budget), disbursement=sum(disbursement), expenditure=sum(expenditure)), by=byVars]



tmp = copy(nat_level)
tmp$start_date = NULL
setnames(tmp, 'end_date', 'start_date')
nat_level$end_date = NULL
nat_level = rbind(nat_level, tmp)


nat_level = melt(nat_level, id.vars=c('source', 'start_date', 'period', 'disease', 'file_origin'))
nat_level$value[nat_level$value == 0] <- NA



ggplot(nat_level, aes(x = start_date, y= value/1000000)) + 
  geom_line(aes(color = source, linetype=variable)) +
  facet_wrap(~disease) +
  labs(x = "Start Date", y = "$$ in mil")


# ----------------------------------------------


##map the program activity to the codes: 
sicoin_mapping_test <- data.table(read.csv('J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/sicoin_test_mapping.csv'))

sicoin_mapped  <- merge(sicoin_data, sicoin_mapping_test, by=.EACHI, allow.cartesian=TRUE)


mapped_sicoin$budget <- mapped_sicoin$budget*mapped_sicoin$coeff
mapped_sicoin$disbursement <- mapped_sicoin$disbursement *mapped_sicoin$coeff
mapped_sicoin$expenditure <- mapped_sicoin$expenditure *mapped_sicoin$coeff



# collapse cost categories
byVars = names(fpm_mapped)[!names(fpm_mapped)%in%c('budget','disbursement','expenditure', 'loc_id')]
fpm_cats = fpm_mapped[, list(budget=sum(budget), disbursement=sum(disbursement), expenditure=sum(expenditure)), by=byVars]


# "melt" long
tmp = copy(fpm_cats)
tmp$start_date = NULL
setnames(tmp, 'end_date', 'start_date')
fpm_cats$end_date = NULL
fpm_cats = rbind(fpm_cats, tmp)


fpm_nat_level = melt(fpm_cats, id.vars=c('source', "cost_category", "period", "start_date", "data_source", "disease", "code", "coeff"))
fpm_nat_level$value[fpm_nat_level$value == 0] <- NA


no_fpm_cats <- copy(fpm_nat_level)
no_fpm_cats$cost_category <- NULL

ggplot(no_fpm_cats, aes(x = start_date, y= value/1000000)) + 
  geom_line(aes(color=disease, line=disease)) +
  # facet_wrap(~disease) +
  ggtitle("FPM Budget Data") +
  #ylim(0, 9) + 
  labs(x = "Start Date", y = "$$ in mil")


# ----------------------------------------------

mapped_sicoin$coeff <- NULL
mapped_sicoin$file_origin <- NULL

# collapse cost categories
byVars = names(mapped_sicoin)[!names(mapped_sicoin)%in%c('budget','disbursement','expenditure','cost_category')]
muni_level = mapped_sicoin[, list(budget=sum(budget), disbursement=sum(disbursement), expenditure=sum(expenditure)), by=byVars]


# "melt" long
tmp = copy(muni_level)
tmp$start_date = NULL
setnames(tmp, 'end_date', 'start_date')
muni_level$end_date = NULL
muni_level = rbind(muni_level, tmp)

muni_melt = melt(muni_level, id.vars=c('loc_id', 'source', 'start_date', 'period', 'disease', 'code'))

muni_melted <- muni_melt[-grep(paste(c("gtm", "GUAT"),  collapse="|"), muni_melt$loc_id),]

muni_melted <- muni_melted[, list(start_date, variable, value),by="loc_id"]
muni_melted$value[muni_melted$value == 0] <- NA

muni_mapping <- cbind(unique(levels(muni_melted$loc_id)), as.numeric(1:length(unique(levels((muni_melted$loc_id))))))

colnames(muni_mapping) <- c("loc_id", "muni_code")

muni_merge <- merge(muni_melted, muni_mapping, by="loc_id")
muni_merge$muni_code <- as.numeric(muni_merge$muni_code)

for (i in 1:6){
  subdata <- subset(muni_merge, muni_code%%6==i)
  print(ggplot(subdata, aes(x = start_date, y = value/100000)) + 
  geom_line(aes(color=variable)) +
  facet_wrap(~loc_id) +
  geom_point() +
  ggtitle("Resource by Municipality")+
  labs(x = "Start Date", y = "$$ in 100ks"))
}
dev.off()


for (k in unique(muni_melted$loc_id)){
  subdata <- subset(muni_melted, loc_id == k)
  print(ggplot(subdata, aes(x = start_date, y = value/100000)) + 
  geom_line(aes(color=variable)) +
  geom_point() +
  ggtitle(k)+
  labs(x = "Start Date", y = "$$ in 100ks"))
}











print(ggplot(subdata, aes(x = start_date, y = value)) +
        geom_line(aes(color=variable, linetype=variable)) +
        geom_point() +
        ggtitle("MIXCO") +
  labs(x = "Start Date", y = "$$ in mil"))


ggplot(muni_melted, aes(x = start_date, y= value/100000)) + 
  geom_point() +
  geom_line(aes(color=variable, linetype=variable)) +
  facet_wrap(~loc_id) +
  labs(x = "Start Date", y = "$$ in 100k") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
