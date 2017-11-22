# ----------------------------------------------
# Irena Chen
#
# 11/16/2017
# Make preliminary graphs of sicoin and fpm budget data 


# ----------------------------------------------
# Set up R

library(ggplot2)
library(dplyr)
library(data.table)



# ----------------------------------------------
## Uncomment if necessary


# sicoin_mapped$budget <- sicoin_mapped$budget*sicoin_mapped$coeff
# sicoin_mapped$disbursement <- sicoin_mapped$disbursement *sicoin_mapped$coeff
# sicoin_mapped$expenditure <- sicoin_mapped$expenditure *sicoin_mapped$coeff

pudr_mapped[,1] <- NULL
pudr_mapped$loc_id <- "gtm"
total_gtm_data[,1] <- NULL
# pudr_mapped[, end_date:=start_date + period-1]
# write.csv(sicoin_mapped, "sicoin_total.csv", fileEncoding="UTF-8")
# 
# 
gtm_total <- rbind(pudr_mapped, total_gtm_data, use.names=TRUE)

# 
# write.csv(gtm_total, "total_gtm_data.csv", fileEncoding="UTF-8")

# ----------------------------------------------
##plot national level data in aggregate 

byVars = names(gtm_total)[!names(gtm_total)%in%c('budget','disbursement','expenditure','cost_category', 'coeff', 'code', 'loc_id')]
nat_level = gtm_total[, list(budget=sum(budget), disbursement=sum(disbursement), expenditure=sum(expenditure)), by=byVars]


ggplot(nat_level, aes(x = budget/1000000, y= disbursement/1000000)) + 
  geom_point(aes(color=start_date, shape=disease)) +
  geom_abline(intercept=0, slope=1) + 
  geom_smooth(method='lm') + 
  facet_wrap(~source, scales='free') +
  ggtitle("GF Resources by Source") +
  #ylim(0, 9) + 
  labs(x = "budget $$ mil", y = "disb $$ in mil") 



# "melt" long
tmp = copy(nat_level)
tmp$start_date = NULL
setnames(tmp, 'end_date', 'start_date')
nat_level$end_date = NULL
nat_level= rbind(nat_level, tmp)


nat_level= melt(nat_level, id.vars=c('source', "period", "start_date", "data_source", "disease", "grant_number"))
nat_level$value[nat_level$value==0] <- NA

##only plot gf data (no ghe)
gf_nat<- subset(nat_level, source=="gf")

## subset by data source type 

ghe_nat <-subset(nat_level, source="ghe")


ghe_plot <- ggplot(ghe_nat, aes(x = start_date, y= value/1000000)) + 
  geom_line(aes(color=data_source, linetype=disease)) +
  facet_wrap(~variable) +
  ggtitle("GHE Resources by Data Source") +
  #ylim(0, 9) + 
  labs(x = "Start Date", y = "$$ in mil") +
  theme_bw()


gf_plot <- ggplot(gf_nat, aes(x = start_date, y= value/1000000)) + 
  geom_line(aes(color=data_source, linetype=disease)) +
  facet_wrap(~variable) +
  ggtitle("GF Resources by Data Source") +
  #ylim(0, 9) + 
  labs(x = "Start Date", y = "$$ in mil") +
  theme_bw()

ggsave("gf resources by data source.pdf", 
       plot = last_plot(), # or give ggplot object name as in myPlot,
        height = 6, width=9,
       units = "in", # other options c("in", "cm", "mm"), 
       dpi = 300)


# ----------------------------------------------

## budget on x axis, expenditure + disbursements on y axis 
gf_plot <- ggplot(gf_nat, aes(x = start_date, y= value/1000000)) + 
  geom_line(aes(color=data_source, linetype=disease)) +
  facet_wrap(~variable) +
  ggtitle("GF Resources by Data Source") +
  #ylim(0, 9) + 
  labs(x = "Start Date", y = "$$ in mil") 




# ----------------------------------------------

nat_plots <- list()
for (k in unique(nat_level$source)){
  subdata <- subset(nat_level, source== k)
  plot <-  ggplot(subdata, aes(x = start_date, y= value/1000000)) + 
    geom_line(aes(color=variable, linetype=data_source)) +
    facet_wrap(~disease) +
    ggtitle(paste(k, "data at national level")) +
  #ylim(0, 9) + 
    labs(x = "Start Date", y = "$$ in mil")
  nat_plots[[k]] <- plot
}

pdf("gtm_by_source1.pdf", height=6, width=9)
invisible(lapply(nat_plots, print))
dev.off()


# ----------------------------------------------
## plot sicoin vs fpm budget over time by disease 

byVars = names(nat_level)[!names(nat_level)%in%c('budget','disbursement','expenditure', 'source', 'period')]
sicoin_vs_fpm = nat_level[, list(budget=sum(budget), disbursement=sum(disbursement), expenditure=sum(expenditure)), by=byVars]

# "melt" long
tmp = copy(sicoin_vs_fpm)
tmp$start_date = NULL
setnames(tmp, 'end_date', 'start_date')
sicoin_vs_fpm$end_date = NULL
sicoin_vs_fpm = rbind(sicoin_vs_fpm, tmp)


sicoin_vs_fpm  = melt(sicoin_vs_fpm , id.vars=c("start_date", "data_source", "disease"))
sicoin_vs_fpm$value[sicoin_vs_fpm$value == 0] <- NA


ggplot(sicoin_vs_fpm, aes(x = start_date, y= value/1000000)) + 
  geom_line(aes(color=variable, linetype=data_source)) +
  facet_wrap(~disease) +
  ggtitle("FPM Budget vs. Sicoin Data") +
  #ylim(0, 9) + 
  labs(x = "Start Date", y = "$$ in mil")

# ----------------------------------------------
##plot sicoin vs fpm budget data only 
byVars = names(nat_level)[!names(nat_level)%in%c('budget','disbursement','expenditure', 'period')]
sicoin_fpm_plot = nat_level[, list(budget=sum(na.omit(budget)), disbursement=sum(na.omit(disbursement)), expenditure=sum(na.omit(expenditure))), by=c("disease", "data_source", "start_date")]


sicoin_fpm_plot  = melt(sicoin_fpm_plot, id.vars=c("data_source", "disease", "start_date"))
sicoin_fpm_plot$value[sicoin_fpm_plot$value == 0] <- NA



ggplot(sicoin_fpm_plot, aes(x = start_date, y=value/100000)) + 
  geom_line(aes(color=disease, linetype=data_source)) +
  facet_wrap(~variable) +
  geom_point() +
  ggtitle("FPM Budget vs. Sicoin Data") +
  #ylim(0, 9) + 
  labs(x = "Start Date", y = "$$ in mil")


pdf("all.pdf", height=6, width=9)
invisible(lapply(plot_list, print))
dev.off()


# ----------------------------------------------
##plot muni level sicoin data 

# collapse cost categories
byVars = names(gtm_total)[!names(gtm_total)%in%c('budget','disbursement','expenditure','cost_category', 'coeff', 'code')]
muni_level = gtm_total[, list(budget=sum(budget), disbursement=sum(disbursement), expenditure=sum(expenditure)), by=byVars]


# "melt" long
tmp = copy(muni_level)
tmp$start_date = NULL
setnames(tmp, 'end_date', 'start_date')
muni_level$end_date = NULL
muni_level = rbind(muni_level, tmp)

muni_melt = melt(muni_level, id.vars=c('loc_id', 'source', 'start_date', 'period', 'disease', 'data_source', 'grant_number'))

muni_melt <- muni_melt[-grep(paste(c("gtm", "GUAT"),  collapse="|"), muni_melt$loc_id),]

muni_melt <- muni_melt[, list(start_date, variable, value),by="loc_id"]
muni_melt$value[muni_melt$value == 0] <- NA
muni_melt$loc_id <- as.factor(muni_melt$loc_id)
muni_mapping <- cbind(unique(levels(muni_melt$loc_id)), as.numeric(1:length(unique(levels((muni_melt$loc_id))))))

colnames(muni_mapping) <- c("loc_id", "muni_code")

muni_merge <- merge(muni_melt, muni_mapping, by="loc_id")
muni_merge$muni_code <- as.numeric(muni_merge$muni_code)

### use a loop to create plots and store them into a pdf 
plot_list = list()
for (i in 1:8){
  subdata <- subset(muni_merge, muni_code%%8==i)
  plot <- ggplot(subdata, aes(x = start_date, y = value/100000)) + 
    geom_line(aes(color=variable)) +
    facet_wrap(~loc_id) +
    geom_point() +
    ggtitle("Municipality Level Resources (Malaria)") +
    labs(x = "Start Date", y = "$$ in 100ks")
  plot_list[[i]] <- plot
  
  }


pdf("gtm municpalities.pdf", height=6, width=9)
invisible(lapply(plot_list, print))
dev.off()

