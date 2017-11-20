# ----------------------------------------------
# Irena Chen
#
# 11/16/2017
# Make preliminary graphs of sicoin and fpm budget data 


# ----------------------------------------------
# Set up R


sicoin_mapped$budget <- sicoin_mapped$budget*sicoin_mapped$coeff
sicoin_mapped$disbursement <- sicoin_mapped$disbursement *sicoin_mapped$coeff
sicoin_mapped$expenditure <- sicoin_mapped$expenditure *sicoin_mapped$coeff


gtm_total <- rbind(sicoin_mapped, fpm_mapped)

write.csv(gtm_total, "total_gtm_data.csv", fileEncoding="UTF-8")
write.csv(...,file=con,...)

## subset by sicoin vs fpm budget 

byVars = names(gtm_total)[!names(gtm_total)%in%c('budget','disbursement','expenditure','cost_category', 'coeff', 'code', 'loc_id')]
nat_level = gtm_total[, list(budget=sum(budget), disbursement=sum(disbursement), expenditure=sum(expenditure)), by=byVars]

# ----------------------------------------------
##plot national level data in aggregate 


# "melt" long
tmp = copy(nat_level)
tmp$start_date = NULL
setnames(tmp, 'end_date', 'start_date')
nat_level$end_date = NULL
nat_level= rbind(nat_level tmp)


gtm_nat = melt(nat_level, id.vars=c('source', "period", "start_date", "data_source", "disease"))

gtm_nat$value[gtm_nat$value==0] <- NA

nat_plots <- list()
for (k in unique(gtm_nat$source)){
  subdata <- subset(gtm_nat, source== k)
  plot <-  ggplot(subdata, aes(x = start_date, y= value/1000000)) + 
    geom_line(aes(color=variable)) +
    facet_wrap(~disease) +
    ggtitle(paste(k, "data at national level")) +
  #ylim(0, 9) + 
    labs(x = "Start Date", y = "$$ in mil")

nat_plots[[k]] <- plot
}
pdf("gtm_nat_plots.pdf")
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


# ----------------------------------------------
##plot muni level sicoin data 

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

plot_list = list()
for (i in 1:6){
  subdata <- subset(muni_merge, muni_code%%6==i)
  plot <- ggplot(subdata, aes(x = start_date, y = value/100000)) + 
    geom_line(aes(color=variable)) +
    facet_wrap(~loc_id) +
    geom_point() +
    ggtitle("Resource by Municipality") +
    labs(x = "Start Date", y = "$$ in 100ks")
  plot_list[[i]] <- plot
  
  }


pdf("all.pdf")
invisible(lapply(plot_list, print))
dev.off()

