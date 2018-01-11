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

## prep some functions for ggplot:
# ----------------------------------------------
as.Date_origin <- function(x){
  as.Date(x, origin = '1970-01-01')
}

# ----------------------------------------------
## Uncomment if necessary
# sicoin_data$start_date <- as.Date(sicoin_data$start_date,"%Y-%m-%d")

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
# 
# gtm_total$data_source <- as.character(gtm_total$data_source)
# gtm_total$data_source[gtm_total$data_source=="pudr_budget"] <- "pudr"
# gtm_total$data_source <- as.factor(gtm_total$data_source)

# ----------------------------------------------
##plot national level data in aggregate 

byVars = names(gtm_total)[!names(gtm_total)%in%c('budget','disbursement','expenditure','cost_category', 'coeff', 'code', 'loc_id')]
nat_level = gtm_total[, list(budget=sum(budget), disbursement=sum(disbursement), expenditure=sum(expenditure)), by=byVars]


# "melt" long
tmp = copy(nat_level)
tmp$start_date = NULL
setnames(tmp, 'end_date', 'start_date')
nat_level$end_date = NULL
nat_level= rbind(nat_level, tmp)


nat_level= melt(nat_level, id.vars=c("program_activity", "source", "period", "start_date", "data_source", "disease", "grant_number"))
nat_level$value[nat_level$value==0] <- NA

## subset by data source type 
gf_nat<- subset(nat_level, source=="gf")
ghe_nat <-subset(nat_level, source="ghe")

##plot by ghe vs gf 

# ----------------------------------------------

nat_plots <- list()
for (k in unique(nat_level$source)){
  subdata <- subset(nat_level, source== k)
  plot <-  ggplot(subdata, aes(x = start_date, y= value/1000000)) + 
    geom_line(aes(color=variable, linetype=data_source)) +
    facet_wrap(~disease,scales='free') +
    ggtitle(paste(k, "data at national level")) +
    #ylim(0, 9) + 
    labs(x = "Start Date", y = "$$ in mil") +
    theme_bw()
  
  nat_plots[[k]] <- plot
}

pdf("gtm_by_source_and_disease.pdf", height=6, width=9)
invisible(lapply(nat_plots, print))
dev.off()

##get total (gf + ghe data) at the national level by disease and data source 
# ----------------------------------------------

byVars = names(nat_level)[!names(nat_level)%in%c('value','period', 'grant_number', 'source')]
aggregated_nat = nat_level[,list(value=sum(value)), by=byVars]

total_plot <- ggplot(aggregated_nat, aes(x = start_date, y= value/1000000)) + 
  geom_line(aes(color=variable, linetype=data_source)) +
  facet_wrap(~disease, scales="free") +
  ggtitle("Total Resources by Data Source (GF and GHE)") +
  #ylim(0, 9) + 
  labs(x = "Start Date", y = "$$ in mil") +
  theme_bw()

nat_plots[[3]] <- total_plot
  
pdf("gtm_by_source_and_disease.pdf", height=6, width=9)
invisible(lapply(nat_plots, print))
dev.off()



gf_nat<- subset(nat_level, source=="gf")
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
  ggtitle(k + "Resources by Data Source") +
  #ylim(0, 9) + 
  labs(x = "Start Date", y = "$$ in mil") +
  theme_bw()

ggsave("gf resources by data source.pdf", 
       plot = last_plot(), # or give ggplot object name as in myPlot,
        height = 6, width=9,
       units = "in", # other options c("in", "cm", "mm"), 
       dpi = 300)





# ----------------------------------------------
##budget vs disbursements

colors = c('#CAF270', '#FF66CC')
mapColors = colorRampPalette(colors)
mapColors = mapColors(10)

budg_disburs<- nat_level


budg_disburs$disbursement[budg_disburs$disbursement==0] <- NA



ggplot(budg_disburs, aes(x = budget/1000000, y= disbursement/1000000)) + 
  geom_point(aes(color=start_date, shape=disease)) +
  geom_abline(intercept=0, slope=1) + 
  geom_smooth(method='lm') + 
  facet_wrap(~source, scales='free') +
  scale_colour_gradient(low = "#73D487", high = "#FF66CC",
                        space = "Lab", na.value = "grey50", guide = "colourbar", labels=as.Date_origin) +
  ggtitle("GTM Resources by Source") +
  #ylim(0, 9) + 
  labs(x = "budget $$ mil", y = "disbursement $$ in mil") 

ggsave("gtm_budget_vs_disbursement.pdf", 
       plot = last_plot(), # or give ggplot object name as in myPlot,
       height = 6, width=9,
       units = "in", # other options c("in", "cm", "mm"), 
       dpi = 300)


# ----------------------------------------------

## FPM x axis, SICOIN on y axis 


gf_plot <- ggplot(gf_nat, aes(x = start_date, y= value/1000000)) + 
  geom_line(aes(color=data_source, linetype=disease)) +
  facet_wrap(~variable) +
  ggtitle("GF Resources by Data Source") +
  #ylim(0, 9) + 
  labs(x = "Start Date", y = "$$ in mil") 



# ----------------------------------------------
##plot sicoin vs fpm budget data only 


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

muni_melt <- muni_melt[, list(start_date, disease, variable, value),by="loc_id"]
muni_melt$value[muni_melt$value == 0] <- NA
muni_melt$loc_id <- as.factor(muni_melt$loc_id)
muni_mapping <- cbind(unique(levels(muni_melt$loc_id)), as.numeric(1:length(unique(levels((muni_melt$loc_id))))))

colnames(muni_mapping) <- c("loc_id", "muni_code")

muni_merge <- merge(muni_melt, muni_mapping, by="loc_id")
muni_merge$muni_code <- as.numeric(muni_merge$muni_code)


muni_merge$loc_id<- chartr("???", " ", muni_merge$loc_id)

library(gsubfn)
muni_merge$loc_id <- gsubfn("Ã'A","ÑA", muni_merge$loc_id)

### use a loop to create plots and store them into a pdf 
plot_list = list()
for (i in 1:48){
  subdata <- subset(muni_merge, muni_code%%48==i)
  plot <- ggplot(subdata, aes(x = start_date, y = value)) + 
    geom_line(aes(color=variable, linetype=disease)) +
    facet_wrap(~loc_id, scales='free') +
    theme(text = element_text(size=5),axis.text.x = element_text(angle=90, hjust=1)) +
    geom_point() +
    ggtitle("Municipality Level Resources by Disease") +
    labs(x = "Start Date", y = "Resource $$", caption="data source:SICOIN")
  plot_list[[i]] <- plot
  
  }


pdf("gtm municipalities by disease.pdf", height=6, width=9)
invisible(lapply(plot_list, print))
dev.off()





### working on this code, it's broken for now:  
##-------------------------------------------
## plot sicoin vs fpm budget over time by disease 


fpm_sicoin <- subset(nat_level, data_source !="pudr")

byVars = names(fpm_sicoin)[!names(fpm_sicoin)%in%c('budget','disbursement','expenditure', 'source')]
fpm_sicoin = fpm_sicoin[, list(budget=sum(budget), disbursement=sum(disbursement), expenditure=sum(expenditure)), by=byVars]

fpm_sicoin  = melt(fpm_sicoin , id.vars=c("start_date", "data_source", "disease", "source", "period", "grant_number"))
fpm_sicoin$value[fpm_sicoin$value == 0] <- NA

fpm_sicoin <- subset(fpm_sicoin, variable=="budget")
fpm_sicoin$sicoin_ind <- 0
fpm_sicoin$fpm_ind <- 0
for(i in 1: length(fpm_sicoin$sicoin_ind)){
  if (fpm_sicoin$data_source[i]=="SICOIN"){
    fpm_sicoin$sicoin_ind[i] <- fpm_sicoin$value[i]
    fpm_sicoin$fpm_ind[i] <- 0
  } else if (fpm_sicoin$data_source[i]!="SICOIN"){
    fpm_sicoin$sicoin_ind[i] = fpm_sicoin$sicoin_ind[i]
    fpm_sicoin$fpm_ind[i] <- fpm_sicoin$value[i]
  }
}


pdf("fpm_vs_sicoin", height=6, width=9)
invisible(lapply(fpm_sicoin_plots, print))
dev.off()

ggsave("fpm vs sicoin budget data.pdf", 
       plot = last_plot(), # or give ggplot object name as in myPlot,
       height = 6, width=9,
       units = "in", # other options c("in", "cm", "mm"), 
       dpi = 300)







