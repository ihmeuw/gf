
# ----------------------------------------------
# Irena Chen
#
# 11/27/2017
# Make graphs of program activities 

# ----------------------------------------------
# Set up R
library(ggplot2)
library(dplyr)
library(data.table)
library(lubridate)
library(grDevices)
library(RColorBrewer)

# ----------------------------------------------
sicoin <- data.table(read.csv("prepped_sicoin_data.csv", fileEncoding="UTF-8"))
sicoin$start_date <- as.Date(sicoin$start_date)

mapping_for_R <- read.csv("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/mapping_for_R.csv", fileEncoding="latin1")
mapping_for_graphs <- read.csv("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/mapping_for_graphs.csv")

gtm_total_data <- merge(sicoin, mapping_for_R,  by=c("disease", "cost_category"))

gtm_total_data$budget <- gtm_total_data$budget*gtm_total_data$coeff
gtm_total_data$disbursement <- gtm_total_data$disbursement*gtm_total_data$coeff
gtm_total_data$expenditure <- gtm_total_data$expenditure*gtm_total_data$coeff

gtm_total <- merge(gtm_total_data, mapping_for_graphs, by="code")

# ----------------------------------------------
byVars = names(total_data)[!names(total_data)%in%c('budget','disbursement','expenditure','cost_category', 'program_activity', 'loc_id', 'code', 'coeff')]
nat_level = total_data[, list(budget=sum(budget), disbursement=sum(disbursement), expenditure=sum(expenditure)), by=byVars]


nat_level = melt(nat_level, id.vars=c('source', 'disease', 'start_date', 'end_date', 'period', 'grant_number', 'data_source'))
nat_level$value[nat_level$value == 0] <- NA

fpm_sicoin <- subset(nat_level, data_source != "pudr")
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
hiv_sub <- subset(fpm_sicoin, disease=="hiv")
hiv_sub$year <- year(hiv_sub$start_date)

byVars = names(hiv_sub)[!names(hiv_sub)%in%c('data_source', 'variable', 'value', 'source', 'sicoin_ind', 'fpm_ind', 'period', 'disease', 'start_date', 'end_date')]
hiv_sub = hiv_sub[, list(sicoin_ind=sum(na.omit(sicoin_ind)), fpm_ind=sum(na.omit(fpm_ind))), by=byVars]

ggplot(hiv_sub, aes(x = sicoin_ind/1000000, y=fpm_ind/1000000)) + 
  geom_point(aes(color=year, shape=grant_number)) +
  geom_abline(intercept=0, slope=1) + 
  geom_smooth(method='lm') + 
  scale_colour_gradient(low = "#73D487", high = "#FF66CC",
                        space = "Lab", na.value = "grey50", guide = "colourbar") +
  ggtitle("SICOIN vs FPM HIV Budget Data") +
  #ylim(0, 9) + 
  labs(x = "SICOIN $$ in mil", y = "FPM $$ in mil") 


ggsave("fpm vs sicoin budget data.pdf", 
       plot = last_plot(), # or give ggplot object name as in myPlot,
       height = 6, width=9,
       units = "in", # other options c("in", "cm", "mm"), 
       dpi = 300)

# ----------------------------------------------
### GOS DATA graphs 

gos_data  <- data.table(read_excel("J:/Project/Evaluation/GF/resource_tracking/gtm/gf/Expenditures from GMS and GOS for PCE IHME countries.xlsx", sheet = "GMS SDAs - extract", col_types = c("text", "text", "date", "date", "date", "date", "numeric", "text", "text", "numeric", "numeric", "text")))

colnames(gos_data)[5] <- "start_date"
colnames(gos_data)[6] <- "end_date"
colnames(gos_data)[8] <- "gf_program"
colnames(gos_data)[9] <- "cost_category"
colnames(gos_data)[10] <- "budget"
colnames(gos_data)[11] <- "expenditure"
colnames(gos_data)[12] <- "gos_disease"

map_disease <- unique(gos_data$gos_disease)
names(map_disease) <- c("tb", "malaria", "hiv", "hss", "hiv/tb")

kDT = data.table(map_disease = names(map_disease), value = TRUE, gos_disease = unname(map_disease))
gos_data[kDT, on=.(gos_disease), disease := i.map_disease]
gost_data$gos_disease <- NULL


gtm_gos <- subset(gos_data, Country=="Guatemala")



### make program activity graphs

## first without grants, just by disease 

byVars = names(gtm_gos)[!names(gtm_gos)%in%c('budget','expenditure', 'Year', 'gf_program', 'Country', "Grant Number")]
program_level = gtm_gos[, list(budget=sum(budget), expenditure=sum(expenditure)), by=byVars]

program_level <- program_level[, -c(1:2)]

mapping_for_R <- read.csv("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/mapping_for_R.csv", fileEncoding="latin1")
mapping_for_graphs <- read.csv("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/mapping_for_graphs.csv")


program_level_mapped <- merge(program_level, mapping_for_R, by=c("disease","cost_category"))\
### check for dropped categories: 
# results1 = setdiff(program_level$cost_category, program_level_mapped$cost_category) 

program_level <- merge(program_level_mapped, mapping_for_graphs, by="code")

program_level$budget <- program_level$budget*program_level$coeff
program_level$expenditure <- program_level$expenditure*program_level$coeff

byVars = names(program_level)[!names(program_level)%in%c('budget','expenditure', 'coeff', 'code', 'cost_category')]
program_level = program_level[, list(budget=sum(budget), expenditure=sum(expenditure)), by=byVars]



## "melt" the start & end dates 
tmp = copy(program_level)
tmp$start_date = NULL
setnames(tmp, 'end_date', 'start_date')
program_level$end_date = NULL
program_level = rbind(program_level , tmp)


program_level= melt(program_level, id.vars=c("program_activity", "disease", "start_date"))
program_level$value[program_level$value==0] <- NA



prog_plots <- list()
set3 <- colorRampPalette(brewer.pal('Set3',n=12))


for (k in unique(program_level$disease)){
  subdata <- subset(program_level, disease==k)
  print(ggplot() + geom_col(aes(x = year(start_date), y= value/1000000, fill=program_activity), data=subdata) + 
             theme_bw(base_size=16) +
             facet_wrap(~variable, drop=T, scales='free') +
             labs(x = "Year", y = "$$ in mil", caption="Source: The Global Fund")+
            scale_color_manual(values = setNames(set3(13), levels(subdata$program_activity)))+
             ggtitle(paste(k, "data at the national level"))) 
}

pdf("gos_activity_bars.pdf", height=6, width=9)
invisible(lapply(prog_plots, print))
dev.off()



