# synthesis report visual for SO1

# read in data
library(data.table)
library(ggplot2)

data <-fread("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/data_for_analysis/cross_consortia_for_synthesis_pf_indicators_9Dec2019.csv")

#library(googledrive)
#drive_find(n_max=30)

#ccdata <- drive_get(as_id("1yIO1dNH_OppKlVNB9ceNoJBklHCLwH7K"))

#drive_download("cross consortia financial analyses for synthesis.xlsx", type="csv")

# rename column names
setnames(data, 
         old = names(data),
         new = c('loc_name', 'grant', 'indicator_long', 'achievement_ratio', 'reporting_period'))

# parameters to edit
graphic_name = "20192020_synthesis_report.pdf"
codebook_name = "module_code_map.csv"

# split data indicator codes
data = data[, indicator_code:=tstrsplit(indicator_long, ":", keep=1)]

# merge with the correct module code and the correct indicator type
codebook <- fread(paste0("C:/Users/frc2/Documents/gf/outcome_measurement/all/performance_indicators/codebooks/", codebook_name))
data <- merge(data, codebook, by.x='indicator_code', by.y="indicator_code", all.x = TRUE, all.y=FALSE)

# merge on information on whether this is a reverse indicator
reverse_codebook <- fread("C:\\Users\\frc2\\Documents\\gf\\outcome_measurement\\all\\performance_indicators\\codebooks\\indicators_codebook_reverse.csv")
data <- merge(data, reverse_codebook, by.x='indicator_code', by.y='indicator_code', all.x = TRUE, all.y = FALSE)


# merge properly cleaned name
clean_codebook <- fread("C:\\Users\\frc2\\Documents\\gf\\outcome_measurement\\all\\performance_indicators\\codebooks\\indicators_codebook.csv")
data <- merge(data, clean_codebook, by.x='indicator_code', by.y='indicator_code', all.x = TRUE, all.y = FALSE)

# set parameters of data to be analyzed
date = c('s1_2019')

# subset as appropriate
DT <- data
#DT <- DT[reporting_period==date]
DT <- DT[,.(loc_name, grant, indicator_code, full_description, achievement_ratio, reverse_indicator_final, module_code, type_desc)]
# change name of full_description to indicator_long
setnames(DT, old=c('full_description'),
         new = "indicator_long")

############################
#### Data cleaning
#############################

# delete specific rows
DT$achievement_ratio[which(DT$grant=="COD-C-CORDAID" & DT$indicator_code=="TB O-6")] <- 2.229 # removing the cordaid value which is incorrect and duplicated in the COD-T-MOH grant PUDR --verified by reding comments in the PUDR

######## find duplicated values that are the same ############
DT <- unique.data.frame(DT) # there are three values that are duplicated across grant, PRs, and achievement ratio

### instances where different PRs report the same value--keep the first
DT <- unique(DT, by=c("loc_name", "indicator_code", "achievement_ratio")) # there are 47 rows which have duplicated values (same value reported by both PRs in the country)
# View(unique(DT, by=c("loc_name", "indicator_code"))) # there are 43 indicators which the two PRs reported different values

####### fixing errors in the data--would be better to create a script for this in the data prep stage


#####################################################################
# collapse countries/grant where module_code/intervention_code is the same
########################################################################
# re-code variables that are reverse indicators
DT$achievement_ratio_final <- ifelse(DT$reverse_indicator_final=="yes", 1/DT$achievement_ratio, DT$achievement_ratio)

dt_subset <- DT[, .(module_code, indicator_code, achievement_ratio_final, type_desc)]
dt_subset <- dt_subset[!is.na(achievement_ratio_final)]
dt_subset <- dt_subset[, .(avg_ach_ratio= mean(achievement_ratio_final),
                           max_ach_ratio= max(achievement_ratio_final),
                           min_ach_ratio= min(achievement_ratio_final)),
                       by=c("module_code", "indicator_code", "type_desc")]
dt_subset[max_ach_ratio>2, max_ach_ratio := 2]
dt_subset[avg_ach_ratio>2, avg_ach_ratio := 2]
dt_subset[min_ach_ratio>2, min_ach_ratio := 2]

dt_subset[, kpi_code:= paste(module_code, indicator_code)]

no_country_per_indicator <- unique(DT[, .(loc_name, module_code, indicator_code)])
no_country_per_indicator <- no_country_per_indicator[, .(no_observ = .N), by=c("module_code", "indicator_code")]

dt_subset <- merge(dt_subset, no_country_per_indicator, by=c("module_code", "indicator_code"))

##############################################################
##### prep visualizations
##############################################################

# original
p1<- ggplot(dt_subset[], aes(x=module_code, y=avg_ach_ratio, color=module_code, size = no_observ)) + theme_bw()+
  geom_pointrange( aes(ymin= min_ach_ratio, ymax= max_ach_ratio), position= position_jitter(width = 0.40), shape = 21, fill = "white") + guides(color=FALSE) +
  scale_size_continuous(breaks= c(1, 2, 3, 4, 5, 6, 7, 8), range=c(0.5,1.5), name = "Number of \ncountries reporting") + 
  ggtitle(paste0("Average achievement ratios of performance indicators by module")) +
  ylab("Achievement ratio") + xlab("Module")  +
  labs(caption = "Bars represent the max and min and points are the mean by module-intervention across countries and grants.
       Note: Where the max, mean, or min for a given module-intervention was higher than 2, it was changed to 2 for clarity in this figure.") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=16), legend.title=element_text(size = 14), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=12),
        axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  geom_hline(yintercept=1, linetype="dashed", color="grey", alpha=0.6, size=2)

### this is just for coverage indicators, not including KP and RSSH
p2 <- ggplot(dt_subset[type_desc=="Coverage" & module_code!="HIV KP" & module_code!="RSSH HMIS, M&E" & module_code!="SSRP" & module_code!="PSM"], aes(x=module_code, y=avg_ach_ratio, color=module_code, size = no_observ)) + theme_bw()+
  geom_pointrange( aes(ymin= min_ach_ratio, ymax= max_ach_ratio), position= position_jitter(width = 0.40), shape = 21, fill = "white") + guides(color=FALSE) +
  scale_size_continuous(breaks= c(1, 2, 3, 4, 5, 6, 7, 8), range=c(0.5,1.5), name = "Number of \ncountries reporting") + 
  ggtitle(paste0("Average achievement ratios of performance indicators by module")) +
  ylab("Achievement ratio") + xlab("Module")  +
  labs(caption = "Bars represent the max and min and points are the mean by module-intervention across countries and grants.
       Note: Where the max, mean, or min for a given module-intervention was higher than 2, it was changed to 2 for clarity in this figure.") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=16), legend.title=element_text(size = 14), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=12),
        axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  geom_hline(yintercept=1, linetype="dashed", color="grey", alpha=0.6, size=2)

### this is just for impact indicators
p3 <- ggplot(dt_subset[type_desc=="Impact"], aes(x=module_code, y=avg_ach_ratio, color=module_code, size = no_observ)) + theme_bw()+
  geom_pointrange( aes(ymin= min_ach_ratio, ymax= max_ach_ratio), position= position_jitter(width = 0.40), shape = 21, fill = "white") + guides(color=FALSE) +
  scale_size_continuous(breaks= c(1, 2, 3, 4, 5, 6, 7, 8), range=c(0.5,1.5), name = "Number of \ncountries reporting") + 
  ggtitle(paste0("Average achievement ratios of performance indicators by module")) +
  ylab("Achievement ratio") + xlab("Module")  +
  labs(caption = "Bars represent the max and min and points are the mean by module-intervention across countries and grants.
       Note: Where the max, mean, or min for a given module-intervention was higher than 2, it was changed to 2 for clarity in this figure.") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=16), legend.title=element_text(size = 14), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=12),
        axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  geom_hline(yintercept=1, linetype="dashed", color="grey", alpha=0.6, size=2)

### this is  for coverage, outcome indicators and not including KPs and RSSH
p4 <- ggplot(dt_subset[type_desc%in%c("Coverage","Outcome") & module_code!="HIV KP" & module_code!="RSSH HMIS, M&E" & module_code!="SSRP" & module_code!="PSM"], aes(x=module_code, y=avg_ach_ratio, color=module_code, size = no_observ)) + theme_bw()+
  geom_pointrange( aes(ymin= min_ach_ratio, ymax= max_ach_ratio), position= position_jitter(width = 0.40), shape = 21, fill = "white") + guides(color=FALSE) +
  scale_size_continuous(breaks= c(1, 2, 3, 4, 5, 6, 7, 8), range=c(0.5,1.5), name = "Number of \ncountries reporting") + 
  ggtitle(paste0("Average achievement ratios of performance indicators by module")) +
  ylab("Achievement ratio") + xlab("Module")  +
  labs(caption = "Bars represent the max and min and points are the mean by module-intervention across countries and grants.
       Note: Where the max, mean, or min for a given module-intervention was higher than 2, it was changed to 2 for clarity in this figure.") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=16), legend.title=element_text(size = 14), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=12),
        axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  geom_hline(yintercept=1, linetype="dashed", color="grey", alpha=0.6, size=2)

### this is  for coverage indicators and not including RSSH
p5 <- ggplot(dt_subset[type_desc%in%c("Coverage") & module_code!="RSSH HMIS, M&E" & module_code!="SSRP" & module_code!="PSM"], aes(x=module_code, y=avg_ach_ratio, color=module_code, size = no_observ)) + theme_bw()+
  geom_pointrange( aes(ymin= min_ach_ratio, ymax= max_ach_ratio), position= position_jitter(width = 0.40), shape = 21, fill = "white") + guides(color=FALSE) +
  scale_size_continuous(breaks= c(1, 2, 3, 4, 5, 6, 7, 8), range=c(0.5,1.5), name = "Number of \ncountries reporting") + 
  ggtitle(paste0("Average achievement ratios of performance indicators by module")) +
  ylab("Achievement ratio") + xlab("Module")  +
  labs(caption = "Bars represent the max and min and points are the mean by module-intervention across countries and grants.
       Note: Where the max, mean, or min for a given module-intervention was higher than 2, it was changed to 2 for clarity in this figure.") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=16), legend.title=element_text(size = 14), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=12),
        axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  geom_hline(yintercept=1, linetype="dashed", color="grey", alpha=0.6, size=2)

### this is  for coverage and outcome indicators and not including RSSH
p6 <- ggplot(dt_subset[type_desc%in%c("Coverage", "Outcome") & module_code!="RSSH HMIS, M&E" & module_code!="SSRP" & module_code!="PSM"], aes(x=module_code, y=avg_ach_ratio, color=module_code, size = no_observ)) + theme_bw()+
  geom_pointrange( aes(ymin= min_ach_ratio, ymax= max_ach_ratio), position= position_jitter(width = 0.40), shape = 21, fill = "white") + guides(color=FALSE) +
  scale_size_continuous(breaks= c(1, 2, 3, 4, 5, 6, 7, 8), range=c(0.5,1.5), name = "Number of \ncountries reporting") + 
  ggtitle(paste0("Average achievement ratios of performance indicators by module")) +
  ylab("Achievement ratio") + xlab("Module")  +
  labs(caption = "Bars represent the max and min and points are the mean by module-intervention across countries and grants.
       Note: Where the max, mean, or min for a given module-intervention was higher than 2, it was changed to 2 for clarity in this figure.") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=16), legend.title=element_text(size = 14), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=12),
        axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  geom_hline(yintercept=1, linetype="dashed", color="grey", alpha=0.6, size=2)

### this is for just outcome indicators
p7 <- ggplot(dt_subset[type_desc%in%c("Outcome") & module_code!="RSSH HMIS, M&E" & module_code!="SSRP" & module_code!="PSM" & module_code!="RSSH Outcome"], aes(x=module_code, y=avg_ach_ratio, color=module_code, size = no_observ)) + theme_bw()+
  geom_pointrange( aes(ymin= min_ach_ratio, ymax= max_ach_ratio), position= position_jitter(width = 0.40), shape = 21, fill = "white") + guides(color=FALSE) +
  scale_size_continuous(breaks= c(1, 2, 3, 4, 5, 6, 7, 8), range=c(0.5,1.5), name = "Number of \ncountries reporting") + 
  ggtitle(paste0("Average achievement ratios of performance indicators by module")) +
  ylab("Achievement ratio") + xlab("Module")  +
  labs(caption = "Bars represent the max and min and points are the mean by module-intervention across countries and grants.
       Note: Where the max, mean, or min for a given module-intervention was higher than 2, it was changed to 2 for clarity in this figure.") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=16), legend.title=element_text(size = 14), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=12),
        axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  geom_hline(yintercept=1, linetype="dashed", color="grey", alpha=0.6, size=2)

p8 <- ggplot(dt_subset[module_code%in%c("HIV Impact", "Malaria Impact", "TB Impact")], aes(x=module_code, y=avg_ach_ratio, color=module_code, size = no_observ)) + theme_bw()+
  geom_pointrange( aes(ymin= min_ach_ratio, ymax= max_ach_ratio), position= position_jitter(width = 0.40), shape = 21, fill = "white") + guides(color=FALSE) +
  scale_size_continuous(breaks= c(1, 2, 3, 4, 5, 6, 7, 8), range=c(0.5,1.5), name = "Number of \ncountries reporting") + 
  ggtitle(paste0("Average achievement ratios of performance indicators by module")) +
  ylab("Achievement ratio") + xlab("Module")  +
  labs(caption = "Bars represent the max and min and points are the mean by module-intervention across countries and grants.
       Note: Where the max, mean, or min for a given module-intervention was higher than 2, it was changed to 2 for clarity in this figure.") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=16), legend.title=element_text(size = 14), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=12),
        axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  geom_hline(yintercept=1, linetype="dashed", color="grey", alpha=0.6, size=2)

#outputFile = paste0("J:\\Project\\Evaluation\\GF\\outcome_measurement\\multi_country\\performance_indicators\\pudr_indicator_extraction\\visualizations\\",graphic_name)
outputFile1 = paste0("C:/Users/frc2/Desktop/",graphic_name)

#ggsave(outputFile1, height = 8, width = 11)

pdf(outputFile1, height = 8, width = 13)
p1
p2
p3
p4
p5
p6
p7
p8
dev.off()



