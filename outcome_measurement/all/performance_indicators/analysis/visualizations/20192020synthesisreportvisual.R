# synthesis report visuals on performance achievement

# read in data
data <- 


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

#outputFile = paste0("J:\\Project\\Evaluation\\GF\\outcome_measurement\\multi_country\\performance_indicators\\pudr_indicator_extraction\\analysis\\visualizations\\",graphic_name)
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



