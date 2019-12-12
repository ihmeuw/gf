# synthesis report visual for SO1

# read in data
library(data.table)
library(ggplot2)

data <-fread("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/data_for_analysis/cross_consortia_for_synthesis_pf_indicators_9Dec2019.csv")

# rename column names
setnames(data, 
         old = names(data),
         new = c('loc_name', 'grant', 'indicator_long', 'achievement_ratio', 'reporting_period'))

# parameters to edit
graphic_name = "cross_country_comparisons_20192020_synthesis_report.png"
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

# subset to only columns of interest

# set parameters of data to be analyzed
#date = c('s1_2019')

# subset as appropriate
DT <- data
#DT <- DT[reporting_period==date]
DT <- DT[,.(loc_name, grant, indicator_code, full_description, brief_description, achievement_ratio, reverse_indicator_final, module_code, reporting_period)]

# change name of full_description to indicator_long
setnames(DT, old=c('full_description'),
         new = "indicator_long")

# create new variable to indicate whether target is being met
DT$target_met <- NA
DT$target_met[which(DT$reverse_indicator_final=="no" & DT$achievement_ratio >= 1)] <- "yes"
DT$target_met[which(DT$reverse_indicator_final=="no" & DT$achievement_ratio < 1)] <- "no"
DT$target_met[which(DT$reverse_indicator_final=="yes" & DT$achievement_ratio <= 1)] <- "yes"
DT$target_met[which(DT$reverse_indicator_final=="yes" & DT$achievement_ratio > 1)] <- "no"


######################
# clean data
######################

# delete specific rows
DT$achievement_ratio[which(DT$grant=="COD-C-CORDAID" & DT$indicator_code=="TB O-6")] <- 2.229 # removing the cordaid value which is incorrect and duplicated in the COD-T-MOH grant PUDR --verified by reding comments in the PUDR

######## find duplicated values that are the same ############
DT <- unique.data.frame(DT) # there are three values that are duplicated across grant, PRs, and achievement ratio

### instances where different PRs report the same value--keep the first
DT1 <- unique(DT, by=c("loc_name", "indicator_code", "achievement_ratio")) # there are 47 rows which have duplicated values (same value reported by both PRs in the country)
# View(unique(DT, by=c("loc_name", "indicator_code"))) # there are 43 indicators which the two PRs reported different values

########################
# optional visualizations
########################


DT2 <- DT[module_code=="HIV KP"]

# visualize targets, results, goals
grants = unique(DT2$grant)
aplots = list()
i=1
for(g in grants) {
  aplots[[i]] = ggplot(DT2[grant==g], aes(x=indicator_long, y=achievement_ratio, fill=target_met)) +
    geom_bar(stat="identity") + 
    labs(fill= "Target Met", title=paste0("HIV KP Indicators in ", DT2[grant==g]$grant), y="Ratio", x="Indicator") +
    theme_bw()+
    coord_flip()+
    theme(legend.position = "bottom")+
    theme_bw(base_size = 10)+
    scale_fill_discrete(labels=c("No", "Yes"))+
    guides(fill=guide_legend(reverse=TRUE))+
    geom_hline(yintercept = 1)
    # a tick mark is shown on every 5
   # scale_y_continuous(breaks=seq(0,1,10))
  i=i+1
}

aplots[[1]]



pdf("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/visualizations/synthesis_hiv_kp_only.pdf", height = 10, width = 18)
aplots[[1]]
aplots[[2]]
aplots[[3]]
aplots[[4]]
aplots[[5]]
aplots[[6]]
aplots[[7]]
aplots[[8]]
aplots[[9]]
aplots[[10]]

dev.off()

#######################
#### save data-set used
######################
outFile <- "J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/prepped_data/synthesis_so3_data.csv"
write.csv(DT, outFile)

write.csv(DT, "C:/Users/frc2/Desktop/synthesis_so3_data_12Dec2019.csv")
write.csv(DT1, "C:/Users/frc2/Desktop/synthesis_so1_data.csv")

