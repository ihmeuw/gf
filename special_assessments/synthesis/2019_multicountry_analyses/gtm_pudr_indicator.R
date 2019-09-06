###############################
# Guatemala TB analyses
# PUDR Performance Indicators Framework
###############################

# ------------------------------------
# set up
library(data.table)
library(ggplot2)

# ------------------------------------
# read in all_prepped_data.rds
DT <- readRDS("J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/prepped_data/gtm_1A.rds")
DT2 <- readRDS("J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/prepped_data/gtm_1A_disagg.rds")

# read in TB codebook
tbcodes <- fread("./special_assessments/synthesis/2019_multicountry_analyses/tb_indicators_codebook.csv")

#--------------------------------------

#--------------------------------------
# essential data prep
#--------------------------------------

# remove rows that are not necessary
DT <- DT[indicator!="[Impact Indicator Name]"]
DT <- DT[indicator!="[Outcome Indicator Name]"]

# remove duplicated column in DT
DT <- DT[,unique(names(DT)),with=FALSE]

# aggregate other_dah to the modular level with proper code value and variable name
DT = DT[, c("indicator_code", "indicator_description", "indicator_misc") := tstrsplit(indicator, ": ", fixed=TRUE)]

# merge codebook to data
DT <- merge(DT, tbcodes, by.x = "indicator_code", by.y = "Indicator Code")

# generate ihme target and performance
DT$ihme_target_n <- NA
DT$ihme_target_n <- ifelse(is.na(DT$`target_%`), DT$target_n, DT$`target_%`)

DT$ihme_result_n <- NA
DT$ihme_result_n <- ifelse(is.na(DT$`lfa_result_%`), DT$lfa_result_n, DT$`lfa_result_%`)

# calculate ihme_results_achievement_ratio
DT$ihme_result_achievement_ratio <-NA
DT$ihme_target_n <- as.numeric(DT$ihme_target_n)
DT$ihme_result_n <- as.numeric(DT$ihme_result_n)
DT$ihme_result_achievement_ratio <- DT$ihme_result_n/DT$ihme_target_n

# Indicate if it's a reverse indicator
DT$reverse_indicator <- 0
DT$reverse_indicator[which(DT$indicator_code=="TB I-2")] <-1
DT$reverse_indicator[which(DT$indicator_code=="TB I-3(M)")] <-1
DT$reverse_indicator[which(DT$indicator_code=="TB/HIV I-1")] <-1
DT$reverse_indicator[which(DT$indicator_code=="TB I-4(M)")] <-1

DT$reverse_indicator <- factor(DT$reverse_indicator)
levels(DT$reverse_indicator) <- c("Higher ratio indicates improvement", "Lower ratio indicates improvement")

# subset data as appropriate:
DT1 <- DT[pudr_sheet %in% c('impact_outcome_indicators_main')]
DT1 <- DT1[file_name=="GTM-T-MSPAS_Progress Report_LFA18Mar19.xlsx"]
DT1 <- DT1[target_year==2018]

# plot of TB Data in GTM
a <- ggplot(DT1, aes(x=short_name_code, y=ihme_result_achievement_ratio, color=reverse_indicator)) +
  geom_point() +
  labs(title="TB Grant Performance in Guatemala", 
       y="Results Achievement Ratio",
       x="Indicator",
       caption = "Source: LFA Verified Progress report 18 March 2019") +
  geom_hline(yintercept = 1) +
  theme_bw()+
  coord_flip()

# save file
jpeg(filename = "J:/Project/Evaluation/GF/special_assessments/pudr_framework_indicators/gtm_tb.jpeg",
     width = 9, height = 2.5, res= 300, units = "in", pointsize = 12)
a
dev.off()