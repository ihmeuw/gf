# target-setting over time in DRC


library(data.table)
library(ggplot2)
library(knitr)
library(dplyr)
library(kableExtra)
library(scales)

# set parameters of data to be analyzed
country = "gtm"
diseases = c("hiv", "tb")
keep = "test"

# load data
data <- readRDS("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/cleaned_data/kpi_data_for_analyses2.RDS")

# subset data
DT <- data
DT <- DT[loc_name==country & disease %in% diseases]

keypopshiv = unique(DT[disease=="hiv"]$`sub-category`)

colnames <- c("brief_description_code")

# subset only to those indicators that relate to the word testing
DT <- DT[grepl("test", DT$brief_description_code)]

# subset to specific columns
DT2 <- DT[,.(indicator_code,full_description, loc_name, grant, disease, start_date_programmatic, end_date_programmatic,
      baseline_value, baseline_source, baseline_year, brief_description_code,
      target_n, target_d, target_pct, target_value, 
      pr_result_n, pr_result_d, pr_result_value,
      lfa_result_n, lfa_result_d, lfa_result_value, 
      gf_result_n, gf_result_d, gf_result_pct, gf_result_value,
      pr_result_achievement_ratio, lfa_result_achievement_ratio, gf_result_achievement_ratio, 
      ihme_result_achievement_ratio, target_met, any_result_value,
      file_name)]

# add date label
DT2$date <- NA
DT2$date[which(DT2$end_date_programmatic=="2019-06-30")] <- "2019 S1"
DT2$date[which(DT2$end_date_programmatic=="2018-12-31")] <- "2018 S2"
DT2$date[which(DT2$end_date_programmatic=="2018-06-30")] <- "2018 S1"
DT2$date[which(DT2$end_date_programmatic=="2017-12-31")] <- "2017 S2"
DT2$date[which(DT2$end_date_programmatic=="2017-06-30")] <- "2017 S2"

# re-shape data to keep 
DT3 <- melt(DT2, id.vars = c("indicator_code", "full_description", "loc_name", "grant", "disease", "start_date_programmatic", "end_date_programmatic", "file_name", "brief_description_code", "date"))
keep <- c("target_value", "any_result_value")
DT3 <- DT3[variable %in% keep]
DT3$value <- as.numeric(DT3$value)

# visualize targets, results, goals
pudrs = unique(DT2$end_date_programmatic)
aplots = list()
i=1
for(p in pudrs) {
   aplots[[i]] = ggplot(DT3[end_date_programmatic==p], aes(x=brief_description_code, y=value, fill=variable)) +
      geom_bar(stat="identity",position = "dodge", alpha=.3) +
      labs(fill= "Values", title=paste0("HIV Testing indicators ", DT3[end_date_programmatic==p]$date), y="Value (%)", x="Indicator", caption=paste0("Source: ", unique(DT[end_date_programmatic==p]$file_name))) +
      theme_bw()+
      coord_flip()+
      theme(legend.position = "bottom")+
      theme_bw(base_size = 18)+
      scale_fill_discrete(labels=c("Target", "Result"))+
      guides(fill=guide_legend(reverse=TRUE))+
      # a tick mark is shown on every 5
      scale_y_continuous(breaks=seq(0,100,10))
   i=i+1
}

aplots[[1]]



# create sample visualizations
pudrs = unique(DT2$end_date_programmatic)
plots = list()
i=1
for(p in pudrs) {
   plots[[i]] = ggplot(DT2[end_date_programmatic==p], aes(x=brief_description_code, y=ihme_result_achievement_ratio, col=target_met)) +
      geom_point() +
      labs(col= "Target Met", title=paste0("HIV Testing Achievement ", DT2[end_date_programmatic==p]$date), y="Achievement Ratio", x="Indicator", caption=paste0("Source: ", unique(DT[end_date_programmatic==p]$file_name))) +
      geom_hline(yintercept = 1) +
      theme_bw()+
      coord_flip()+
      ylim(0,2.5)+
      theme(legend.position = "bottom")+
      theme_bw(base_size = 18)
   i=i+1
}

pdf("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/visualizations/gtm_hiv_testing.pdf", height = 10, width = 15)
aplots[[1]]
aplots[[2]]
aplots[[3]]
aplots[[4]]
aplots[[5]]
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
dev.off()

# save data-set used
outFile <- "J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/prepped_data/gtm_hiv_testing.csv"
write.csv(DT2, outFile)

