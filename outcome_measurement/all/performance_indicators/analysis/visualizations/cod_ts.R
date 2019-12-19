# target-setting over time in DRC


library(data.table)
library(ggplot2)
library(knitr)
library(dplyr)
library(kableExtra)
library(scales)

# set parameters of data to be analyzed
country = "cod"
main_indicators = c('impact_outcome_indicators_main', 'coverage_indicators_main')
main_indicators = c('impact_outcome_indicators_main', 'coverage_indicators_main')
recent_pudrs = c('Malaria_MOH_PUDR_S1 2019_LFA verified.xlsx',
                 'Malaria_SANRU_PUDR S1 2019_LFA verified.xlsx',
                 'CORDAID_PUDR_S1 2019_not verified.xlsx',
                 'HIV_MOH_PUDR_S1 2019_not verified.xlsx',
                 'Copy of LFA_COD-T-MOH_Progress Report_30Jun2019_CCF_Final_10092019.xlsx')
LFA_verified_grants = c('Malaria_MOH_PUDR_S1 2019_LFA verified.xlsx',
                        'Malaria_SANRU_PUDR S1 2019_LFA verified.xlsx',
                        'LFA_Review_COD-C-CORDAID_PU 30 June 2018_Sent_27092018_OK.xlsx',
                        'LFA Review_COD-H-MOH_Progress  Report_30Jun2018_07092018 ok_Sent 01102018.OK.xlsx',
                        'Copy of LFA_COD-T-MOH_Progress Report_30Jun2019_CCF_Final_10092019.xlsx') # file names of most-recent LFA-verified grants

# load data
data <- readRDS("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/prepped_data/cleaned_pfi.RDS")


# subset as appropriate
DT <- data
DT = DT[loc_name==country]
DT = DT[pudr_sheet %in% main_indicators]
DT = DT[file_name %in% recent_pudrs]


# subset parameters
trg_variables_to_plot = c("target_value")

# subset to most recent pudrs and main_indicators
data3 <- data[loc_name==country & pudr_sheet%in%main_indicators]

# re-shape the data
data3 <- melt(data3, id.vars = c("indicator_code", "grant", "brief_description_code", "end_date_programmatic", "file_name", "type", "pudr_sheet"))
data3 <- data3[variable %in% trg_variables_to_plot]

# change format
data3$value <- as.numeric(data3$value)
data3 <- data3[!is.na(data3$value),]

# subset data to only those indicators with three time points per grant
keeps <- data3[,.N,by=indicator_code] #keep indicators with 3 time points
trg_indicators <- keeps[N==3,indicator_code] # create vector of indicators with 3 time point stop
data3 <- data3[indicator_code %in% trg_indicators]

grants = unique(data3$grant)

cplots = list()
i=1
for(g in grants) {
  cplots[[i]] = ggplot(data3[grant==g], aes(x=end_date_programmatic, y=value, group=brief_description_code, col=brief_description_code))+
    geom_point()+
    geom_line()+
    theme_bw()+
    facet_wrap(~variable)+
    scale_x_date(date_breaks = "6 month", 
                 labels=date_format("%b-%Y"))+
    labs(x="Date of PUDR", y="Target Value", title=paste0(g, "\n", "Target-setting Over Time"), caption = paste0("Data source: ", unique(data3$file_name)))+
    scale_colour_discrete(name="")+
    theme(legend.position="bottom")+
    guides(col=guide_legend(ncol=1))
  
  i=i+1
}

# Save output
pdf(paste0('J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/visualizations/cod_ts.pdf'), height=6, width=9)
cplots[[1]]
cplots[[2]]
cplots[[3]]
cplots[[4]]
cplots[[5]]
dev.off()